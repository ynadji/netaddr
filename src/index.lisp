(in-package :netaddr)

;;;; A lazily maintained index over the members of an IP-SET.
;;;;
;;;; For each IP version the members are held in parallel vectors sorted by
;;;; first address ascending, then last address descending, so that a member
;;;; precedes anything it contains (the same order as COMPARE). PARENTS holds,
;;;; for each entry, the index of the smallest earlier entry enclosing it (or
;;;; -1), which lets a lookup walk from the rightmost candidate to the most
;;;; specific enclosing member in O(log n + depth). MAXLASTS holds the running
;;;; maximum of LASTS, which bounds the scan when enumerating every member
;;;; overlapping a query.
;;;;
;;;; The index is never mutated, so copies of a set may share it. Members
;;;; prepended to a set are searched linearly until enough accumulate, at which
;;;; point they are merged into fresh vectors in O(n). Removals and
;;;; replacements are likewise applied by filtering and merging rather than by
;;;; re-sorting.

(defstruct (index (:constructor %make-index))
  (firsts #() :type simple-vector)
  (lasts #() :type simple-vector)
  (members #() :type simple-vector)
  (parents #() :type simple-vector)
  (maxlasts #() :type simple-vector))

(defun bounds (ip-like)
  "Returns the first and last address of IP-LIKE as integers."
  (etypecase ip-like
    (ip-address (let ((int (int ip-like))) (values int int)))
    (ip-pair (values (int (first-ip ip-like)) (int (last-ip ip-like))))))

(declaim (inline entry<))
(defun entry< (f1 l1 m1 f2 l2 m2)
  "The ordering of COMPARE for members of one version, on precomputed bounds:
first ascending, last descending, and IP-PAIRs before IP-ADDRESSes."
  (or (< f1 f2)
      (and (= f1 f2)
           (or (> l1 l2)
               (and (= l1 l2)
                    (typep m1 'ip-pair)
                    (not (typep m2 'ip-pair)))))))

(defun sorted-entries (members)
  "Returns MEMBERS as a vector of (first last member) entries in ENTRY< order."
  (let ((entries (map 'vector (lambda (m) (multiple-value-bind (f l) (bounds m) (list f l m))) members)))
    (sort entries (lambda (a b) (entry< (first a) (second a) (third a) (first b) (second b) (third b))))))

(defun index-from-vectors (firsts lasts members)
  "Builds an INDEX from already sorted parallel vectors."
  (let* ((n (length members))
         (parents (make-array n))
         (maxlasts (make-array n))
         (stack '())
         (maxlast -1))
    (dotimes (i n)
      (let ((l (svref lasts i)))
        (loop while (and stack (< (svref lasts (car stack)) l)) do (pop stack))
        (setf (svref parents i) (if stack (car stack) -1))
        (push i stack)
        (setf maxlast (max maxlast l)
              (svref maxlasts i) maxlast)))
    (%make-index :firsts firsts :lasts lasts :members members :parents parents :maxlasts maxlasts)))

(defun build-index (members)
  "Builds an INDEX over the list MEMBERS, which must all be of one IP version."
  (let ((entries (sorted-entries members)))
    (index-from-vectors (map 'simple-vector #'first entries)
                        (map 'simple-vector #'second entries)
                        (map 'simple-vector #'third entries))))

(defun index-merge (index members)
  "Returns a fresh INDEX holding INDEX's members plus the list MEMBERS."
  (if (null members)
      index
      (let* ((firsts (index-firsts index)) (lasts (index-lasts index)) (olds (index-members index))
             (news (sorted-entries members))
             (n (+ (length olds) (length news)))
             (nf (make-array n)) (nl (make-array n)) (nm (make-array n))
             (i 0) (j 0))
        (declare (fixnum i j))
        (dotimes (k n)
          (let ((take-old (cond ((= i (length olds)) nil)
                                ((= j (length news)) t)
                                (t (let ((e (svref news j)))
                                     (not (entry< (first e) (second e) (third e)
                                                  (svref firsts i) (svref lasts i) (svref olds i))))))))
            (if take-old
                (setf (svref nf k) (svref firsts i) (svref nl k) (svref lasts i) (svref nm k) (svref olds i) i (1+ i))
                (let ((e (svref news j)))
                  (setf (svref nf k) (first e) (svref nl k) (second e) (svref nm k) (third e) j (1+ j))))))
        (index-from-vectors nf nl nm))))

(defun index-remove (index table)
  "Returns a fresh INDEX without the members that are keys of the EQ hash TABLE."
  (let ((keep (loop for i below (length (index-members index))
                    unless (gethash (svref (index-members index) i) table) collect i)))
    (if (= (length keep) (length (index-members index)))
        index
        (flet ((pick (v) (map 'simple-vector (lambda (i) (svref v i)) keep)))
          (index-from-vectors (pick (index-firsts index)) (pick (index-lasts index)) (pick (index-members index)))))))

(declaim (inline rightmost-first<=))
(defun rightmost-first<= (firsts x)
  "Index of the last entry of FIRSTS that is <= X, or -1."
  (declare (simple-vector firsts))
  (let ((lo 0) (hi (length firsts)))
    (declare (fixnum lo hi))
    (loop while (< lo hi)
          do (let ((mid (ash (+ lo hi) -1)))
               (if (<= (svref firsts mid) x) (setf lo (1+ mid)) (setf hi mid))))
    (1- lo)))

(defun index-containing (index first last)
  "Returns the most specific member enclosing [FIRST, LAST], or NIL."
  (let ((lasts (index-lasts index))
        (members (index-members index))
        (parents (index-parents index))
        (i (rightmost-first<= (index-firsts index) first)))
    (declare (simple-vector lasts members parents) (fixnum i))
    (loop while (>= i 0)
          do (if (<= last (svref lasts i))
                 (return (svref members i))
                 (setf i (svref parents i))))))

(defun index-overlapping (index first last)
  "Returns the members overlapping [FIRST, LAST] in ascending order."
  (let ((lasts (index-lasts index))
        (members (index-members index))
        (maxlasts (index-maxlasts index))
        (i (rightmost-first<= (index-firsts index) last))
        (result '()))
    (declare (simple-vector lasts members maxlasts) (fixnum i))
    (loop while (and (>= i 0) (<= first (svref maxlasts i)))
          do (when (<= first (svref lasts i))
               (push (svref members i) result))
             (decf i))
    result))

;;; IP-SET accessors that maintain the index.

(defun members (set)
  "The list of IP-LIKEs in IP-SET SET."
  (slot-value set 'set))

(defun by-version (members)
  "Returns the v4 members and the v6 members of the list MEMBERS as two values."
  (values (remove 6 members :key #'version)
          (remove 4 members :key #'version)))

(defun set-members! (set members &key sorted)
  "Replaces the members of IP-SET SET with the list MEMBERS, dropping the index.
If SORTED is true, MEMBERS are in COMPARE order and the index is built directly."
  (with-slots (index indexed scans) set
    (setf (slot-value set 'set) members
          indexed members
          scans 0
          index (when sorted
                  (multiple-value-bind (v4 v6) (by-version members)
                    (flet ((direct (ms)
                             (let ((entries (map 'vector (lambda (m) (multiple-value-bind (f l) (bounds m) (list f l m))) ms)))
                               (index-from-vectors (map 'simple-vector #'first entries)
                                                   (map 'simple-vector #'second entries)
                                                   (map 'simple-vector #'third entries)))))
                      (cons (direct v4) (direct v6)))))))
  set)

(defun add-members! (set members)
  "Prepends the list MEMBERS to the members of IP-SET SET. The index is kept;
the new members are searched linearly until they are merged in."
  (setf (slot-value set 'set) (append members (slot-value set 'set)))
  set)

(defun pending-members (set)
  "The members prepended to SET since its index was last built."
  (with-slots (indexed) set
    (loop for cell on (members set) until (eq cell indexed) collect (car cell))))

(defun replace-members! (set removed new-members)
  "Replaces the members of SET with the list NEW-MEMBERS, which is the old list
with the members in the EQ hash table REMOVED taken out and any other members
added. Maintains the index incrementally when one exists."
  (with-slots (index indexed scans) set
    (when index
      (let ((added (loop for m in new-members
                         until (eq m (car indexed))
                         unless (gethash m removed) collect m)))
        ;; NB: NEW-MEMBERS shares its tail with the old list only when the
        ;; caller preserves order; otherwise ADDED is everything.
        (multiple-value-bind (v4 v6) (by-version added)
          (setf index (cons (index-merge (index-remove (car index) removed) v4)
                            (index-merge (index-remove (cdr index) removed) v6))))))
    (setf (slot-value set 'set) new-members
          indexed new-members
          scans 0))
  set)

(defun set-index (set version)
  "Returns the INDEX for the members of SET of the given VERSION and, as a second
value, the members prepended since the index was built. Builds the index if
there is none, and merges the pending members in once there are more than
sqrt(n) of them or once linear scans of them have cost as much as a merge."
  (with-slots (index indexed scans) set
    (let ((members (members set)))
      (cond ((null index)
             (multiple-value-bind (v4 v6) (by-version members)
               (setf index (cons (build-index v4) (build-index v6))
                     indexed members
                     scans 0)))
            (t
             (let ((pending (pending-members set)))
               (when pending
                 (let ((n (+ (length (index-members (car index))) (length (index-members (cdr index))))))
                   (incf scans (length pending))
                   (when (or (> (length pending) (max 64 (isqrt n)))
                             (> scans n))
                     (multiple-value-bind (v4 v6) (by-version pending)
                       (setf index (cons (index-merge (car index) v4)
                                         (index-merge (cdr index) v6))
                             indexed members
                             scans 0))))))))
      (values (if (= version 4) (car index) (cdr index))
              (pending-members set)))))

(defun longest-match (set ip-like)
  "Returns the most specific member of IP-SET SET that contains IP-LIKE, i.e.,
the longest prefix match, or NIL if no member contains it."
  (check-type set ip-set)
  (check-type ip-like ip-like)
  (let ((version (version ip-like)))
    (multiple-value-bind (first last) (bounds ip-like)
      (multiple-value-bind (index pending) (set-index set version)
        (let* ((best (index-containing index first last))
               (best-size (when best (multiple-value-bind (f l) (bounds best) (- l f)))))
          (dolist (m pending best)
            (when (= (version m) version)
              (multiple-value-bind (f l) (bounds m)
                (when (and (<= f first) (<= last l)
                           (or (null best) (< (- l f) best-size)))
                  (setf best m best-size (- l f)))))))))))

(defun overlapping-members (set ip-like)
  "Returns the members of IP-SET SET that share at least one address with IP-LIKE."
  (let ((version (version ip-like)))
    (multiple-value-bind (first last) (bounds ip-like)
      (multiple-value-bind (index pending) (set-index set version)
        (nconc (loop for m in pending
                     when (and (= (version m) version)
                               (multiple-value-bind (f l) (bounds m) (and (<= f last) (<= first l))))
                       collect m)
               (index-overlapping index first last))))))
