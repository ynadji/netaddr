(in-package :netaddr)

;;;; Adding and removing IP-LIKEs to an IP-SET, and set theoretic operations.

(defun compact! (set)
  "Sorts the members of IP-SET SET and removes duplicates and members that are
subsets of other members. Returns SET."
  (let ((kept '())
        (version 0)
        (max-last -1))
    (dolist (m (sort (copy-list (members set)) #'compare))
      (multiple-value-bind (first last) (bounds m)
        (declare (ignore first))
        (unless (= (version m) version)
          (setf version (version m)
                max-last -1))
        ;; Members are sorted by first address ascending, so M is a subset of
        ;; some kept member iff its last address is within the furthest last
        ;; address kept so far.
        (unless (<= last max-last)
          (push m kept)
          (setf max-last last))))
    (set-members! set (nreverse kept) :sorted t)))

(defun %addnew! (set ip-like)
  (check-type set ip-set)
  (check-type ip-like ip-like)
  (unless (longest-match set ip-like)
    (multiple-value-bind (first last) (bounds ip-like)
      (progn
        (let ((subsumed (make-hash-table :test #'eq)))
          (dolist (m (overlapping-members set ip-like))
            (multiple-value-bind (f l) (bounds m)
              (when (and (<= first f) (<= l last))
                (setf (gethash m subsumed) t))))
          (if (zerop (hash-table-count subsumed))
              (add-members! set (list ip-like))
              (replace-members! set subsumed
                                (cons ip-like (remove-if (lambda (m) (gethash m subsumed)) (members set)))))))))
  set)

(defun addnew! (set &rest ip-likes)
  "Push IP-LIKES to IP-SET SET if they are not already a member or a subset of a member of SET. Members of SET that are subsets of an added IP-LIKE are removed. Returns the modified IP-SET."
  (loop for ip-like in ip-likes do (%addnew! set ip-like))
  set)

(defun addnew (set &rest ip-likes)
  "Creates a fresh IP-SET that contains the original contents of SET as well as the IP-LIKES that are not already a member or a subset of a member of SET. Returns the fresh IP-SET."
  (let ((new-set (copy-ip-set set)))
    (apply #'addnew! new-set ip-likes)
    new-set))

(defun add! (set &rest ip-likes)
  "Prepend in place IP-LIKES to the IP-SET SET. Returns the modified IP-SET."
  (check-type set ip-set)
  (add-members! set ip-likes))

(defun add (set &rest ip-likes)
  "Creates a copy of SET with IP-LIKES prepended."
  (let ((new-set (copy-ip-set set)))
    (apply #'add! new-set ip-likes)
    new-set))

(defun subtract (ip-like-1 ip-like-2)
  "Return a fresh list of IP-RANGEs that represents IP-LIKE-1 after removing all IPs in IP-LIKE-2. If IP-LIKE-1 and IP-LIKE-2 are disjoint, a list containing the original IP-LIKE-1 is returned."
  (check-type ip-like-1 ip-like)
  (check-type ip-like-2 ip-like)
  (if (= (version ip-like-1) (version ip-like-2))
      (let ((r1 (->ip-range ip-like-1))
            (r2 (->ip-range ip-like-2)))
        (cond ((disjoint? r1 r2) (list ip-like-1))
              ((subset? r1 r2) nil)
              ;; TODO: Refactor.
              (t (let ((r1f (int (first-ip r1)))
                       (r2f (int (first-ip r2)))
                       (r1l (int (last-ip r1)))
                       (r2l (int (last-ip r2))))
                   (cond
                     ((= r1f r2f) (list (make-ip-range (1+ r2l) r1l)))
                     ((= r1l r2l) (list (make-ip-range r1f (1- r2f))))
                     (t (list (make-ip-range (int (first-ip r1))
                                             (1- (int (first-ip r2))))
                              (make-ip-range (1+ (int (last-ip r2)))
                                             (int (last-ip r1))))))))))
      (list ip-like-1)))

(defun sub! (set &rest ip-likes)
  "Remove IP-LIKES from IP-SET SET in place. IP-LIKES that are a member or superset of a member of SET are removed. IP-LIKES that are a subset of a member of SET are SUBTRACTed in place."
  (check-type set ip-set)
  (dolist (ip-like ip-likes set)
    (check-type ip-like ip-like)
    ;; Only rebuild the member list when something actually overlaps.
    (let ((hits (overlapping-members set ip-like)))
      (when hits
        (let ((table (make-hash-table :test #'eq))
              (pieces '()))
          (dolist (hit hits) (setf (gethash hit table) t))
          (dolist (hit hits) (setf pieces (append (subtract hit ip-like) pieces)))
          (replace-members! set table
                            (append pieces (remove-if (lambda (m) (gethash m table)) (members set)))))))))

(defun sub (set &rest ip-likes)
  "Like SUB! but return a fresh IP-SET without modifying the argument SET in place."
  (let ((new-set (copy-ip-set set)))
    (apply #'sub! new-set ip-likes)
    new-set))

(defun ip-set-union (&rest ip-sets)
  "Returns a fresh IP-SET that is the set union of all IP-SETS."
  (let ((res (make-instance 'ip-set)))
    (set-members! res (loop for ip-set in ip-sets
                            do (check-type ip-set ip-set)
                            append (members ip-set)))
    (compact! res)))

(defun ip-set-intersection (&rest ip-sets)
  "Returns a fresh IP-SET that is the set intersection of all IP-SETS."
  (if (null ip-sets)
      (make-ip-set nil)
      (let ((inter (copy-ip-set (first ip-sets))))
        (check-type inter ip-set)
        (dolist (ip-set (rest ip-sets) inter)
          (check-type ip-set ip-set)
          (set-members! inter (loop for x in (members inter)
                                    append (loop for y in (overlapping-members ip-set x)
                                                 for i = (intersect x y)
                                                 when i collect i)))))))

(defun ip-set-difference (&rest ip-sets)
  "Returns a fresh IP-SET that is the set difference of (first IP-SETS) from (rest IP-SETS)."
  (if (null ip-sets)
      (make-ip-set nil)
      (let ((diff (copy-ip-set (first ip-sets))))
        (check-type diff ip-set)
        (dolist (ip-set (rest ip-sets) diff)
          (check-type ip-set ip-set)
          ;; Each subtrahend overlapping X may split it into pieces, so
          ;; subtract them one at a time from the accumulated pieces.
          (set-members! diff (loop for x in (members diff)
                                   append (let ((pieces (list x)))
                                            (dolist (y (overlapping-members ip-set x) pieces)
                                              (setf pieces (loop for piece in pieces
                                                                 append (subtract piece y)))))))))))

(defun ip-set-symmetric-difference (&rest ip-sets)
  "Returns a fresh IP-SET that is the set symmetric difference of IP-SETS, i.e., the difference of the union and intersection of IP-SETS."
  (ip-set-difference (apply #'ip-set-union ip-sets)
                     (apply #'ip-set-intersection ip-sets)))
