(in-package :netaddr)

;;;; Adding and removing IP-LIKEs to an IP-SET, and set theoretic operations.

(defun %addnew! (set ip-like)
  (check-type set ip-set)
  (check-type ip-like ip-like)
  (with-slots (set) set
    (if (loop with changed? = nil
              for sub on set
              for (x) = sub
              do (cond ((subset? x ip-like)
                        (setf (car sub) ip-like
                              changed? t))
                       ((superset? x ip-like)
                        (setf changed? t)))
              finally
                 (return changed?))
        set
        (push ip-like set))))

(defun addnew! (set &rest ip-likes)
  "Push IP-LIKES to IP-SET SET if they are not already a member or a subset of a member of SET. Returns the modified IP-SET."
  (loop for ip-like in ip-likes do (%addnew! set ip-like))
  set)

(defun addnew (set &rest ip-likes)
  "Creates a fresh IP-SET that contains the original contents of SET as well as the IP-LIKES that are not already a member or a subset of a member of SET. Returns the fresh IP-SET."
  (let ((new-set (shallow-copy-object set)))
    (apply #'addnew! new-set ip-likes)
    new-set))

(defun add! (set &rest ip-likes)
  "Prepend in place IP-LIKES to the IP-SET SET. Returns the modified IP-SET."
  (with-slots (set) set
    (setf set (append ip-likes set)))
  set)

(defun add (set &rest ip-likes)
  "Creates a copy of SET with IP-LIKES prepended."
  (let ((new-set (shallow-copy-object set)))
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
  (if (null ip-likes)
      set
      (progn
        (with-slots (set) set
          (setf set
                (loop for range in set
                      append (subtract range (first ip-likes)))))
        (apply #'sub! set (rest ip-likes)))))

(defun sub (set &rest ip-likes)
  "Like SUB! but return a fresh IP-SET without modifying the argument SET in place."
  (let ((new-set (shallow-copy-object set)))
    (apply #'sub! new-set ip-likes)
    new-set))

(defun ip-set-union (&rest ip-sets)
  "Returns a fresh IP-SET that is the set union of all IP-SETS."
  (let ((res (make-ip-set nil)))
    (loop for ip-set in ip-sets do
      (with-slots ((set1 set)) res
        (with-slots ((set2 set)) ip-set
          ;; APPEND does not copy the last argument, CONCATENATE does.
          (progn (check-type ip-set ip-set)
                 (setf set1 (concatenate 'list set1 set2))))))
    (compact! res)))

(defun ip-set-intersection (&rest ip-sets)
  "Returns a fresh IP-SET that is the set intersection of all IP-SETS."
  (if (null ip-sets)
      (make-ip-set nil)
      (let ((inter (shallow-copy-object (first ip-sets))))
        (check-type inter ip-set)
        (loop for ip-set in (rest ip-sets) do
          (progn (check-type ip-set ip-set)
                 (setf (slot-value inter 'set)
                       (remove nil (loop for x in (slot-value inter 'set)
                                         append (loop for y in (slot-value ip-set 'set)
                                                      collect (intersect x y)))))))
        inter)))

(defun ip-set-difference (&rest ip-sets)
  "Returns a fresh IP-SET that is the set difference of (first IP-SETS) from (rest IP-SETS)."
  (if (null ip-sets)
      (make-ip-set nil)
      (let ((diff (shallow-copy-object (first ip-sets))))
        (check-type diff ip-set)
        (loop for ip-set in (rest ip-sets) do
          (progn (check-type ip-set ip-set)
                 (with-slots ((set1 set)) diff
                   (with-slots ((set2 set)) ip-set
                     (setf set1
                           ;; We must compute the difference pair-wise
                           ;; because the subtrahend may be a superset of
                           ;; multiple minuends.
                           (loop for x in set1
                                 append (loop for y in set2
                                              with new-xs = (list x)
                                              do (setf new-xs (loop for new-x in new-xs
                                                                 append (subtract new-x y)))
                                              finally (return new-xs))))))))
        diff)))

(defun ip-set-symmetric-difference (&rest ip-sets)
  "Returns a fresh IP-SET that is the set symmetric difference of IP-SETS, i.e., the difference of the union and intersection of IP-SETS."
  (ip-set-difference (apply #'ip-set-union ip-sets)
                     (apply #'ip-set-intersection ip-sets)))
