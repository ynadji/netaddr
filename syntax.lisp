(in-package :netaddr)

(defun |#i-reader| (stream sub-char infix)
  (declare (ignore sub-char infix))
  (let ((ip-likes (read stream)))
    (if (= 1 (length ip-likes))
        `(make-ip-like ,(car ip-likes))
        `(list ,@(mapcar (lambda (x) `(make-ip-like ,x)) ip-likes)))))

(defun %enable-ip-syntax ()
  (setf *readtable* (copy-readtable))
  (set-dispatch-macro-character #\# #\I #'|#i-reader|))

(defmacro enable-ip-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-ip-syntax)))
