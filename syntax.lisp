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
  "Enables short-hand character reader macro for creating IP-LIKEs using #I. Using #I with a single argument like #I(\"192.168.0.0/16\") or #I(\"::-ffff::\") returns a single IP-LIKE and with multiple arguments like #I(\"1.2.3.4\" \"10.20.30.0/24\" \"::dada:beef\") returns a list of IP-LIKEs."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-ip-syntax)))
