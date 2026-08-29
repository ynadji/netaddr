(in-package :netaddr)

(defun ip-token-delimiter? (char)
  (or (member char '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed))
      (member char '(#\( #\) #\" #\' #\` #\, #\;))))

(defun read-ip-token (stream)
  "Reads a bare IP-LIKE token such as 10.0.0.0/8 or ::1-::ff from STREAM."
  (let ((token (with-output-to-string (out)
                 (loop for char = (peek-char nil stream nil nil)
                       until (or (null char) (ip-token-delimiter? char))
                       do (write-char (read-char stream) out)))))
    (when (string= token "")
      (error "Expected an IP address, network, or range after #i"))
    token))

(defun read-ip-element (stream)
  "Reads one element of the #i syntax: a bare token such as 10.0.0.0/8, a
string, or ,FORM where FORM is evaluated at run time. Returns a form that
produces the element's string."
  (case (peek-char t stream)
    (#\" (read stream))
    (#\, (read-char stream) (read stream))
    (t (read-ip-token stream))))

(defun |#i-reader| (stream sub-char infix)
  (declare (ignore sub-char infix))
  (cond (*read-suppress* (read stream) nil)
        ((char= (peek-char nil stream) #\()
         (read-char stream)
         `(list ,@(loop for char = (peek-char t stream)
                        until (char= char #\))
                        collect `(make-ip-like ,(read-ip-element stream))
                        finally (read-char stream))))
        (t `(make-ip-like ,(read-ip-element stream)))))

(defun %enable-ip-syntax ()
  (setf *readtable* (copy-readtable))
  (set-dispatch-macro-character #\# #\I #'|#i-reader|))

(defmacro enable-ip-syntax ()
  "Enables the #i reader macro for writing IP-LIKEs directly: #i192.168.0.0,
#i10.0.0.0/8 and #i::-::ff each read as a single IP-LIKE, and a parenthesized
list like #i(192.168.0.0 10.0.0.0/8 ::-::ff) reads as a list of IP-LIKEs.
An element may also be a string, or ,FORM to use the string FORM evaluates to
at run time, e.g. #i,prefix or #i(10.0.0.0/8 ,(format nil \"~a/24\" prefix))."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-ip-syntax)))
