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

(defun ip-reader (stream sub-char infix)
  "The #i dispatch macro function; see ENABLE-IP-SYNTAX. Exported so the syntax
can be installed into any readtable, e.g. a NAMED-READTABLES one."
  (declare (ignore sub-char infix))
  (cond (*read-suppress* (read stream) nil)
        ((char= (peek-char nil stream) #\()
         (read-char stream)
         `(list ,@(loop for char = (peek-char t stream)
                        until (char= char #\))
                        collect `(make-ip-like ,(read-ip-element stream))
                        finally (read-char stream))))
        (t `(make-ip-like ,(read-ip-element stream)))))

(defvar *ip-syntax-readtable*
  (let ((readtable (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\I #'ip-reader readtable)
    readtable)
  "The standard readtable plus the #i syntax. Bind *READTABLE* to it, or merge it
into your own readtable.")

(defvar *previous-readtables* '()
  "Readtables replaced by ENABLE-IP-SYNTAX, restored by DISABLE-IP-SYNTAX.")

(defun %enable-ip-syntax ()
  (push *readtable* *previous-readtables*)
  (setf *readtable* (copy-readtable))
  (set-dispatch-macro-character #\# #\I #'ip-reader))

(defun %disable-ip-syntax ()
  (setf *readtable* (if *previous-readtables*
                        (pop *previous-readtables*)
                        (copy-readtable nil))))

(defmacro enable-ip-syntax ()
  "Enables the #i reader macro for writing IP-LIKEs directly: #i192.168.0.0,
#i10.0.0.0/8 and #i::-::ff each read as a single IP-LIKE, and a parenthesized
list like #i(192.168.0.0 10.0.0.0/8 ::-::ff) reads as a list of IP-LIKEs.
An element may also be a string, or ,FORM to use the string FORM evaluates to
at run time, e.g. #i,prefix or #i(10.0.0.0/8 ,(format nil \"~a/24\" prefix)).

The current readtable is copied and #i added to it, so other reader extensions
already enabled are kept; DISABLE-IP-SYNTAX restores the previous readtable.
Within a file being compiled or loaded the change is local to that file. To
use the syntax without changing *READTABLE*, bind it to *IP-SYNTAX-READTABLE*
or install IP-READER into a readtable of your own."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-ip-syntax)))

(defmacro disable-ip-syntax ()
  "Restores the readtable that was current before the matching ENABLE-IP-SYNTAX."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%disable-ip-syntax)))
