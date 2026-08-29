(in-package :netaddr)

;;;; Small helpers shared across the library.

(declaim (inline split-char))
(defun split-char (char string)
  "Split STRING on every occurrence of CHAR, returning a list of substrings.
Empty substrings are kept, matching SPLIT-SEQUENCE:SPLIT-SEQUENCE."
  (declare (character char)
           (optimize (speed 3)))
  (let ((result '())
        (start 0))
    (declare (simple-string string) (fixnum start))
    (loop for pos = (position char string :start start)
          do (push (subseq string start (or pos (length string))) result)
          while pos do (setf start (1+ (the fixnum pos))))
    (nreverse result)))

(defun integer-from-n-bits (n)
  "Returns the integer with its low N bits set."
  (1- (ash 1 n)))
