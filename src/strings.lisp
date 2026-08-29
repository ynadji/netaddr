(in-package :netaddr)

;;;; Parsing IP address strings to integers and printing integers as strings.

(defun parse-ipv4 (str)
  "If STR is a dotted-quad IPv4 address, return its integer value, otherwise NIL.
Octets must be decimal, in [0, 255], and have no leading zeroes."
  (declare (simple-string str) (optimize speed))
  (let ((int 0)
        (start 0))
    (declare ((unsigned-byte 32) int) (fixnum start))
    (dotimes (i 4 int)
      (let* ((end (if (= i 3) (length str) (or (position #\. str :start start) (return nil))))
             (digits (- end start)))
        (declare (fixnum end digits))
        (unless (and (<= 1 digits 3)
                     (loop for j from start below end always (digit-char-p (schar str j)))
                     (or (= digits 1) (char/= (schar str start) #\0)))
          (return nil))
        (let ((octet (parse-integer str :start start :end end)))
          (declare ((integer 0 999) octet))
          (when (> octet 255) (return nil))
          (setf int (logior int (ash octet (- 24 (* 8 i))))
                start (1+ end)))))))

(defun parse-ipv6-groups (str start end)
  "Parse the colon-separated 1-4 digit hex groups in STR between START and END.
Returns the list of group values, or :INVALID if any group is malformed. An
empty region yields NIL."
  (declare (simple-string str) (fixnum start end) (optimize speed))
  (if (= start end)
      '()
      (loop with groups = '()
            for group-start fixnum = start then (1+ group-end)
            for group-end fixnum = (or (position #\: str :start group-start :end end) end)
            do (unless (and (<= 1 (- group-end group-start) 4)
                            (loop for j from group-start below group-end
                                  always (digit-char-p (schar str j) 16)))
                 (return :invalid))
               (push (parse-integer str :start group-start :end group-end :radix 16) groups)
            until (= group-end end)
            finally (return (nreverse groups)))))

(defun parse-ipv6 (str)
  "If STR is a colon-separated hexadecimal IPv6 address (with at most one \"::\"),
return its integer value, otherwise NIL. Zone IDs and embedded IPv4 addresses
are not supported."
  ;; NB: The result is a 128-bit integer, so the arithmetic here is bignum
  ;; arithmetic by nature and SBCL's efficiency notes about it are noise.
  (declare (simple-string str) (optimize speed)
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((len (length str))
         (dbl (search "::" str)))
    (flet ((groups->int (groups)
             (loop with int = 0
                   for g fixnum in groups
                   do (setf int (logior (ash int 16) g))
                   finally (return int))))
      (if dbl
          (let ((left (parse-ipv6-groups str 0 dbl))
                (right (parse-ipv6-groups str (+ dbl 2) len)))
            (unless (or (eq left :invalid)
                        (eq right :invalid)
                        (> (+ (length left) (length right)) 7))
              (logior (ash (groups->int left) (* 16 (- 8 (length left))))
                      (groups->int right))))
          (let ((groups (parse-ipv6-groups str 0 len)))
            (unless (or (eq groups :invalid) (/= (length groups) 8))
              (groups->int groups)))))))

(defun ip-int-to-str-v4 (int)
  (declare ((unsigned-byte 32) int) (optimize speed))
  (let ((out (make-string 15 :element-type 'base-char))
        (pos 0))
    (declare (fixnum pos))
    (loop for shift from 24 downto 0 by 8
          for octet = (ldb (byte 8 shift) int)
          do (flet ((put (c) (setf (schar out pos) c) (incf pos)))
               (when (>= octet 100) (put (code-char (+ 48 (floor octet 100)))))
               (when (>= octet 10) (put (code-char (+ 48 (mod (floor octet 10) 10)))))
               (put (code-char (+ 48 (mod octet 10))))
               (when (plusp shift) (put #\.))))
    (subseq out 0 pos)))

(defun ip-int-to-str-v6 (int)
  "Returns the canonical (RFC 5952) text form of the IPv6 address INT: lowercase
hex groups without leading zeroes, and the first longest run of two or more zero
groups replaced by \"::\"."
  (declare ((unsigned-byte 128) int))
  (let ((groups (make-array 8 :element-type '(unsigned-byte 16)))
        (best-start -1)
        (best-len 1))
    (declare (fixnum best-start best-len))
    (dotimes (i 8)
      (setf (aref groups i) (ldb (byte 16 (* 16 (- 7 i))) int)))
    ;; Find the first longest run of zero groups, if any is at least 2 long.
    (let ((i 0))
      (declare (fixnum i))
      (loop while (< i 8)
            do (if (zerop (aref groups i))
                   (let ((j i))
                     (declare (fixnum j))
                     (loop while (and (< j 8) (zerop (aref groups j))) do (incf j))
                     (when (> (- j i) best-len)
                       (setf best-start i best-len (- j i)))
                     (setf i j))
                   (incf i))))
    (let ((out (make-string 39 :element-type 'base-char))
          (pos 0))
      (declare (fixnum pos))
      (flet ((put (c) (setf (schar out pos) c) (incf pos)))
        (flet ((emit (from to)
                 (loop for i from from below to
                       do (when (> i from) (put #\:))
                          (let ((g (aref groups i)) (started nil))
                            (loop for shift from 12 downto 0 by 4
                                  for d = (ldb (byte 4 shift) g)
                                  do (when (or started (plusp d) (zerop shift))
                                       (setf started t)
                                       (put (schar "0123456789abcdef" d))))))))
          (if (minusp best-start)
              (emit 0 8)
              (progn (emit 0 best-start)
                     (put #\:) (put #\:)
                     (emit (+ best-start best-len) 8)))))
      (subseq out 0 pos))))

(defun ip-int-to-str (int &optional (type 4))
  (ecase type
    (4 (ip-int-to-str-v4 int))
    (6 (ip-int-to-str-v6 int))))

(defun compress-ipv6-str (str)
  "Returns the canonical (RFC 5952) form of the IPv6 address string STR."
  (ip-int-to-str-v6 (or (parse-ipv6 str) (error "~a is not an IPv6 address string" str))))
