(in-package :netaddr)

;;;; Class definitions and constructors.

(defclass ip+ () ())

(defclass ip-like (ip+) ((version :reader version)))

(defgeneric str (ip-address)
  (:documentation "Returns the string representation of IP-ADDRESS."))

(defgeneric int (ip-address)
  (:documentation "Returns the integer representation of IP-ADDRESS."))

(defclass ip-address (ip-like)
  ((str :initarg :str :reader str
        :documentation "String representation of the IP-ADDRESS.")
   (version :initarg :version :reader version)
   (int :initarg :int :reader int
        :documentation "Integer representation of the IP-ADDRESS.")))

(defmethod initialize-instance :after ((ip ip-address) &key)
  (cond ((slot-boundp ip 'str)
         (with-slots (str) ip
           (let (int)
             (cond ((setf int (parse-ipv4 str))
                    (setf (slot-value ip 'version) 4
                          (slot-value ip 'int) int))
                   ((setf int (parse-ipv6 str))
                    (setf (slot-value ip 'str) (ip-int-to-str-v6 int)
                          (slot-value ip 'version) 6
                          (slot-value ip 'int) int))
                   (t (error "~a is not an IP address string" str))))))
        ((slot-boundp ip 'int)
         (with-slots (int) ip
           (unless (<= 0 int (1- (expt 2 128)))
             (error "INT is not 0 <= ~a <= (1- (expt 2 128))" int))
           ;; An explicit :VERSION lets callers build small IPv6 addresses (e.g., ::1) from integers, which would
           ;; otherwise be inferred to be IPv4.
           (let ((version (if (and (slot-boundp ip 'version) (version ip))
                              (version ip)
                              (if (< int (expt 2 32)) 4 6))))
             (when (and (= version 4) (>= int (expt 2 32)))
               (error "INT ~a is too large for an IPv4 address" int))
             (setf (slot-value ip 'version) version
                   (slot-value ip 'str) (ip-int-to-str int version)))))
        (t (error "Must specify either STR or INT."))))

(defgeneric make-ip-address (str-or-int &key version)
  (:documentation "Make an IP-ADDRESS object from a STRING or INTEGER representation.")
  (:method ((str string) &key &allow-other-keys)
    (make-instance 'ip-address :str str))
  (:method ((int integer) &key version)
    (make-instance 'ip-address :int int :version version))
  (:method ((foo t) &key &allow-other-keys)
    (declare (ignore foo))
    (error "Must specify either STR or INT.")))

(defmethod print-object ((ip ip-address) out)
  (print-unreadable-object (ip out :type t)
    (format out "~a" (str ip))))

(defgeneric first-ip (ip-pair)
  (:documentation "Returns the first IP-ADDRESS of an IP-NETWORK or IP-RANGE."))

(defgeneric last-ip (ip-pair)
  (:documentation "Returns the last IP-ADDRESS of an IP-NETWORK or IP-RANGE."))

(defclass ip-pair (ip-like)
  ((first-ip :reader first-ip)
   (last-ip :reader last-ip)))

(defclass ip-network (ip-pair)
  ((str :initarg :str :reader str)
   (mask :reader mask)))

(defun make-ip-network (str)
  "Make an IP-NETWORK object from a string STR in CIDR notation, e.g., \"10.20.30.40/24\" or \"ffff::/96\"."
  (make-instance 'ip-network :str str))

(defun network-bounds (int version mask)
  "Returns the first and last address, as integers, of the network with prefix
length MASK that contains the address INT of the given VERSION."
  (let ((max-bits (ecase version (4 32) (6 128))))
    (ecase version
      (4 (check-type mask (integer 0 32) "in [0, 32] for IPv4 masks"))
      (6 (check-type mask (integer 0 128) "in [0, 128] for IPv6 masks")))
    (let* ((host-bits (- max-bits mask))
           (first (logand int (ash (integer-from-n-bits mask) host-bits))))
      (values first (logior first (integer-from-n-bits host-bits))))))

(defun set-network! (net first-ip last-ip mask)
  (setf (slot-value net 'first-ip) first-ip
        (slot-value net 'last-ip) last-ip
        (slot-value net 'mask) mask
        (slot-value net 'version) (version first-ip)
        (slot-value net 'str) (format nil "~a/~a" (str first-ip) mask)))

(defun %make-ip-address (int version)
  "Builds an IP-ADDRESS from an INT already known to be valid for VERSION,
skipping INITIALIZE-INSTANCE's checks."
  (let ((ip (allocate-instance (find-class 'ip-address))))
    (setf (slot-value ip 'int) int
          (slot-value ip 'version) version
          (slot-value ip 'str) (ip-int-to-str int version))
    ip))

(defun set-network-from! (net ip mask)
  "Sets NET to the network with prefix length MASK containing IP-ADDRESS IP.
Fresh endpoint addresses are built; IP is not modified."
  (let ((version (version ip)))
    (multiple-value-bind (first last) (network-bounds (int ip) version mask)
      (set-network! net
                    (%make-ip-address first version)
                    (%make-ip-address last version)
                    mask))))

(defmethod initialize-instance :after ((net ip-network) &key)
  (when (slot-boundp net 'str)
    (destructuring-bind (ip mask) (split-char #\/ (str net))
      (set-network-from! net (make-ip-address ip) (parse-integer mask)))))

(defmethod print-object ((net ip-network) out)
  (print-unreadable-object (net out :type t)
    (with-slots (str mask) net
      (format out "~a" str))))

(defun apply-mask (ip mask)
  "Make a fresh IP-NETWORK by applying MASK to IP-ADDRESS IP."
  (check-type ip ip-address)
  (let ((net (make-instance 'ip-network)))
    (set-network-from! net ip mask)
    net))

(defclass ip-range (ip-pair)
  ((first-ip :initarg :first-ip :accessor first-ip)
   (last-ip :initarg :last-ip :accessor last-ip)))

(defmethod initialize-instance :after ((range ip-range) &key)
  (when (< (int (last-ip range)) (int (first-ip range)))
    (error "FIRST-IP (~a) must be less than LAST-IP (~a)"
           (first-ip range) (last-ip range)))
  (unless (= (version (first-ip range)) (version (last-ip range)))
    (error "FIRST-IP (~a) and LAST-IP (~a) must be the same IP version"
           (first-ip range) (last-ip range)))
  (setf (slot-value range 'version) (version (first-ip range))))

(defun make-ip-range (first last)
  "Make an IP-RANGE object given two STRINGs or INTEGERs that represent valid IP addresses as expected by MAKE-IP-ADDRESS. LAST must be greater than or equal to FIRST."
  (make-instance 'ip-range :first-ip (make-ip-address first) :last-ip (make-ip-address last)))

(defmethod print-object ((range ip-range) out)
  (print-unreadable-object (range out :type t)
    (format out "~a-~a" (str (first-ip range)) (str (last-ip range)))))

(defun make-ip-like (ip-like-str)
  "Given a string for an IP-LIKE, infer the concrete type and return an object."
  (check-type ip-like-str string)
  (cond ((find #\/ ip-like-str) (make-ip-network ip-like-str))
        ((find #\- ip-like-str) (apply #'make-ip-range (split-char #\- ip-like-str)))
        (t (make-ip-address ip-like-str))))

(defclass ip-set (ip+)
  ((set :initarg :entries :initform '()
        :documentation "The list of member IP-LIKEs.")
   (index :initform nil
          :documentation "Lazily built lookup index over INDEXED; see index.lisp.")
   (indexed :initform nil
            :documentation "The tail of SET that INDEX was built from. Members
prepended since then precede it and are searched linearly.")
   (scans :initform 0
          :documentation "Linear-scan work done on the pending members; see SET-INDEX.")))

(defun make-ip-set (set)
  "Make an IP-SET object given a list of IP-LIKEs."
  (check-type set list)
  (dolist (set-element set)
    (check-type set-element ip-like))
  (let ((s (make-instance 'ip-set :entries (copy-list set))))
    (compact! s)))

(defmethod print-object ((set ip-set) out)
  (print-unreadable-object (set out :type t)
    (format out "(~a)" (length (slot-value set 'set)))))
