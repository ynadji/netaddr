(in-package :netaddr)

;; When it comes time to make things faster:
;; https://github.com/AccelerationNet/cl-cidr-notation/blob/master/src/cl-cidr-notation.lisp

(defclass ip-address ()
  ((str :initarg :str :reader str)
   (version :reader version)
   (int :reader int)))

(defun ipv4-str? (str) (find #\. str))
(defun ipv6-str? (str) (find #\: str))

;; TODO: IPv6
(defun ip-int-to-str (int &optional (type 4))
  (declare (ignore type))
  ;; (ldb (byte 8 24) (int (make-ip-address "10.20.30.40")))
  ;; gets the 10. what about the reverse?
  (let ((octets (list (ash (logand int (ash #xFF 24)) -24)
                      (ash (logand int (ash #xFF 16)) -16)
                      (ash (logand int (ash #xFF 8)) -8)
                      (logand int #xFF))))
    (str:join "." (mapcar #'write-to-string octets))))

;; TODO: IPv6
(defmethod initialize-instance :after ((ip ip-address) &key)
  (let ((octets (->> ip str (str:split ".") (mapcar #'parse-integer)))
        (int 0))
    (setf (slot-value ip 'version) 4)
    (incf int (ash (first octets) 24))
    (incf int (ash (second octets) 16))
    (incf int (ash (third octets) 8))
    (incf int (fourth octets))
    (setf (slot-value ip 'int) int)))

(defmethod print-object ((ip ip-address) out)
  (print-unreadable-object (ip out :type t)
    (with-slots (str version int) ip
      (format out "str: ~a, version: ~a, int: ~a" str version int))))

;; TODO: IPv6
(defmethod mask-ip ((ip ip-address) mask)
  (let ((masked-ip-int (logand (int ip) (- (expt 2 32)
                                           (expt 2 (- 32 mask))))))
    (setf (slot-value ip 'int) masked-ip-int)
    (setf (slot-value ip 'str) (ip-int-to-str masked-ip-int))
    ip))

(defun make-ip-address (str)
  (make-instance 'ip-address :str str))

(defclass ip-network ()
  ((str :initarg :str :reader str)
   (mask :reader mask)
   (first-ip :reader first-ip)
   (last-ip :reader last-ip)))

(defmethod slot-unbound (class (instance ip-network) last-ip)
  (declare (ignorable class))
  ;; TODO
  (setf (slot-value instance 'last-ip) (first-ip instance)))

(defmethod initialize-instance :after ((net ip-network) &key)
  (destructuring-bind (ip mask) (str:split "/" (str net))
    (let ((mask (parse-integer mask))
          (ip (make-ip-address ip)))
      (mask-ip ip mask)
      (setf (slot-value net 'first-ip) ip)
      (setf (slot-value net 'mask) mask)
      (setf (slot-value net 'str) (format nil "~a/~a" (str ip) mask)))))

(defmethod print-object ((net ip-network) out)
  (print-unreadable-object (net out :type t)
    (with-slots (str mask) net
      (format out "str: ~a, mask: ~a" str mask))))

(defmethod print-object ((net ip-network) out)
  (print-unreadable-object (net out :type t)
    (with-slots (str mask) net
      (format out "str: ~a, mask: ~a" str mask))))

(defclass ip-range ()
  ((start :initarg :start :accessor start)
   (end :initarg :end :accessor end)))

(defmethod initialize-instance :after ((range ip-range) &key)
  (when (< (-> range end int) (-> range start int))
    (error "START (~a) must be less than END (~a)"
           (start range) (end range))))

(defmethod print-object ((range ip-range) out)
  (print-unreadable-object (range out :type t)
    (format out "~a--~a" (start range) (end range))))

;; DEFGENERIC for MEMBER of IP-RANGE and IP-NETWORK on IP-ADDRESS or STRING

;; DEFCLASS for IP-SET
