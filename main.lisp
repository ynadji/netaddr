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
(defun mask-ip (ip mask)
  (let ((masked-ip-int (logand (int ip) (- (expt 2 32)
                                           (expt 2 (- 32 mask))))))
    (setf (slot-value ip 'int) masked-ip-int)
    (setf (slot-value ip 'str) (ip-int-to-str masked-ip-int))
    ip))

(defun integer-from-n-bits (n)
  (loop repeat n with int = 0
        do (setf int (logior 1 (ash int 1)))
        finally (return int)))

;; TODO: IPv6
;; TODO: Can you generalize this so MASK-IP and MASK-IP-LOWER are the same
;; function?
(defun mask-ip-lower (ip mask)
  (let ((masked-ip-int (logior (int ip) (integer-from-n-bits (- 32 mask)))))
    (setf (slot-value ip 'int) masked-ip-int)
    (setf (slot-value ip 'str) (ip-int-to-str masked-ip-int))
    ip))

(defun make-ip-address (str)
  (make-instance 'ip-address :str str))

(defclass ip-pair ()
    ((first-ip :reader first-ip)
     (last-ip :reader last-ip)))

(defclass ip-network (ip-pair)
  ((str :initarg :str :reader str)
   (mask :reader mask)))

(defmethod initialize-instance :after ((net ip-network) &key)
  (destructuring-bind (ip mask) (str:split "/" (str net))
    (let ((mask (parse-integer mask))
          (first-ip (make-ip-address ip))
          (last-ip (make-ip-address ip)))
      (mask-ip first-ip mask)
      (mask-ip-lower last-ip mask)
      (setf (slot-value net 'first-ip) first-ip)
      (setf (slot-value net 'last-ip) last-ip)
      (setf (slot-value net 'mask) mask)
      (setf (slot-value net 'str) (format nil "~a/~a" (str first-ip) mask)))))

(defmethod print-object ((net ip-network) out)
  (print-unreadable-object (net out :type t)
    (with-slots (str mask) net
      (format out "str: ~a, mask: ~a" str mask))))

(defmethod print-object ((net ip-network) out)
  (print-unreadable-object (net out :type t)
    (with-slots (str mask) net
      (format out "str: ~a, mask: ~a" str mask))))

(defun make-ip-network (str)
  (make-instance 'ip-network :str str))

(defclass ip-range (ip-pair)
  ((first-ip :initarg :first-ip :accessor first-ip)
   (last-ip :initarg :last-ip :accessor last-ip)))

(defmethod initialize-instance :after ((range ip-range) &key)
  (when (< (-> range last-ip int) (-> range first-ip int))
    (error "FIRST-IP (~a) must be less than LAST-IP (~a)"
           (first-ip range) (last-ip range))))

(defmethod print-object ((range ip-range) out)
  (print-unreadable-object (range out :type t)
    (format out "~a--~a" (first-ip range) (last-ip range))))

(defun make-ip-range (first last)
  (make-instance 'ip-range :first-ip (make-ip-address first) :last-ip (make-ip-address last)))

;; TODO: inherit from base class so you only need one of these. You'll probably
;; still need a separate one for IP-SET when you make that.
(defgeneric contains? (network ip)
  (:method ((network ip-pair) (ip ip-address))
    (<= (-> network first-ip int) (int ip) (-> network last-ip int)))
  (:method ((network ip-pair) (ip string))
    (<= (-> network first-ip int) (int (make-ip-address ip)) (-> network last-ip int))))

;; DEFCLASS for IP-SET
