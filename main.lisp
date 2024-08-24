(in-package :netaddr)

;; When it comes time to make things faster:
;; https://github.com/AccelerationNet/cl-cidr-notation/blob/master/src/cl-cidr-notation.lisp

(defclass ip-address ()
  ((str :initarg :str :reader str)
   (version :reader version)
   (int :reader int)))

;; TODO: Better regex?
(defun ipv4-str? (str) (find #\. str))
(defun ipv6-str? (str) (find #\: str))

(defun ip-int-to-str-v4 (int)
  (->> (loop for offset from 24 downto 0 by 8 collect (ldb (byte 8 offset) int))
    (mapcar #'write-to-string)
    (str:join ".")))

(defun ip-int-to-str-v6 (int)
  (let ((*print-base* 16))
    (->> (loop for offset from 112 downto 0 by 16 collect (ldb (byte 16 offset) int))
      (mapcar #'write-to-string)
      (str:join ":"))))

(defun ip-int-to-str (int &optional (type 4))
  (ecase type
    (4 (ip-int-to-str-v4 int))
    (6 (ip-int-to-str-v6 int))))

(defun expand-ipv6-addr-to-parts (str)
  (let* ((parts (str:split ":" str))
         (num-non-empties (count "" parts :test-not #'equal))
         (zeroes (loop repeat (- 8 num-non-empties) collect "0")))
    (mapcar (lambda (x) (parse-integer x :radix 16))
            ;; TODO: use -<>
            (remove "" (ax:flatten (substitute zeroes "" parts :test #'equal :count 1)) :test #'equal))))

(defun ipv6-parts-to-int (parts)
    (loop for x from 112 downto 0 by 16
          for part in parts sum (ash part x)))

(defun ipv6-str-to-int (str)
  (-> str expand-ipv6-addr-to-parts ipv6-parts-to-int))

(defun ipv4-str-to-int (str)
  (let ((octets (->> str (str:split ".") (mapcar #'parse-integer))))
    (loop for x from 24 downto 0 by 8
          for octet in octets sum (ash octet x))))

(defmethod initialize-instance :after ((ip ip-address) &key)
  (with-slots (str) ip
    (cond ((ipv4-str? str)
           (setf (slot-value ip 'version) 4)
           (setf (slot-value ip 'int) (ipv4-str-to-int str)))
          ((ipv6-str? str)
           (setf (slot-value ip 'version) 6)
           (setf (slot-value ip 'int) (ipv6-str-to-int str)))
          (t (error "~a is not an IP address string" str)))))

(defmethod print-object ((ip ip-address) out)
  (print-unreadable-object (ip out :type t)
    (with-slots (str int) ip
      (format out "~a (~a)" str int))))

(defun integer-from-n-bits (n)
  (loop repeat n with int = 0
        do (setf int (logior 1 (ash int 1)))
        finally (return int)))

(defun mask-ip! (ip mask &optional (upper-or-lower :upper))
  (let ((max-bits (ecase (version ip) (4 32) (6 128))))
    (with-slots (int version) ip
     (ecase upper-or-lower
       (:lower (setf (slot-value ip 'int)
                     (logand int (ash (integer-from-n-bits mask) (- max-bits mask)))))
       (:upper (setf (slot-value ip 'int)
                     (logior int (integer-from-n-bits (- max-bits mask))))))
      (setf (slot-value ip 'str) (ip-int-to-str (int ip) version))
      ip)))

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
      (mask-ip! first-ip mask :lower)
      (mask-ip! last-ip mask :upper)
      (setf (slot-value net 'first-ip) first-ip)
      (setf (slot-value net 'last-ip) last-ip)
      (setf (slot-value net 'mask) mask)
      (setf (slot-value net 'str) (format nil "~a/~a" (str first-ip) mask)))))

(defmethod print-object ((net ip-network) out)
  (print-unreadable-object (net out :type t)
    (with-slots (str mask) net
      (format out "~a" str))))

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
    (format out "~a--~a" (-> range first-ip str) (-> range last-ip str))))

(defun make-ip-range (first last)
  (make-instance 'ip-range :first-ip (make-ip-address first) :last-ip (make-ip-address last)))

(defgeneric contains? (network ip)
  (:method ((network ip-pair) (ip ip-address))
    (<= (-> network first-ip int) (int ip) (-> network last-ip int)))
  (:method ((network ip-pair) (ip string))
    (<= (-> network first-ip int) (int (make-ip-address ip)) (-> network last-ip int))))

;; DEFCLASS for IP-SET
;;
;; MERGE-CIDR function from netaddr just converts CIDRs to ranges, merges the
;; ranges, then converts back to CIDRs.

;;; Character dispatch macros
(set-dispatch-macro-character
 #\# #\I
 (lambda (stream sub-char infix)
   (declare (ignore sub-char infix))
   ;; Returns (list 'quote (ip1 ip2 ...)) so (ip1 ip2 ...) isn't interpreted as
   ;; a function call.
   (list 'quote
         (let ((list (mapcar #'make-ip-address (read stream))))
           (if (= 1 (length list))
               (car list)
               list)))))

(set-dispatch-macro-character
 #\# #\N
 (lambda (stream sub-char infix)
   (declare (ignore sub-char infix))
   (list 'quote
         (let ((list (mapcar #'make-ip-network (read stream))))
           (if (= 1 (length list))
               (car list)
               list)))))

