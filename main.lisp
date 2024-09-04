(in-package :netaddr)

;; TODOs:
;;
;; Refactor to separate files.
;;
;; COPY methods for each data structure
;;
;; When it comes time to make things faster:
;; https://github.com/AccelerationNet/cl-cidr-notation/blob/master/src/cl-cidr-notation.lisp
;;
;; IPv6 stuff:
;; * Handle all kinds of string formats: https://stackoverflow.com/questions/53497/regular-expression-that-matches-valid-ipv6-addresses
;;
;; Features:
;; * IP-SET data structure. See https://github.com/netaddr/netaddr/blob/master/netaddr/ip/sets.py
;; Required functions:
;; * COMPACT
;; * UNION
;; * INTERSECTION
;; * SYMMETRIC-DIFFERENCE
;; * DIFFERENCE
;;
;; Add DECLARE types for functions to give better error messages.
;;
;; Add documentation with STAPLE. See the following for examples:
;; * https://shinmera.github.io/open-with/
;; * https://github.com/Shinmera/open-with/blob/main/README.mess

;; I made an interesting design choice to let CONTAINS? work with all IP-LIKEs, e.g.,
;;
;; NETADDR> (contains? #I("1.2.3.4") #I("1.2.3.4"))
;; T
;; NETADDR> (contains? #I("1.2.3.4") #I("1.2.3.4/32"))
;; T
;; NETADDR> (contains? #I("1.2.3.4-1.2.3.4") #I("1.2.3.4/32"))
;; T
;;
;; IP=/IP-EQUAL are the same and only compare equivalent types, i.e., IP-LIKEs
;; and IP-ADDRESSes are considered separately. But an IP-RANGE and IP-NETWORK
;; that represent the same group of IPs can be IP-EQUAL. IP-EQUALP is more
;; flexible, and considered IPs to be equal to their single IP IP-RANGE and
;; IP-NETWORK counterparts.
;;
;; NETADDR> (ip= #I("1.2.3.4-1.2.3.4") #I("1.2.3.4/32"))
;; T
;; NETADDR> (ip= #I("1.2.3.4") #I("1.2.3.4/32"))
;; NIL
;; NETADDR> (ip= #I("1.2.3.4") #I("1.2.3.4-1.2.3.4"))
;; NIL
;; NETADDR> (ip-equalp #I("1.2.3.4") #I("1.2.3.4/32"))
;; T
;; NETADDR> (ip-equalp #I("1.2.3.4") #I("1.2.3.4-1.2.3.4"))
;; T
;;
;; I _think_ this makes sense, because the underlying data being represented are
;; identical. This isn't exactly the same as in Python's netaddr:
;;
;; >>> '1.0.0.0/8' in IPNetwork('1.0.0.0/8')
;; True
;; >>> IPRange('1.0.0.0', '1.255.255.255') == IPNetwork('1.0.0.0/8')
;; True
;; >>> IPAddress('1.2.3.4') == IPNetwork('1.2.3.4/32')
;; False
;; >>> IPAddress('1.2.3.4') in IPNetwork('1.2.3.4/32')
;; True
;;
;; That amount of flexibility with CONTAINS? probably makes sense, and you can
;; just make SUBSET?/etc. _use_ CONTAINS? other the hood so you get that for
;; free. If you make DISJOINT? and CONTIGUOUS? work with IPs as well, that makes
;; all of those nice and generic.
;;
;; Spend time thinking about if IP= makes sense to consider ranges/CIDRs of one
;; IP equal to just that IP. Maybe it's worth doing IP-EQUAL and IP-EQUALP? I
;; guess you could have all three and have IP= be the same as IP-EQUAL, since
;; that's mostly what users would actually want. For IP-SETs, however, it's
;; clear I want to consider those the same.

;; NB: Needed to bump this to ensure I can compute enough bits in RANGE->CIDR
;; for very large IPv6 integers.
(setf cr:*creal-tolerance* 120)

(defclass ip-like () ((version :reader version)))

(defclass ip-address (ip-like)
  ((str :initarg :str :reader str)
   (version :reader version)
   (int :initarg :int :reader int)))

;; TODO: Better regex?
;; https://stackoverflow.com/questions/53497/regular-expression-that-matches-valid-ipv6-addresses
;; https://stackoverflow.com/questions/5284147/validating-ipv4-addresses-with-regexp
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

(defun remove-leading-zeroes (str)
  (let ((parts (str:split ":" str)))
    (str:join ":"
     (loop for part in parts
           collect (cl-ppcre:regex-replace "^0+(.+)$" part "\\1")))))

(defun largest-run (item sequence)
  (loop for i from 0 for x in sequence
        with len = 0 with pos = -1 with max = 0 with max-pos = 0 do
          (if (string= item x)
              (progn (incf len)
                     (when (minusp pos)
                      (setf pos i))
                     (when (> len max)
                       (setf max len max-pos pos)))
              (setf len 0 pos -1))
        finally (return (values max max-pos))))

(defun compress-ipv6-str (str)
  (let* ((str (remove-leading-zeroes str))
         (colon-list (list ":"))
         (parts (str:split ":" str)))
    (multiple-value-bind (len pos) (largest-run "0" parts)
      (let* ((parts (if (< len 2)
                        parts
                        (-<> parts
                          (replace <> colon-list :start1 pos :end1 (+ len pos))
                          (delete "0" <> :test #'string= :start pos :end (+ len pos)))))
            (new-parts-len (length parts)))
        (cond ((< len 2) str) ; No run of 0s
              ((= new-parts-len 1) "::") ; All run of 0s
              ((and (plusp pos)
                    (< (+ pos len) 8)) ; Run of 0s was in the middle
               (str:join ":" (nsubstitute "" ":" parts :test #'string=)))
              (t (str:join ":" parts)))))))

(defun ipv6-parts-to-int (parts)
  (loop for x from 112 downto 0 by 16
        for part in parts sum (ash part x)))

(defun ipv6-str-to-int (str)
  (ipv6-parts-to-int (expand-ipv6-addr-to-parts str)))

(defun ipv4-str-to-int (str)
  (let ((octets (->> str (str:split ".") (mapcar #'parse-integer))))
    (loop for x from 24 downto 0 by 8
          for octet in octets sum (ash octet x))))

(defmethod initialize-instance :after ((ip ip-address) &key)
  (cond ((slot-boundp ip 'str)
         (with-slots (str) ip
            (cond ((ipv4-str? str)
                   (setf (slot-value ip 'version) 4)
                   (setf (slot-value ip 'int) (ipv4-str-to-int str)))
                  ((ipv6-str? str)
                   (setf (slot-value ip 'str) (compress-ipv6-str str))
                   (setf (slot-value ip 'version) 6)
                   (setf (slot-value ip 'int) (ipv6-str-to-int str)))
                  (t (error "~a is not an IP address string" str)))))
        ((slot-boundp ip 'int)
         (with-slots (int) ip
           ;; TODO: cleaner way to do this maybe?
           (if (< int (expt 3 32))
               (progn (setf (slot-value ip 'version) 4)
                      (setf (slot-value ip 'str) (ip-int-to-str int)))
               (progn (setf (slot-value ip 'version) 6)
                      (setf (slot-value ip 'str) (ip-int-to-str int 6))))))
        (t (error "Must specify either STR or INT."))))

(defgeneric make-ip-address (str-or-int)
  (:method ((str string))
    (make-instance 'ip-address :str str))
  (:method ((int integer))
    (make-instance 'ip-address :int int)))

(defmethod print-object ((ip ip-address) out)
  (print-unreadable-object (ip out :type t)
    (format out "~a" (str ip))))

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

(defclass ip-pair (ip-like)
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
      (setf (slot-value net 'version) (version first-ip))
      (when (= 6 (version first-ip))
        (setf (slot-value first-ip 'str)
              (compress-ipv6-str (str first-ip))))
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
  (when (< (int (last-ip range)) (int (first-ip range)))
    (error "FIRST-IP (~a) must be less than LAST-IP (~a)"
           (first-ip range) (last-ip range)))
  (setf (slot-value range 'version)
        (if (< (int (last-ip range)) (expt 2 32))
            4
            6)))

(defmethod print-object ((range ip-range) out)
  (print-unreadable-object (range out :type t)
    (format out "~a--~a" (str (first-ip range)) (str (last-ip range)))))

(defun make-ip-range (first last)
  (make-instance 'ip-range :first-ip (make-ip-address first) :last-ip (make-ip-address last)))

(defgeneric ->ip-range (ip-like)
  (:method ((ip-like ip-address))
    (let ((s (str ip-like)))
     (make-ip-range s s)))
  (:method ((ip-like ip-pair))
    (with-slots (first-ip last-ip) ip-like
      (make-ip-range (str first-ip) (str last-ip))))
  (:method ((ip-like ip-range))
    ip-like))

(defun in-set? (ip ip-block)
  (contains? ip-block ip))

(defgeneric ip-equal (ip-like-1 ip-like-2)
  (:method ((ip1 ip-address) (ip2 ip-address))
    (and (= (int ip1) (int ip2))
         (= (version ip1) (version ip2))))
  (:method ((p1 ip-pair) (p2 ip-pair))
    (and (ip= (first-ip p1) (first-ip p2))
         (ip= (last-ip p1) (last-ip p2))))
  ;; Default case when the types of the two arguments do not match.
  (:method ((x ip-like) (y ip-like))
    nil))

(defun ip= (ip-like-1 ip-like-2)
  (ip-equal ip-like-1 ip-like-2))

(defgeneric ip-equalp (ip-like-1 ip-like-2)
  (:method ((ip ip-address) (pair ip-pair))
    (and (= (int ip) (int (first-ip pair)) (int (last-ip pair)))
         (= (version ip) (version pair))))
  (:method ((pair ip-pair) (ip ip-address))
    (ip-equalp ip pair))
  (:method ((x ip-like) (y ip-like))
    (ip-equal x y)))

(defun make-ip-like (ip-or-network-or-range-str)
  (cond ((find #\/ ip-or-network-or-range-str) (make-ip-network ip-or-network-or-range-str))
        ((find #\- ip-or-network-or-range-str) (apply #'make-ip-range (str:split "-" ip-or-network-or-range-str)))
        (t (make-ip-address ip-or-network-or-range-str))))

;; NB: Wraps. Can I use DEFINE-SETF-EXPANDER for this?
(defun ip-incf (ip &optional (n 1))
  (setf (slot-value ip 'int) (+ n (int ip)))
  (setf (slot-value ip 'str) (ip-int-to-str (int ip) (slot-value ip 'version)))
  ip)

;; Might not be super efficient from allocations and recursiveness. Particularly
;; for poorly CIDR-aligned IPv6 ranges. Lots of CONSing.
(defun range->cidrs (ip-range)
  (flet ((get-bits (first-int last-int version)
           (let ((diff+1 (1+ (- last-int first-int))))
             (ecase version
               (4 (floor (log (coerce diff+1 'double-float) 2)))
               (6 (multiple-value-bind (q r) (cr:floor-r (cr:log-r diff+1 2))
                    ;; NB: We only need 1 bit of information (was the remainder
                    ;; 0 or not-zero.
                    (values q (cr:rational-approx-r r 1))))))))
    (let ((first-str (str (first-ip ip-range)))
          (first-int (int (first-ip ip-range)))
          (last-str (str (last-ip ip-range)))
          (last-int (int (last-ip ip-range)))
          (version (version (first-ip ip-range)))
          (max-bits (ecase (version (first-ip ip-range)) (4 32) (6 128))))
      (multiple-value-bind (bits remainder) (get-bits first-int last-int version)
        (let ((net (make-ip-network (format nil "~a/~a" first-str (- max-bits bits)))))
          (if (= remainder 0)
              (list net)
              (cons net (range->cidrs (make-ip-range (str (make-instance 'ip-address
                                                                         :int (1+ (int (last-ip net)))))
                                                     last-str)))))))))

(defun subset? (pair1 pair2)
  (<= (int (first-ip pair2))
      (int (first-ip pair1))
      (int (last-ip pair1))
      (int (last-ip pair2))))

(defun strict-subset? (pair1 pair2)
  (and (not (ip= pair1 pair2))
       (subset? pair1 pair2)))

(defun superset? (pair1 pair2)
  (subset? pair2 pair1))

(defun strict-superset? (pair1 pair2)
  (strict-subset? pair2 pair1))

(defun disjoint? (pair1 pair2)
  (or (< (int (last-ip pair1))
         (int (first-ip pair2)))
      (< (int (last-ip pair2))
         (int (first-ip pair1)))))

(defun contiguous? (pair1 pair2)
  (or (= 1 (abs (- (int (last-ip pair1))
                   (int (first-ip pair2)))))
      (= 1 (abs (- (int (last-ip pair2))
                   (int (first-ip pair1)))))))

(defgeneric compare (ip-like-1 ip-like-2)
  (:method ((ip1 ip-address) (ip2 ip-address))
    (< (int ip1) (int ip2)))
  (:method ((ip ip-address) (p ip-pair))
    (< (int ip) (int (first-ip p))))
  (:method ((p ip-pair) (ip ip-address))
    (not (compare ip p))) ; NB: this works because we always want ranges/subnets to be before IPs.
  (:method ((p1 ip-pair) (p2 ip-pair))
    (or (< (int (first-ip p1)) (int (first-ip p2)))
        (and (ip= (first-ip p1) (first-ip p2))
             (> (int (last-ip p1))
                (int (last-ip p2)))))))

(defclass ip-set ()
  ((set :initarg :entries :initform '())))

(defmethod print-object ((set ip-set) out)
  (print-unreadable-object (set out :type t)
    (format out "(~a)" (length (slot-value set 'set)))))

;; We only need to COMPACT if we don't merge when adding _or_ the initial set
;; isn't already compacted. Hmmm.
(defun compact (set)
  (declare (ignore set)))

;; So since we use ADJOIN when adding, we may be iterating down the entire list
;; anyway. If that's the case, it probably makes more sense LOOP across the list
;; manually, if ranges are adjacent merge them then and there, and if a new
;; addition is a supserset of an existing one, just replace it. Otherwise, just
;; CONS it on there.
(defun add (set ip-like)
  (declare (ip-set set)
           (ip-like ip-like))
  (with-slots (set) set
    (let ((adjoined (adjoin ip-like set :test #'in-set?)))
      (if (eq set adjoined)
          set
          (remove-if (lambda (x)
                       (and (not (ip= x ip-like))
                            (contains? x ip-like))) adjoined)))))

(defun add! (set ip-like)
  (setf (slot-value set 'set) (add set ip-like)))

(defun subtract (ip-like-1 ip-like-2)
  (let ((r1 (->ip-range ip-like-1))
        (r2 (->ip-range ip-like-2)))
    (cond ((disjoint? r1 r2) (list ip-like-1))
          ((subset? r1 r2) nil)
          ;; TODO: Refactor.
          (t (let ((r1f (int (first-ip r1)))
                   (r2f (int (first-ip r2)))
                   (r1l (int (last-ip r1)))
                   (r2l (int (last-ip r2))))
               (cond
                 ((= r1f r2f) (list (make-ip-range (1+ r2l) r1l)))
                 ((= r1l r2l) (list (make-ip-range r1f (1- r2f))))
                 (t (list (make-ip-range (int (first-ip r1))
                                         (1- (int (first-ip r2))))
                          (make-ip-range (1+ (int (last-ip r2)))
                                         (int (last-ip r1)))))))))))

(defun sub (set ip-like)
  (with-slots (set) set
    (ax:flatten
     (loop for range in set
           collect (subtract range ip-like)))))

(defun sub! (set ip-like)
  (setf (slot-value set 'set) (sub set ip-like)))

(defgeneric contains? (network ip)
  (:method ((ip1 ip-address) (ip2 ip-address))
    (ip= ip1 ip2))
  (:method ((network ip-pair) (ip ip-address))
    (<= (int (first-ip network)) (int ip) (int (last-ip network))))
  (:method ((network ip-pair) (ip string))
    (<= (int (first-ip network)) (int (make-ip-address ip)) (int (last-ip network))))
  (:method ((pair1 ip-pair) (pair2 ip-pair))
    (subset? pair2 pair1))
  (:method ((ip ip-address) (pair ip-pair))
    (= (int ip) (int (first-ip pair)) (int (last-ip pair))))
  (:method ((set ip-set) (ip ip-address))
    (car (member ip (slot-value set 'set) :test #'in-set?)))
  (:method ((set ip-set) (range-or-network ip-pair))
    (car (member range-or-network (slot-value set 'set) :test #'subset?))) ;; to CONTAINS?
  (:method ((set ip-set) (ip string))
    (let ((ip (make-ip-like ip)))
      (contains? set ip))))

(defgeneric size (network-or-range)
  (:method ((ip ip-address)) 1)
  (:method ((pair ip-pair))
    (1+ (- (int (last-ip pair))
           (int (first-ip pair)))))
  (:method ((set ip-set))
    (with-slots (set) set
     (apply #'+ (mapcar #'size set)))))

;;; Character dispatch macros
(defun |#i-reader| (stream sub-char infix)
  (declare (ignore sub-char infix))
  (let ((ip-likes (read stream)))
    (if (= 1 (length ip-likes))
        `(make-ip-like ,(car ip-likes))
        `(list ,@(mapcar (lambda (x) `(make-ip-like ,x)) ip-likes)))))

(set-dispatch-macro-character #\# #\I #'|#i-reader|)
