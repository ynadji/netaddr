(in-package :netaddr)

;; Needed to bump this to ensure I can compute enough bits in RANGE->CIDR for
;; very large IPv6 integers.
(setf cr:*creal-tolerance* 120)

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
   (version :reader version)
   (int :initarg :int :reader int
        :documentation "Integer representation of the IP-ADDRESS.")))

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
           (unless (<= 0 int (1- (expt 2 128)))
             (error "INT is not 0 <= ~a <= (1- (expt 2 128))" int))
           ;; TODO: cleaner way to do this maybe?
           (if (< int (expt 2 32))
               (progn (setf (slot-value ip 'version) 4)
                      (setf (slot-value ip 'str) (ip-int-to-str int)))
               (progn (setf (slot-value ip 'version) 6)
                      (setf (slot-value ip 'str) (ip-int-to-str int 6))))))
        (t (error "Must specify either STR or INT."))))

(defgeneric make-ip-address (str-or-int)
  (:documentation "Make an IP-ADDRESS object from a STRING or INTEGER representation.")
  (:method ((str string))
    (make-instance 'ip-address :str str))
  (:method ((int integer))
    (make-instance 'ip-address :int int))
  (:method ((foo t))
    (declare (ignore foo))
    (error "Must specify either STR or INT.")))

(defmethod print-object ((ip ip-address) out)
  (print-unreadable-object (ip out :type t)
    (format out "~a" (str ip))))

(defclass ip-pair (ip-like)
    ((first-ip :reader first-ip)
     (last-ip :reader last-ip)))

(defgeneric first-ip (ip-pair)
  (:documentation "Returns the first IP-ADDRESS of an IP-NETWORK or IP-RANGE."))
(defgeneric last-ip (ip-pair)
  (:documentation "Returns the last IP-ADDRESS of an IP-NETWORK or IP-RANGE."))

(defclass ip-network (ip-pair)
  ((str :initarg :str :reader str)
   (mask :reader mask)))

(defun make-ip-network (str)
  "Make an IP-NETWORK object from a string STR in CIDR notation, e.g., \"10.20.30.40/24\" or \"ffff::/96\"."
  (make-instance 'ip-network :str str))

(defmethod initialize-instance :after ((net ip-network) &key)
  (destructuring-bind (ip mask) (split-sequence #\/ (str net))
    (let* ((mask (parse-integer mask))
          (first-ip (make-ip-address ip))
          (last-ip (shallow-copy-object first-ip)))
      (mask-ip! first-ip mask :lower)
      (mask-ip! last-ip mask :upper)
      (setf (slot-value net 'version) (version first-ip))
      (when (= 4 (version first-ip))
        (check-type mask (integer 0 32) "in [0, 32] for IPv4 masks"))
      (when (= 6 (version first-ip))
        (check-type mask (integer 0 128) "in [0, 128] for IPv6 masks"))
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
        ((find #\- ip-like-str) (apply #'make-ip-range (split-sequence #\- ip-like-str)))
        (t (make-ip-address ip-like-str))))

(defclass ip-set (ip+)
  ((set :initarg :entries :initform '())))

(defun make-ip-set (set)
  "Make an IP-SET object given a list of IP-LIKEs."
  (check-type set list)
  (dolist (set-element set)
    (check-type set-element ip-like))
  (let ((s (make-instance 'ip-set :entries set)))
    (compact! s)))

(defmethod print-object ((set ip-set) out)
  (print-unreadable-object (set out :type t)
    (format out "(~a)" (length (slot-value set 'set)))))

(defun shallow-copy-object (original)
  (let* ((class (class-of original))
         (copy (allocate-instance class)))
    (dolist (slot (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots class)))
      (when (slot-boundp original slot)
        (setf (slot-value copy slot)
              (slot-value original slot))))
    copy))

;;;; String and integer manipulation and interop.

;; Grabbed from
;; https://stackoverflow.com/questions/5284147/validating-ipv4-addresses-with-regexp
;; and
;; https://stackoverflow.com/questions/53497/regular-expression-that-matches-valid-ipv6-addresses
(defun ipv4-str? (str) (cl-ppcre:scan "^((25[0-5]|(2[0-4]|1\\d|[1-9]|)\\d)\.?\\b){4}$" str))
(defun ipv6-str? (str) (cl-ppcre:scan "(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))" str))

(defun ip-int-to-str-v4 (int)
  (let ((bytes (loop for offset from 24 downto 0 by 8 collect (ldb (byte 8 offset) int))))
    (str:join "." (mapcar #'write-to-string bytes))))

(defun ip-int-to-str-v6 (int)
  (let ((*print-base* 16)
        (bytes (loop for offset from 112 downto 0 by 16 collect (ldb (byte 16 offset) int))))
    (str:join ":" (mapcar #'write-to-string bytes))))

(defun ip-int-to-str (int &optional (type 4))
  (ecase type
    (4 (ip-int-to-str-v4 int))
    (6 (ip-int-to-str-v6 int))))

(defun expand-ipv6-addr-to-parts (str)
  (let* ((parts (split-sequence #\: str))
         (num-non-empties (count "" parts :test-not #'equal))
         (zeroes (loop repeat (- 8 num-non-empties) collect "0")))
    (mapcar (lambda (x) (parse-integer x :radix 16))
            (remove "" (ax:flatten (substitute zeroes "" parts :test #'equal :count 1)) :test #'equal))))

(defun remove-leading-zeroes (str)
  (let ((parts (split-sequence #\: str)))
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
         (parts (split-sequence #\: str)))
    (multiple-value-bind (len pos) (largest-run "0" parts)
      (let* ((end (+ len pos))
             (parts (if (< len 2)
                        parts
                        (delete "0"
                                (replace parts colon-list :start1 pos :end1 end)
                                :test #'string=
                                :start pos :end end)))
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
  (let ((octets (mapcar #'parse-integer (split-sequence #\. str))))
    (loop for x from 24 downto 0 by 8
          for octet in octets sum (ash octet x))))

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

;;;; CIDRs and ranges interop.

(defgeneric ->ip-range (ip-like)
  (:method ((ip-like ip-address))
    (let ((s (str ip-like)))
      (make-ip-range s s)))
  (:method ((ip-like ip-pair))
    (with-slots (first-ip last-ip) ip-like
      (make-ip-range (str first-ip) (str last-ip))))
  (:method ((ip-like ip-range))
    ip-like)
  (:method ((ip-like t))
    (check-type ip-like ip-like)))

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

;;;; Equality methods.

(defgeneric ip-equal (ip+1 ip+2)
  (:documentation "Returns T if IP+1 and IP+2 represent the same underlying IP address(es), are the same version of IP, and are instances of the same class (one of IP-ADDRESS, IP-PAIR, or IP-SET), or otherwise NIL.")
  (:method ((ip1 ip-address) (ip2 ip-address))
    (and (= (int ip1) (int ip2))
         (= (version ip1) (version ip2))))
  (:method ((p1 ip-pair) (p2 ip-pair))
    (and (eq (class-of p1) (class-of p2))
         (ip= (first-ip p1) (first-ip p2))
         (ip= (last-ip p1) (last-ip p2))))
  (:method ((s1 ip-set) (s2 ip-set))
    (with-slots ((set1 set)) s1
      (with-slots ((set2 set)) s2
        (if (not (= (length set1) (length set2)))
            nil
            (progn (sort set1 #'compare)
                   (sort set2 #'compare)
                   (every #'ip-equal set1 set2))))))
  ;; Default case when the types of the two arguments do not match.
  (:method ((x ip+) (y ip+))
    nil)
  (:method ((ip+1 t) (ip+2 t))
    (check-type ip+1 ip+)
    (check-type ip+2 ip+)))

(defun ip= (ip+1 ip+2)
  "Synonym for IP-EQUAL."
  (ip-equal ip+1 ip+2))

(defgeneric ip-equalp (ip+1 ip+2)
  (:documentation "Returns T if IP+1 and IP+2 represent the same underlying IP address(es), and are the same version of IP, or otherwise NIL. IP-RANGEs or IP-NETWORKs that contain a single IP will be IP-EQUALP to the IP-ADDRESS. See Equality in the README for details.")
  (:method ((ip ip-address) (pair ip-pair))
    (and (= (int ip) (int (first-ip pair)) (int (last-ip pair)))
         (= (version ip) (version pair))))
  (:method ((pair ip-pair) (ip ip-address))
    (ip-equalp ip pair))
  (:method ((p1 ip-pair) (p2 ip-pair))
    (and (ip= (first-ip p1) (first-ip p2))
         (ip= (last-ip p1) (last-ip p2))))
  ;; TODO: Duplicating this just to change IP-EQUAL to IP-EQUALP sucks. I could
  ;; just include a &KEY argument, but that seems clunky. Not sure what to do.
  (:method ((s1 ip-set) (s2 ip-set))
    (with-slots ((set1 set)) s1
      (with-slots ((set2 set)) s2
        (if (not (= (length set1) (length set2)))
            nil
            (progn (sort set1 #'compare)
                   (sort set2 #'compare)
                   (every #'ip-equalp set1 set2))))))
  (:method ((x ip+) (y ip+))
    (ip-equal x y))
  (:method ((ip+1 t) (ip+2 t))
    (check-type ip+1 ip+)
    (check-type ip+2 ip+)))

;;;; Set-like predicates and operations for IP-LIKEs.

(defun subset? (ip-like-1 ip-like-2)
  "Returns T or an IP-LIKE if IP-LIKE-1 is a subset of IP-LIKE-2. This is synonymous with (CONTAINS? IP-LIKE-2 IP-LIKE-1). Otherwise, returns NIL."
  (check-type ip-like-1 ip-like)
  (check-type ip-like-2 ip-like)
  (contains? ip-like-2 ip-like-1))

(defun strict-subset? (ip-like-1 ip-like-2)
  "Returns T or an IP-LIKE if IP-LIKE-1 is a subset and not IP-EQUAL to IP-LIKE-2."
  (check-type ip-like-1 ip-like)
  (check-type ip-like-2 ip-like)
  (and (not (ip= ip-like-1 ip-like-2))
       (subset? ip-like-1 ip-like-2)))

(defun superset? (ip-like-1 ip-like-2)
  "Returns T or an IP-LIKE if IP-LIKE-1 is a superset of IP-LIKE-2. Otherwise, NIL."
  (check-type ip-like-1 ip-like)
  (check-type ip-like-2 ip-like)
  (subset? ip-like-2 ip-like-1))

(defun strict-superset? (ip-like-1 ip-like-2)
  "Returns T or an IP-LIKE if IP-LIKE-1 is a superset of and not IP-EQUAL to IP-LIKE-2. Otherwise, NIL."
  (check-type ip-like-1 ip-like)
  (check-type ip-like-2 ip-like)
  (strict-subset? ip-like-2 ip-like-1))

(defgeneric disjoint? (ip-like-1 ip-like-2)
  (:documentation "Return T if IP-LIKE-1 and IP-LIKE-2 are disjoint, i.e., they share no IP addresses in common.")
  (:method ((i1 ip-address) (i2 ip-address))
    (not (ip= i1 i2)))
  (:method ((p ip-pair) (i ip-address))
    (not (contains? p i)))
  (:method ((i ip-address) (p ip-pair))
    (disjoint? p i))
  (:method ((p1 ip-pair) (p2 ip-pair))
    (or (< (int (last-ip p1))
           (int (first-ip p2)))
        (< (int (last-ip p2))
           (int (first-ip p1)))))
  (:method ((ip-like-1 t) (ip-like-2 t))
    (check-type ip-like-1 ip-like)
    (check-type ip-like-2 ip-like)))

(defgeneric contiguous? (ip-like-1 ip-like-2)
  (:documentation "Returns T if IP-LIKE-1 and IP-LIKE-2 are contiguous, NIL otherwise.")
  (:method ((i1 ip-address) (i2 ip-address))
    (= 1 (abs (- (int i1) (int i2)))))
  (:method ((p ip-pair) (i ip-address))
    (or (= 1 (abs (- (int (first-ip p)) (int i))))
        (= 1 (abs (- (int (last-ip p)) (int i))))))
  (:method ((i ip-address) (p ip-pair))
    (contiguous? p i))
  (:method ((p1 ip-pair) (p2 ip-pair))
    (or (= 1 (abs (- (int (last-ip p1))
                     (int (first-ip p2)))))
        (= 1 (abs (- (int (last-ip p2))
                     (int (first-ip p1)))))))
  (:method ((ip-like-1 t) (ip-like-2 t))
    (check-type ip-like-1 ip-like)
    (check-type ip-like-2 ip-like)))

(defgeneric %compare (ip-like-1 ip-like-2)
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

(defun compare (ip-like-1 ip-like-2)
  ;; Explicitly make v4 always less than v6 so when we sort we are consistent
  ;; when two IPs have the same value but different versions, e.g., #I("::") vs.
  ;; #I("0.0.0.0").
  (check-type ip-like-1 ip-like)
  (check-type ip-like-2 ip-like)
  (if (= (version ip-like-1) (version ip-like-2))
      (%compare ip-like-1 ip-like-2)
      (< (version ip-like-1) (version ip-like-2))))

(defgeneric intersect (ip-like-1 ip-like-2)
  (:method ((ip1 ip-address) (ip2 ip-address))
    (when (ip= ip1 ip2)
      ip1))
  (:method ((ip ip-address) (p ip-pair))
    (when (contains? p ip)
      ip))
  (:method ((p ip-pair) (ip ip-address))
    (intersect ip p))
  (:method ((p1 ip-pair) (p2 ip-pair))
    (cond ((contains? p1 p2) p2)
          ((contains? p2 p1) p1)
          (t nil)))
  (:method ((ip-like-1 t) (ip-like-2 t))
    (check-type ip-like-1 ip-like)
    (check-type ip-like-2 ip-like)))

(defun compact! (set)
  "Compact an IP-SET's internal data structure to remove duplicates and IP-LIKEs
that are subsets of other IP-LIKEs already contained within the internal SET."
  (with-slots (set) set
    (setf set (delete-duplicates (sort set #'compare)
                                 :test #'subset? :from-end t)))
  set)

;;;; Methods for CONTAINS? and SIZE for IP+ subclasses.

;; To make CONTAINS? play nice with MEMBER.
(defun in-set? (ip ip-block)
  (contains? ip-block ip))

(defgeneric contains? (ip+ ip-like)
  (:documentation "Returns T or an IP-LIKE if IP+ contains IP-LIKE, NIL otherwise.")
  (:method ((ip1 ip-address) (ip2 ip-address))
    (ip= ip1 ip2))
  (:method ((pair ip-pair) (ip ip-address))
    (and (= (version pair) (version ip))
         (<= (int (first-ip pair)) (int ip) (int (last-ip pair)))))
  (:method ((pair1 ip-pair) (pair2 ip-pair))
    (and (= (version pair1) (version pair2))
         (<= (int (first-ip pair1))
             (int (first-ip pair2))
             (int (last-ip pair2))
             (int (last-ip pair1)))))
  (:method ((ip ip-address) (pair ip-pair))
    (and (= (version ip) (version pair))
         (= (int ip) (int (first-ip pair)) (int (last-ip pair)))))
  (:method ((set ip-set) (ip-like ip-like))
    (car (member ip-like (slot-value set 'set) :test #'in-set?)))
  (:method ((ip+ t) (ip-like t))
    (check-type ip+ ip+)
    (check-type ip-like ip-like)))

(defgeneric size (ip+)
  (:documentation "Returns an INTEGER of the number of IP addresses contained in IP+.")
  (:method ((ip ip-address)) 1)
  (:method ((pair ip-pair))
    (1+ (- (int (last-ip pair))
           (int (first-ip pair)))))
  (:method ((set ip-set))
    (with-slots (set) set
     (reduce #'+ (mapcar #'size set))))
  (:method ((ip+ t))
    (check-type ip+ ip+)))

;;;; Functions for adding and removing IP-LIKEs to an IP-SET.

(defun %addnew! (set ip-like)
  (check-type set ip-set)
  (check-type ip-like ip-like)
  (with-slots (set) set
    (if (loop with changed? = nil
              for sub on set
              for (x) = sub
              do (cond ((subset? x ip-like)
                        (setf (car sub) ip-like
                              changed? t))
                       ((superset? x ip-like)
                        (setf changed? t)))
              finally
                 (return changed?))
        set
        (push ip-like set))))

(defun addnew! (set &rest ip-likes)
  "Push IP-LIKES to IP-SET SET if they are not already a member or a subset of a member of SET. Returns the modified IP-SET."
  (loop for ip-like in ip-likes do (%addnew! set ip-like))
  set)

(defun addnew (set &rest ip-likes)
  "Creates a fresh IP-SET that contains the original contents of SET as well as the IP-LIKES that are not already a member or a subset of a member of SET. Returns the fresh IP-SET."
  (let ((new-set (shallow-copy-object set)))
    (apply #'addnew! new-set ip-likes)
    new-set))

(defun add! (set &rest ip-likes)
  "Prepend in place IP-LIKES to the IP-SET SET. Returns the modified IP-SET."
  (with-slots (set) set
    (setf set (append ip-likes set)))
  set)

(defun add (set &rest ip-likes)
  "Creates a copy of SET with IP-LIKES prepended."
  (let ((new-set (shallow-copy-object set)))
    (apply #'add! new-set ip-likes)
    new-set))

(defun subtract (ip-like-1 ip-like-2)
  "Return a fresh list of IP-RANGEs that represents IP-LIKE-1 after removing all IPs in IP-LIKE-2. If IP-LIKE-1 and IP-LIKE-2 are disjoint, a list containing the original IP-LIKE-1 is returned."
  (check-type ip-like-1 ip-like)
  (check-type ip-like-2 ip-like)
  (if (= (version ip-like-1) (version ip-like-2))
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
                                             (int (last-ip r1))))))))))
      (list ip-like-1)))

(defun sub! (set &rest ip-likes)
  "Remove IP-LIKES from IP-SET SET in place. IP-LIKES that are a member or superset of a member of SET are removed. IP-LIKES that are a subset of a member of SET are SUBTRACTed in place."
  (check-type set ip-set)
  (if (null ip-likes)
      set
      (progn
        (with-slots (set) set
          (setf set
                (ax:flatten
                 (loop for range in set
                       collect
                       (subtract range (first ip-likes))))))
        (apply #'sub! set (rest ip-likes)))))

(defun sub (set &rest ip-likes)
  "Like SUB! but return a fresh IP-SET without modifying the argument SET in place."
  (let ((new-set (shallow-copy-object set)))
    (apply #'sub! new-set ip-likes)
    new-set))

;;;; IP-SET set theoretic operations.

(defun ip-set-union (&rest ip-sets)
  "Returns a fresh IP-SET that is the set union of all IP-SETS."
  (let ((res (make-ip-set nil)))
    (loop for ip-set in ip-sets do
      (with-slots ((set1 set)) res
        (with-slots ((set2 set)) ip-set
          ;; APPEND does not copy the last argument, CONCATENATE does.
          (progn (check-type ip-set ip-set)
                 (setf set1 (concatenate 'list set1 set2))))))
    (compact! res)))

(defun ip-set-intersection (&rest ip-sets)
  "Returns a fresh IP-SET that is the set intersection of all IP-SETS."
  (if (null ip-sets)
      (make-ip-set nil)
      (let ((inter (shallow-copy-object (first ip-sets))))
        (check-type inter ip-set)
        (loop for ip-set in (rest ip-sets) do
          (progn (check-type ip-set ip-set)
                 (setf (slot-value inter 'set)
                       (remove nil (loop for x in (slot-value inter 'set)
                                         append (loop for y in (slot-value ip-set 'set)
                                                      collect (intersect x y)))))))
        inter)))

(defun ip-set-difference (&rest ip-sets)
  "Returns a fresh IP-SET that is the set difference of (first IP-SETS) from (rest IP-SETS)."
  (if (null ip-sets)
      (make-ip-set nil)
      (let ((diff (shallow-copy-object (first ip-sets))))
        (check-type diff ip-set)
        (loop for ip-set in (rest ip-sets) do
          (progn (check-type ip-set ip-set)
                 (with-slots ((set1 set)) diff
                   (with-slots ((set2 set)) ip-set
                     (setf set1
                           ;; We must compute the difference pair-wise
                           ;; because the subtrahend may be a superset of
                           ;; multiple minuends.
                           (loop for x in set1
                                 append (loop for y in set2
                                              with new-xs = (list x)
                                              do (setf new-xs (ax:mappend (lambda (new-x)
                                                                            (subtract new-x y))
                                                                          new-xs))
                                              finally (return new-xs))))))))
        diff)))

(defun ip-set-symmetric-difference (&rest ip-sets)
  "Returns a fresh IP-SET that is the set symmetric difference of IP-SETS, i.e., the difference of the union and intersection of IP-SETS."
  (ip-set-difference (apply #'ip-set-union ip-sets)
                     (apply #'ip-set-intersection ip-sets)))
