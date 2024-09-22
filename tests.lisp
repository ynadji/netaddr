(in-package :cl-user)
(defpackage netaddr/tests
  (:use #:cl #:netaddr #:arrow-macros)
  (:import-from #:fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is
                #:*test-dribble*)
  (:export #:tests))

(in-package :netaddr/tests)

(def-suite tests)
(in-suite tests)

(enable-ip-syntax)

(defun random-ipv4-str ()
  (format nil "~{~a~^.~}" (loop repeat 4 collect (random 256))))

(defun random-ipv6-str ()
  (format nil "~{~x~^:~}" (loop repeat 8 collect (random 65536))))

(defun random-ipv4-network ()
  #I((format nil "~a/~a" (random-ipv4-str) (random 33))))

(defun random-ipv6-network ()
  #I((format nil "~a/~a" (random-ipv6-str) (random 129))))

(test error-checking
  (is (null (ignore-errors (make-ip-address "not-and-ip-address"))))
  (is (null (ignore-errors (make-instance 'ip-address))))
  (is (null (ignore-errors (make-ip-address nil))))
  (is (null (ignore-errors (make-ip-address '(1 2 3)))))
  (is (null (ignore-errors (make-ip-range "1.1.1.1" "0.0.0.0"))))
  (is (null (ignore-errors (make-ip-range "::ffff" "::"))))
  (is (null (ignore-errors (make-ip-network "wutang/16"))))
  (is (null (ignore-errors (make-ip-network "1.2.3.4/wutang"))))
  (is (null (ignore-errors (make-ip-network "0.0.0.0/33"))))
  (is (null (ignore-errors (make-ip-network "0.0.0.0/-1"))))
  (is (null (ignore-errors (make-ip-network "::/-1"))))
  (is (null (ignore-errors (make-ip-network "::/129"))))
  (is (null (ignore-errors (make-ip-set "::/129"))))
  (is (null (ignore-errors (make-ip-set '("foo" "bar"))))))

(test subnet-corrects-str
  (is (string= "10.20.30.0/24" (str (make-ip-network "10.20.30.40/24"))))
  (is (string= "0.0.0.0/0" (str (make-ip-network "255.255.255.255/0")))))

(test ip-int-to-str
  (is (string= "0.0.0.0" (netaddr::ip-int-to-str 0)))
  (is (string= "255.255.255.255" (netaddr::ip-int-to-str (- (expt 2 32) 1))))
  ;; This is excessive, but I want to be sure when we don't break this function
  ;; for now.
  (let ((*test-dribble* nil))
    (loop for fourth upto 255 do
      (loop repeat 13 for third = (random 256) do
        (loop repeat 13 for second = (random 256) do
          (loop repeat 13 for first = (random 256) do
            (is (string= (format nil "~a.~a.~a.~a" first second third fourth)
                         (netaddr::ip-int-to-str (+ (ash first 24)
                                                    (ash second 16)
                                                    (ash third 8)
                                                    fourth))))))))))

(test ipv4-v6-boundary
  (is (ip= (make-ip-address (1- (expt 2 32))) #I("255.255.255.255")))
  (is (ip= (make-ip-address (expt 2 32)) #I("0:0:0:0:0:1:0:0")))
  (is (ip= (make-ip-address (1+ (expt 2 32))) #I("0:0:0:0:0:1:0:1"))))

(test contains?
  (let ((net4 (make-ip-network "10.20.30.40/24"))
        (range4 (make-ip-range "192.168.0.0" "192.168.125.255"))
        (net6 (make-ip-network "dada:beef::/64"))
        (range6 (make-ip-range "::1" "babe::")))
    (is (contains? net4 #I("10.20.30.0")))
    (is (contains? net4 #I("10.20.30.255")))
    (is (not (contains? net4 #I("10.20.31.0"))))
    (is (not (contains? net4 #I("0.0.0.0"))))
    (is (not (contains? net4 #I("255.255.255.255"))))
    (is (not (contains? net4 #I("10.20.29.255"))))

    (is (contains? range4 #I("192.168.0.0")))
    (is (contains? range4 #I("192.168.125.255")))
    (is (contains? range4 #I("192.168.13.79")))
    (is (not (contains? range4 #I("192.167.255.255"))))
    (is (not (contains? range4 #I("192.168.126.0"))))
    (is (not (contains? range4 #I("0.0.0.0"))))
    (is (not (contains? range4 #I("255.255.255.255"))))

    (is (contains? net6 #I("dada:beef::")))
    (is (contains? net6 #I("dada:beef::ffff:ffff:ffff:ffff")))
    (is (contains? net6 #I("dada:beef::ffff:ffff:ffff:ffff")))
    (is (contains? net6 #I("dada:beef:0:0:1::")))
    (is (not (contains? net6 #I("dada:beef::1:ffff:ffff:ffff:ffff"))))
    (is (not (contains? net6 #I("dada:beef:ffff:0:ffff:ffff:ffff:ffff"))))

    (is (not (contains? range6 #I("::"))))
    (is (contains? range6 #I("::1")))
    (is (contains? range6 #I("::2")))
    (is (contains? range6 #I("babe::")))
    (is (not (contains? range6 #I("babe::1"))))

    (is (not (contains? #I("::/96") #I("0.0.0.0"))))
    (is (not (contains? #I("::/96") #I("255.255.255.255"))))))

(test compress-ipv6-str
  (is (string= "0:0:1::" (netaddr::compress-ipv6-str "0:0:1:0:0:0:0:0")))
  (is (string= "0:0:1::" (netaddr::compress-ipv6-str "0000:0000:0001:0000:0000:0000:0000:0000")))
  (is (string= "0:0:1::1" (netaddr::compress-ipv6-str "0:0:1:0:0:0:0:1")))
  (is (string= "::1:0:0:1:0:0" (netaddr::compress-ipv6-str "0:0:1:0:0:1:0:0")))
  (is (string= "::" (netaddr::compress-ipv6-str "0:0:0:0:0:0:0:0")))
  (is (string= "::" (netaddr::compress-ipv6-str "0000:0000:0000:0000:0000:0000:0000:0000")))
  (is (string= "1:23:444:1000:123:100::" (netaddr::compress-ipv6-str "0001:0023:0444:1000:0123:0100:0000:0000"))))

(test size
  (is (= 1 (-> "0.0.0.0/32" make-ip-network size)))
  (is (= (expt 2 32) (-> "0.0.0.0/0" make-ip-network size)))
  (is (= 1 (-> "::/128" make-ip-network size)))
  (is (= (expt 2 128) (-> "::/0" make-ip-network size))))

(test range->cidrs
  (is (= (size #I("::-ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffe"))
         (apply #'+ (mapcar #'size (netaddr::range->cidrs #I("::-ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffe"))))))
  (is (= (size #I("0.0.0.0-255.255.255.254"))
         (apply #'+ (mapcar #'size (netaddr::range->cidrs #I("0.0.0.0-255.255.255.254")))))))

(test ->ip-range
  (loop repeat 100 do
    (let* ((ip4 #I((random-ipv4-str)))
           (ip4r (netaddr::->ip-range ip4))
           (ip6 #I((random-ipv6-str)))
           (ip6r (netaddr::->ip-range ip6))
           (net4 (random-ipv4-network))
           (net4r (netaddr::->ip-range net4))
           (net6 (random-ipv6-network))
           (net6r (netaddr::->ip-range net6)))
      (is (= (size ip4) (size ip4r)))
      (is (= (netaddr::int ip4) (netaddr::int (netaddr::first-ip ip4r)) (netaddr::int (netaddr::last-ip ip4r))))
      (is (= (size ip6) (size ip6r)))
      (is (= (netaddr::int ip6) (netaddr::int (netaddr::first-ip ip6r)) (netaddr::int (netaddr::last-ip ip6r))))

      (is (= (size net4) (size net4r)))
      (is (= (netaddr::int (netaddr::first-ip net4)) (netaddr::int (netaddr::first-ip net4r))))
      (is (= (netaddr::int (netaddr::last-ip net4)) (netaddr::int (netaddr::last-ip net4r))))
      (is (= (size net6) (size net6r)))
      (is (= (netaddr::int (netaddr::first-ip net6)) (netaddr::int (netaddr::first-ip net6r))))
      (is (= (netaddr::int (netaddr::last-ip net6)) (netaddr::int (netaddr::last-ip net6r)))))))

(test contiguous?
  (is (contiguous? #I("10.0.0.0/24") #I("10.0.1.0/24")))
  (is (contiguous? #I("0.0.0.0/1") #I("128.0.0.0/1")))
  (is (contiguous? #I("::/1") #I("8000::/1")))
  (is (not (contiguous? #I("10.0.0.0/24") #I("10.0.2.0/24"))))
  (is (contiguous? #I("0.0.0.0-1.0.0.0") #I("1.0.0.1-2.0.0.0")))
  (is (contiguous? #I("0.0.0.0-1.0.0.0") #I("1.0.0.1-1.0.0.1")))
  (is (not (contiguous? #I("0.0.0.0-1.0.0.0") #I("1.0.0.0-1.0.0.1"))))
  (is (not (contiguous? #I("0.0.0.0-1.0.0.0") #I("1.0.0.0/8"))))
  (is (contiguous? #I("0.0.0.0-0.255.255.255") #I("1.0.0.0/8")))
  (is (contiguous? #I("1.0.0.0/8") #I("0.0.0.0-0.255.255.255")))
  (is (contiguous? #I("1.2.3.4/32") #I("1.2.3.5/32")))
  (is (contiguous? #I("1.2.3.4") #I("1.2.3.5")))
  (is (contiguous? #I("10.0.0.0/24") #I("9.255.255.255")))
  (is (contiguous? #I("10.0.1.0") #I("10.0.0.0/24")))
  (is (not (contiguous? #I("10.0.0.0/24") #I("10.0.0.41"))))
  (is (not (contiguous? #I("255.255.255.255") #I("0.0.0.0")))))

;; Lots of duplicates from above since CONTIGUOUS? networks are by definition
;; DISJOINT?.
(test disjoint?
  (is (disjoint? #I("10.0.0.0/24") #I("10.0.1.0/24")))
  (is (disjoint? #I("0.0.0.0/1") #I("128.0.0.0/1")))
  (is (disjoint? #I("::/1") #I("8000::/1")))
  (is (disjoint? #I("10.0.0.0/24") #I("10.0.2.0/24")))
  (is (disjoint? #I("0.0.0.0-1.0.0.0") #I("1.0.0.1-2.0.0.0")))
  (is (disjoint? #I("0.0.0.0-1.0.0.0") #I("1.0.0.1-1.0.0.1")))
  (is (not (disjoint? #I("0.0.0.0-1.0.0.0") #I("1.0.0.0-1.0.0.1"))))
  (is (not (disjoint? #I("0.0.0.0-1.0.0.0") #I("1.0.0.0/8"))))
  (is (disjoint? #I("0.0.0.0-0.255.255.255") #I("1.0.0.0/8")))
  (is (disjoint? #I("1.0.0.0/8") #I("0.0.0.0-0.255.255.255")))
  (is (disjoint? #I("1.2.3.4/32") #I("1.2.3.5/32")))
  (is (disjoint? #I("1.2.3.4") #I("1.2.3.5"))))

(test subset?
  (loop for x from 31 downto 0 do
    (is (subset? #I((format nil "0.0.0.0/~a" (1+ x)))
                 #I((format nil "0.0.0.0/~a" x)))))
  (loop for x from 127 downto 0 do
    (is (subset? #I((format nil "::/~a" (1+ x)))
                 #I((format nil "::/~a" x)))))
  (let ((r4 (make-ip-range "0.0.0.0" "255.255.255.255"))
        (r6 (make-ip-range "::" "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")))
    (loop repeat 100 do
      (progn
        (is (subset? #I((random-ipv4-str)) r4))
        (is (subset? #I((random-ipv6-str)) r6))
        (is (subset? (random-ipv4-network) r4))
        (is (subset? (random-ipv6-network) r6))))))

(test superset?
  (loop for x from 31 downto 0 do
    (is (superset? #I((format nil "0.0.0.0/~a" x))
                   #I((format nil "0.0.0.0/~a" (1+ x))))))
  (loop for x from 127 downto 0 do
    (is (superset? #I((format nil "::/~a" x))
                   #I((format nil "::/~a" (1+ x))))))
  ;; Add for ranges, IPs, mixtures, etc.
  (let ((r4 (make-ip-range "0.0.0.0" "255.255.255.255"))
        (r6 (make-ip-range "::" "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")))
    (loop repeat 100 do
      (progn
        (is (superset? r4 #I((random-ipv4-str))))
        (is (superset? r6 #I((random-ipv6-str))))
        (is (superset? r4 (random-ipv4-network)))
        (is (superset? r6 (random-ipv6-network)))))))

(test ip=/ip-equal
  (is (ip= #I("1.2.3.4") #I("1.2.3.4")))
  (is (not (ip= #I("1.2.3.4") #I("1.2.3.4/32"))))
  (is (not (ip= #I("1.2.3.4") #I("1.2.3.4-1.2.3.4"))))
  (let ((s1 (make-instance 'netaddr::ip-set))
        (s2 (make-instance 'netaddr::ip-set)))
   (loop repeat 100 do
     (let ((net4 (random-ipv4-network))
           (net6 (random-ipv6-network)))
       (is (ip= net4 net4))
       (is (ip-equal (netaddr::->ip-range net4) net4))
       (is (ip-equal net4 (netaddr::->ip-range net4)))

       (is (ip= net6 net6))
       (is (ip-equal (netaddr::->ip-range net6) net6))
       (is (ip-equal net6 (netaddr::->ip-range net6)))

       (is (ip= s1 s2))
       (add! s1 net4)
       (add! s1 net6)
       (add! s2 (netaddr::->ip-range net4))
       (add! s2 (netaddr::->ip-range net6))))))

(test ip-equalp
  (is (ip-equalp #I("1.2.3.4") #I("1.2.3.4")))
  (is (ip-equalp #I("1.2.3.4") #I("1.2.3.4/32")))
  (is (ip-equalp #I("1.2.3.4/32") #I("1.2.3.4")))
  (is (ip-equalp #I("1.2.3.4") #I("1.2.3.4-1.2.3.4")))
  (is (ip-equalp #I("1.2.3.4-1.2.3.4") #I("1.2.3.4")))
  (loop repeat 100 do
    (let ((str4 (random-ipv4-str))
          (str6 (random-ipv6-str)))
      (is (ip-equalp #I(str4) #I((format nil "~a/~a" str4 32))))
      (is (not (ip-equalp #I(str4) #I((format nil "~a/~a" str4 31)))))
      (is (ip-equalp #I(str6) #I((format nil "~a/~a" str6 128))))
      (is (not (ip-equalp #I(str6) #I((format nil "~a/~a" str6 127))))))))

(test subtract
  (let* ((cidr4 #I("10.0.0.0/8"))
         (s1 (netaddr::subtract cidr4 #I("10.0.0.0")))
         (s2 (netaddr::subtract cidr4 #I("10.127.0.0")))
         (s3 (netaddr::subtract cidr4 #I("10.0.0.0-11.0.0.0"))))
    (is (ip= #I("10.0.0.1-10.255.255.255") (first s1)))
    (is (and (ip= #I("10.0.0.0-10.126.255.255") (first s2))
             (ip= #I("10.127.0.1-10.255.255.255") (second s2))))
    (is (null s3))
    ;; add ipv6 ones, range ones, ip-address ones.
    ))

(test sub
  (let* ((s (make-ip-set #I("10.0.0.0/24" "1.1.1.1")))
         (orig (netaddr::shallow-copy-object s)))
    (is (ip= s orig))
    (is (ip= (sub s #I("10.0.0.0/24")) (make-ip-set (list #I("1.1.1.1")))))
    (is (ip= s orig))
    (is (ip= (sub s #I("10.0.0.0/8")) (make-ip-set (list #I("1.1.1.1")))))
    (is (ip= (sub s #I("1.0.0.0/8")) (make-ip-set (list #I("10.0.0.0/24")))))))

(test add
  (let* ((s (make-ip-set #I("0.0.0.0/24" "1.1.1.1"))))
    (loop for x upto 255 do
      (is (= 3 (length (slot-value (add s (make-ip-address x)) 'set))))
      (is (ip= s (make-ip-set #I("0.0.0.0/24" "1.1.1.1")))))))

(test addnew
  (let* ((s (make-ip-set #I("10.0.0.0/24" "1.1.1.1")))
         (orig (netaddr::shallow-copy-object s)))
    (is (ip= s orig))
    (is (ip= (addnew s #I("10.0.0.0/8")) (make-ip-set #I("10.0.0.0/8" "1.1.1.1"))))
    (is (ip= (addnew s #I("10.0.0.0/24")) orig))
    (is (ip= (addnew s #I("10.0.0.0/27")) orig))
    (is (ip= (addnew s #I("10.0.0.128")) orig))
    (is (ip= (addnew s #I("10.0.0.0/8")) (make-ip-set #I("10.0.0.0/8" "1.1.1.1"))))))

(test ip-set
  (let* ((s (make-ip-set #I("10.0.0.0/24" "1.1.1.1")))
         (orig (netaddr::shallow-copy-object s)))
    (is (ip= s orig))
    (is (contains? s #I("10.0.0.0/25")))
    (is (contains? s #I("1.1.1.1")))
    (is (not (contains? s #I("192.168.0.0"))))
    (add! s #I("192.168.0.0/16"))
    (is (contains? s #I("192.168.0.0")))
    (is (contains? s #I("192.168.0.0-192.168.255.255")))
    (sub! s #I("192.168.0.0/24"))
    (is (not (contains? s #I("192.168.0.0"))))
    (is (not (contains? s #I("192.168.0.0-192.168.255.255"))))
    (is (contains? s #I("192.168.1.0")))
    (is (contains? s #I("192.168.1.0-192.168.255.255")))
    (is (ip= (make-ip-set #I("10.0.0.0/8" "10.0.0.0/7" "10.0.0.0/6" "10.0.0.0/5" "10.0.0.0/4" "10.0.0.0/3"))
             (make-ip-set (list #I("0.0.0.0/3")))))
    (is (ip= (make-ip-set (list #I("0.0.0.0/3")))
             (make-ip-set #I("10.0.0.0/8" "10.0.0.0/7" "10.0.0.0/6" "10.0.0.0/5" "10.0.0.0/4" "10.0.0.0/3"))))))

(test ip-set-union
  (is (ip= (ip-set-union (make-ip-set #I("10.0.0.0/24" "192.168.0.0/24" "ffff::/128"))
                         (make-ip-set #I("10.0.0.0/8" "192.168.0.0/16" "ffff::/96")))
           (make-ip-set #I("10.0.0.0/8" "192.168.0.0/16" "ffff::/96")))))

(test ip-set-intersection
  (is (ip= (ip-set-intersection (make-ip-set #I("10.0.0.0/24" "192.168.0.0/24" "ffff::/128"))
                                (make-ip-set #I("10.0.0.0/8" "192.168.0.0/16" "ffff::/96")))
           (make-ip-set #I("10.0.0.0/24" "192.168.0.0/24" "ffff::/128")))))

(test ip-set-difference
  (is (ip= (ip-set-difference (make-ip-set #I("10.0.0.0/8" "192.168.0.0/24" "192.168.1.0/24" "2.3.4.5"))
                              (make-ip-set #I("1.2.3.4" "10.0.0.0/24" "192.168.0.0/16" "10.127.0.0-10.255.255.253")))
           (make-ip-set #I("2.3.4.5" "10.0.1.0-10.126.255.255" "10.255.255.254-10.255.255.255")))))

(test ip-set-symmetric-difference
  (is (ip= (ip-set-symmetric-difference (make-ip-set #I("10.0.0.0/8" "192.168.0.0/24" "192.168.1.0/24" "2.3.4.5"))
                                        (make-ip-set #I("1.2.3.4" "10.0.0.0/24" "192.168.0.0/16" "10.127.0.0-10.255.255.253")))
           (make-ip-set #I("1.2.3.4" "2.3.4.5" "10.0.1.0-10.126.255.255" "10.255.255.254-10.255.255.255" "192.168.2.0-192.168.255.255")))))

(test public?
  (is (not (public? #I("192.168.0.0/16"))))
  (is (not (public? #I("192.168.0.0"))))
  (is (public? #I("192.167.255.255")))
  (is (not (public? #I("192.168.255.255"))))
  (is (public? #I("192.169.0.0")))

  (is (not (public? #I("10.0.0.0/8"))))
  (is (not (public? #I("10.0.0.0"))))
  (is (public? #I("9.255.255.255")))
  (is (not (public? #I("10.255.255.255"))))
  (is (public? #I("11.0.0.0")))

  (is (not (public? #I("172.16.0.0/12"))))
  (is (not (public? #I("172.16.0.0"))))
  (is (public? #I("172.15.255.255")))
  (is (not (public? #I("172.31.255.255"))))
  (is (public? #I("172.32.0.0"))))

(test private?
  (is (private? #I("192.168.0.0/16")))
  (is (private? #I("192.168.0.0")))
  (is (not (private? #I("192.167.255.255"))))
  (is (private? #I("192.168.255.255")))
  (is (not (private? #I("192.169.0.0"))))

  (is (private? #I("10.0.0.0/8")))
  (is (private? #I("10.0.0.0")))
  (is (not (private? #I("9.255.255.255"))))
  (is (private? #I("10.255.255.255")))
  (is (not (private? #I("11.0.0.0"))))

  (is (private? #I("172.16.0.0/12")))
  (is (private? #I("172.16.0.0")))
  (is (not (private? #I("172.15.255.255"))))
  (is (private? #I("172.31.255.255")))
  (is (not (private? #I("172.32.0.0")))))

(test reserved?
  )
