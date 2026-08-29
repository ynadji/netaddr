(defpackage netaddr/tests
  (:use #:cl #:netaddr)
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
  (make-ip-like (format nil "~a/~a" (random-ipv4-str) (random 33))))

(defun random-ipv6-network ()
  (make-ip-like (format nil "~a/~a" (random-ipv6-str) (random 129))))

(test error-checking
  (is (null (ignore-errors (make-ip-address "not-and-ip-address"))))
  (dolist (str '("" ":" ":::" "1:::2" "1::2::3" "1:2:3:4:5:6:7:8:9" "1:2:3:4:5:6:7"
                 "12345::" "-1::" " ::1" "::1 " "g::" "::ffff:1.2.3.4" "fe80::1%eth0"
                 "256.0.0.0" "01.0.0.0" "1.2.3" "1.2.3.4.5" "1..2.3" ".1.2.3" "1.2.3."
                 "1.42.250.2113" "+1.2.3.4" "1.2.3.4 "))
    (is (null (ignore-errors (make-ip-address str)))))
  (is (null (ignore-errors (make-instance 'ip-address))))
  (is (null (ignore-errors (make-ip-address nil))))
  (is (null (ignore-errors (make-ip-address -1))))
  (is (null (ignore-errors (make-ip-address (expt 2 128)))))
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
  (is (null (ignore-errors (make-ip-set '("foo" "bar")))))
  (is (null (ignore-errors (apply-mask (make-ip-address "0.0.0.0") 64))))
  (is (null (ignore-errors (apply-mask (make-ip-address "::") 129)))))

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

(test apply-mask
  (is (ip= (make-ip-network "10.0.0.0/24") (apply-mask (make-ip-address "10.0.0.123") 24)))
  (is (ip= (make-ip-network "::/128") (apply-mask (make-ip-address "::") 128)))
  (is (ip= (make-ip-network "::dada:beef/64") (apply-mask (make-ip-address "::cafe:babe") 64))))

(test ipv4-v6-boundary
  (is (ip= (make-ip-address (1- (expt 2 32))) #i255.255.255.255))
  (is (ip= (make-ip-address (expt 2 32)) #i0:0:0:0:0:1:0:0))
  (is (ip= (make-ip-address (1+ (expt 2 32))) #i0:0:0:0:0:1:0:1)))

(test contains?
  (let ((net4 (make-ip-network "10.20.30.40/24"))
        (range4 (make-ip-range "192.168.0.0" "192.168.125.255"))
        (net6 (make-ip-network "dada:beef::/64"))
        (range6 (make-ip-range "::1" "babe::")))
    (is (contains? net4 #i10.20.30.0))
    (is (contains? net4 #i10.20.30.255))
    (is (not (contains? net4 #i10.20.31.0)))
    (is (not (contains? net4 #i0.0.0.0)))
    (is (not (contains? net4 #i255.255.255.255)))
    (is (not (contains? net4 #i10.20.29.255)))

    (is (contains? range4 #i192.168.0.0))
    (is (contains? range4 #i192.168.125.255))
    (is (contains? range4 #i192.168.13.79))
    (is (not (contains? range4 #i192.167.255.255)))
    (is (not (contains? range4 #i192.168.126.0)))
    (is (not (contains? range4 #i0.0.0.0)))
    (is (not (contains? range4 #i255.255.255.255)))

    (is (contains? net6 #idada:beef::))
    (is (contains? net6 #idada:beef::ffff:ffff:ffff:ffff))
    (is (contains? net6 #idada:beef::ffff:ffff:ffff:ffff))
    (is (contains? net6 #idada:beef:0:0:1::))
    (is (not (contains? net6 #idada:beef::1:ffff:ffff:ffff:ffff)))
    (is (not (contains? net6 #idada:beef:ffff:0:ffff:ffff:ffff:ffff)))

    (is (not (contains? range6 #i::)))
    (is (contains? range6 #i::1))
    (is (contains? range6 #i::2))
    (is (contains? range6 #ibabe::))
    (is (not (contains? range6 #ibabe::1)))

    (is (not (contains? #i::/96 #i0.0.0.0)))
    (is (not (contains? #i::/96 #i255.255.255.255)))))

(test compress-ipv6-str
  (is (string= "0:0:1::" (netaddr::compress-ipv6-str "0:0:1:0:0:0:0:0")))
  (is (string= "0:0:1::" (netaddr::compress-ipv6-str "0000:0000:0001:0000:0000:0000:0000:0000")))
  (is (string= "0:0:1::1" (netaddr::compress-ipv6-str "0:0:1:0:0:0:0:1")))
  (is (string= "::1:0:0:1:0:0" (netaddr::compress-ipv6-str "0:0:1:0:0:1:0:0")))
  (is (string= "::" (netaddr::compress-ipv6-str "0:0:0:0:0:0:0:0")))
  (is (string= "::" (netaddr::compress-ipv6-str "0000:0000:0000:0000:0000:0000:0000:0000")))
  (is (string= "1:23:444:1000:123:100::" (netaddr::compress-ipv6-str "0001:0023:0444:1000:0123:0100:0000:0000"))))

(test size
  (is (= 1 (size #i0.0.0.0/32)))
  (is (= (expt 2 32) (size #i0.0.0.0/0)))
  (is (= 1 (size #i::/128)))
  (is (= (expt 2 128) (size #i::/0))))

(test range->cidrs
  (is (= (size #i::-ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffe)
         (apply #'+ (mapcar #'size (netaddr::range->cidrs #i::-ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffe)))))
  (is (= (size #i0.0.0.0-255.255.255.254)
         (apply #'+ (mapcar #'size (netaddr::range->cidrs #i0.0.0.0-255.255.255.254)))))
  (dolist (range #i(2001:db8::-2001:db8::8
                    2001:db8::-2001:db8::10
                    2001:db8::-2001:db8::1:0
                    2001:db8::1-2001:db8::9
                    2001:db8::1-2001:db8::1
                    ::-::9
                    ::1-::9
                    10.0.0.0-10.0.0.8
                    10.0.0.0-10.0.0.16
                    10.0.0.1-10.0.0.9
                    10.0.0.255-10.0.1.0))
    (let ((cidrs (netaddr::range->cidrs range)))
      (is (= (size range) (apply #'+ (mapcar #'size cidrs))))
      (is (ip-equal (first-ip range) (first-ip (first cidrs))))
      (is (ip-equal (last-ip range) (last-ip (car (last cidrs)))))
      ;; Contiguous and non-overlapping.
      (is (loop for (a b) on cidrs while b
                always (= (1+ (int (last-ip a))) (int (first-ip b)))))
      (is (every (lambda (net) (= (version range) (version net))) cidrs))))
  (is (equal '("10.0.0.1/32" "10.0.0.2/31" "10.0.0.4/30" "10.0.0.8/31")
             (mapcar #'str (netaddr::range->cidrs #i10.0.0.1-10.0.0.9))))
  (is (equal '("::/125" "::8/127")
             (mapcar #'str (netaddr::range->cidrs #i::-::9))))
  (is (= 6 (version #i::-::9))))

(test ->ip-range
  (loop repeat 100 do
    (let* ((ip4 (make-ip-like (random-ipv4-str)))
           (ip4r (netaddr::->ip-range ip4))
           (ip6 (make-ip-like (random-ipv6-str)))
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

(test compare
  (is (netaddr::compare #i0.0.0.0 #i255.255.255.255))
  (is (not (netaddr::compare #i255.255.255.255 #i0.0.0.0)))
  (is (netaddr::compare #i1.0.0.0 #i::))
  (is (netaddr::compare #i255.255.255.255 #i::))
  (is (not (netaddr::compare #i:: #i1.0.0.0)))
  (is (not (netaddr::compare #i:: #i255.255.255.255))))

(test contiguous?
  (is (contiguous? #i10.0.0.0/24 #i10.0.1.0/24))
  (is (contiguous? #i0.0.0.0/1 #i128.0.0.0/1))
  (is (contiguous? #i::/1 #i8000::/1))
  (is (not (contiguous? #i10.0.0.0/24 #i10.0.2.0/24)))
  (is (contiguous? #i0.0.0.0-1.0.0.0 #i1.0.0.1-2.0.0.0))
  (is (contiguous? #i0.0.0.0-1.0.0.0 #i1.0.0.1-1.0.0.1))
  (is (not (contiguous? #i0.0.0.0-1.0.0.0 #i1.0.0.0-1.0.0.1)))
  (is (not (contiguous? #i0.0.0.0-1.0.0.0 #i1.0.0.0/8)))
  (is (contiguous? #i0.0.0.0-0.255.255.255 #i1.0.0.0/8))
  (is (contiguous? #i1.0.0.0/8 #i0.0.0.0-0.255.255.255))
  (is (contiguous? #i1.2.3.4/32 #i1.2.3.5/32))
  (is (contiguous? #i1.2.3.4 #i1.2.3.5))
  (is (contiguous? #i10.0.0.0/24 #i9.255.255.255))
  (is (contiguous? #i10.0.1.0 #i10.0.0.0/24))
  (is (not (contiguous? #i10.0.0.0/24 #i10.0.0.41)))
  (is (not (contiguous? #i255.255.255.255 #i0.0.0.0))))

;; Lots of duplicates from above since CONTIGUOUS? networks are by definition
;; DISJOINT?.
(test disjoint?
  (is (disjoint? #i10.0.0.0/24 #i10.0.1.0/24))
  (is (disjoint? #i0.0.0.0/1 #i128.0.0.0/1))
  (is (disjoint? #i::/1 #i8000::/1))
  (is (disjoint? #i10.0.0.0/24 #i10.0.2.0/24))
  (is (disjoint? #i0.0.0.0-1.0.0.0 #i1.0.0.1-2.0.0.0))
  (is (disjoint? #i0.0.0.0-1.0.0.0 #i1.0.0.1-1.0.0.1))
  (is (not (disjoint? #i0.0.0.0-1.0.0.0 #i1.0.0.0-1.0.0.1)))
  (is (not (disjoint? #i0.0.0.0-1.0.0.0 #i1.0.0.0/8)))
  (is (disjoint? #i0.0.0.0-0.255.255.255 #i1.0.0.0/8))
  (is (disjoint? #i1.0.0.0/8 #i0.0.0.0-0.255.255.255))
  (is (disjoint? #i1.2.3.4/32 #i1.2.3.5/32))
  (is (disjoint? #i1.2.3.4 #i1.2.3.5)))

(test subset?
  (loop for x from 31 downto 0 do
    (is (subset? (make-ip-like (format nil "0.0.0.0/~a" (1+ x)))
                 (make-ip-like (format nil "0.0.0.0/~a" x)))))
  (loop for x from 127 downto 0 do
    (is (subset? (make-ip-like (format nil "::/~a" (1+ x)))
                 (make-ip-like (format nil "::/~a" x)))))
  (let ((r4 (make-ip-range "0.0.0.0" "255.255.255.255"))
        (r6 (make-ip-range "::" "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")))
    (loop repeat 100 do
      (progn
        (is (subset? (make-ip-like (random-ipv4-str)) r4))
        (is (subset? (make-ip-like (random-ipv6-str)) r6))
        (is (subset? (random-ipv4-network) r4))
        (is (subset? (random-ipv6-network) r6))))))

(test superset?
  (loop for x from 31 downto 0 do
    (is (superset? (make-ip-like (format nil "0.0.0.0/~a" x))
                   (make-ip-like (format nil "0.0.0.0/~a" (1+ x))))))
  (loop for x from 127 downto 0 do
    (is (superset? (make-ip-like (format nil "::/~a" x))
                   (make-ip-like (format nil "::/~a" (1+ x))))))
  ;; Add for ranges, IPs, mixtures, etc.
  (let ((r4 (make-ip-range "0.0.0.0" "255.255.255.255"))
        (r6 (make-ip-range "::" "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")))
    (loop repeat 100 do
      (progn
        (is (superset? r4 (make-ip-like (random-ipv4-str))))
        (is (superset? r6 (make-ip-like (random-ipv6-str))))
        (is (superset? r4 (random-ipv4-network)))
        (is (superset? r6 (random-ipv6-network)))))))

(test ip=/ip-equal
  (is (ip= #i1.2.3.4 #i1.2.3.4))
  (is (not (ip= #i1.2.3.4 #i1.2.3.4/32)))
  (is (not (ip= #i1.2.3.4 #i1.2.3.4-1.2.3.4)))
  (let ((s1 (make-instance 'netaddr::ip-set))
        (s2 (make-instance 'netaddr::ip-set)))
   (loop repeat 100 do
     (let ((net4 (random-ipv4-network))
           (net6 (random-ipv6-network)))
       (is (ip= net4 net4))
       (is (ip-equalp (netaddr::->ip-range net4) net4))
       (is (ip-equalp net4 (netaddr::->ip-range net4)))

       (is (ip= net6 net6))
       (is (ip-equalp (netaddr::->ip-range net6) net6))
       (is (ip-equalp net6 (netaddr::->ip-range net6)))

       (is (ip-equalp s1 s2))
       (add! s1 net4)
       (add! s1 net6)
       (add! s2 (netaddr::->ip-range net4))
       (add! s2 (netaddr::->ip-range net6))))))

(test ip-equalp
  (is (ip-equalp #i1.2.3.4 #i1.2.3.4))
  (is (ip-equalp #i1.2.3.4 #i1.2.3.4/32))
  (is (ip-equalp #i1.2.3.4/32 #i1.2.3.4))
  (is (ip-equalp #i1.2.3.4 #i1.2.3.4-1.2.3.4))
  (is (ip-equalp #i1.2.3.4-1.2.3.4 #i1.2.3.4))
  (loop repeat 100 do
    (let ((str4 (random-ipv4-str))
          (str6 (random-ipv6-str)))
      (is (ip-equalp (make-ip-like str4) (make-ip-like (format nil "~a/~a" str4 32))))
      (is (not (ip-equalp (make-ip-like str4) (make-ip-like (format nil "~a/~a" str4 31)))))
      (is (ip-equalp (make-ip-like str6) (make-ip-like (format nil "~a/~a" str6 128))))
      (is (not (ip-equalp (make-ip-like str6) (make-ip-like (format nil "~a/~a" str6 127))))))))

(test subtract
  (let* ((cidr4 #i10.0.0.0/8)
         (s1 (netaddr::subtract cidr4 #i10.0.0.0))
         (s2 (netaddr::subtract cidr4 #i10.127.0.0))
         (s3 (netaddr::subtract cidr4 #i10.0.0.0-11.0.0.0)))
    (is (ip= #i10.0.0.1-10.255.255.255 (first s1)))
    (is (and (ip= #i10.0.0.0-10.126.255.255 (first s2))
             (ip= #i10.127.0.1-10.255.255.255 (second s2))))
    (is (null s3))
    ;; add ipv6 ones, range ones, ip-address ones.
    ))

(test sub
  (let* ((s (make-ip-set #i(10.0.0.0/24 1.1.1.1)))
         (orig (netaddr::shallow-copy-object s)))
    (is (ip= s orig))
    (is (ip= (sub s #i10.0.0.0/24) (make-ip-set #i(1.1.1.1))))
    (is (ip= s orig))
    (is (ip= (sub s #i10.0.0.0/8) (make-ip-set #i(1.1.1.1))))
    (is (ip= (sub s #i1.0.0.0/8) (make-ip-set #i(10.0.0.0/24))))))

(test add
  (let* ((s (make-ip-set #i(0.0.0.0/24 1.1.1.1))))
    (loop for x upto 255 do
      (is (= 3 (length (slot-value (add s (make-ip-address x)) 'set))))
      (is (ip-equalp s (make-ip-set #i(0.0.0.0/24 1.1.1.1)))))))

(test addnew
  (let* ((s (make-ip-set #i(10.0.0.0/24 1.1.1.1)))
         (orig (netaddr::shallow-copy-object s)))
    (is (ip= s orig))
    (is (ip= (addnew s #i10.0.0.0/8) (make-ip-set #i(10.0.0.0/8 1.1.1.1))))
    (is (ip= (addnew s #i10.0.0.0/24) orig))
    (is (ip= (addnew s #i10.0.0.0/27) orig))
    (is (ip= (addnew s #i10.0.0.128) orig))
    (is (ip= (addnew s #i10.0.0.0/8) (make-ip-set #i(10.0.0.0/8 1.1.1.1))))))

(test ip-set
  (let* ((s (make-ip-set #i(10.0.0.0/24 1.1.1.1)))
         (orig (netaddr::shallow-copy-object s))
         (networks (mapcar #'make-ip-network (list "10.0.0.0/24" "10.0.0.0/16")))
         (networks-orig (copy-seq networks)))
    (is (ip= s orig))
    (is (contains? s #i10.0.0.0/25))
    (is (contains? s #i1.1.1.1))
    (is (not (contains? s #i192.168.0.0)))
    (add! s #i192.168.0.0/16)
    (is (contains? s #i192.168.0.0))
    (is (contains? s #i192.168.0.0-192.168.255.255))
    (sub! s #i192.168.0.0/24)
    (is (not (contains? s #i192.168.0.0)))
    (is (not (contains? s #i192.168.0.0-192.168.255.255)))
    (is (contains? s #i192.168.1.0))
    (is (contains? s #i192.168.1.0-192.168.255.255))
    (is (ip= (make-ip-set #i(10.0.0.0/8 10.0.0.0/7 10.0.0.0/6 10.0.0.0/5 10.0.0.0/4 10.0.0.0/3))
             (make-ip-set #i(0.0.0.0/3))))
    (is (ip= (make-ip-set #i(0.0.0.0/3))
             (make-ip-set #i(10.0.0.0/8 10.0.0.0/7 10.0.0.0/6 10.0.0.0/5 10.0.0.0/4 10.0.0.0/3))))
    (is (ip= (make-ip-set #i(0.0.0.0/0 ::/0))
             (ip-set-union (make-ip-set #i(0.0.0.0/0 ::))
                           (make-ip-set #i(::/0 0.0.0.0)))))
    (is (ip= (make-ip-set #i(::/0 0.0.0.0/0))
             (ip-set-union (make-ip-set #i(0.0.0.0/0 ::))
                           (make-ip-set #i(::/0 0.0.0.0)))))
    (make-ip-set networks)
    (is (every #'ip= networks networks-orig))))

(test ip-set-union
  (is (ip= (ip-set-union (make-ip-set #i(10.0.0.0/24 192.168.0.0/24 ffff::/128))
                         (make-ip-set #i(10.0.0.0/8 192.168.0.0/16 ffff::/96)))
           (make-ip-set #i(10.0.0.0/8 192.168.0.0/16 ffff::/96))))
  (is (ip= (ip-set-union (make-ip-set #i(10.0.0.0/24 192.168.0.0/24 ffff::/128))
                         (make-ip-set #i(10.0.0.0/8 192.168.0.0/16 ffff::/96))
                         (make-ip-set #i(:: 6.7.8.9 10.20.30.40)))
           (make-ip-set #i(10.0.0.0/8 192.168.0.0/16 ffff::/96 :: 6.7.8.9))))
  (is (ip= (ip-set-union (make-ip-set #i(10.0.0.0/24 192.168.0.0/24 ffff::/128))
                         (make-ip-set #i(10.0.0.0/8 192.168.0.0/16 ffff::/96))
                         (make-ip-set #i(:: 6.7.8.9 10.20.30.40))
                         (make-ip-set nil)
                         (make-ip-set nil))
           (make-ip-set #i(10.0.0.0/8 192.168.0.0/16 ffff::/96 :: 6.7.8.9)))))

(test ip-set-intersection
  (is (ip= (ip-set-intersection (make-ip-set #i(10.0.0.0/24 192.168.0.0/24 ffff::/128))
                                (make-ip-set #i(10.0.0.0/8 192.168.0.0/16 ffff::/96)))
           (make-ip-set #i(10.0.0.0/24 192.168.0.0/24 ffff::/128))))
  (is (ip= (ip-set-intersection (make-ip-set #i(10.0.0.0/24 192.168.0.0/24 ffff::/128))
                                (make-ip-set #i(10.0.0.0/8 192.168.0.0/16 ffff::/96))
                                (make-ip-set #i(10.0.0.0/25 192.168.0.0)))
           (make-ip-set #i(10.0.0.0/25 192.168.0.0))))
  (is (ip= (ip-set-intersection (make-ip-set #i(10.0.0.0/24 192.168.0.0/24 ffff::/128))
                                (make-ip-set #i(10.0.0.0/8 192.168.0.0/16 ffff::/96))
                                (make-ip-set nil))
           (make-ip-set nil))))

(test ip-set-difference
  (is (ip= (ip-set-difference (make-ip-set #i(10.0.0.0/8 192.168.0.0/24 192.168.1.0/24 2.3.4.5))
                              (make-ip-set #i(1.2.3.4 10.0.0.0/24 192.168.0.0/16 10.127.0.0-10.255.255.253)))
           (make-ip-set #i(2.3.4.5 10.0.1.0-10.126.255.255 10.255.255.254-10.255.255.255))))
  (is (ip= (ip-set-difference (make-ip-set #i(10.0.0.0/8 192.168.0.0/24 192.168.1.0/24 2.3.4.5))
                              (make-ip-set #i(1.2.3.4 10.0.0.0/24 192.168.0.0/16 10.127.0.0-10.255.255.253))
                              (make-ip-set #i(::/0))
                              (make-ip-set nil))
           (make-ip-set #i(2.3.4.5 10.0.1.0-10.126.255.255 10.255.255.254-10.255.255.255))))
  (is (ip= (ip-set-difference (make-ip-set #i(10.0.0.0/8 192.168.0.0/24 192.168.1.0/24 2.3.4.5))
                              (make-ip-set #i(1.2.3.4 10.0.0.0/24 192.168.0.0/16 10.127.0.0-10.255.255.253))
                              (make-ip-set nil))
           (make-ip-set #i(2.3.4.5 10.0.1.0-10.126.255.255 10.255.255.254-10.255.255.255)))))

(test ip-set-symmetric-difference
  (is (ip= (ip-set-symmetric-difference (make-ip-set #i(10.0.0.0/8 192.168.0.0/24 192.168.1.0/24 2.3.4.5))
                                        (make-ip-set #i(1.2.3.4 10.0.0.0/24 192.168.0.0/16 10.127.0.0-10.255.255.253)))
           (make-ip-set #i(1.2.3.4 2.3.4.5 10.0.1.0-10.126.255.255 10.255.255.254-10.255.255.255 192.168.2.0-192.168.255.255)))))

(test public?
  (is (not (public? #i192.168.0.0/16)))
  (is (not (public? #i192.168.0.0)))
  (is (public? #i192.167.255.255))
  (is (not (public? #i192.168.255.255)))
  (is (public? #i192.169.0.0))

  (is (not (public? #i10.0.0.0/8)))
  (is (not (public? #i10.0.0.0)))
  (is (public? #i9.255.255.255))
  (is (not (public? #i10.255.255.255)))
  (is (public? #i11.0.0.0))

  (is (not (public? #i172.16.0.0/12)))
  (is (not (public? #i172.16.0.0)))
  (is (public? #i172.15.255.255))
  (is (not (public? #i172.31.255.255)))
  (is (public? #i172.32.0.0)))

(test private?
  (is (private? #i192.168.0.0/16))
  (is (private? #i192.168.0.0))
  (is (not (private? #i192.167.255.255)))
  (is (private? #i192.168.255.255))
  (is (not (private? #i192.169.0.0)))

  (is (private? #i10.0.0.0/8))
  (is (private? #i10.0.0.0))
  (is (not (private? #i9.255.255.255)))
  (is (private? #i10.255.255.255))
  (is (not (private? #i11.0.0.0)))

  (is (private? #i172.16.0.0/12))
  (is (private? #i172.16.0.0))
  (is (not (private? #i172.15.255.255)))
  (is (private? #i172.31.255.255))
  (is (not (private? #i172.32.0.0))))

(test reserved?
  (is (reserved? #i0.0.0.0))
  (is (reserved? #i192.0.2.0))
  (is (reserved? #i255.255.255.255))
  (is (reserved? #i233.252.0.255)))

;;;; Coverage gaps.

(test strict-subset?/strict-superset?
  (is (netaddr::strict-subset? #i10.0.0.1 #i10.0.0.0/24))
  (is (netaddr::strict-superset? #i10.0.0.0/24 #i10.0.0.1))
  (is (not (netaddr::strict-subset? #i10.0.0.0/24 #i10.0.0.0/24)))
  (is (not (netaddr::strict-superset? #i10.0.0.0/24 #i10.0.0.0/24)))
  (is (not (netaddr::strict-subset? #i10.0.0.0/24 #i10.0.0.1))))

(test mixed-address/pair-methods
  (is (not (disjoint? #i10.0.0.1 #i10.0.0.0/24)))
  (is (not (disjoint? #i10.0.0.0/24 #i10.0.0.1)))
  (is (disjoint? #i10.0.1.1 #i10.0.0.0/24))
  (is (contiguous? #i10.0.1.0 #i10.0.0.0/24))
  (is (contiguous? #i10.0.0.0/24 #i10.0.1.0))
  (is (not (contiguous? #i10.0.0.1 #i10.0.0.1)))
  (is (ip= #i10.0.0.1 (netaddr::intersect #i10.0.0.1 #i10.0.0.1)))
  (is (null (netaddr::intersect #i10.0.0.1 #i10.0.0.2)))
  (is (ip= #i10.0.0.1 (netaddr::intersect #i10.0.0.0/24 #i10.0.0.1)))
  (is (null (netaddr::intersect #i10.0.0.0/24 #i10.0.1.0/24))))

(test type-errors
  (dolist (fn (list #'subset? #'superset? #'netaddr::strict-subset? #'netaddr::strict-superset? #'disjoint?
                    #'contiguous? #'contains? #'netaddr::intersect))
    (fiveam:signals type-error (funcall fn "10.0.0.1" #i10.0.0.1))
    (fiveam:signals type-error (funcall fn #i10.0.0.1 42)))
  (fiveam:signals type-error (size "10.0.0.1"))
  (fiveam:signals error (make-ip-range "::1" "10.0.0.1"))
  (fiveam:signals error (make-instance 'ip-address :int (expt 2 32) :version 4)))

(test print-object
  (is (search "10.0.0.1" (princ-to-string #i10.0.0.1)))
  (is (search "10.0.0.0/24" (princ-to-string #i10.0.0.0/24)))
  (is (search "10.0.0.1-10.0.0.9" (princ-to-string #i10.0.0.1-10.0.0.9)))
  (is (search "(2)" (princ-to-string (make-ip-set #i(10.0.0.0/24 ::1))))))

(test shallow-copy-object
  (dolist (ip-like #i(10.0.0.1 10.0.0.0/24 10.0.0.1-10.0.0.9))
    (let ((copy (netaddr::shallow-copy-object ip-like)))
      (is (not (eq ip-like copy)))
      (is (ip= ip-like copy)))))

(test split-char
  ;; Call through the function object so the out-of-line definition runs.
  (let ((split #'netaddr::split-char))
    (is (equal '("1" "2" "3") (funcall split #\. "1.2.3")))
    (is (equal '("" "" "1") (funcall split #\: "::1")))
    (is (equal '("abc") (funcall split #\. "abc")))
    (is (equal '("") (funcall split #\. "")))))

(test multicast?/route-type
  (is (multicast? #i224.0.0.1))
  (is (multicast? #iff02::1))
  (is (not (multicast? #i8.8.8.8)))
  (is (eq :private (route-type #i10.0.0.1)))
  (is (eq :reserved (route-type #i127.0.0.1)))
  (is (eq :multicast (route-type #i224.0.0.1)))
  (is (eq :public (route-type #i8.8.8.8)))
  (is (eq :other (route-type #i100.64.0.1))))

(test remaining-gaps
  ;; SIZE of an IP-SET.
  (is (= (+ 256 1) (size (make-ip-set #i(10.0.0.0/24 ::1)))))
  ;; SUBTRACT where the subtrahend shares the last address.
  (is (ip= #i10.0.0.0-10.0.0.7 (first (netaddr::subtract #i10.0.0.0/24 #i10.0.0.8-10.0.0.255))))
  ;; Zero-argument set operations.
  (is (zerop (size (ip-set-intersection))))
  (is (zerop (size (ip-set-difference))))
  ;; ADDNEW! of a disjoint element pushes it onto the set.
  (is (= 2 (length (slot-value (addnew (make-ip-set #i(10.0.0.0/24)) #i192.168.0.1) 'netaddr::set))))
  ;; CHECK-TYPE fallbacks.
  (fiveam:signals type-error (netaddr::->ip-range "10.0.0.1"))
  (fiveam:signals type-error (ip-equal "10.0.0.1" #i10.0.0.1))
  (fiveam:signals type-error (ip-equalp "10.0.0.1" #i10.0.0.1)))

(test longest-match
  ;; MAKE-IP-SET compacts away nested members, so nest them with ADD!.
  (let ((s (make-ip-set #i(10.0.0.0/8 192.168.0.0/16 2001:db8::/32))))
    (add! s #i10.1.0.0/16 #i10.1.2.0/24 #i2001:db8:1::/48)
    (is (ip= #i10.1.2.0/24 (longest-match s #i10.1.2.3)))
    (is (ip= #i10.1.0.0/16 (longest-match s #i10.1.3.3)))
    (is (ip= #i10.0.0.0/8 (longest-match s #i10.2.0.0/16)))
    (is (ip= #i10.0.0.0/8 (longest-match s #i10.1.0.0-10.2.0.0)))
    (is (ip= #i2001:db8:1::/48 (longest-match s #i2001:db8:1::1)))
    (is (ip= #i2001:db8::/32 (longest-match s #i2001:db8:2::1)))
    (is (null (longest-match s #i11.0.0.0)))
    (is (null (longest-match s #i::1)))
    ;; CONTAINS? on a set is the same operation.
    (is (ip= #i10.1.2.0/24 (contains? s #i10.1.2.3)))
    ;; Members added after the index is built are found, and are preferred when
    ;; more specific; SUB! keeps the index consistent.
    (add! s #i10.1.2.0/25)
    (is (ip= #i10.1.2.0/25 (longest-match s #i10.1.2.3)))
    (is (ip= #i10.1.2.0/24 (longest-match s #i10.1.2.200)))
    (sub! s #i10.1.2.0/24)
    (is (null (longest-match s #i10.1.2.3)))
    (is (contains? (longest-match s #i10.1.3.3) #i10.1.3.3))
    (fiveam:signals type-error (longest-match s "10.1.2.3"))
    (fiveam:signals type-error (longest-match "set" #i10.1.2.3))))

(test ip-syntax
  (is (ip= (make-ip-address "1.2.3.4") #i1.2.3.4))
  (is (ip= (make-ip-network "10.0.0.0/8") #i10.0.0.0/8))
  (is (ip= (make-ip-range "::" "::ff") #i::-::ff))
  (is (ip= (make-ip-network "2001:db8::/32") #I2001:db8::/32))
  ;; Tokens end at delimiters, so they compose inside forms.
  (is (equal '(t t) (mapcar #'ip-address-p (list #i1.2.3.4 #i::1))))
  ;; Parenthesized form is always a list.
  (is (equal '(t) (mapcar #'ip-address-p #i(1.2.3.4))))
  (is (= 3 (length #i(1.2.3.4 10.0.0.0/8 ::-::ff))))
  (is (every #'ip= #i(1.2.3.4 10.0.0.0/8 ::-::ff) (list #i1.2.3.4 #i10.0.0.0/8 #i::-::ff)))
  ;; Strings and evaluated forms.
  (is (ip= #i1.2.3.4 #i"1.2.3.4"))
  (let ((x "10.0.0.0") (plen 8))
    (is (ip= #i10.0.0.0 #i,x))
    (is (ip= #i10.0.0.0/8 #i,(format nil "~a/~a" x plen)))
    (is (every #'ip= #i(1.2.3.4 ,x "::1") (list #i1.2.3.4 #i10.0.0.0 #i::1))))
  ;; Malformed input errors at read time or construction time.
  (fiveam:signals error (read-from-string "#i"))
  (fiveam:signals error (eval (read-from-string "#inot-an-ip"))))

(defun ip-address-p (x) (typep x 'netaddr::ip-address))
