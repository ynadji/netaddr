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

;; TODO: add a shitload of tests!

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
(test contains?
  (let ((net4 (make-ip-network "10.20.30.40/24"))
        (range4 (make-ip-range "192.168.0.0" "192.168.125.255"))
        (net6 (make-ip-network "dada:beef::/64"))
        (range6 (make-ip-range "::1" "babe::")))
    (is (contains? net4 "10.20.30.0"))
    (is (contains? net4 "10.20.30.255"))
    (is (not (contains? net4 "10.20.31.0")))
    (is (not (contains? net4 "0.0.0.0")))
    (is (not (contains? net4 "255.255.255.255")))
    (is (not (contains? net4 "10.20.29.255")))

    (is (contains? range4 "192.168.0.0"))
    (is (contains? range4 "192.168.125.255"))
    (is (contains? range4 "192.168.13.79"))
    (is (not (contains? range4 "192.167.255.255")))
    (is (not (contains? range4 "192.168.126.0")))
    (is (not (contains? range4 "0.0.0.0")))
    (is (not (contains? range4 "255.255.255.255")))

    (is (contains? net6 "dada:beef::"))
    (is (contains? net6 "dada:beef::ffff:ffff:ffff:ffff"))
    (is (contains? net6 "dada:beef::ffff:ffff:ffff:ffff"))
    (is (contains? net6 "dada:beef:0:0:1::"))
    (is (not (contains? net6 "dada:beef::1:ffff:ffff:ffff:ffff")))
    (is (not (contains? net6 "dada:beef:ffff:0:ffff:ffff:ffff:ffff")))

    (is (not (contains? range6 "::")))
    (is (contains? range6 "::1"))
    (is (contains? range6 "::2"))
    (is (contains? range6 "babe::"))
    (is (not (contains? range6 "babe::1")))))

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

(test contiguous?
  )

(test disjoint?
  )

(test subset?
  )

(test superset?
  )

(test ip=
  )
