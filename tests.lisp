(in-package :cl-user)
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
  (let ((net (make-ip-network "10.20.30.40/24"))
        (range (make-ip-range "192.168.0.0" "192.168.125.255")))
    (is (contains? net "10.20.30.0"))
    (is (contains? net "10.20.30.255"))
    (is (not (contains? net "10.20.31.0")))
    (is (not (contains? net "0.0.0.0")))
    (is (not (contains? net "255.255.255.255")))
    (is (not (contains? net "10.20.29.255")))

    (is (contains? range "192.168.0.0"))
    (is (contains? range "192.168.125.255"))
    (is (contains? range "192.168.13.79"))
    (is (not (contains? range "192.167.255.255")))
    (is (not (contains? range "192.168.126.0")))
    (is (not (contains? range "0.0.0.0")))
    (is (not (contains? range "255.255.255.255")))))

(test compress-ipv6-str
  (is (string= "0:0:1::" (netaddr::compress-ipv6-str "0:0:1:0:0:0:0:0")))
  (is (string= "0:0:1::1" (netaddr::compress-ipv6-str "0:0:1:0:0:0:0:1")))
  (is (string= "::1:0:0:1:0:0" (netaddr::compress-ipv6-str "0:0:1:0:0:1:0:0")))
  (is (string= "::" (netaddr::compress-ipv6-str "0:0:0:0:0:0:0:0"))))
