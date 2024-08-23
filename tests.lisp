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
