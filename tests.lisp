(in-package :cl-user)
(defpackage netaddr/tests
  (:use #:cl #:netaddr)
  (:import-from #:fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is)
  (:export #:tests))

(in-package :netaddr/tests)

(def-suite tests)
(in-suite tests)

;; TODO: add a shitload of tests!

(test subnet-correct-str
  (is (string= "10.20.30.0/24" (str (make-ip-network "10.20.30.40/24")))))
