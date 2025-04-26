(asdf:defsystem netaddr
  :serial t
  :description "A library for manipulating IP addresses, subnets, ranges, and sets."
  :author "Yacin Nadji <yacin@defmacro.cc>"
  :license "MIT"
  :version "1.0.0"
  :depends-on ("str" "uiop" "alexandria" "cl-ppcre" "computable-reals" "closer-mop" "split-sequence")
  :components ((:file "packages")
               (:file "main")
               (:file "syntax")
               (:file "reserved"))
  :in-order-to ((test-op (test-op :netaddr/tests))))

(asdf:defsystem :netaddr/tests
  :author "Yacin Nadji <yacin@defmacro.cc>"
  :license "MIT"
  :depends-on ("netaddr" "fiveam")
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run!
                                       (uiop:find-symbol* '#:tests
                                                          '#:netaddr/tests))))
