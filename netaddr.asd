(asdf:defsystem netaddr
  :serial t
  :description "IP and CIDR manipulation library"
  :author "Yacin Nadji <ynadji@gmail.com>"
  :license "MIT"
  :depends-on ("str" "uiop" "alexandria" "cl-ppcre" "computable-reals" "closer-mop")
  :components ((:file "packages")
               (:file "main")
               (:file "syntax")
               (:file "reserved"))
  :in-order-to ((test-op (test-op :netaddr/tests))))

(asdf:defsystem :netaddr/tests
  :author "Yacin Nadji <ynadji@gmail.com>"
  :license "MIT"
  :depends-on ("netaddr" "fiveam" "arrow-macros")
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run!
                                       (uiop:find-symbol* '#:tests
                                                          '#:netaddr/tests))))
