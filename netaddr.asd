(asdf:defsystem netaddr
  :serial t
  :description "A library for manipulating IP addresses, subnets, ranges, and sets."
  :author "Yacin Nadji <yacin@defmacro.cc>"
  :license "MIT"
  :version "2.0.1"
  :pathname "src"
  :components ((:file "package")
               (:file "util")
               (:file "strings")
               (:file "classes")
               (:file "index")
               (:file "compare")
               (:file "predicates")
               (:file "ranges")
               (:file "sets")
               (:file "syntax")
               (:file "reserved"))
  :in-order-to ((test-op (test-op :netaddr/tests))))

(asdf:defsystem :netaddr/tests
  :author "Yacin Nadji <yacin@defmacro.cc>"
  :license "MIT"
  :pathname "t"
  :depends-on ("netaddr" "fiveam")
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run!
                                       (uiop:find-symbol* '#:tests
                                                          '#:netaddr/tests))))
