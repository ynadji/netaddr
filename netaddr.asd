(asdf:defsystem netaddr
  :serial t
  :description "IP and CIDR manipulation library"
  :author "Yacin Nadji <ynadji@gmail.com>"
  :license "MIT"
  :depends-on ("str" "uiop" "arrow-macros")
  :components ((:file "packages")
               (:file "main"))
  :in-order-to ((test-op (test-op :netaddr/test))))
