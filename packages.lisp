(in-package :cl-user)
(defpackage :netaddr
  (:use :cl :arrow-macros)
  (:export :make-ip-address
           :make-ip-network
           :make-ip-range
           :contains?
           :str))
