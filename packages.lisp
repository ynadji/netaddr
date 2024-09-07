(in-package :cl-user)
(defpackage :netaddr
  (:use :cl :arrow-macros)
  (:local-nicknames (:ax :alexandria))
  (:export :make-ip-address
           :make-ip-network
           :make-ip-range
           :make-ip-set
           :add
           :add!
           :sub
           :sub!
           :ip=
           :contains?
           :contiguous?
           :disjoint?
           :subset?
           :superset?
           :ip-equal
           :ip-equalp
           :int
           :str
           :size))
