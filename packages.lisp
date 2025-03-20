(defpackage :netaddr
  (:use :cl)
  (:local-nicknames (:ax :alexandria))
  (:import-from :split-sequence #:split-sequence)
  (:export :make-ip-address
           :make-ip-network
           :make-ip-range
           :make-ip-set
           :add
           :add!
           :addnew
           :addnew!
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
           :first-ip
           :last-ip
           :size
           :ip-set-union
           :ip-set-intersection
           :ip-set-difference
           :ip-set-symmetric-difference
           :enable-ip-syntax
           :public?
           :private?
           :reserved?))
