(in-package :netaddr)

;;;; CIDR and range interop.

(defgeneric ->ip-range (ip-like)
  (:method ((ip-like ip-address))
    (let ((s (str ip-like)))
      (make-ip-range s s)))
  (:method ((ip-like ip-pair))
    (with-slots (first-ip last-ip) ip-like
      (make-ip-range (str first-ip) (str last-ip))))
  (:method ((ip-like ip-range))
    ip-like)
  (:method ((ip-like t))
    (check-type ip-like ip-like)))

(defun range->cidrs (ip-range)
  "Return the list of IP-NETWORKs that exactly cover IP-RANGE, in ascending order."
  (let* ((version (version (first-ip ip-range)))
         (max-bits (ecase version (4 32) (6 128)))
         (first (int (first-ip ip-range)))
         (last (int (last-ip ip-range))))
    (loop while (<= first last)
          collect (let* ((span (1+ (- last first)))
                         ;; Largest block that both fits in the remaining span
                         ;; and is aligned at FIRST (i.e., FIRST is a multiple
                         ;; of its size).
                         (bits (min (1- (integer-length span))
                                    (if (zerop first)
                                        max-bits
                                        (1- (integer-length (logand first (- first)))))))
                         (net (apply-mask (%make-ip-address first version)
                                           (- max-bits bits))))
                    (setf first (1+ (int (last-ip net))))
                    net))))
