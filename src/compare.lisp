(in-package :netaddr)

;;;; Equality and ordering.

(defgeneric ip-equal (ip+1 ip+2)
  (:documentation "Returns T if IP+1 and IP+2 represent the same underlying IP address(es), are the same version of IP, and are instances of the same class (one of IP-ADDRESS, IP-PAIR, or IP-SET), or otherwise NIL.")
  (:method ((ip1 ip-address) (ip2 ip-address))
    (and (= (int ip1) (int ip2))
         (= (version ip1) (version ip2))))
  (:method ((p1 ip-pair) (p2 ip-pair))
    (and (eq (class-of p1) (class-of p2))
         (ip= (first-ip p1) (first-ip p2))
         (ip= (last-ip p1) (last-ip p2))))
  (:method ((s1 ip-set) (s2 ip-set))
    (with-slots ((set1 set)) s1
      (with-slots ((set2 set)) s2
        (if (not (= (length set1) (length set2)))
            nil
            (every #'ip-equal
                   (sort (copy-list set1) #'compare)
                   (sort (copy-list set2) #'compare))))))
  ;; Default case when the types of the two arguments do not match.
  (:method ((x ip+) (y ip+))
    nil)
  (:method ((ip+1 t) (ip+2 t))
    (check-type ip+1 ip+)
    (check-type ip+2 ip+)))

(defun ip= (ip+1 ip+2)
  "Synonym for IP-EQUAL."
  (ip-equal ip+1 ip+2))

(defgeneric ip-equalp (ip+1 ip+2)
  (:documentation "Returns T if IP+1 and IP+2 represent the same underlying IP address(es), and are the same version of IP, or otherwise NIL. IP-RANGEs or IP-NETWORKs that contain a single IP will be IP-EQUALP to the IP-ADDRESS. See Equality in the README for details.")
  (:method ((ip ip-address) (pair ip-pair))
    (and (= (int ip) (int (first-ip pair)) (int (last-ip pair)))
         (= (version ip) (version pair))))
  (:method ((pair ip-pair) (ip ip-address))
    (ip-equalp ip pair))
  (:method ((p1 ip-pair) (p2 ip-pair))
    (and (ip= (first-ip p1) (first-ip p2))
         (ip= (last-ip p1) (last-ip p2))))
  ;; TODO: Duplicating this just to change IP-EQUAL to IP-EQUALP sucks. I could
  ;; just include a &KEY argument, but that seems clunky. Not sure what to do.
  (:method ((s1 ip-set) (s2 ip-set))
    (with-slots ((set1 set)) s1
      (with-slots ((set2 set)) s2
        (if (not (= (length set1) (length set2)))
            nil
            (every #'ip-equalp
                   (sort (copy-list set1) #'compare)
                   (sort (copy-list set2) #'compare))))))
  (:method ((x ip+) (y ip+))
    (ip-equal x y))
  (:method ((ip+1 t) (ip+2 t))
    (check-type ip+1 ip+)
    (check-type ip+2 ip+)))

(defgeneric %compare (ip-like-1 ip-like-2)
  (:method ((ip1 ip-address) (ip2 ip-address))
    (< (int ip1) (int ip2)))
  (:method ((ip ip-address) (p ip-pair))
    (< (int ip) (int (first-ip p))))
  (:method ((p ip-pair) (ip ip-address))
    (not (compare ip p))) ; NB: this works because we always want ranges/subnets to be before IPs.
  (:method ((p1 ip-pair) (p2 ip-pair))
    (or (< (int (first-ip p1)) (int (first-ip p2)))
        (and (ip= (first-ip p1) (first-ip p2))
             (> (int (last-ip p1))
                (int (last-ip p2)))))))

(defun compare (ip-like-1 ip-like-2)
  ;; Explicitly make v4 always less than v6 so when we sort we are consistent
  ;; when two IPs have the same value but different versions, e.g., #i:: vs.
  ;; #i0.0.0.0.
  (check-type ip-like-1 ip-like)
  (check-type ip-like-2 ip-like)
  (if (= (version ip-like-1) (version ip-like-2))
      (%compare ip-like-1 ip-like-2)
      (< (version ip-like-1) (version ip-like-2))))
