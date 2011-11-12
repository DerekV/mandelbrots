(in-package #:com.derekv.mandlebrots.lisp.2011-11-12.generator)


;; for now, since state generator really has is maxitors, we will simply make it that.
;; (simulating a clos like object with a maxitor slot)
(defun make-generator (&key maxiter)
  maxiter)

(defun maxiter (generator)
  generator)

;; unimplemented!
(defun get-value-at-point (generator C)
  (loop for i below (maxiter generator)
       for Z = C then (+ (* Z Z) C)
       while (< (abs Z) 2.0)
     finally (return i)))


   
    