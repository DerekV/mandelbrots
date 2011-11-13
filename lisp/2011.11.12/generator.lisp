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

;; (defclass crawler () 
;;   ((cmin
;;     :initarg :from
;;     :reader :from)
;;    (cmax
;;     :initarg :to
;;     :reader :to)
;;    (resolution
;;     :initarg :resolution
;;     :reader :resolution)))

;; (defgeneric get-next-point (crawler))

(defun make-crawler (&key from to resolution)
  (let ((increment (/ (- to from) (- resolution 1)))
	(next from))
    (lambda ()
      (if next
	  (let ((current next))
	    (if (or (> (realpart current) (realpart to))
		    (> (imagpart current) (imagpart to)))
		(setf next nil)
		(progn 
		  (setf next (+ current increment))
		  current)))))))

(defun get-next-point (crawler)
  (funcall crawler))
	    
      




   
  