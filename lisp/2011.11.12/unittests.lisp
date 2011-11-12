(in-package #:com.derekv.mandlebrots.lisp.2011-11-12.generator)


;; Store a list of known values in the set.
;; note 
;; as we get more detailed, 
;;  we'll need to adjust this to take into account the max iterations 
(defparameter mandlebrot-set-known-values 
  '((#C(10.0 10.0) 0)
    (#C(0.0 0.0) 100)))

(defparameter maxiter-for-test 100 ) 

;; 
(defun test-generator-on-known-values ()
  (let ((generator (make-generator :maxiter maxiter-for-test)))
    (mapcar 
     (lambda (pair)
       (equal
	(get-value-at-point generator (car pair))
	(cadr pair)))
     mandlebrot-set-known-values)))

(defun run-unit-tests ()
  (and 
   (test-generator-on-known-values)))
