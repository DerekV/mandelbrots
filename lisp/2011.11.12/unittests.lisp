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
    (loop        
       with success-so-far = t
       for pair in mandlebrot-set-known-values
 	 do (setf success-so-far 
		  (let* ((point (first pair))
			 (expected (second pair))
			 (result (get-value-at-point generator point))
			 (this-test-did-pass (equal result expected)))
		    (format t "at ~16,16f expected ~16a got ~16a : ~a~%"
			    point expected result
			    (if this-test-did-pass "PASS" "FAIL"))
		    (and success-so-far this-test-did-pass)))
       finally (return success-so-far))))

(defun run-unit-tests ()
  (and 
   (test-generator-on-known-values)))
