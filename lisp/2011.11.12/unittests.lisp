(in-package #:com.derekv.mandelbrots.lisp.2011-11-12.generator)


;; Store a list of known values in the set.
;; note 
;; as we get more detailed, 
;;  we'll need to adjust this to take into account the max iterations 
(defparameter mandelbrot-set-known-values 
  '((#C(10.0 10.0) 0)
    (#C(0.0 0.0) 100)))

(defparameter maxiter-for-test 100 ) 

;; 
(defun test-generator-on-known-values ()
  (let ((generator (make-generator :maxiter maxiter-for-test)))
    (loop        
       with success-so-far = t
       for pair in mandelbrot-set-known-values
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


(defun test-crawler ()
  (let* (clist 
	elist
	(start-at-point #C(-1.0 -1.0))
	(end-at-point #C(1.2 1.2))
	(res-to-test 10)
	(crawler (make-crawler :from start-at-point 
			       :to end-at-point
			       :resolution res-to-test)))
    (loop
       for cpoint = (get-next-point crawler) then (get-next-point crawler)
       for epoint = start-at-point then (+ epoint 
					   (/ 
					    (- end-at-point start-at-point)
					    (- res-to-test 1)))
       while (not (null cpoint))
       collect cpoint into cpointcollection
       collect epoint into epointcollection
       finally (progn (setf clist cpointcollection)
		      (setf elist epointcollection)))
    (format t "~{~a, ~}~%" clist)
    (format t "~{~a, ~}~%" elist)
    (let 
	((test-is-succeeding t))
      (mapcar 
       (lambda (pair)
	      (if (second pair)
		  (format t "~a : PASS ~%" (first pair))
		  (format t "~a : FAIL ~%" (first pair)))
	      (setf test-is-succeeding (and test-is-succeeding (second pair))))
       `(
	 ("first point should be same as 'from'" ,(equalp (first clist) start-at-point))
	 ("end point should be pretty close 'to:" , (< (abs (- (car (last clist)) end-at-point))
						      (* 0.0000001 res-to-test)))
	 ("should be same length as simulated list" ,(equalp (length clist) (length elist)))
	 ("length should be same as requested resolution" ,(equalp (length clist) res-to-test))
	 ("should be equal to simulated list" ,(equalp clist elist)))))))

  

(defun run-unit-tests ()
  (and 
   (test-generator-on-known-values)
   (test-crawler)))
