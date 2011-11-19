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


(defun test-crawler (start-at-point end-at-point resolution)
  (format t "testing crawler from ~a to ~a resolution ~a~%" start-at-point end-at-point resolution)
  (let* (clist 
	elist
	(crawler (make-crawler :from start-at-point 
			       :to end-at-point
			       :resolution resolution)))
    (cond 
      ((= resolution 1) 
       ;; in this case, get-next point should return 'to' and then nil
       (let 
	   ((test-is-succeeding t))
	 (mapcar 
	  (lambda (pair)
	    (if (second pair)
		(format t "~a : PASS ~%" (first pair))
		(format t "~a : FAIL ~%" (first pair)))
	    (setf test-is-succeeding (and test-is-succeeding (second pair))))
	  `(
	    ("case resolution = 1, first point returned should be same as destination" ,
	     (equalp (get-next-point crawler) end-at-point))
	    ("case resolution = 1, calling get-next-point a second time should return nil" , 
	     (null (get-next-point crawler)))))))
      ((> resolution 1) 
       (loop
	  repeat resolution   
	  ;; this fails to test for the possibility that clist 
	  ;; would have been longer than desired
	  for cpoint = (get-next-point crawler) then (get-next-point crawler)
	  for epoint = start-at-point then (+ epoint 
					      (/ 
					       (- end-at-point start-at-point)
					       (- resolution 1)))
	  while (not (null cpoint))
	  collect cpoint into cpointcollection
	  collect epoint into epointcollection
	  finally (progn (setf clist cpointcollection)
			 (setf elist epointcollection)))
  ;;     (format t "~{~a, ~}~%" clist)
  ;;     (format t "~{~a, ~}~%" elist)
       (values
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
							  (* 0.0000001 resolution)))
	    ("should be same length as simulated list" ,(equalp (length clist) (length elist)))
	    ("length should be same as requested resolution" ,(equalp (length clist) resolution))
	    ("should be equal to simulated list" ,(equalp clist elist))
	    ("calling get-next-point again should return null after all is finished",
	     (null (get-next-point crawler))))))
	elist
	clist)))))

(defun run-unit-tests ()
  (and
   (test-generator-on-known-values)
   (test-crawler #C(-1.0 -1.0) #C(1.0 1.0)  1)
   (test-crawler #C(-1.0 -1.0) #C(1.0 1.0)  2)
   (test-crawler #C(-1.0 -1.0) #C(1.0 1.0)  10)
   (test-crawler #C(-1.0 -1.0) #C(1.0 1.0)  100)
   (test-crawler #C(-0.01 0)   #C(0 0)      1)
   (test-crawler #C(-0.01 0)   #C(0 1.0)    200)
   (test-crawler #C(1 0)       #C(-1 0)     77)
   (test-crawler #C(0 1.5)     #C(0 -.002)  15)
   (test-crawler #C(-0.2 0.5)  #C(-0.3 0.5) 55)))
