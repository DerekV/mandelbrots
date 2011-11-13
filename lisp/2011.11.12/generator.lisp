(in-package #:com.derekv.mandelbrots.lisp.2011-11-12.generator)


;; for now, since state generator really has is maxitors, we will simply make it that.
;; (simulating a clos like object with a maxitor slot)
(defun make-generator (&key maxiter)
  maxiter)

(defun maxiter (generator)
  generator)

(defun get-value-at-point (generator C)
  (declare (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0) (speed 3))
	   (type (complex (single-float)) C))
  (let* ((realz (realpart C))
	(imagz (imagpart C))
	(realc (realpart C))
	(imagc (imagpart C))
	(i 0)
	(maxiter (maxiter generator))
	(real-sq (* realz realz))
	(imag-sq (* imagz imagz)))
    (declare (type fixnum i)
	     (type fixnum maxiter)
	     (type single-float imagz)
	     (type single-float realz)
	     (type single-float imagc)
	     (type single-float real-sq))
    (loop 
       (if (> (incf i) maxiter) (return i))
       (if (> (+ real-sq imag-sq) 4) (return i))
       (setq imagz (+ (* (* realz imagz) 2) imagc))
       (setq realz (+ (- real-sq imag-sq) realc))
       (setq real-sq (* realz realz))
       (setq imag-sq (* imagz imagz)))))

;; (loop for i below (maxiter generator)
;;    while (< (abs Z) 2.0)
;;    do (+ (* Z Z) C)
;;    finally (return i))))
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
	    
      
(defun create-list-accross-line (from to resolution maxiters)
  (let ((c (make-crawler :from from :to to :resolution resolution))
	(g (make-generator :maxiter maxiters)))
    (loop for point = (get-next-point c) then (get-next-point c) while point
       collect (get-value-at-point g point))))


;; (defun create-set-in-memory 
;;     (let ((row-crawler (make-crawler :from #C(-1.2 -1.2) :to #C(-1.2 1.5) :resolution 198)))
;;       (loop for row = (get-next-point row-crawler) then (get-next-point row-crawler)
;; 	 while row
;; 	 collect (create-list-accross-line row (+ row #C(1.7 0.0)) 198 255))) )

;; (let* ((png (make-instance 'zpng:png
;; 			    :color-type :grayscale-alpha
;; 			    :width 199
;; 			    :height 199))
;; 	(image (zpng:data-array png))
;;        (rownum 0) (colnum 0))
;;   (dolist (row data) (incf rownum) (setf colnum 0)
;; 	  (dolist (item row) (incf colnum)
;; 		  (setf (aref image colnum rownum 1) item)))
;;   (zpng:write-png png "/tmp/mandelbrot.png"))

(defun create-png (from to resolution maxiters)
  (let* ((row-crawler (make-crawler 
		       :from from 
		       :to (+ (* #C(1 0) (realpart from)) (* #C(0 1) (imagpart to)))
		       :resolution resolution))
	 (g (make-generator :maxiter maxiters))
	 (png (make-instance 'zpng:png
			     :color-type :grayscale
			     :width resolution
			     :height resolution))
	 (image (zpng:data-array png)))
     (loop 
	for row = #1=(get-next-point row-crawler) then #1#
	for rownum from 1 upto resolution
	while row
	do (let ((col-crawler 
		  (make-crawler 
		   :from row
		   :to (+ (* #C(1 0) (realpart to)) (* #C(0 1) (imagpart row)))
		   :resolution resolution)))
	     (loop
		for col = #2=(get-next-point col-crawler) then #2#
		for colnum from 0 upto resolution
		while col	
		do (setf (aref image colnum rownum 0)
			 (- 255 
			    (mod 
			     (* 7 
				(get-value-at-point g col)) 
			     255))))))
     (zpng:write-png png "/tmp/mandelbrot.png")))