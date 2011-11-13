(in-package #:com.derekv.mandelbrots.lisp.2011-11-12.generator)

;; quick start :
;; try this from repl
;; (create-png :from #C(-0.8 -0.8) :to #C(0.3 0.3) :resolution 3004 :maxiters 254 :filepath "~/tmp/mand.png")

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
       (if (>= i maxiter) (return i))
       (if (> (+ real-sq imag-sq) 4) (return i))
       (setq imagz (+ (* (* realz imagz) 2) imagc))
       (setq realz (+ (- real-sq imag-sq) realc))
       (setq real-sq (* realz realz))
       (setq imag-sq (* imagz imagz))
       (incf i))))

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

(defmacro map-iteration-to-color (iteration slot-for-red slot-for-blue slot-for-green)
 `(progn 
    (setf ,slot-for-red (mod (* ,iteration 77) 255))
    (setf ,slot-for-blue (mod (* ,iteration 21) 255))
    (setf ,slot-for-green (mod (* ,iteration 5) 255))))

(defun create-png (&key from to resolution maxiters filepath)
  (let* ((row-crawler (make-crawler
		       :from from
		       :to (complex (realpart from) (imagpart to))
		       :resolution resolution))
	 (g (make-generator :maxiter maxiters))
	 (png (make-instance 'zpng:png
			     :color-type :truecolor
			     :width resolution
			     :height resolution))
	 (image (zpng:data-array png)))
     (loop
	for row = #1=(get-next-point row-crawler) then #1#
	for rownum from 0 upto resolution
	while row
	do (let ((col-crawler
		  (make-crawler
		   :from row
		   :to (complex (realpart to) (imagpart row))
		   :resolution resolution)))
	     (loop
		for col = #2=(get-next-point col-crawler) then #2#
		for colnum from 0 upto resolution
		while col
		do (map-iteration-to-color 
		     (get-value-at-point g col)
		     (aref image colnum rownum 0)
		     (aref image colnum rownum 1)
		     (aref image colnum rownum 2)))))
     (zpng:write-png png filepath)))