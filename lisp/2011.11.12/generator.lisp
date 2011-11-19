(in-package #:com.derekv.mandelbrots.lisp.2011-11-12.generator)

;; quick start :
;; try this from repl
;; (create-png :from #C(-0.8 -0.8) :to #C(0.3 0.3) :resolution 3004 :maxiters 254 :filepath "~/tmp/mand.png")

;; Generator object:
;; Responsible for determining whether a point is within the set,
;; or more acurately, if it is not in the set, or stopping at maxiters.
;; Call get-value-at-point against it, to return the number of iterations up to maxiters
;;   for the point.

;; for now, since the only state a generator really has is maxitors, 
;;  we will simply make it that instead of a complex type.
;; In other words, we are simulating a clos like object with a maxitor slot,
;;  but its just a variable we expect to hold an int.

(declaim (inline make-generator))
(defun make-generator (&key maxiter)
  maxiter)

(declaim (inline maxiter))
(defun maxiter (generator)
  generator)

(defun get-value-at-point (generator C)
  (declare (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0) (speed 3))
	   (type (complex single-float) C))
  (let* ((realz (realpart C))
	(imagz (imagpart C))
	(realc realz)
	(imagc imagz)
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
       (if (or (>= i maxiter)
               (> (+ real-sq imag-sq) 4.0s0))
           (return i))
       (psetq imagz (+ (* 2.0s0 (* realz imagz)) imagc)
              realz (+ (- real-sq imag-sq) realc))
       (psetq real-sq (* realz realz)
              imag-sq (* imagz imagz))
       (incf i))))

;; Crawler object : 
;;  In essense this is an iterator of a complex type.
;;  It iterates accross a range linearly in even increments.
;; Calling "get-next-point" against it returns a complex value and
;;  as a side effect increments the crawler.

(defun make-crawler (&key from to resolution)
  (cond 
    ((< resolution 1)
     (lambda () nil))
    ((= resolution 1) 
     (let ((spent '()))
       (lambda () 
	 (if (not spent)
	     (progn
	       (setq spent 't)
	       to)))))
    ((> resolution 1)     
     (let ((increment (/ (- to from) (- resolution 1)))
	   (times-left resolution)
	   (current from))
       (lambda ()
	 (if (> times-left 0)
	     (let ((temp current))
	       (setq temp current)
	       (setq current (+ current increment))
	       (incf times-left -1)
	       temp)))))))

(defun get-next-point (crawler)
  (funcall crawler))

;; -------------
;; The following functions are useful for actually getting data about the set.


;; given a range in the complex plain, calculate the values at :resolution points
;;  linearly accross this range and returns them as a list.
 
(defun create-list-accross-line (from to resolution maxiters)
  (let ((c (make-crawler :from from :to to :resolution resolution))
	(g (make-generator :maxiter maxiters)))
    (loop for point = (get-next-point c) then (get-next-point c) while point
       collect (get-value-at-point g point))))

;; given a integer _iteration_, maps RGB values into three setf-able slots (8 bits per field)
(defmacro map-iteration-to-color (iteration slot-for-red slot-for-blue slot-for-green)
 `(progn 
    (setf ,slot-for-red (mod (* ,iteration 77) 255))
    (setf ,slot-for-blue (mod (* ,iteration 21) 255))
    (setf ,slot-for-green (mod (* ,iteration 5) 255))))

;; create a png image on the disk at :filepath for the set spanning acrross :from :to,
;; with :resolution x resolution pixels, :maxiters iteration limit

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
		     (aref image colnum rownum 2)
		     (aref image colnum rownum 1)))))
     (zpng:write-png png filepath)))