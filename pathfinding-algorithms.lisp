;;;; pathfinding-algorithms.lisp

(in-package #:pathfinding-algorithms)

;;; Data structure providiers
;; These are wrapped so that we can test different datastructures for speed later.

(defun make-unbounded-fifo-queue ()
  (make-instance 'jpl-queues:unbounded-fifo-queue))

(defun enqueue (item queue)
  (jpl-queues:enqueue item queue))

(defun dequeue (queue)
  (jpl-queues:dequeue queue))

;; API

(defun find-minimum-distance (start end get-adjacent-fn &key (test #'eql))
  "Breadth first search for finding the minimum distance between two points.
If points cannot be reached, the result will be -1
get-adjacent-fn should be a lambda taking in a node, yielding a list of adjactent nodes."
  (or (loop named outer
	 with q = (make-unbounded-fifo-queue)
	 with hash = (make-hash-table :test test)
	 for next = t then (dequeue q)
	 while next
	 for item = start then (car next)
	 for distance = 0 then (cdr next)
	 for next-distance = (1+ distance)
	 do (loop for next-item in (funcall get-adjacent-fn item)
	       do (cond ((funcall test next-item end) (return-from outer next-distance))
			((not (gethash next-item hash))
			 (setf (gethash next-item hash) next-distance)
			 (enqueue (cons next-item next-distance) q)))))
      -1 ; If there are no results
      ))

(defun find-minimum-distances (points get-adjacent-fn &key (test #'eql))
  "Make a map of the distances between a set of points.
If points cannot be reached, the result will be -1
get-adjacent-fn should be a lambda taking in a node and yielding a list of adjactent nodes."
  (let* ((point-len (length points))
	 (result (make-array (list point-len point-len) :element-type 'fixnum))
	 (pointhash (make-hash-table :test test)))
    (loop for p in points
       for i from 0
       do (setf (gethash p pointhash) i))
    (loop for start in points
       for start-index from 0
       do
	 (loop
	    with q = (make-unbounded-fifo-queue)
	    with hash = (make-hash-table :test test)
	    for next = t then (dequeue q)
	    while next
	    for item = start then (car next)
	    for distance = 0 then (cdr next)
	    for next-distance = (1+ distance)
	    do (loop for next-item in (funcall get-adjacent-fn item)
		  for end-index = (gethash next-item pointhash)
		  do
		    (unless (gethash next-item hash)
		      (when (and end-index
				 (not (= end-index start-index)))
			(setf (aref result start-index end-index) next-distance
			      (aref result end-index start-index) next-distance))
		      (setf (gethash next-item hash) next-distance)
		      (enqueue (cons next-item next-distance) q)))))
    result))

(defun make-distance-map (points get-distance-fn)
  "Make a 2-dimensional lookup array for distances between a set of points.
* points should be a list of points.
* get-distance-fun should be a lambda that takes in two points."
  (let* ((point-count (length points))
	 (distances (make-array (list point-count point-count))))
    (loop for (point1 . rest) on points
       for point1index from 0
       do (loop for point2 in rest
	     for point2index from (1+ point1index)
	     for distance = (funcall get-distance-fn point1 point2)
	     do
	       (setf (aref distances point1index point2index) distance
		     (aref distances point2index point1index) distance)))
    distances))

(defun int-array-swap! (arr index1 index2)
  (declare (optimize (speed 3) (safety 0))
	   (type (simple-array fixnum) arr))
  (let ((x (aref arr index1)))
    (setf (aref arr index1) (aref arr index2)
	  (aref arr index2) x)))

(defun fixnum-array-replace-subseq! (source dest start end)
  (declare (optimize (speed 3) (safety 0))
	   (type (simple-array fixnum) source dest)
	   (type fixnum start end))
  (loop for i from start to end
       do (setf (aref dest i) (aref source i))))

(defun factorial (n)
  "If the result is more than fixnum.max we are the algorithm will run for way too long so just return 0."
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum n))
  (let ((result 1))
    (declare (type fixnum result))
    (loop for x fixnum from n downto 2
       do (setf result (* result x)))
    result))

(defun find-shortest-path (distance-map)
  "Return the the shortest path through the enumerated set of points using brute-force.
Assumes that the first and last points in the map are mandatory start and end points.

Expected input is a (simple-array (n n)) where (aref arr x y) returns the distance between x and y as a numeral."
  (declare (optimize (speed 3))
	   (type (simple-array fixnum) distance-map))
  (let* ((result-size (array-dimension distance-map 0))
	 (path (make-array result-size
			   :element-type 'fixnum
			   :initial-contents (loop for i fixnum from 0 to (1- result-size) collect i)))
	 (min-path (make-array result-size :element-type 'fixnum :initial-contents path))
	 (min-distance 0)
	 (swap-limit (- result-size 2))
	 ;; Index an stack for the heap permutation algorithm embedded in this function.
	 (i 0)
	 (perm-stack (make-array swap-limit :element-type 'fixnum)))
    (declare (type fixnum result-size swap-limit i)
	     (type (simple-array fixnum) path min-path perm-stack))
    (labels ((find-distance (point1 point2) (aref distance-map point1 point2))
	     (find-full-distance ()
	       (loop for i fixnum from 0 to (- result-size 2)
		  sum (find-distance (aref path i) (aref path (1+ i))))))
      
      (setf min-distance (find-full-distance))
	
      (loop while (< i swap-limit)
	 do
	   (if (< (aref perm-stack i) i)
	       (progn
		 ;; Heap permutation algorithm stuff
		 (let ((source (if (evenp i) 1 (1+ (aref perm-stack i))))
		       (dest (1+ i)))
		   (declare (type fixnum source dest))
		   (array-swap! path source dest)
		   (incf (aref perm-stack i))
		   (setf i 0))
		 ;; What we do with each permutation
		 (let ((dist (find-full-distance)))
		   (when (< dist min-distance)
		     (setf min-distance dist)
		     (fixnum-array-replace-subseq! path min-path 1 (- result-size 2)))))
	       ;; else, more heap permutation algorithm stuff
	       (setf (aref perm-stack i) 0
		     i (1+ i))))
      (values min-path min-distance))))
