;;;; test.lisp

(defpackage #:pathfinding-algorithms.test
  (:use #:cl #:pathfinding-algorithms #:fiveam))

(in-package #:pathfinding-algorithms.test)

(defstruct maze
  start end map)

;; A set of mazes where;
;; 0 := space
;; 1 := wall
;; 2 := intemediary objective
;; It is invalid to have intemediary objectives on start and end point for now
;; This will work, it just produces wierd behaviour.

(defvar *maze0*
  (make-maze
   :start '(0 0)
   :end '(1 1)
   :map '((0 0)
	  (0 0))))

(defvar *maze1*
  (make-maze
   :start '(0 0)
   :end '(2 2)
   :map '((0 0 0)
	  (0 0 0)
	  (0 0 0))))

(defvar *maze2*
  (make-maze
   :start '(0 0)
   :end '(2 2)
   :map '((0 2 1)
	  (1 2 1)
	  (1 0 0))))

(defvar *maze3*
  (make-maze
   :start '(0 0)
   :end '(2 2)
   :map '((0 2 2)
	  (2 2 2)
	  (2 2 0))))

(defvar *maze4*
  (make-maze
   :start '(0 0)
   :end '(2 0)
   :map '((0 1 0 0)
	  (0 1 1 0)
	  (0 1 1 0)
	  (0 0 0 0))))

(defun is-cell-free (maze x y)
  (and (>= y 0)
       (>= x 0)
       (let ((n (nth x (nth y maze))))
	 (and n (not (eq 1 n))))))

(defun find-adjacent (maze)
  (lambda (node)
    (let ((x (first node))
	  (y (second node))
	  result)
      (flet ((add-adjacent (x1 y1)
	       (when (is-cell-free maze x1 y1)
		 (push (list x1 y1) result))))
	(add-adjacent (1+ x) y)
	(add-adjacent (1- x) y)
	(add-adjacent x (1+ y))
	(add-adjacent x (1- y)))
      result)))

(defun find-checkpoints (maze)
  (loop for l in (maze-map maze)
     for y from 0
     nconc (loop for r in l
	      for x from 0
	      when (= 2 r) collect (list x y))))

(test maze-distance
  "Find the distance between the start and end pints of the maze"
  (flet ((min-dist (maze)
	   (find-minimum-distance
	    (maze-start maze)
	    (maze-end maze)
	    (find-adjacent (maze-map maze))
	    :test #'equal)))
    (is (= 2 (min-dist *maze0*)))
    (is (= 4 (min-dist *maze1*)))
    (is (= 4 (min-dist *maze2*)))
    (is (= 4 (min-dist *maze3*)))
    (is (= 10 (min-dist *maze4*)))))

(defun shortest-path-test (maze)
  (let* ((points (concatenate 'list
			      (list (maze-start maze))
			      (find-checkpoints maze)
			      (list (maze-end maze))))
	 (distances (find-minimum-distances
		     points
		     (find-adjacent (maze-map maze))
		     :test #'equal)))
    (multiple-value-bind (arr dist)
	(find-shortest-path distances)
      (values (list dist (coerce arr 'list)) points distances))))

(test maze-shortest-path
  "Find the shortest path between a set of points, with fixed start and end points"
  (is (equal '(2 (0 1)) (shortest-path-test *maze0*)))
  (is (equal '(4 (0 1)) (shortest-path-test *maze1*)))
  (is (equal '(4 (0 1 2 3)) (shortest-path-test *maze2*)))
  (is (equal 8 (car (shortest-path-test *maze3*))))
  (is (equal '(10 (0 1)) (shortest-path-test *maze4*))))
