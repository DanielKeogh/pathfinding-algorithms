;;;; pathfinding-algorithms.asd

(asdf:defsystem #:pathfinding-algorithms
  :description "A pile of pathfinding algorithms"
  :author "Daniel Keogh <keogh.daniel@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :depends-on (:fiveam :jpl-queues)
  :serial t
  :components ((:file "package")
               (:file "pathfinding-algorithms")
	       (:file "test")))
