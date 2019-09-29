(defpackage galax
  (:use :cl
        :beast
        :cl-oju
        :syllab
        :cl-utilities
        :trivialtests
        :arrow-macros))

(in-package :galax)

(define-aspect ident id)
(define-aspect location x y z)
(define-aspect magnitude m)
(define-aspect name n)
(define-entity star (ident location magnitude name))

(defparameter *galaxy-radius-ly* (/ 105700 2))
(defparameter *galaxy-thickness-ly* 1000)

(defun make-stars (n)
  (loop repeat n
     for id from 1
     collect (let* (;;(r (random *galaxy-radius-ly*))
                    ;;(th (random (* 2 pi)))
                    ;;(x (* r (cos th)))
                    ;;(y (* r (sin th)))
                    #++(z (- (random *galaxy-thickness-ly*)
                             (/ *galaxy-thickness-ly* 2)))
                    (x (random 1000))
                    (y (random 1000))
                    (z (random 1000)))
               (create-entity 'star
                              :ident/id id
                              :name/n (make-name)
                              :location/x x
                              :location/y y
                              :location/z z
                              :magnitude/m (rand-nth '(o b a f g k m))))))

(defun init-random-number-generator ()
  (setf *random-state* (make-random-state t)))

(defun main (&rest _)
  (declare (ignore _))
  (init-random-number-generator)
  (loop for s in (make-stars 20)
     do (format t "~10@a ~20a ~10,2@f ~10,2@f ~10,2@f ~a~%"
                (format nil "~@R" (ident/id s))
                (name/n s)
                (location/x s)
                (location/y s)
                (location/z s)
                (magnitude/m s))))

(defun mknode (id coords axis l r)
  (make-array 5 :initial-contents (list id axis l r coords)))

(defmacro node-coords (node)
  `(elt ,node 4))

(defmacro node-axis (node)
  `(elt ,node 1))

(defmacro node-left (node)
  `(elt ,node 2))

(defmacro node-right (node)
  `(elt ,node 3))

(defun kdtree (points axis depth)
  (when points
    (let* ((k (length (car points))))
      (setf points (sort points
                         #'<
                         :key #'(lambda (a) (nth axis a))))
      (let ((median (floor (/ (length points) 2)))
            (axis (mod (1+ axis) k)))
        (mknode 1
                (nth median points)
                axis
                (kdtree (take median points)
                        axis
                        (1+ depth))
                (kdtree (drop (1+ median) points)
                        axis
                        (1+ depth)))))))

;; Agrees with https://en.wikipedia.org/wiki/K-d_tree:
(kdtree '((7 2) (5 4) (9 6) (4 7) (8 1) (2 3)) 0 0)
;;=>
'#((7 2) 1
   #((5 4) 0
     #((2 3) 1 NIL NIL)
     #((4 7) 1 NIL NIL))
   #((9 6) 0
     #((8 1) 1 NIL NIL)
     NIL))

(defun dist (p1 p2)
  (->> p2
    (mapcar #'- p1)
    (mapcar #'(lambda (x) (* x x)))
    (apply #'+)))

(defun nodedist (treenode p)
  (assert treenode)
  (dist (node-coords treenode) p))

(defun star->pos (s)
  (list (location/x s)
        (location/y s)
        (location/z s)))

(defun stardist (s p)
  (dist (star->pos s) p))

(defun printall (&rest xs)
  (loop for x in xs do (print x)))

;; Adapted from http://code.activestate.com/recipes/\
;; 577497-kd-tree-for-nearest-neighbor-search-in-a-k-dimensi/
(defun nearest-neighbor (tree origin)
  (let ((best #(nil 1E20)))
    (labels ((find-best (tree)
               (when tree
                 (let ((here-sd (nodedist tree origin))
                       (axis (node-axis tree)))
                   (when (< here-sd (elt best 1))
                     (setf best `#(,tree ,here-sd)))
                   (let* ((diff (- (elt origin axis)
                                   (elt (node-coords tree) axis)))
                          (close (if (> diff 0)
                                     (node-right tree)
                                     (node-left tree)))
                          (away (if (> diff 0)
                                    (node-left tree)
                                    (node-right tree))))
                     (find-best close)
                     (if (< (* diff diff) (elt best 1))
                         (find-best away)))))))
      (find-best tree))
    (node-coords (elt best 0))))

(let* ((stars (make-stars 10000))
       (points (mapcar #'star->pos stars))
       (tree (kdtree points 0 0))
       (ref-point '(0 0 0))
       (best-pos (nearest-neighbor tree ref-point)))
  (loop for p in points
     if (< (dist p ref-point) (dist best-pos ref-point))
     do (format t "Warning: ~a is smaller! (~a vs ~a) ~%"
                p
                (dist p ref-point)
                (dist best-pos ref-point)))
  (loop for c from 0 to 2
     do (assert (< (nth c best-pos) 100)))
  best-pos)
