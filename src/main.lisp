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

(defun mknode (obj axis l r)
  (make-array 4 :initial-contents (list obj axis l r)))

(defmacro definline (name args &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args ,@body)))

(definline node-obj (node)
  (elt node 0))

(definline node-axis (node)
  (elt node 1))

(definline node-left (node)
  (elt node 2))

(definline node-right (node)
  (elt node 3))

(defun kdtree (objects coords-key axis)
  (when objects
    (let* ((kdims (->> objects
                    car
                    (funcall coords-key)
                    length)))
      (setf objects (sort objects
                          #'<
                          :key #'(lambda (a)
                                   (nth axis (funcall coords-key a)))))
      (let ((median (floor (/ (length objects) 2)))
            (next-axis (mod (1+ axis) kdims)))
        (mknode (nth median objects)
                axis
                (kdtree (take median objects)
                        coords-key
                        next-axis)
                (kdtree (drop (1+ median) objects)
                        coords-key
                        next-axis))))))

;; Agrees with https://en.wikipedia.org/wiki/K-d_tree:
(kdtree '((7 2) (5 4) (9 6) (4 7) (8 1) (2 3))
        #'identity
        0)
;;=>
'#(1 1 #(1 0 #(1 1 NIL NIL (2 3)) #(1 1 NIL NIL (4 7)) (5 4))
   #(1 0 #(1 1 NIL NIL (8 1)) NIL (9 6)) (7 2))
;;=>
'#((7 2) 1
   #((5 4) 0
     #((2 3) 1 NIL NIL)
     #((4 7) 1 NIL NIL))
   #((9 6) 0
     #((8 1) 1 NIL NIL)
     NIL))

(defun star->pos (s)
  (when s
    (list (location/x s)
          (location/y s)
          (location/z s))))

(defun dist (p1 p2)
  (->> p2
    (mapcar #'- p1)
    (mapcar #'(lambda (x) (* x x)))
    (apply #'+)))

(defun nodedist (treenode p)
  (assert treenode)
  (dist (node-coords treenode) p))

(defun star-dist (s p)
  (dist (star->pos s) p))

(defun printall (&rest xs)
  (loop for x in xs do (progn (princ x) (princ " "))))

(defun print-node (x tree)
  (when tree
    (printall x (node-obj tree) (star->pos (node-obj tree))))
  (terpri))

;; Adapted from http://code.activestate.com/recipes/\
;; 577497-kd-tree-for-nearest-neighbor-search-in-a-k-dimensi/
(defun nearest-neighbor (tree coord-fn origin)
  (let ((best '(nil 1E20)))
    (labels ((find-best (tree)
               (when tree
                 (let* ((tree-pos (funcall coord-fn (node-obj tree)))
                        (here-sd (dist tree-pos origin))
                        (axis (node-axis tree)))
                   (when (and (not (equal origin (star->pos (node-obj tree))))
                              (< here-sd (cadr best)))
                     #++(format t "best: ~15a ~{~4a ~}   cur: ~15a ~{~4a ~}  dist: ~a~%"
                                (if (car best)
                                    (name/n (node-obj (car best)))
                                    nil)
                                (if (car best)
                                    (star->pos (node-obj (car best)))
                                    '(nil nil nil))
                                (name/n (node-obj tree))
                                (star->pos (node-obj tree))
                                here-sd)
                     (setf best (list tree here-sd)))
                   (let* ((diff (- (elt origin axis)
                                   (elt (funcall coord-fn (node-obj tree)) axis)))
                          (close (if (> diff 0)
                                     (node-right tree)
                                     (node-left tree)))
                          (away (if (> diff 0)
                                    (node-left tree)
                                    (node-right tree))))
                     (find-best close)
                     (when (< (* diff diff) (cadr best))
                       (find-best away)))))))
      (find-best tree))
    (node-obj (car best))))

(let* ((stars (make-stars 1000))
       (tree (kdtree (copy-seq stars) #'star->pos 0))
       (ref-star (rand-nth stars))
       (ref-point (star->pos ref-star))
       (best-star (nearest-neighbor tree #'star->pos ref-point)))
  (format t "REF STAR: ~10a ~{~4a ~}~%"
          (name/n ref-star)
          (star->pos ref-star))
  (format t "BEST STAR: ~a~%" (name/n best-star))
  (loop for s in stars
     for j from 0
     do
       (format t "~3a ~a~15a ~{~4a ~} dist: ~a~a~a~%"
               j
               (if (and (not (eq s ref-star))
                        (< (star-dist s ref-point) (star-dist best-star ref-point)))
                   "WARNING "
                   "")
               (name/n s)
               (star->pos s)
               (star-dist s ref-point)
               (if (eq s ref-star)
                   "<--------- SELF"
                   "")
               (if (eq s best-star)
                   "<----- BEST"
                   ""))))
