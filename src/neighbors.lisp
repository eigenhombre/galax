(in-package :galax)

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
'#((7 2) 0 #((5 4) 1 #((2 3) 0 NIL NIL) #((4 7) 0 NIL NIL))
   #((9 6) 1 #((8 1) 0 NIL NIL) NIL))

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

(defun star-dist (s p)
  (dist (star->pos s) p))

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
