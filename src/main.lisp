(in-package :galax)

(define-aspect ident id)
(define-aspect location x y z)
(define-aspect magnitude m)
(define-aspect name n)
(define-entity star (ident location magnitude name))

(defparameter *galaxy-radius-ly* (/ 105700 2))
(defparameter *galaxy-thickness-ly* 1000)

(defun init-random-number-generator ()
  (setf *random-state* (make-random-state t)))

(defun xyz (&key dist-type)
  (case dist-type
    (:galaxy (let* ((r (random *galaxy-radius-ly*))
                    (th (random (* 2 pi))))
               (list
                (* r (cos th))
                (* r (sin th))
                (- (random *galaxy-thickness-ly*)
                   (/ *galaxy-thickness-ly* 2)))))
    (:cube (list (random 1000)
                 (random 1000)
                 (random 1000)))
    (otherwise (error "bad dist-type"))))

(defun make-stars (n)
  (loop repeat n
     for id from 1
     collect
       (progn
         (destructuring-bind (x y z) (xyz :dist-type :galaxy)
           (create-entity 'star
                          :ident/id id
                          :name/n (make-name)
                          :location/x x
                          :location/y y
                          :location/z z
                          :magnitude/m (rand-nth '(o b a f g k m)))))))

(defun main (&rest _)
  (declare (ignore _))

  (init-random-number-generator)
  (let* ((stars (make-stars 10))
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
         (format t "~3a ~a~15a ~{~15,5f ~} ~a dist: ~15,0f~a~a~%"
                 j
                 (if (and (not (eq s ref-star))
                          (< (star-dist s ref-point) (star-dist best-star ref-point)))
                     "WARNING "
                     "")
                 (name/n s)
                 (star->pos s)
                 (magnitude/m s)
                 (star-dist s ref-point)
                 (if (eq s ref-star)
                     "<--------- SELF"
                     "")
                 (if (eq s best-star)
                     "<----- BEST"
                     "")))))

(comment (main))
