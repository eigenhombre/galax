(in-package :galax)

(define-aspect ident id)
(define-aspect name n)

;; Planets
(define-aspect habitability h)
(define-entity planet (ident name habitability))

;; Stars
(define-aspect location x y z)
(define-aspect magnitude m)
(define-aspect planets p)
(define-entity star (ident location magnitude name planets))

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
    (:cube (list (random 10000)
                 (random 10000)
                 (random 10000)))
    (otherwise (error "bad dist-type"))))

(defun planets-for-star (starname)
  (loop repeat (rand-int 10)
     for id from 1
     collect (create-entity 'planet
                            :ident/id id
                            :name/n (format nil "~a (~a-~a)"
                                            (make-name)
                                            starname
                                            id)
                            :habitability/h (random (1+ (random 99))))))
(defun make-stars (n)
  (loop repeat n
     for id from 1
     collect
       (let ((num-planets (rand-int 10))
             (star-name (make-name)))
         (destructuring-bind (x y z) (xyz :dist-type :cube)
           (create-entity 'star
                          :ident/id id
                          :name/n star-name
                          :location/x x
                          :location/y y
                          :location/z z
                          :planets/p (planets-for-star star-name)
                          :magnitude/m (rand-nth '(o b a f g k m)))))))

(defun main (&rest _)
  (declare (ignore _))

  (init-random-number-generator)
  (let* ((stars (make-stars 30))
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
         (progn
           (format t "~3a ~a~15a ~{~15,5f ~} M~a dist: ~15,0f~a~a~%"
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
                       ""))
           (loop for p in (planets/p s)
              do (format t
                         "   ~a ~25a H~a~%"
                         (ident/id p)
                         (name/n p)
                         (habitability/h p)))))))

(comment (main))
