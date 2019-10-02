(in-package :galax)

(define-aspect ident id)
(define-aspect name n)

(defparameter *planets-with-life* 0)
(defparameter *planets-with-intelligent-life* 0)

(defparameter *genesis-factor* 1E-8)
(defparameter *life->intelligence* 1.0)
(defparameter *initial-life-level* 1E-3)
(defparameter *knowledge-factor* 1E-4)
(defparameter *probe-knowledge* 10)
(defparameter *evolution-probability* 1E-2)
(defparameter *evolution-factor* 1E-2)

;; Planets
(define-aspect habitability h)
(define-aspect knowledge (level :initform 0))
(define-aspect life (level :initform 0))
(define-aspect probe (ready :initform nil))
(define-entity planet (ident name habitability life knowledge probe))

(define-system genesis ((p habitability life))
  ;;(format t "genesis on ~a~%" (name/n p))
  (when (and (zerop (life/level p))
             (< (random 1.0) (* *genesis-factor*
                                (habitability/h p))))
    (format t "Life has started on ~a!~%"
            (name/n p))
    (incf *planets-with-life*)
    (setf (life/level p) *initial-life-level*)))

(define-system evolution ((p habitability knowledge life))
  (cond
    ;; nothing to evolve:
    ((zerop (life/level p)) nil)
    ;; intelligence has started:
    ((> (life/level p) *life->intelligence*)
     (if (zerop (knowledge/level p))
         (progn
           (format t "Intelligent life has started on ~a!~%"
                   (name/n p))
           (incf *planets-with-intelligent-life*)
           (setf (knowledge/level p) 1.0))
         (progn
           (incf (knowledge/level p)
                 (* *knowledge-factor*
                    (knowledge/level p)))
           (when (and (not (probe/ready p))
                      (> (knowledge/level p)
                         *probe-knowledge*))
             (format t "~a is ready to send its first probe!~%"
                     (name/n p))
             (setf (probe/ready p) t)))))
    ;; intelligence is still evolving:
    (t
     (when (< (random 1.0) (* *evolution-probability*
                              (habitability/h p)))
       (incf (life/level p)
             (* *evolution-factor* (life/level p)))))))

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
                            :habitability/h (random (1+ (random 99)))
                            :life/level 0.0)))

(defun make-stars (n)
  (loop repeat n
     for id from 1
     collect
       (let ((star-name (make-name)))
         (destructuring-bind (x y z) (xyz :dist-type :cube)
           (create-entity 'star
                          :ident/id id
                          :name/n star-name
                          :location/x x
                          :location/y y
                          :location/z z
                          :planets/p (planets-for-star star-name)
                          :magnitude/m (rand-nth '(o b a f g k m)))))))

(defun show-stars (stars)
  (let* ((tree (kdtree (copy-seq stars) #'star->pos 0))
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

(defun show-stats (counter)
  (format t
          "T ~a: L ~a K ~a~%"
          counter
          *planets-with-life*
          *planets-with-intelligent-life*))

(defun reset ()
  (clear-entities)
  (setf *planets-with-intelligent-life* 0)
  (setf *planets-with-life* 0))

(defun main (&rest _)
  (declare (ignore _))

  (reset)
  (init-random-number-generator)
  (make-stars 10000)
  (loop repeat 40000
     for counter from 1
     do (progn
          (run-genesis)
          (run-evolution)
          (when (zerop (mod counter 100))
            (show-stats counter))))
  (clear-entities))

(comment
 (main)

 (reset)
 (make-stars 30)
 (progn
   (loop repeat 100000 do
        (progn
          (run-genesis)
          (run-evolution)))
   (format t "<<<loop done>>>~%"))
 (show-stats 0))

