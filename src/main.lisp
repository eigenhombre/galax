(in-package :galax)


(defparameter *planets-with-life* 0)
(defparameter *planets-with-intelligent-life* 0)
(defparameter *probes-sent* 0)

(defparameter *speedup* 5)
(defparameter *genesis-factor* (* *speedup* 1E-10))
(defparameter *life->intelligence* 1.0)
(defparameter *initial-life-level* (* *speedup* 1E-3))
(defparameter *knowledge-factor* (* *speedup* 4E-4))
(defparameter *probe-knowledge* 10)
(defparameter *evolution-probability* (* *speedup* 1E-2))
(defparameter *evolution-factor* (* *speedup* 1E-2))

(defparameter *galaxy-radius-ly* (/ 105700 2))
(defparameter *galaxy-thickness-ly* 1000)

;; Common aspects
(define-aspect ident id)
(define-aspect name n)

;; Planets
(define-aspect habitability h)
(define-aspect knowledge (level :initform 0))
(define-aspect life (level :initform 0))
(define-aspect probe-sending
  (ready :initform nil)
  (launched :initform nil))

;; Stars
(define-aspect location x y z)
(define-aspect magnitude m)

(define-entity star (ident location magnitude name)
  (planets :initarg :planets :accessor planets :initform nil))

(define-entity planet (ident name habitability life knowledge probe-sending)
  (star :initarg :star))

;; Probes
(defun make-probe-name ()
  (format nil "~:@(~{~a~}~)~x"
          (take 20 (make-name))
          (rand-int 1000)))

(define-entity probe (ident name location)
  (source-planet :initarg :source-planet)
  (source-star :initarg :source-star)
  (dest-star :initarg :dest-star))

(define-system genesis ((p habitability life))
  ;; (format t "genesis on ~a~%" (name/n p))
  (when (and (zerop (life/level p))
             (< (random 1.0) (* *genesis-factor*
                                (habitability/h p))))
    (format t "~a~%" (itsalive (name/n p)))
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
           (format t "~a~%" (itthinks (name/n p)))
           (incf *planets-with-intelligent-life*)
           (setf (knowledge/level p) 1.0))
         (progn
           (incf (knowledge/level p)
                 (* *knowledge-factor*
                    (knowledge/level p)))
           (when (and (not (probe-sending/ready p))
                      (> (knowledge/level p)
                         *probe-knowledge*))
             (format t "~a is ready to send its first probe!~%"
                     (name/n p))
             (setf (probe-sending/ready p) t)))))
    ;; intelligence is still evolving:
    (t
     (when (< (random 1.0) (* *evolution-probability*
                              (habitability/h p)))
       (incf (life/level p)
             (* *evolution-factor* (life/level p)))))))

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

(defun planets-for-star (star)
  (loop repeat (rand-int 10)
     for id from 1
     collect (create-entity 'planet
                            :ident/id id
                            :name/n (format nil
                                            "~a (~a-~@R)"
                                            (format nil "~@(~a~)"
                                                    (make-name))
                                            (name/n star)
                                            id)
                            :habitability/h (random (1+ (random 99)))
                            :life/level 0.0
                            :star star)))

(defun make-stars (n)
  (loop repeat n
     for id from 1
     collect
       (let ((star-name (format nil "~@(~a~)" (make-name))))
         (destructuring-bind (x y z) (xyz :dist-type :cube)
           (let ((star
                  (create-entity 'star
                                 :ident/id id
                                 :name/n star-name
                                 :location/x x
                                 :location/y y
                                 :location/z z
                                 :magnitude/m (rand-nth '(o b a f g k m)))))
             (setf (planets star) (planets-for-star star))
             star)))))

(defun count-planets ()
  (let ((ret 0))
    (map-entities #'(lambda (_) (declare (ignore _)) (incf ret)) 'planet)
    ret))

(defun maybe-no (n)
  (if (zerop n)
      "no"
      n))

(defun show-stats (counter)
  (format t
          (strcat "Time ~a kyr~p; "
                  "~a/~a planets ha~[ve~;s~:;ve~] life; "
                  "~a planet~p ha~[ve~;s~:;ve~] intelligent life; "
                  "~a probe~p sent.~%")
          counter
          counter
          *planets-with-life*
          (count-planets)
          *planets-with-life*
          (maybe-no *planets-with-intelligent-life*)
          *planets-with-intelligent-life*
          *planets-with-intelligent-life*
          (maybe-no *probes-sent*)
          *probes-sent*))

(defun reset ()
  (clear-entities)
  (setf *planets-with-intelligent-life* 0)
  (setf *planets-with-life* 0))

(defun launch-probe (star planet tree)
  (let* ((neighbor (nearest-neighbor tree #'star->pos (star->pos star)))
         (probe-name (make-probe-name)))
    (format t
            "Launching probe ~a from planet ~@(~a~) to nearest star, ~:(~a~)!~%"
            probe-name
            (name/n planet)
            (name/n neighbor))
    (make-instance 'probe
                   :ident/id *probes-sent*
                   :name/n probe-name
                   :location/x (location/x star)
                   :location/y (location/y star)
                   :location/z (location/z star)
                   :source-star star
                   :source-planet planet
                   :dest-star neighbor)
    (setf (probe-sending/launched planet) t)
    (incf *probes-sent*)))

(defun run ()
  (reset)
  (init-random-number-generator)
  (let* ((num-stars 10000)
         (stars (make-stars num-stars))
         (tree (kdtree (copy-seq stars) #'star->pos 0)))
    (format t "~%~%~@(~r~) planets surround ~a stars.~%~%"
            (count-planets) num-stars)
    (with-perd nextp
      (loop named edward repeat 20000
         for counter from 1
         do (progn
              (run-genesis)
              (run-evolution)
              (loop for s in stars do
                   (loop for p in (planets s)
                      when (and (probe-sending/ready p)
                                (not (probe-sending/launched p)))
                      do
                        (launch-probe s p tree)
                        (return-from edward)))
              (when-perd nextp (show-stats counter))))))
  (clear-entities))

(defun main (&rest _)
  (declare (ignore _))
  (handler-case
      (progn
        (run))
    (t (e)
      (declare (ignore e))
      (format t "~%The universe comes to a sudden end.~%"))))

(comment
 (main))
