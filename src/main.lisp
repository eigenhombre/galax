(in-package :galax)


(defparameter *planets-with-life* 0)
(defparameter *planets-with-intelligent-life* 0)
(defparameter *probes-in-flight* 0)

(defparameter *speedup* 0.5)
(defparameter *max-iters* 200000)
(defparameter *num-stars* 10000)
(defparameter *genesis-factor* (* *speedup* 1D-10))
(defparameter *life->intelligence* 1.0)
(defparameter *probe-failure-rate* 1D-4)
(defparameter *initial-life-level* (* *speedup* 1D-3))
(defparameter *knowledge-factor* (* *speedup* 4D-4))
(defparameter *probe-knowledge* 10)
(defparameter *evolution-probability* (* *speedup* 1D-2))
(defparameter *evolution-factor* (* *speedup* 1D-2))

(defparameter *galaxy-radius-ly* (/ 105700 2))
(defparameter *galaxy-thickness-ly* 10000)
(defparameter *starbox-thickness* 100000)

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

;; Probes
(define-aspect kinetics pos vel)

(defun make-probe-name ()
  (format nil "~:@(~{~a~}~)~x"
          (take 20 (make-name))
          (rand-int 1000)))

(define-entity probe (ident name kinetics)
  (source-planet :initarg :source-planet)
  (source-star :initarg :source-star :accessor source-star)
  (dest-star :initarg :dest-star :accessor dest-star))

(define-entity star (ident location magnitude name)
  (planets :initarg :planets :accessor planets :initform nil))

(define-entity planet (ident name habitability life knowledge probe-sending)
  (star :initarg :star))

(define-entity prime-mover (name kinetics))

(define-system genesis ((p habitability life))
  (when (and (zerop (life/level p))
             (< (random 1.0) (* *genesis-factor*
                                (habitability/h p))))
    (format t " ~a~%" (itsalive (name/n p)))
    (finish-output)
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
           (format t "  ~a ~%" (itthinks (name/n p)))
           (finish-output)
           (incf *planets-with-intelligent-life*)
           (setf (knowledge/level p) 1.0))
         (progn
           (incf (knowledge/level p)
                 (* *knowledge-factor*
                    (knowledge/level p)))
           (when (and (not (probe-sending/ready p))
                      (> (knowledge/level p)
                         *probe-knowledge*))
             (format t "   ~a is ready to send its first probe!~%"
                     (name/n p))
             (finish-output)
             (setf (probe-sending/ready p) t)))))
    ;; intelligence is still evolving:
    (t
     (when (< (random 1.0) (* *evolution-probability*
                              (habitability/h p)))
       (incf (life/level p)
             (* *evolution-factor* (life/level p)))))))

(define-system travel ((p kinetics))
  ;; For now, probe speed = speed of light == 1 (distances in years)
  (let* ((probe-pos (kinetics/pos p))
         (dest (dest-star p))
         (source (source-star p))
         (dest-loc (list (location/x dest)
                         (location/y dest)
                         (location/z dest)))
         (distance (vmag (v- dest-loc probe-pos))))
    (cond
      ((< (random 1.0) *probe-failure-rate*)
       (progn
         (format t "    ~a!~%" (failure-msg (name/n p)
                                            (name/n source)
                                            (name/n dest)))
         (destroy-entity p)))
      ((< distance 10)
       (progn
         (format t "     Probe ~a arrives at its destination star, ~a!~%"
                 (name/n p) (name/n dest))
         (finish-output)
         (decf *probes-in-flight*)
         (destroy-entity p)))
      (t (progn
           (incf (nth 0 probe-pos) (nth 0 (kinetics/vel p)))
           (incf (nth 1 probe-pos) (nth 1 (kinetics/vel p)))
           (incf (nth 2 probe-pos) (nth 2 (kinetics/vel p))))))))

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
    (:cube (list (random *starbox-thickness*)
                 (random *starbox-thickness*)
                 (random *starbox-thickness*)))
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
    (map-entities #'(lambda (_)
                      (declare (ignore _))
                      (incf ret)) 'planet)
    ret))

(defun show-stats (counter)
  (format t
          (strcat "After ~a kyr~p, "
                  "~a/~a planets ha~[ve~;s~:;ve~] life; "
                  "~a planet~p ha~[ve~;s~:;ve~] intelligent life; "
                  "~a probe~p ~[are~;is~:;are~] in flight.~%")
          counter
          counter
          *planets-with-life*
          (count-planets)
          *planets-with-life*
          (number-word *planets-with-intelligent-life*)
          *planets-with-intelligent-life*
          *planets-with-intelligent-life*
          (number-word *probes-in-flight*)
          *probes-in-flight*
          *probes-in-flight*))

(defun reset ()
  (clear-entities)
  (setf *planets-with-intelligent-life* 0)
  (setf *planets-with-life* 0))

(defun launch-probe (star planet tree)
  (let* ((neighbor (nearest-neighbor tree #'star->pos (star->pos star)))
         (probe-name (make-probe-name))
         (star-pos (list (location/x star)
                         (location/y star)
                         (location/z star)))
         (neighbor-pos (list (location/x neighbor)
                             (location/y neighbor)
                             (location/z neighbor))))
    (format t
            "    Launching probe ~a from planet ~@(~a~) to nearest star, ~:(~a~)!~%"
            probe-name
            (name/n planet)
            (name/n neighbor))
    (finish-output)
    (create-entity 'probe
                   :ident/id *probes-in-flight*
                   :name/n probe-name
                   :kinetics/pos star-pos
                   :kinetics/vel (vnorm (v- neighbor-pos star-pos))
                   :source-star star
                   :source-planet planet
                   :dest-star neighbor)
    (setf (probe-sending/launched planet) t)
    (incf *probes-in-flight*)))

(defun run ()
  (reset)
  (init-random-number-generator)
  (let* ((stars (make-stars *num-stars*))
         (tree (kdtree (copy-seq stars) #'star->pos 0)))
    (format t "~%~%~@(~r~) planets orbit ~r stars.~%~%"
            (count-planets) *num-stars*)
    (with-perd nextp
      (loop named edward repeat *max-iters*
            for counter from 1
            do (progn
                 (run-genesis)
                 (run-evolution)
                 (run-travel)
                 (loop for s in stars do
                   (loop for p in (planets s)
                         when (and (probe-sending/ready p)
                                   (not (probe-sending/launched p)))
                           do
                              (launch-probe s p tree)
                              ;;(return-from edward)
                         ))
                 (when-perd nextp (show-stats counter))))))
  (clear-entities)
  (format t "Done.~&"))

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
