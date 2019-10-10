(in-package :galax)

(defun nextnext (n)
  (cond
    ((< n 10) (1+ n))
    ((< n 1000) (* 2 n))
    (t (* 100 (floor (/ (* 1.2 n) 100))))))

(defmacro with-perd (cnt-fn &body body)
  (let ((cur (gensym))
        (next (gensym))
        (nextp (gensym)))
    `(let* ((,cur 0)
            (,next 1)
            (,nextp (lambda ()
                      (incf ,cur)
                      (when (>= ,cur ,next)
                        (progn
                          (setf ,next (nextnext ,cur))
                          t))))
            (,cnt-fn ,nextp))
       ,@body)))

(defmacro when-perd (f &body body)
  `(when (funcall ,f)
     ,@body))

(defun strcat (&rest args) (format nil "~{~a~}" args))

(defun cap (w) (format nil "~@(~a~)" w))

(defun lower-sym (s)
  (if (symbolp s) (format nil "~(~a~)" s) s))
(lower-sym 'A)  ;;=> "a"
(lower-sym "A") ;;=> "A"

(defun capcar (words)
  (cons (cap (car words))
        (mapcar #'lower-sym (cdr words))))
(capcar '(a b c d e)) ;;=> '("A" "b" "c" "d" "e")

(defun seqstr (s)
  (format nil "~{~a~}" (interpose " " s)))

(defun x->str (x) (if (symbolp x) (symbol-name x) x))

(defun v* (a v)
  (loop for c in v collect (* a c)))

(defun v+ (v1 v2)
  (loop for c1 in v1 for c2 in v2 collect (+ c1 c2)))

(defun v- (v2 v1)
  (v+ v2 (v* -1 v1)))

(defun vdot (v1 v2)
  (loop for c1 in v1 for c2 in v2 sum (* c1 c2)))

(defun vmagsq (v) (vdot v v))

(defun vmag (v) (sqrt (vmagsq v)))

(defun vnorm (v) (v* (/ 1 (vmag v)) v))
