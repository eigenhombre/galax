(in-package :galax)

(defmacro defnth (name l) `(defun ,name () (rand-nth ,l)))
(defun cap (w) (format nil "~@(~a~)" w))

(defun lower-sym (s)
  (if (symbolp s) (format nil "~(~a~)" s) s))
(lower-sym 'A)  ;;=> "a"
(lower-sym "A") ;;=> "A"

(defun capcar (words)
  (cons (cap (car words))
        (mapcar #'lower-sym (cdr words))))
(capcar '(a b c d e)) ;;=> '("A" "b" "c" "d" "e")

(defun seqstr (s) (format nil "~{~a ~}" s))

(defun x->str (x) (if (symbolp x) (symbol-name x) x))
(defun strcat (a b) (concatenate 'string (x->str a) (x->str b)))

(defnth color `(grey white yellow orange red purple blue black green))

(defnth color-pair `(greyish-white
                     greyish-yellow
                     yellow-orange
                     orange-red
                     purplish-blue
                     greenish-black
                     blue-green
                     grey-green
                     purple-grey
                     reddish-black))
(defnth lifeforms `(algae nanobes viruses
                          "carbon chains"
                          "complex molecules"))
(defnth adjective `(,(color) ,(color-pair) slimy oozing gaseous vitreous microscopic metallic))
(defnth evolved `("evolved" "started reproducing" "begun self-replicating"))
(defnth place-adj `(dark deep smoky lava-filled wet icy sunny scorched))

(defnth place-noun `(canyons pits valleys mountaintops))

(defmacro one-of (&rest forms)
  (let* ((l (length forms))
         (cases (loop for f in forms
                   for i from 0
                   collect `(,i ,f))))
    `(case (random ,l) ,@cases)))

(defmacro either (a b) `(one-of ,a ,b))

(defun itsalive (planet)
  (seqstr (capcar (either
                   `(,(adjective) ,(lifeforms) have ,(evolved)
                      in the ,(place-adj) ,(place-noun) of planet ,(strcat planet "!"))
                   `(in the ,(place-adj) ,(place-noun) of planet ,(strcat planet ",")
                        ,(adjective) ,(lifeforms) have ,(strcat (evolved) "!"))))))

(defnth intelligence `(intelligent conscious self-aware))

(defun lifeforms-have () (one-of `(life forms have)
                                 `(life has)
                                 `(living beings have)))

(defun itthinks (planet)
  (seqstr (capcar
           (either `(,@(lifeforms-have) become ,(intelligence) on ,(strcat planet "!"))
                   `(on ,(strcat planet ",") ,@(lifeforms-have) become ,(strcat (lower-sym (intelligence)) "!"))))))

(comment
 (itthinks "x") ;;=> '"Life has become conscious on x! "

 (loop repeat 10 collect (itsalive "X"))
 ;;=>
 '("In the wet canyons of planet x, grey-green algae have begun self-replicating! "
   "Microscopic nanobes have started reproducing in the lava-filled canyons of planet x! "
   "Greyish-white nanobes have begun self-replicating in the scorched pits of planet x! "
   "Grey-green nanobes have begun self-replicating in the sunny mountaintops of planet x! "
   "Purplish-blue algae have evolved in the icy mountaintops of planet x! "
   "In the dark valleys of planet x, gaseous algae have started reproducing! "
   "Greyish-yellow viruses have begun self-replicating in the sunny canyons of planet x! "
   "Oozing viruses have started reproducing in the scorched valleys of planet x! "
   "Greyish-white viruses have started reproducing in the wet valleys of planet x! "
   "In the sunny canyons of planet x, oozing viruses have begun self-replicating! "))
