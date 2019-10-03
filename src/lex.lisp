(in-package :galax)

(defmacro defnth (name l) `(defun ,name () (rand-nth ,l)))

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

(defnth lifeforms `(algae nanobes viruses))
(defnth adjective `(,(color) ,(color-pair) slimy oozing gaseous microscopic))
(defnth evolved `("evolved" "started reproducing" "begun self-replicating"))
(defnth place-adj `(dark deep smoky lava-filled wet icy sunny scorched))
(defnth place-noun `(canyons pits valleys mountaintops))

(defun alive-a (planet)
  `(,(adjective) ,(lifeforms) have ,(evolved)
     in the ,(place-adj) ,(place-noun) of planet ,(concatenate 'string planet "!")))

(defun alive-b (planet)
  `(in the ,(place-adj) ,(place-noun) of planet ,(concatenate 'string planet ",")
       ,(adjective) ,(lifeforms) have ,(concatenate 'string (evolved) "!")))

(defun cap (w) (format nil "~@(~a~)" w))

(defun lower-sym (s)
  (if (symbolp s) (format nil "~(~a~)" s) s))

(defun capcar (words)
  (cons (cap (car words))
        (mapcar #'lower-sym (cdr words))))

(defun itsalive (planet)
  (format nil "~{~a ~}" (capcar (if (zerop (random 2))
                                    (alive-a planet)
                                    (alive-b planet)))))

(comment
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
