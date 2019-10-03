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
(defnth evolved `(evolved "started reproducing" "begun self-replicating"))
(defnth place-adj `(dark deep smoky lava-filled wet icy sunny scorching))
(defnth place-noun `(canyons pits valleys mountaintops))


(defun itsalive ()
  (format nil "~@(~{~a ~}~)" `(,(adjective) ,(lifeforms) have ,(evolved)
                                in the ,(place-adj) ,(place-noun) of planet x!)))

(comment
 (loop repeat 10 collect (itsalive))
 ;;=>
 '("Green algae have started reproducing in the deep valleys of planet x! "
   "Gaseous algae have started reproducing in the lava-filled canyons of planet x! "
   "Blue algae have evolved in the dark mountaintops of planet x! "
   "Microscopic nanobes have started reproducing in the sunny mountaintops of planet x! "
   "Gaseous viruses have begun self-replicating in the smoky canyons of planet x! "
   "Slimy viruses have started reproducing in the scorching pits of planet x! "
   "Oozing viruses have begun self-replicating in the dark mountaintops of planet x! "
   "Yellow algae have evolved in the wet pits of planet x! "
   "Microscopic viruses have evolved in the sunny pits of planet x! "
   "Slimy viruses have started reproducing in the lava-filled canyons of planet x! "))
