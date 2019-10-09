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
(defnth lifeforms `(algae nanobes viruses
                          "carbon chains"
                          "complex molecules"))
(defnth adjective `(,(color) ,(color-pair) slimy oozing gaseous vitreous microscopic metallic))
(defnth evolved `("evolved" "started reproducing" "begun self-replicating"))
(defnth place-adj `(dark smoky lava-filled wet icy sunny scorched))
(defun place-phrase ()
  (either
   `(on the ,(place-adj) ,(on-place-noun))
   `(in the ,(place-adj) ,(in-place-noun))))
(defnth on-place-noun `(plateaus peaks mountaintops hills deserts))
(defnth in-place-noun `(canyons rifts pits valleys craters))

(defmacro one-of (&rest forms)
  (let* ((l (length forms))
         (cases (loop for f in forms
                   for i from 0
                   collect `(,i ,f))))
    `(case (random ,l) ,@cases)))

(defmacro either (a b) `(one-of ,a ,b))

(defun itsalive (planet)
  (->> (either
        `(,(adjective) ,(lifeforms) have ,(evolved)
           ,@(place-phrase) of planet ,(strcat (x->str planet) "!"))
        `(,@(place-phrase) of planet ,(strcat (x->str planet) ",")
            ,(adjective) ,(lifeforms) have ,(strcat (evolved) "!")))
       capcar
       seqstr))

(defnth intelligence `(intelligent conscious self-aware))

(defun lifeforms-have () (one-of `(life forms have)
                                 `(life has)
                                 `(living beings have)))

(defun itthinks (planet)
  (->> (either `(,@(lifeforms-have) become ,(intelligence)
                   on ,(strcat planet "!"))
               `(on ,(strcat planet ",") ,@(lifeforms-have)
                    become ,(strcat (lower-sym (intelligence)) "!")))
       capcar
       seqstr))

(comment
 (itthinks "x") ;;=> '"Life has become conscious on x! "

 (loop repeat 10 collect (itsalive "X"))
 ;;=>
 '("Reddish-black viruses have started reproducing on the sunny hills of planet X!"
   "White viruses have evolved in the smoky canyons of planet X!"
   "On the wet deserts of planet X, grey-green complex molecules have started reproducing!"
   "Red complex molecules have evolved on the dark mountaintops of planet X!"
   "Metallic nanobes have evolved in the sunny valleys of planet X!"
   "In the lava-filled craters of planet X, vitreous carbon chains have evolved!"
   "Orange-red carbon chains have evolved in the smoky pits of planet X!"
   "Metallic algae have begun self-replicating in the scorched valleys of planet X!"
   "Oozing carbon chains have begun self-replicating in the scorched canyons of planet X!"
   "On the dark peaks of planet X, oozing complex molecules have evolved!")
 )

