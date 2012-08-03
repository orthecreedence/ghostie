(in-package :ghostie)

(defparameter *actor-directory* "resources/actors")

(defclass actor (game-object)
  ((is-main :accessor actor-is-main :initform nil)
   (is-jumping :accessor actor-is-jumping :initform nil)))

(defun load-actors (actors-meta &key (scale '(1 1 1)))
  (let ((actors nil)
        (scale (if scale scale '(1 1 1))))
    (dolist (actor-info actors-meta)
      (let  ((svg-objs (svgp:parse-svg-file (format nil "~a/~a/objects.svg" *actor-directory* (getf actor-info :actor))
                                            :curve-resolution 20
                                            :invert-y t
                                            :ignore-errors t)))
        (let ((position (if (getf actor-info :start-pos)
                            (getf actor-info :start-pos)
                            '(0 0 0)))
              (actor (car (svg-to-game-objects svg-objs nil :object-type 'actor :scale scale))))
          (setf (game-object-position actor) position
                (actor-is-main actor) (getf actor-info :main))
          (push actor actors))))
    actors))

(defun move-actor (actor x)
  "Move the character on the HORizonal plane."
  (incf (car (game-object-position actor)) x))
