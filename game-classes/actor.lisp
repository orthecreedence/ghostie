(in-package :game-level)

(defparameter *actor-directory* "resources/actors")

(defclass actor (game-object) ())

(defun load-actors (actors-meta &key (scale '(1 1 1)))
  (let ((actors nil))
    (dolist (actor-info actors-meta)
      (let  ((svg-objs (svgp:parse-svg-file (format nil "~a/~a/objects.svg" *actor-directory* (getf actor-info :actor))
                                            :curve-resolution 20
                                            :invert-y t
                                            :ignore-errors t)))
        (format t "LOADING GHOSTIE!!~%")
        (let ((position (if (getf actor-info :start-pos)
                            (getf actor-info :start-pos)
                            '(0 0)))
              (actor (car (svg-to-game-objects svg-objs nil :object-type 'actor :scale scale))))
          (setf (game-object-position actor) position)
          (push actor actors))))
    actors))


