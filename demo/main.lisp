(in-package :ghostie-demo)

(defun sync-actor-position-window (world actor)
  "Keeps the camera position in sync with an actor."
  (let* ((position (game-object-position actor))
         (x (- (* (car position) .5)))
         (y (- (* (cadr position) .5))))
    (setf (world-position world) (list x (- y 50) (caddr (world-position world))))))

(defun start ()
  (let* ((game (ghostie::create-game "trees"))
         (pill nil))
    (bind :level-load (level)
      (setf pill (find-if (lambda (actor)
                            (eq (actor-name actor) :pilly))
                          (level-actors level))))

    (bind :key-release (key)
      (input-key-release game key))

    (bind :key-press (key)
      (input-key-press game key))

    (bind (:render-step 'render-step-key-handler) (world dt)
      (input-key-handler game world dt pill)
      (sync-actor-position-window (game-game-world game) pill))))

