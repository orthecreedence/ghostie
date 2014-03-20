(in-package :ghostie-demo)

(defun sync-actor-position-window (world actor)
  "Keeps the camera position in sync with an actor."
  (let* ((position (object-position actor))
         (x (- (* (car position) .5)))
         (y (- (* (cadr position) .5))))
    (setf (world-position world) (list x (- y 50) (caddr (world-position world))))))

(defun start ()
  (let* ((pill nil))
    (ghostie:create-game "trees"
      (lambda (game)
        ;(setf *log-level* :notice)

        ;; grab the "main actor" from the level (it has the name :pilly defined in
        ;; the level meta) once the level has loaded
        (bind :level-load (level)
          (setf pill (find-if (lambda (obj)
                                (and (subtypep (type-of obj) 'dynamic-object)
                                     (eq (object-id obj) :pilly)))
                              (level-objects level))))

        (bind :key-release (key)
          (input-key-release game key))

        (bind :key-press (key)
          (input-key-press game key))

        (bind :step (world dt)
          (input-key-handler game world dt pill)
          (sync-actor-position-window (game-world game) pill))))))

;(defun update-actor-state (actor)
;  (when (and actor (object-physics-body actor))
;    (let ((alpha 1/1000)
;          (vel-x (cp-a:body-v-x (cpw:base-c (object-physics-body actor))))
;          (vel-y (cp-a:body-v-y (cpw:base-c (object-physics-body actor)))))
;      ;(dbg :debug "avg-y: ~s~%" (+ (* alpha vel-y) (* (- 1d0 alpha) (actor-vel-avg-y actor))))
;      (setf (actor-vel-avg-x actor) (+ (* alpha vel-x)
;                                       (* (- 1d0 alpha) (actor-vel-avg-x actor)))
;            (actor-vel-avg-y actor) (+ (* alpha vel-y)
;                                       (* (- 1d0 alpha) (actor-vel-avg-y actor)))))))

