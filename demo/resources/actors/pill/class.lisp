(in-package :ghostie-demo)

(defactor pill (actor)
  ((on-ground :accessor pill-on-ground :initform nil)
   (feet :accessor pill-feet :initform nil)))

(defmethod load-physics-body ((pill pill) actor-meta)
  (declare (ignore actor-meta))
  (let* ((body (call-next-method))
         (mass (cp-a:body-m (cpw:base-c body))))
    (setf (pill-feet pill) (car (cpw:body-shapes body)))
    (in-game (world)
      (let ((space (world-physics world)))
        ;; fix the character's rotation
        (let ((joint (cpw:make-joint (cpw:space-static-body space) body
                                     (lambda (body1 body2)
                                       (cp:damped-rotary-spring-new (cpw:base-c body1) (cpw:base-c body2)
                                                                    0d0 (* mass 240000d0) (* mass 25000d0))))))
          (cpw:space-add-joint space joint))))
    body))

;; track when our pill lands on the ground
(bind (:collision-pre :pill-start) ((pill pill) (object game-object) arbiter)
  (declare (ignore object))
  (let ((normal-y (cadar (cpw:arbiter-normals arbiter))))
    (when (and normal-y
               (<= normal-y -.6))
      (setf (pill-on-ground pill) t)
      ;; this helps disable momentary disconnects from the ground registering as
      ;; being midair
      (disable-binding :collision-separate :pill-separate :time .1))))

;; track when our pill separates from the ground
(bind (:collision-separate :pill-separate) ((pill pill) (object game-object) arbiter)
  (declare (ignore object arbiter))
  (setf (pill-on-ground pill) nil))

(defun pill-stop (pill)
  (when (and pill (game-object-physics-body pill))
    (let ((shape-c (cpw:base-c (pill-feet pill))))
      (setf (cp-a:shape-surface_v-x shape-c) 0d0
            (cp-a:shape-surface_v-y shape-c) 0d0))))

(defun pill-run (pill x)
  "Move the character on the HORizonal plane."
  (when (and pill (game-object-physics-body pill))
    (let ((body-c (cpw:base-c (game-object-physics-body pill))))
      (let ((vel (cp-a:body-v-x body-c))
            (y (/ (abs x) 3)))
        (when (< (abs vel) *character-max-run*)
          ;(setf (cp-a:shape-u shape-c) (if (zerop x) 0.4d0 0.1d0))
          (if (pill-on-ground pill)
              ;; if walking, apply a surface velocity
              (let ((shape-c (cpw:base-c (pill-feet pill))))
                (cp:body-activate body-c)
                (setf (cp-a:shape-surface_v-x shape-c) (coerce x 'double-float)
                      (cp-a:shape-surface_v-y shape-c) (coerce y 'double-float)))
              ;; if midair, apply a slight impulse
              (cp:body-apply-impulse body-c
                                     (* 0.02d0 x (cp-a:body-m body-c))
                                     0d0
                                     0d0 0d0)))))))

(defun pill-jump (pill &key (x 0d0) (y 300d0))
  "Make the character jump."
  (when (and pill (game-object-physics-body pill))
    (let* ((body-c (cpw:base-c (game-object-physics-body pill))))
      ;(dbg :debug "v-y: ~s ~s~%" (cp-a:body-v-y body-c) (actor-vel-avg-y pill))
      (when (and (pill-on-ground pill)
                 (< (abs (cp-a:body-v-y body-c)) 160)
                 (< (abs (actor-vel-avg-y pill)) 160))
        (setf (pill-on-ground pill) nil)
        ;(disable-binding :collision-separate :pill-start :time .5)
        (let* ((vel-x (cp-a:body-v-x body-c))
               (x (* x (- 1 (/ (abs vel-x) *character-max-run*)))))
          (cp:body-apply-impulse body-c
                                 (* (cp-a:body-m body-c) (coerce x 'double-float))
                                 (* (cp-a:body-m body-c) (coerce y 'double-float))
                                 0d0 0d0))))))

