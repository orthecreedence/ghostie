(in-package :ghostie)
;THE END.

(defclass actor (dynamic-object)
  ((vel-avg-x :accessor actor-vel-avg-x :initform 0d0)
   (vel-avg-y :accessor actor-vel-avg-y :initform 0d0)))

(defmacro defactor (class-name superclasses slots &rest class-options)
  "Abstraction of defclass, solves inter-package issues (in other words, allows
   a game to add its own actor class, and allows ghostie to see it in its
   namespace by importing it)."
  ;; just simple wrap around defobject
  `(defobject ,class-name ,superclasses ,slots ,@class-options))

(defmethod load-physics-body ((actor actor) actor-meta)
  (let ((mass (coerce (getf actor-meta :mass 50d0) 'double-float))
        (max-vel (coerce (getf actor-meta :max-velocity 1000d0) 'double-float))
        (friction (coerce (getf actor-meta :friction 0.9d0) 'double-float))
        (bb (calculate-game-object-bb actor))
        (physics-objects (getf actor-meta :physics))
        (position (mapcar (lambda (v)
                            (coerce v 'double-float))
                          (getf actor-meta :start-position '(0 0 0)))))
    (let* ((body (cpw:make-body (lambda () (cp:body-new mass 1d0))))
           (body-c (cpw:base-c body))
           (moment 0d0))
      (if physics-objects
          ;; load the physics objects from the meta
          (let ((bb-max (apply #'max bb)))
            (dolist (phys-obj physics-objects)
              (destructuring-bind (&key type position radius) phys-obj
                (let ((r (* radius bb-max))
                      (x (* (car position) bb-max))
                      (y (* (cadr position) bb-max)))
                  (incf moment (cp:moment-for-circle mass r 0d0 x y))
                  (unless (eq type :circle)
                    (error (format nil "Unsupported physics type: ~a~%" type)))
                  (let ((shape (cpw:make-shape :circle body
                                               (lambda (body)
                                                 (cp:circle-shape-new (cpw:base-c body)
                                                                      r x y)))))
                    (setf (cp-a:shape-u (cpw:base-c shape)) friction))))))

          ;; load a default physics object (a stupid circle in the center of
          ;; the actor)
          (let* ((radius (/ (- (cadddr bb) (cadr bb)) 2.5d0))
                 (x 0d0)
                 (y 0d0))
            (incf moment (cp:moment-for-circle mass radius 0d0 x y))
            (let ((shape (cpw:make-shape :circle body
                                         (lambda (body)
                                           (cp:circle-shape-new (cpw:base-c body)
                                                                radius x y)))))
              (setf (cp-a:shape-u (cpw:base-c shape)) friction))))
      (setf (cp-a:body-v-limit body-c) max-vel)
      (cp:body-set-pos body-c (car position) (cadr position))
      (cp:body-set-moment body-c moment)
      body)))

(defun update-actor-state (actor)
  (when (and actor (game-object-physics-body actor))
    (let ((alpha 1/1000)
          (vel-x (cp-a:body-v-x (cpw:base-c (game-object-physics-body actor))))
          (vel-y (cp-a:body-v-y (cpw:base-c (game-object-physics-body actor)))))
      ;(dbg :debug "avg-y: ~s~%" (+ (* alpha vel-y) (* (- 1d0 alpha) (actor-vel-avg-y actor))))
      (setf (actor-vel-avg-x actor) (+ (* alpha vel-x)
                                       (* (- 1d0 alpha) (actor-vel-avg-x actor)))
            (actor-vel-avg-y actor) (+ (* alpha vel-y)
                                       (* (- 1d0 alpha) (actor-vel-avg-y actor)))))))

