(in-package :ghostie)
;THE END.

(defclass actor (game-object)
  ((is-main :accessor actor-is-main :initform nil)
   (vel-avg-x :accessor actor-vel-avg-x :initform 0d0)
   (vel-avg-y :accessor actor-vel-avg-y :initform 0d0)))

(defun load-actor-physics-body- (actor actor-meta)
  (let ((mass (getf actor-meta :mass 50d0))
        (num-circles (getf actor-meta :num-circles 3))
        (bb (calculate-game-object-bb actor)))
    (let ((body (cpw:make-body (lambda () (cp:body-new mass 1d0))))
          (position (getf actor-meta :start-pos '(0 0 0))))
      (let* ((max-vel (getf actor-meta :max-vel 200d0))
             (height (- (cadddr bb) (cadr bb)))
             (radius (/ (/ height num-circles) 2d0))
             (moment 0d0))
        (dotimes (i num-circles)
          (let ((x 0d0)
                (y (- (* i (* 2 radius)) (- (/ height 2) radius))))
            (incf moment (cp:moment-for-circle mass radius 0d0 x y))
            (let ((shape (cpw:make-shape :circle body (lambda (body) (cp:circle-shape-new (cpw:base-c body) radius x y)))))
              ;(when (zerop i) (setf (actor-feet actor) shape))
              (setf (cp-a:shape-u (cpw:base-c shape)) 0.9d0))))
        (let ((body-c (cpw:base-c body)))
          (cp:body-set-moment body-c moment)
          (cp:body-set-pos body-c
                           (coerce (car position) 'double-float)
                           (coerce (cadr position) 'double-float))
          (setf (cp-a:body-v-limit body-c) max-vel))
        (enqueue (lambda (world)
                   (let ((space (world-physics world)))
                     ;; fix the character's rotation
                     (let ((joint (cpw:make-joint (cpw:space-static-body space) body
                                                  (lambda (body1 body2)
                                                    (cp:damped-rotary-spring-new (cpw:base-c body1) (cpw:base-c body2)
                                                                                 0d0 (* mass 240000d0) (* mass 25000d0))))))
                       (cpw:space-add-joint space joint))
                     ;; add the body/shapes to the world
                     (cpw:space-add-body space body)
                     (dolist (shape (cpw:body-shapes body))
                       (cpw:space-add-shape space shape))))
                 :game)
        body))))

(defgeneric load-actor-physics-body (actor actor-meta)
  (:documentation
    "Load the physics body and shapes associated with this actor (along with
     any other setup the body needs)."))

(defmethod load-actor-physics-body ((actor actor) actor-meta)
  (let ((mass (coerce (getf actor-meta :mass 50d0) 'double-float))
        (max-vel (coerce (getf actor-meta :max-velocity 200d0) 'double-float))
        (friction (coerce (getf actor-meta :friction 0.9d0) 'double-float))
        (bb (calculate-game-object-bb actor))
        (physics-objects (getf actor-meta :physics))
        (position (mapcar (lambda (v)
                            (coerce v 'double-float))
                          (getf actor-meta :start-pos '(0 0 0)))))
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

(defmacro defactor (class-name superclasses slots &rest class-options)
  "Abstraction of defclass, solves inter-package issues (in other words, allows
   a game to add its own actor class, and allows ghostie to see it in its
   namespace by importing it)."
  `(progn
     ,(append `(defclass ,class-name ,superclasses
                 ,slots)
              (when class-options
                (list class-options)))
     (import ',class-name :ghostie)))

(defun load-actors (actors-meta)
  (let ((actors nil))
    (dolist (actor-info actors-meta)
      (let* ((scale (getf actor-info :scale '(1 1 1)))
             (actor-name (getf actor-info :actor))
             (actor-directory (format nil "~a/~a/~a/~a/"
                                      (namestring *game-directory*)
                                      *resource-path*
                                      *actor-path*
                                      actor-name))
             (meta (read-file (format nil "~a/meta.lisp" actor-directory)))
             (svg-objs (svgp:parse-svg-file (format nil "~a/objects.svg" actor-directory)
                                            :curve-resolution 20
                                            :scale (list (car scale) (- (cadr scale))))))
        ;; set the actor's global meta into the level meta
        (setf actor-info (append actor-info meta))

        ;; load the actor's class file, if it has one
        (let ((class-file (format nil "~a/class.lisp" actor-directory)))
          (when (probe-file class-file)
            (load class-file)))

        (let* ((actor-symbol (intern (string-upcase actor-name) :ghostie))
               (actor-class (if (find-class actor-symbol nil)
                                actor-symbol
                                'actor))
               (actor (car (svg-to-game-objects svg-objs nil :object-type actor-class :center-objects t))))
          (setf (game-object-physics-body actor) (load-actor-physics-body actor actor-info)
                (actor-is-main actor) (getf actor-info :main))
          (push actor actors))))
    actors))

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

