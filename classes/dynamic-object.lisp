(in-package :ghostie)

(defclass dynamic-object (game-object)
  ((id :accessor object-id :initarg :id :initform nil))
  (:documentation
    "Describes an object that acts as part of the level, but is more dynamic
     than terrain. For instance, a plant, a moving platform, a rope, a bridge,
     etc. Things that are pre-defined that the player can interact with.
     
     This directly extends game-object.
     
     It is meant to be extended in your resources/objects/[object]/class.lisp
     file or by your actor classes (actors are dynamic objects!)"))

(defmacro defobject (class-name superclasses slots &rest class-options)
  "Abstraction of defclass, solves inter-package issues (in other words, allows
   a game to add its own object class, and allows ghostie to see it in its
   namespace by importing it)."
  `(progn
     ,(append `(defclass ,class-name ,superclasses
                 ,slots)
              (when class-options
                (list class-options)))
     (import ',class-name :ghostie)))

(defgeneric load-physics-body (object object-meta)
  (:documentation
    "Load the physics body and shapes associated with this object (along with
     any other setup the body needs). Generally, the physics objects for a body
     are given in that object's meta.lisp under the :physics section. It
     describes the physics shapes attached to the body and what position they
     are on the body. This allows a developer to construct a fairly decent
     outline of the body. load-physics-body is defined as a method so that
     custom behavior can be implemented if needed."))

(defmethod load-physics-body ((object dynamic-object) object-meta)
  (dbg :debug "(object) Loading physics shapes for ~s~%" (list (object-id object) (getf object-meta :type)))
  (let ((max-vel (coerce (getf object-meta :max-velocity 1000d0) 'double-float))
        (friction (coerce (getf object-meta :friction 0.9d0) 'double-float))
        (static (getf object-meta :static))
        (bb (calculate-game-object-bb object))
        (physics-objects (getf object-meta :physics))
        (position (mapcar (lambda (v)
                            (coerce v 'double-float))
                          (getf object-meta :start-position '(0 0 0)))))
    (let* ((body (cpw:make-body (lambda ()
                                  (if static
                                      (cp:body-new-static)
                                      (cp:body-new 1d0 1d0)))))
           (body-c (cpw:base-c body))
           (mass 0d0)
           (moment 0d0))
      (if physics-objects
          ;; load the physics objects from the meta
          (let ((bb-max (apply #'max bb))) ; grab our biggest coordinate
            ;; loop over each physics object in this body and attach it
            (dolist (phys-obj physics-objects)
              (let ((physics-obj-mass (coerce (getf phys-obj :mass) 'double-float)))
                (incf mass physics-obj-mass)
                (case (getf phys-obj :type)
                  (:circle
                    (let ((position (getf phys-obj :position))
                          (radius (getf phys-obj :radius)))
                      (let ((r (* radius bb-max))
                            (x (* (car position) bb-max))
                            (y (* (cadr position) bb-max)))
                        (incf moment (cp:moment-for-circle physics-obj-mass r 0d0 x y))
                        (let ((shape (cpw:make-shape :circle body
                                                     (lambda (body)
                                                       (cp:circle-shape-new (cpw:base-c body)
                                                                            r x y)))))
                          (setf (cp-a:shape-u (cpw:base-c shape)) friction)))))
                  (:box
                    (let ((width (getf phys-obj :width))
                          (height (getf phys-obj :height))
                          (bb-max (* bb-max 2)))
                      (let ((w (* width bb-max))
                            (h (* height bb-max)))
                        (incf moment (cp:moment-for-box physics-obj-mass w h))
                        (let ((shape (cpw:make-shape :box body
                                                     (lambda (body)
                                                       (cp:box-shape-new (cpw:base-c body)
                                                                         w h)))))
                          (setf (cp-a:shape-u (cpw:base-c shape)) friction)))))
                  (t
                    (error (format nil "Unsupported physics type: ~a~%" (getf phys-obj :type))))))))

          ;; load a default physics object (a stupid circle in the center of
          ;; the object)
          (let* ((radius (/ (- (cadddr bb) (cadr bb)) 2.5d0))
                 (x 0d0)
                 (y 0d0))
            (incf moment (cp:moment-for-circle mass radius 0d0 x y))
            (let ((shape (cpw:make-shape :circle body
                                         (lambda (body)
                                           (cp:circle-shape-new (cpw:base-c body)
                                                                radius x y)))))
              (setf (cp-a:shape-u (cpw:base-c shape)) friction))))

      ;; finalize the body: set position, velocity, mass, moment of inertia
      (setf (cp-a:body-v-limit body-c) max-vel)
      (cp:body-set-pos body-c (car position) (cadr position))
      (unless static
        (cp:body-set-mass body-c (coerce mass 'double-float))
        (cp:body-set-moment body-c moment))
      ;; add the body to the physics world (async)
      (in-game (world)
        (let ((space (world-physics world)))
          (unless static
            (cpw:space-add-body space body))
          (dolist (shape (cpw:body-shapes body))
            (cpw:space-add-shape space shape))))
      body)))

(defun load-objects (objects-meta &key (type :objects))
  "Load objects in a level defined by that level's meta. This can be dynamic
   objects (moving platforms, plants, bridges, etc) or actors as well."
  (let ((objects nil)
        ;; we'll have a different path depending on what kind of objects we're
        ;; loading
        (path (case type
                (:actors *actor-path*)
                (:objects *object-path*))))
    (dolist (object-info objects-meta)
      (let* ((scale (getf object-info :scale '(1 1 1)))
             (object-type (getf object-info :type))
             (object-id (getf object-info :id))
             (object-directory (format nil "~a/~a/~a/~a/"
                                      (namestring *game-directory*)
                                      *resource-path*
                                      path
                                      object-type))
             (meta (read-file (format nil "~a/meta.lisp" object-directory)))
             (svg-objs (svgp:parse-svg-file (format nil "~a/objects.svg" object-directory)
                                            :curve-resolution 20
                                            :scale (list (car scale) (- (cadr scale))))))
        (dbg :debug "(object) Loading object ~s~%" (list :id object-id :type type))
        ;; set the object's global meta into the level meta
        (setf object-info (append object-info meta))

        ;; load the object's class file, if it has one
        (let ((class-file (format nil "~a/class.lisp" object-directory)))
          (when (probe-file class-file)
            (load class-file)))

        ;; attempt to load the object class
        (let* ((object-symbol (intern (string-upcase object-type) :ghostie))
               (object-class (if (find-class object-symbol nil)
                                object-symbol
                                'object))
               (object (car (svg-to-game-objects svg-objs nil :object-type object-class :center-objects t))))
          (when object-id
            (setf (object-id object) object-id))
          ;; load the object's physics body
          (setf (game-object-physics-body object) (load-physics-body object object-info))
          (push object objects))))
    objects))

