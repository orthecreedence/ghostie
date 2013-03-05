(in-package :ghostie)

(defclass dynamic-object (game-object)
  ((name :accessor object-name :initarg :name :initform nil))
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
     any other setup the body needs)."))

(defmethod load-physics-body ((object dynamic-object) object-meta)
  (declare (ignore object object-meta)))

(defun load-objects (objects-meta &key (type :objects))
  "Load objects in a level defined by that level's meta. This can be dynamic
   objects (moving platforms, plants, bridges, etc) or actors as well."
  (let ((objects nil)
        (path (case type
                (:actors *actor-path*)
                (:objects *object-path*))))
    (dolist (object-info objects-meta)
      (let* ((scale (getf object-info :scale '(1 1 1)))
             (object-type (getf object-info :type))
             (object-name (getf object-info :name))
             (object-directory (format nil "~a/~a/~a/~a/"
                                      (namestring *game-directory*)
                                      *resource-path*
                                      path
                                      object-type))
             (meta (read-file (format nil "~a/meta.lisp" object-directory)))
             (svg-objs (svgp:parse-svg-file (format nil "~a/objects.svg" object-directory)
                                            :curve-resolution 20
                                            :scale (list (car scale) (- (cadr scale))))))
        ;; set the object's global meta into the level meta
        (setf object-info (append object-info meta))

        ;; load the object's class file, if it has one
        (let ((class-file (format nil "~a/class.lisp" object-directory)))
          (when (probe-file class-file)
            (load class-file)))

        (let* ((object-symbol (intern (string-upcase object-type) :ghostie))
               (object-class (if (find-class object-symbol nil)
                                object-symbol
                                'object))
               (object (car (svg-to-game-objects svg-objs nil :object-type object-class :center-objects t))))
          (when object-name
            (setf (object-name object) object-name))
          (setf (game-object-physics-body object) (load-physics-body object object-info))
          (push object objects))))
    objects))

