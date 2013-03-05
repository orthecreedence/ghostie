(in-package :ghostie)

(defclass level ()
  ((objects :accessor level-objects :initform nil)
   (actors :accessor level-actors :initform nil)
   (collision-depth :accessor level-collision-depth :initform 0)
   (meta :accessor level-meta :initarg :meta :initform nil))
  (:documentation "Describes a level, and the objects in that level."))

(defclass level-object (game-object) ()
  (:documentation
    "A direct extension of game-object, helps collision handlers tell if an
     object is interacting with part of the level, or with an object inside the
     level."))

(defun load-level (level-name)
  "Load a level via its SVG/meta.lisp file."
  (let* ((level (make-instance 'level))
         (level-directory (format nil "~a/~a/~a/~a/"
                                  (namestring *game-directory*)
                                  *resource-path*
                                  *level-path*
                                  level-name))
         (level-meta (read-file (format nil "~a/meta.lisp" level-directory)))
         (scale (getf level-meta :scale))
         (objects (svgp:parse-svg-file (format nil "~a/objects.svg" level-directory)
                                       :curve-resolution 20
                                       :group-id-attribute-name "label"
                                       :scale (list (car scale) (- (cadr scale))))))
    ;; make sure the level in the render world as a copy of our meta
    (in-render (world)
      (dbg :info "(level) Copying level to render~%")
      (setf (world-level world) (make-instance 'level :meta (copy-tree level-meta))))
    ;; here, we convert the objects in the level SVG to displayable/collidable
    ;; objects, load the actors for the level, and store the level meta info
    (setf (level-objects level) (append (svg-to-game-objects objects level-meta :center-objects t :object-type 'level-object)
                                        ;(load-objects (getf level-meta :objects))
                                        )
          (level-actors level) (load-objects (getf level-meta :actors) :type :actors) ;(load-actors (getf level-meta :actors))
          (level-meta level) level-meta)
    ;; level loaded!
    (trigger :level-load level)
    level))

(defun add-level-object (level object)
  "Add an object to a level. The object will be processed every game loop, and
   if it has physics, will be simulated in the physics world."
  (trigger :object-add level object)
  (push object (level-objects level)))

(defun remove-level-object (level object)
  "Remove an object from a level's simulation. If the object doesn't exist in
   the level, nothing happens."
  (trigger :object-remove level object)
  (setf (level-objects level) (delete object (level-objects level) :test #'equal)))

(defun level-cleanup (level)
  "Clean up the objects in a level (in the game thread) and reset the level."
  (dolist (game-object (append (level-objects level)
                               (level-actors level)))
    (destroy-game-object game-object))
  (setf (level-objects level) nil
        (level-actors level) nil
        (level-meta level) nil)
  level)

(defun init-level-physics-objects (world)
  "Determine the objects used as collision objects in this level and create
   physics bodies for them.
   
   Generally this happens for the ground/walls/etc of a level that are
   positioned at the level's collision depth (default 0)."
  (let* ((level (world-level world))
         (collision-objects (remove-if (lambda (game-object)
                                         ;; grab objects in the same plane as collision-depth
                                         (not (eq (caddr (game-object-position game-object))
                                                  (level-collision-depth level))))
                                       (level-objects level)))
         (space (world-physics world)))
    (dolist (object collision-objects)
      (let ((body (cpw:make-body (lambda () (cp:body-new-static))))
            (position-x (car (game-object-position object)))
            (position-y (cadr (game-object-position object))))
        (cp:body-set-pos (cpw:base-c body)
                         (coerce position-x 'double-float)
                         (coerce position-y 'double-float))
        (dolist (gl-object (game-object-gl-objects object))
          (let* ((disconnected (getf (gl-object-shape-meta gl-object) :disconnected))
                 (verts (gl-object-shape-points gl-object))
                 (last-pt (if disconnected
                              nil
                              (list (car (aref verts (- (length verts) 1)))
                                    (cadr (aref verts (- (length verts) 1)))))))
            (loop for (x y) across verts do
              (let ((x (- x position-x))
                    (y (- y position-y)))
                (when last-pt
                  (let ((shape (cpw:make-shape :segment
                                               body
                                               (lambda (body) (cpw:shape-segment body (car last-pt) (cadr last-pt) x y *physics-segment-thickness*)))))
                    (setf (cp-a:shape-u (cpw:base-c shape)) 0.8d0
                          (cp-a:shape-e (cpw:base-c shape)) 0.0d0)
                    (cpw:space-add-shape space shape)))
                (setf last-pt (list x y))))))))))

(defun draw-level (level)
  "Draw the entire level (all contained objects)."
  (dolist (game-obj (level-objects level))
    (draw game-obj))
  (dolist (actor (level-actors level))
    (draw actor)))

