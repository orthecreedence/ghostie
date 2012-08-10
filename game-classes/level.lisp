(in-package :ghostie)

(defparameter *level-directory* "resources/levels")
(defparameter *physics-segment-thickness* 0.0d0)

(defclass level ()
  ((objects :accessor level-objects :initform nil)
   (main-actor :accessor level-main-actor :initform nil)
   (actors :accessor level-actors :initform nil)
   (collision-depth :accessor level-collision-depth :initform 0)
   (meta :accessor level-meta :initarg :meta :initform nil)))

(defun load-level (level-name)
  "Load the level-name level! Does this by loading the SVG file holding the
  objects for the level, and the associated meta file that describes the scene
  and the actors in the level."
  (let* ((level (make-instance 'level))
         (level-meta (read-file (format nil "~a/~a/meta.lisp" *level-directory* level-name)))
         (scale (getf level-meta :scale))
         (objects (svgp:parse-svg-file (format nil "~a/~a/objects.svg" *level-directory* level-name)
                                       :curve-resolution 20
                                       :scale (list (car scale) (- (cadr scale)))
                                       :ignore-errors t)))
    (enqueue (lambda (world)
               (dbg :info "Copying level to render.~%")
               (setf (world-level world) (make-instance 'level :meta (copy-tree level-meta))))
             :render)
    (setf (level-objects level) (svg-to-game-objects objects level-meta :center-objects t)
          (level-actors level) (load-actors (getf level-meta :actors) :scale scale)
          (level-main-actor level) (find-if (lambda (actor) (actor-is-main actor))
                                            (level-actors level))
          (level-meta level) level-meta)
    level))

(defun init-level-physics-objects (world)
  "Determine the objects used as collision objects in this level and create
  physics bodies for them."
  (let* ((level (world-level world))
         (collision-objects (remove-if (lambda (game-object) (not (eq (caddr (game-object-position game-object)) (level-collision-depth level))))
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
                    (setf (cp-a:shape-u (cpw:base-c shape)) 0.7d0
                          (cp-a:shape-e (cpw:base-c shape)) 0.1d0)
                    (cpw:space-add-shape space shape)))
                (setf last-pt (list x y))))))))))

(defun draw-level (level)
  "...draw the entire level..."
  (dolist (game-obj (level-objects level))
    (draw game-obj))
  (dolist (actor (level-actors level))
    (draw actor)))

