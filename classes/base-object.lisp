(in-package :ghostie)

(defclass base-object ()
  ((position :accessor object-position :initarg :position :initform '(0 0 0))
   (rotation :accessor object-rotation :initarg :rotation :initform 0.0)
   (gl-objects :accessor object-gl-objects :initarg :gl-objects :initform nil)
   (physics-body :accessor object-physics-body :initform nil)
   (meta :accessor object-meta :initarg :meta :initform nil)
   (display :accessor object-display :initarg :display :initform t)
   (draw-offset :accessor object-draw-offset :initarg :draw-offset :initform '(0 0 0))
   (render-ref :accessor object-render-ref :initarg :render-ref :initform nil)
   (last-sync :accessor object-last-sync :initform nil)
   (bb :accessor object-bb :initform nil)))

(defun make-base-object (&key (type 'base-object) gl-objects physics (position '(0 0 0)) (rotation 0.0))
  (let ((obj (make-instance type :position position :rotation rotation)))
    (setf (object-gl-objects obj) gl-objects
          (object-physics-body obj) physics)
    obj))

(defun destroy-base-object (base-object)
  "Clear out a game object, and free all its non-lisp data (gl objects and
   physics)."
  (let ((body (object-physics-body base-object)))
    (when body
      (cpw:destroy (object-physics-body base-object))))
  (dolist (gl-object (object-gl-objects base-object))
    (in-render ()
      (free-gl-object gl-object))))

(defmethod draw ((object base-object))
  (dolist (gl-object (object-gl-objects object))
    (unless (getf (gl-object-shape-meta gl-object) :disconnected)
      (draw-gl-object gl-object
                      :color (when (getf (object-meta object) :sleeping) (hex-to-rgb "#333333"))
                      :position (object-position object)
                      :rotation (list 0 0 1 (object-rotation object))))))

(defun sync-base-object-to-physics (base-object &key render)
  "Sync an object's position/rotation with its physics body."
  (let ((body (object-physics-body base-object)))
    (when body
      (cpw:sync-body body)
      (let ((position (mapcar #'+
                              (list (cpw:body-x body)
                                    (cpw:body-y body)
                                    0)
                              (object-draw-offset base-object)))
            (rotation (- (cpw:body-angle body)))
            (sleeping (cpw:body-sleeping-p body))
            (last-sync (object-last-sync base-object)))
        (unless (and (equal (getf last-sync :position) position)
                     (equal (getf last-sync :rotation) rotation)
                     (equal (getf last-sync :sleeping) sleeping))
          (setf (object-position base-object) position
                (object-rotation base-object) rotation
                (getf (object-meta base-object) :sleeping) sleeping)
          (let ((render-base-object (object-render-ref base-object)))
            (when (and render (object-display base-object) render-base-object)
              ;; run the sleep/wake events for this object (if needed)
              (cond ((and sleeping (not (getf (object-last-sync base-object) :sleeping)))
                     (trigger :object-sleep base-object))
                    ((and (not sleeping) (getf (object-last-sync base-object) :sleeping))
                     (trigger :object-wake base-object)))
              (setf (object-last-sync base-object) (list :position position
                                                              :rotation rotation
                                                              :sleeping sleeping))
              (in-render ()
                (setf (object-position render-base-object) position
                      (object-rotation render-base-object) rotation
                      (getf (object-meta render-base-object) :sleeping) sleeping))))))))
  base-object)

(defun parse-svg-styles (styles &key fill opacity)
  (let ((fill (if (stringp fill) fill "#000000"))
        (opacity (if (stringp opacity) (read-from-string opacity) 1.0)))
    (let ((directives (split-sequence:split-sequence #\; styles)))
      (dolist (directive directives)
        (let* ((parts (split-sequence:split-sequence #\: directive))
               (name (string-downcase (remove-if (lambda (c) (eq c #\space)) (car parts))))
               (value (remove-if (lambda (c) (eq c #\space)) (cadr parts))))
          (when (equal name "opacity")
            (setf opacity value))
          (when (equal name "fill")
            (setf fill value)))))
    (list :fill fill :opacity opacity)))

(defun sync-base-objects-to-render (base-objects)
  (dolist (base-object base-objects)
    (in-render (world)
      (let ((level (world-level world))
            (gl-objects (loop for fake-gl-object in (object-gl-objects base-object)
                              for gl-object = (make-gl-object-from-fake fake-gl-object)
                              collect gl-object)))
        (dbg :debug "(object) Initializing game object in render.~%")
        (let ((render-base-object (make-instance 'base-object
                                                 :gl-objects gl-objects
                                                 :position (copy-tree (object-position base-object))
                                                 :rotation (copy-tree (object-rotation base-object)))))
          (push render-base-object (level-objects level))
          (setf (object-render-ref base-object) render-base-object))))))

(defun svg-to-base-objects (svg-objects objects-meta &key (object-type 'base-object) (scale '(1 1 1)) (draw-offset '(0 0)) center-objects)
  (let ((obj-hash (make-hash-table :test #'equal))
        (base-objects nil)
        (scale (if scale scale '(1 1 1))))
    (loop for i from 0
          for obj in svg-objects do
      (when (< 0 (length (getf obj :point-data)))
        (let* ((styles (parse-svg-styles (getf obj :style)
                                         :fill (getf obj :fill)
                                         :opacity (getf obj :opacity)))
               (opacity (getf styles :opacity))
               (color (if (getf styles :fill)
                          (hex-to-rgb (getf styles :fill) :opacity opacity)
                          (vector 0 0 0 opacity)))
               (triangles (glu-tessellate:tessellate (getf obj :point-data) :holes (getf obj :holes)))
               (group-name (car (getf obj :group)))
               (meta (getf obj :meta))
               (disconnected (getf meta :disconnected)))
          (when (or (< 0 (length triangles)) disconnected)
            (push (make-fake-gl-object :data triangles :color color :scale scale :shape-meta meta :shape-points (getf obj :point-data))
                  (gethash group-name obj-hash))))))
    (loop for group-name being the hash-keys of obj-hash
          for gl-objects being the hash-values of obj-hash do
      (let* ((meta (find-if (lambda (property) (equal (getf property :name) group-name))
                            (getf objects-meta :object-properties)))
             (depth (getf meta :layer-depth)))
        (let ((base-object (make-base-object :type object-type
                                             :gl-objects gl-objects
                                             :position (list 0 0 (if depth depth 0)))))
          (when center-objects
            (center-base-object base-object))
          (push base-object base-objects))))
    (sync-base-objects-to-render base-objects)
    base-objects))

(defun center-base-object (base-object)
  "Center a game object's gl objects based on the min/max sums of all of their
  coords, then take that difference and apply it to the object's position. This
  effectively centers the game object around 0,0 and applies a position to it
  that puts it in its original place."
  (let ((gl-objects (remove-if (lambda (gl-object)
                                 (zerop (length (gl-object-vertex-data gl-object))))
                               (object-gl-objects base-object))))
    (when gl-objects
      (let* ((bb (calculate-object-bb base-object))
             (min-x (car bb))
             (min-y (cadr bb))
             (min-z 0)
             (max-x (caddr bb))
             (max-y (cadddr bb))
             (max-z 0))
        (let ((diff-x (- (- min-x) (/ (- max-x min-x) 2)))
              (diff-y (- (- min-y) (/ (- max-y min-y) 2)))
              (diff-z (- (- min-z) (/ (- max-z min-z) 2))))
          (dolist (gl-object (object-gl-objects base-object))
            (let ((disconnected (getf (gl-object-shape-meta gl-object) :disconnected)))
              (unless disconnected
                (let ((vertex-data (gl-object-vertex-data gl-object)))
                  (dotimes (i (/ (length vertex-data) 3))
                    (let ((i (* i 3)))
                      (setf (aref vertex-data i) (+ (aref vertex-data i) diff-x)
                            (aref vertex-data (+ i 1)) (+ (aref vertex-data (+ i 1)) diff-y)
                            (aref vertex-data (+ i 2)) (+ (aref vertex-data (+ i 2)) diff-z))))
                  (update-gl-object-vertex-data gl-object vertex-data)))))
          (setf (object-position base-object) (list (- diff-x)
                                                         (- diff-y)
                                                         (caddr (object-position base-object))))))))
  base-object)

(defun calculate-object-bb (base-object)
  (let ((min-x most-positive-double-float)
        (min-y most-positive-double-float)
        (max-x most-negative-double-float)
        (max-y most-negative-double-float))
    (dolist (gl-object (object-gl-objects base-object))
      (loop for (x y nil)
            on (coerce (gl-object-vertex-data gl-object) 'list)
            by #'cdddr do
        (setf min-x (min x min-x)
              min-y (min y min-y)
              max-x (max x max-x)
              max-y (max y max-y))))
    (list min-x min-y max-x max-y)))

