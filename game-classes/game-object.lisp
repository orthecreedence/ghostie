(in-package :game-level)

(defclass game-object ()
  ((name :accessor game-object-name :initform "game-object")
   (position :accessor game-object-position :initarg :position :initform '(0 0 0))
   (gl-objects :accessor game-object-gl-objects)
   (physics :accessor game-object-physics)))

(defun create-game-object (&key (type 'game-object) gl-objects physics (position '(0 0 0)))
  (let ((obj (make-instance type :position position)))
    (setf (game-object-gl-objects obj) gl-objects
          (game-object-physics obj) physics)
    obj))

(defmethod draw ((object game-object))
  (dolist (gl-object (game-object-gl-objects object))
    (draw-gl-object gl-object :position (game-object-position object))))

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

(defun svg-to-game-objects (svg-objects objects-meta &key (object-type 'game-object) (scale '(1 1 1)))
  (let ((obj-hash (make-hash-table :test #'equal))
        (game-objects nil))
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
               (triangles (glu-tessellate:tessellate (getf obj :point-data)))
               (group-name (car (getf obj :group))))
          (when (> (length triangles) 0)
            (push (make-gl-object :data triangles :color color :scale scale)
                  (gethash group-name obj-hash))))))
    (loop for group-name being the hash-keys of obj-hash
          for gl-objects being the hash-values of obj-hash do
      (let* ((meta (find-if (lambda (p) (equal (getf p :name) group-name)) (getf objects-meta :object-properties)))
             (depth (getf meta :layer-depth)))
        (push (create-game-object :type object-type
                                  :gl-objects gl-objects
                                  :position (list 0 0 (if depth depth 0)))
              game-objects)))
    game-objects))

