(in-package :game-level)

(defparameter *level-directory* "resources/levels")

(defclass level ()
  ((objects :accessor level-objects :initform nil)
   (actors :accessor level-actors :initform nil)
   (group-depths :accessor level-group-depths :initarg :group-depths :initform nil)
   (collision-depth :accessor level-collision-depth :initform 0)))

(defun load-level (level-name group-depths)
  (let ((level (make-instance 'level :group-depths group-depths))
        (objects (svgp:parse-svg-file (format nil "~a/~a/objects.svg" *level-directory* level-name)
                                      :curve-resolution 20
                                      :invert-y t
                                      :ignore-errors t))
        (actors (parse-actor-file (format nil "~a/~a/actors.lgd" *level-directory* level-name))))
    (setf (level-objects level) (svg-to-game-objects objects group-depths)
          (level-actors level) actors)
    level))

(defun svg-to-game-objects (objects depths)
  (let ((game-objects nil))
    (loop for i from 0
          for obj in objects do
      (when (< 0 (length (getf obj :point-data)))
        (let* ((opacity (if (getf obj :opacity) (getf obj :opacity) 1))
               (color (if (getf obj :fill)
                          (hex-to-rgb (getf obj :fill) :opacity opacity)
                          (vector 0 0 0 opacity)))
               (depth (let ((depth (remove-if-not (lambda (d) (equal (car d) (car (getf obj :group)))) depths)))
                        (if depth
                            (cadr (car depth))
                            0)))
               (triangles (glu-tessellate:tessellate (getf obj :point-data))))
          (format t "color: ~a~%" color)
          (when (> (length triangles) 0)
            ;(format t "Loading object ~a~%" i)
            (push (create-game-object :gl (make-gl-object :data triangles :color color :scale '(.04 .04 .04) :position (list 0 0 depth))
                                      :physics nil)
                  game-objects)))))
    game-objects))

(defun draw-level (level)
  (dolist (obj (level-objects level))
    (draw (game-object-gl obj))))

