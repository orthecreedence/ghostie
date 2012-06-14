(in-package :game-level)

(defvar *shaders* nil)
(defvar *shader-unif-locations* nil)

(defun get-shader-unif (name)
  (unless *shader-unif-locations*
    (setf *shader-unif-locations* (make-hash-table :test #'equal)))
  (let* ((cur-program (gl:get-integer :current-program))
         (hname (format nil "prog:~d:~a" cur-program name)))
    (multiple-value-bind (unif exists) (gethash hname *shader-unif-locations*)
      (unless exists
        (setf unif (gl:get-uniform-location cur-program name))
        (setf (gethash hname *shader-unif-locations*) unif))
      unif)))

(defun set-shader-matrix (name matrix &key (size 4))
  (let ((unif (get-shader-unif name)))
    (gl:uniform-matrix unif size (vector matrix) t)))

(defun create-shader (type src)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader src)
    (gl:compile-shader shader)
    (let ((log (gl:get-shader-info-log shader)))
      (when log (format t "Shader log (~a): ~a~%" type (gl:get-shader-info-log shader))))
    shader))

(defun create-shader-program (shader-pairs)
  (let ((program (gl:create-program))
        (shaders nil))
    (loop for (type . src) in shader-pairs do
      (let ((shader (create-shader type src)))
        (push shader shaders)
        (gl:attach-shader program shader)))
    (gl:link-program program)
    (let ((log (gl:get-program-info-log program)))
      (when log (format t "Program log: ~a~%" (gl:get-program-info-log program))))
    (dolist (shader shaders)
      (gl:detach-shader program shader)
      (gl:delete-shader shader))
    program))

(defun make-shader (vert-filename frag-filename)
  (create-shader-program 
    `((:vertex-shader . ,(file-contents vert-filename))
      (:fragment-shader . ,(file-contents frag-filename)))))

