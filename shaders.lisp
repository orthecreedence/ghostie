(in-package :game-level)

(defun create-shader (type src)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader src)
    (gl:compile-shader shader)
    (format t "Shader log (~a): ~a~%" type (gl:get-shader-info-log shader))
    shader))

(defun create-shader-program (shader-pairs)
  (let ((program (gl:create-program))
        (shaders nil))
    (loop for (type . src) in shader-pairs do
      (let ((shader (create-shader type src)))
        (push shader shaders)
        (gl:attach-shader program shader)))
    (gl:link-program program)
    (format t "Program log: ~a~%" (gl:get-program-info-log program))
    (dolist (shader shaders)
      (gl:detach-shader program shader)
      (gl:delete-shader shader))
    program))

(defun create-default-shader-program ()
  (create-shader-program
    `((:vertex-shader . ,(file-contents #P"shaders/vertex.v1.glsl"))
      (:fragment-shader . ,(file-contents #P"shaders/fragment.v1.glsl")))))
