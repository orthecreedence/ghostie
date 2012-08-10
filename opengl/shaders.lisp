(in-package :ghostie)

(defvar *shaders* nil)
(defvar *shader-unif-locations* nil)
(defvar *current-program* 0)

(defun get-shader (program-key)
  (getf *shaders* program-key))

(defun use-shader (program-key)
  (let ((shader (if (symbolp program-key)
                    (get-shader program-key)
                    program-key)))
    (setf *current-program* shader)
    (gl:use-program shader)))

(defun (setf get-shader) (val program-key)
  (setf (getf *shaders* program-key) val))

(defun get-shader-unif (name)
  (unless *shader-unif-locations*
    (setf *shader-unif-locations* (make-hash-table :test #'equal)))
  (let* ((cur-program *current-program*)
         (hname (format nil "prog:~d:~a" cur-program name)))
    (multiple-value-bind (unif exists) (gethash hname *shader-unif-locations*)
      (unless exists
        (setf unif (gl:get-uniform-location cur-program name))
        (setf (gethash hname *shader-unif-locations*) unif))
      unif)))

(defun set-shader-var (fn location &rest args)
  (let ((location (if (stringp location)
                      (get-shader-unif location)
                      location)))
    (when (<= 0 location)
      (apply fn (append (list location) args)))))

(defun set-shader-matrix (name matrix &key (size 4))
  (let ((unif (get-shader-unif name)))
    (when (<= 0 unif)
      (gl:uniform-matrix unif size (vector matrix) t))))

(defun create-shader (type src)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader src)
    (gl:compile-shader shader)
    (let ((log (gl:get-shader-info-log shader)))
      (when log (dbg :info "  Shader log (~a): ~a~%" type (gl:get-shader-info-log shader))))
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
      (when log (dbg :info "  Program log: ~a~%" (gl:get-program-info-log program))))
    (dolist (shader shaders)
      (gl:detach-shader program shader)
      (gl:delete-shader shader))
    (dbg :info "Shaders (re)compiled.~%")
    program))

(defun make-shader (vert-filename frag-filename)
  (create-shader-program 
    `((:vertex-shader . ,(file-contents vert-filename))
      (:fragment-shader . ,(file-contents frag-filename)))))

(defun free-shaders ()
  (loop for (nil program) on *shaders* by #'cddr do
        (gl:delete-program program))
  (setf *shaders* nil)
  (setf *shader-unif-locations* (make-hash-table :test #'equal)))

(defun recompile-shaders ()
  (free-shaders)
  (setf (get-shader :main) (make-shader #P"opengl/glsl/main.vert"
                                        #P"opengl/glsl/main.frag")
        (get-shader :dof) (make-shader #P"opengl/glsl/dof.vert"
                                       #P"opengl/glsl/dof.frag")))

