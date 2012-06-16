(in-package :game-level)

(defvar *window-width* 0)
(defvar *window-height* 0)

(defvar *render-objs* nil)

(defun free-shaders ()
  (loop for (nil program) on *shaders* by #'cddr do
        (gl:delete-program program))
  (setf *shaders* nil))

(defun recompile-shaders ()
  (free-shaders)
  (setf (getf *shaders* :main) (make-shader #P"opengl/shaders/main.vert"
                                            #P"opengl/shaders/main.frag")
        (getf *shaders* :dof) (make-shader #P"opengl/shaders/dof.vert"
                                           #P"opengl/shaders/dof.frag")))

(defun init-opengl (background)
  ;; set up blending
  (gl:enable :blend :texture-2d)
  (gl:blend-func :src-alpha :one-minus-src-alpha)

  ;; set up culling
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :ccw)

  ;; create our shader programs
  (setf *shaders* nil)
  (recompile-shaders)

  ;; set our camera matrix into the program
  (gl:use-program (getf *shaders* :main))
  (setf *view-matrix* (id-matrix 4))

  ;; enable depth testing
  (gl:enable :depth-test :depth-clamp)
  (gl:depth-mask :true)
  (gl:depth-func :lequal)
  (gl:depth-range 0 1)
  (gl:clear-depth 1.0)

  ;; antialiasing (or just fixes gaps betwen polygon triangles)
  (gl:shade-model :smooth)
  (gl:enable :multisample-arb)

  ;; set up the viewport
  (let* ((vport (gl:get-integer :viewport))
         (width (aref vport 2))
         (height (aref vport 3)))
    ;; set window size AND setup our view translation matrices
    (resize-window width height)
    (setf *render-objs* nil
          (getf *render-objs* :fbo1) (make-fbo width height :depth-type :tex)))

  ;; set the background/clear color
  (apply #'gl:clear-color background))

(defun window-quit ()
  (setf *quit* t)
  (cleanup-opengl))

(defun free-fbos ()
  (loop for (nil fbo) on *render-objs* by #'cddr do
        (free-fbo fbo)))

(defun cleanup-opengl ()
  (free-fbos)
  (free-shaders))

(defun create-window (draw-fn &key (title "windowLOL") (width 800) (height 600) (background '(1 1 1 0)))
  "Create an SDL window with the given draw function and additional options."
  (sdl:with-init ()
    (let ((window (sdl:window width height
                              :title-caption title
                              :resizable t
                              :opengl t
                              :opengl-attributes '((:sdl-gl-doublebuffer 1)
                                                   (:sdl-gl-red-size 8)
                                                   (:sdl-gl-green-size 8)
                                                   (:sdl-gl-blue-size 8)
                                                   (:sdl-gl-depth-size 24)
                                                   (:sdl-gl-multisamplebuffers 2)
                                                   (:sdl-gl-multisamplesamples 2)))))
      ;; fixes some straight up bulllllshit (http://www.cliki.net/lispbuilder-sdl) thxlol
      ;; glfwGetProcAddress for GLFW...
      (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)

      ;; set up key repeat (so we can hold a key for rapid fire)
      (sdl:enable-key-repeat 200 30)

      ;; setup opengl
      (init-opengl background)

      ;; run the world...this calls our game loop
      (funcall draw-fn window)
      window)))

(defun resize-window (width height)
  (setf height (max height 1))
  (setf *perspective-matrix* (m-perspective 45.0 (/ width height) 0.001 100.0))
  (setf *ortho-matrix* (m-ortho -1.0 1.0 -1.0 1.0 -1.0 1.0))
  (gl:use-program (getf *shaders* :main))
  (setf *window-width* width
        *window-height* height)
  (gl:viewport 0 0 width height))

(defun window-event-handler (w)
  (declare (ignore w))
  (load-assets)
  (sdl:with-events (:poll)
    (:quit-event ()
     (window-quit)
     (sdl:quit-sdl))
    (:video-expose-event () (sdl:update-display))
    (:video-resize-event (:w width :h height)
      (resize-window width height))
    (:key-down-event (:key key)
      (key-handler key))
    (:idle ()
      (step-world *world*)
      (draw-world *world*)
      (sdl:update-display))))
