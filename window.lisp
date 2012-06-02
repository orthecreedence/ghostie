(in-package :game-level)

(defvar *window-width* 0)
(defvar *window-height* 0)
(defvar *default-shader-program* nil)

(defun init-opengl (background)
  ;; set up blending
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  ;; set up culling
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :ccw)
  ;; set up the viewport
  (let* ((vport (gl:get-integer :viewport))
         (width (aref vport 2))
         (height (aref vport 3)))
    (resize-window width height))
  ;; enable depth testing
  (gl:enable :depth-test :depth-clamp)
  (gl:depth-mask :true)
  (gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0)
  (gl:clear-depth 1.0)
  ;; antialiasing (or just fixes gaps betwen polygon triangles)
  (gl:shade-model :smooth)
  (gl:enable :multisample-arb)
  ;; create the shader program
  (setf *default-shader-program* (create-default-shader-program))
  ;; set the background/clear color
  (apply #'gl:clear-color background))

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
                                                   (:sdl-gl-depth-size 16)))))
      ;; don't know why i'm doing this. someone said to here: http://www.cliki.net/lispbuilder-sdl
      (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
      ;; set up key repeat (so we can hold a key for rapid fire)
      (sdl:enable-key-repeat 200 30)
      (init-opengl background)
      ;; run the world...this calls our game loop
      (funcall draw-fn window)
      window)))

(defun resize-window (width height)
  (setf height (max height 1))
  (setf *window-width* width
        *window-height* height)
  (gl:viewport 0 0 width height))

#|
(defun create-window (draw-fn &key (title "windowLOL") (width 800) (height 600) (background '(1 1 1 0)))
  (glfw:do-window (:title title :width width :height height :mode glfw:+window+)
    ((setf *quit* nil)
     (glfw:open-window-hint glfw:+window-no-resize+ glfw:+false+)
     ;(glfw:open-window-hint glfw:+opengl-version-major+ 3)
     ;(glfw:open-window-hint glfw:+opengl-version-minor+ 3)
     ;(glfw:open-window-hint glfw:+opengl-profile+ #x00050001) ;glfw:+opengl-core-profile+
     ;(glfw:open-window-hint glfw:+opengl-forward-compat+ glfw:+true+)

     (glfw:set-window-size-callback 'resize-window)
     (glfw:set-key-callback #'key-handler)
     (glfw:enable glfw:+key-repeat+)
     (init-opengl background))
    (funcall draw-fn)))
|#

