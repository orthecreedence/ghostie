(in-package :game-level)

(defvar *default-shader-program* nil)

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
  	  (gl:enable :line-smooth :blend :polygon-smooth :depth-test :cull-face)
      (gl:shade-model :smooth)
  	  (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:line-width 1.5)        
  	  (let* ((vport (gl:get-integer :viewport))
             (width (aref vport 2))
             (height (aref vport 3)))
        (resize-window width height))
      (gl:enable :fog)
      (gl:fog :fog-mode :linear)
      (gl:fog :fog-color '(.8 .8 .8 1.0))
      (gl:fog :fog-density 0.004)
      (gl:fog :fog-start 240.0)
      (gl:fog :fog-end 550.0)
      (gl:hint :fog-hint :nicest)
      (gl:hint :polygon-smooth-hint :nicest)
      (apply #'gl:clear-color background)
      (setf *default-shader-program* (create-default-shader-program))
      ;; run the world...this calls our game loop
      (funcall draw-fn window)
      window)))

(defun resize-window (width height)
  (setf height (max height 1))
  ;(gl:matrix-mode :projection)
  ;(gl:load-identity)
  ;(gl:scale (/ height width) 1 1)
  ;(gl:frustum -1 1 -1 1 1 2000)
  ;(gl:ortho 0 (aref vport 2) 0 (aref vport 3) -50 50)
  ;(gl:matrix-mode :modelview)
  ;(gl:load-identity)
  (gl:viewport 0 0 width height))
