(in-package :game-level)

(defun create-window (draw-fn &key (title "windowLOL") (width 800) (height 600) (background '(1 1 1 0)))
  "Create an SDL window with the given draw function and additional options."
  (sdl:with-init ()
    (let ((window (sdl:window width height
                              :title-caption title
                              :opengl t
                              :opengl-attributes '((:sdl-gl-doublebuffer 1)
                                                   (:sdl-gl-red-size 8)
                                                   (:sdl-gl-green-size 8)
                                                   (:sdl-gl-blue-size 8)
                                                   (:sdl-gl-depth-size 16)))))
      ;; set up key repeat (so we can hold a key for rapid fire)
      (sdl:enable-key-repeat 200 30)
      ;; who knows - figure out what this does
  	  (gl:enable :texture-2d :blend)
  	  (gl:blend-func :src-alpha :one-minus-src-alpha)
      ;; set up 2D rendering in opengl
      ;; consider using the vector provided instead of coercing?
  	  (let ((vport (coerce (gl:get-integer :viewport) 'list)))
  	    (gl:matrix-mode :projection)
  	    (gl:push-matrix)
  	    (gl:load-identity)
  	    (gl:ortho 0 (nth 2 vport) 0 (nth 3 vport) -1 1)   ; here's what does the actual 2D
  	    (gl:matrix-mode :modelview)
  	    (gl:push-matrix)
  	    (gl:load-identity))
      ;; disable depth-testing (not needed in 2d)
  	  (gl:disable :depth-test)
      (apply #'gl:clear-color background)
      ;; run the world...this calls our game loop
      (funcall draw-fn window)
      window)))

