(in-package :game-level)

(defvar *window-width* 0)
(defvar *window-height* 0)

(defparameter *render-objs* nil)

(defun init-opengl (background)
  ;; set up blending
  (gl:enable :blend :texture-2d)
  (gl:blend-func :src-alpha :one-minus-src-alpha)

  ;; set up culling
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :ccw)

  ;; create our shader programs
  (setf (getf *shaders* :main) (make-shader #P"opengl/shaders/main.vert"
                                            #P"opengl/shaders/main.frag")
        (getf *shaders* :fov) (make-shader #P"opengl/shaders/fov.vert"
                                           #P"opengl/shaders/fov.frag"))

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
  ;(gl:shade-model :smooth)
  (gl:enable :multisample)

  ;; set up the viewport
  (let* ((vport (gl:get-integer :viewport))
         (width (aref vport 2))
         (height (aref vport 3)))
    ;; set window size AND setup our view translation matrices
    (resize-window width height)
    ;; set up our FBO n shiiii
    (let ((fbo (car (gl:gen-framebuffers-ext 1)))
          (rbo (car (gl:gen-renderbuffers-ext 1)))
          (tex (car (gl:gen-textures 1))))

      ;; bind framebuffer
      (gl:bind-framebuffer-ext :framebuffer-ext fbo)

      ;; setup texture we render to
      (gl:bind-texture :texture-2d tex)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
      (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
      ;(gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
      ;(gl:generate-mipmap-ext :texture-2d)
      ;(gl:tex-parameter :texture-2d :generate-mipmap :true)
      (gl:tex-image-2d :texture-2d 0 :rgba width height 0 :rgba :unsigned-byte (cffi:null-pointer))
      (gl:bind-texture :texture-2d 0)
      (gl:framebuffer-texture-2d-ext :framebuffer-ext :color-attachment0-ext :texture-2d tex 0)

      ;; set up depth buffer
      (gl:bind-renderbuffer-ext :renderbuffer-ext rbo)
      (gl:renderbuffer-storage-ext :renderbuffer-ext :depth-component width height)
      (gl:framebuffer-renderbuffer-ext :framebuffer-ext :depth-attachment-ext :renderbuffer-ext rbo)

      (gl:bind-renderbuffer-ext :renderbuffer-ext 0)

      ;; check status of FBO
      (let ((fbo-status (gl:check-framebuffer-status-ext :framebuffer-ext)))
        (unless (gl::enum= fbo-status :framebuffer-complete-ext)
          (error "Framebuffer failed: ~a" fbo-status)))

      ;; unbind it and save our objs for later rendering
      (gl:bind-framebuffer :framebuffer-ext 0)
      (setf (getf *render-objs* :fbo1) fbo
            (getf *render-objs* :fbo1-tex) tex
            (getf *render-objs* :frb1-rbo) rbo)))

  ;; set the background/clear color
  (apply #'gl:clear-color background))

(defun cleanup-opengl ()
  (loop for (nil program) on *shaders* by #'cddr do
        (gl:delete-program program)))

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
      ;; don't know why i'm doing this. someone said to here: http://www.cliki.net/lispbuilder-sdl
      (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
      ;; set up key repeat (so we can hold a key for rapid fire)
      (sdl:enable-key-repeat 200 30)
      (init-opengl background)
      ;; run the world...this calls our game loop
      (funcall draw-fn window)
      (cleanup-opengl)
      window)))

(defun resize-window (width height)
  (setf height (max height 1))
  (setf *perspective-matrix* (m-perspective 45.0 (/ width height) 0.001 100.0))
  (setf *ortho-matrix* (m-ortho -1.0 1.0 -1.0 1.0 -1.0 1.0))
  (setf *window-width* width
        *window-height* height)
  (gl:viewport 0 0 width height))

(defun window-event-handler (w)
  (declare (ignore w))
  (load-assets)
  (sdl:with-events (:poll)
    (:quit-event () t)
    (:video-expose-event () (sdl:update-display))
    (:video-resize-event (:w width :h height)
      (resize-window width height))
    (:key-down-event (:key key)
      (key-handler key))
    (:idle ()
      (step-world *world*)
      (draw-world *world*)
      (sdl:update-display))))
