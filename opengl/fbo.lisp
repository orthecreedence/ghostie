(in-package :game-level)

(defclass gl-fbo ()
  ((fbo :accessor gl-fbo-fbo :initform nil)
   (tex :accessor gl-fbo-tex :initform nil)
   (depth :accessor gl-fbo-depth :initform nil)
   (depth-type :accessor gl-fbo-depth-type :initform nil)))

(defun make-fbo (width height &key (depth-type :rbo))
  (let ((gl-fbo (make-instance 'gl-fbo))
        (fbo (car (gl:gen-framebuffers-ext 1)))
        ;(depth (car (gl:gen-renderbuffers-ext 1)))
        (depth (car (gl:gen-textures 1)))
        (tex (car (gl:gen-textures 1))))

    ;; bind framebuffer
    (gl:bind-framebuffer-ext :framebuffer-ext fbo)

    ;; setup texture we render to
    (gl:bind-texture :texture-2d tex)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
    (gl:tex-parameter :texture-2d :generate-mipmap :true)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (gl:tex-image-2d :texture-2d 0 :rgba width height 0 :rgba :unsigned-byte (cffi:null-pointer))
    (gl:bind-texture :texture-2d 0)
    (gl:framebuffer-texture-2d-ext :framebuffer-ext :color-attachment0-ext :texture-2d tex 0)
    ;(gl:generate-mipmap-ext :texture-2d)

    ;; set up depth buffer
    (if (eq depth-type :tex)
        (progn
          (gl:bind-texture :texture-2d depth)
          (gl:tex-image-2d :texture-2d 0 :depth-component width height 0 :depth-component :unsigned-short (cffi:null-pointer))
          (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
          (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
          (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
          (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
          (gl:framebuffer-texture-2d-ext :framebuffer-ext :depth-attachment-ext :texture-2d depth 0))
        (progn
          (gl:bind-renderbuffer-ext :renderbuffer-ext depth)
          (gl:renderbuffer-storage-ext :renderbuffer-ext :depth-component width height)
          (gl:framebuffer-renderbuffer-ext :framebuffer-ext :depth-attachment-ext :renderbuffer-ext depth)
          (gl:bind-renderbuffer-ext :renderbuffer-ext 0)))

    ;; check status of FBO
    (let ((fbo-status (gl:check-framebuffer-status-ext :framebuffer-ext)))
      (unless (gl::enum= fbo-status :framebuffer-complete-ext)
        (error "Framebuffer failed: ~a" fbo-status)))

    ;; unbind it so we don't screw up everything when we render
    (gl:bind-framebuffer :framebuffer-ext 0)

    ;; set all the right objects
    (setf (gl-fbo-fbo gl-fbo) fbo
          (gl-fbo-tex gl-fbo) tex
          (gl-fbo-depth gl-fbo) depth
          (gl-fbo-depth-type gl-fbo) depth-type)

    ;; aaaand return the lisp gl-fbo object
    gl-fbo))

(defmethod free-fbo ((fbo gl-fbo))
  (gl:delete-textures (list (gl-fbo-tex fbo)))
  (if (eq (gl-fbo-depth-type fbo) :tex)
      (gl:delete-textures (list (gl-fbo-depth fbo)))
      (gl:delete-renderbuffers-ext (list (gl-fbo-depth fbo))))
  (gl:delete-framebuffers-ext (list (gl-fbo-fbo fbo))))
