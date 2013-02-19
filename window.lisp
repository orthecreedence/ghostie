(in-package :ghostie)

(defvar *quit* nil)

(defvar *window-width* 0)
(defvar *window-height* 0)
(defvar *last-time* 0)

(defvar *render-objs* nil)

(defun init-opengl ()
  ;; set up blending
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)

  ;; set up culling
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :ccw)

  ;; create our shader programs
  (setf *shaders* nil)
  (recompile-shaders)

  ;; set our camera matrix into the program
  (use-shader :main)
  (setf *view-matrix* (id-matrix 4))

  ;; enable depth testing
  (gl:enable :depth-test :depth-clamp)
  (gl:depth-mask :true)
  (gl:depth-func :lequal)
  (gl:depth-range 0 1)
  (gl:clear-depth 1.0)

  ;; antialiasing (or just fixes gaps betwen polygon triangles)
  (gl:enable :multisample-arb)

  ;; set up the viewport
  (let* ((vport (gl:get-integer :viewport))
         (width (aref vport 2))
         (height (aref vport 3)))
    ;; set window size AND setup our view translation matrices
    (resize-window width height)))

(defun free-fbos ()
  (loop for (nil fbo) on *render-objs* by #'cddr do
        (free-fbo fbo))
  (setf *render-objs* nil))

(defun cleanup-opengl ()
  (free-fbos)
  (free-shaders))

(defun create-window (setup-fn render-fn cleanup-fn &key (title "windowLOL") (width 800) (height 600))
  "Create a window with an opengl context and spray vomit onto the user from the
  new window."
  (glfw:do-window (:width width :height height
                   :redbits 8 :greenbits 8 :bluebits 8 :alphabits 8
                   :depthbits 16 :stencilbits 16
                   :mode glfw:+window+  ; glfw:+fullscreen+
                   :title title
                   :window-no-resize nil
                   :opengl-version-major 3
                   :opengl-version-minor 3
                   :opengl-forward-compat t
                   :opengl-profile glfw::+opengl-core-profile+)
    ;; run our init forms
    ((glfw:set-window-size width height)
     ;; use the window manager's getProceAddress, which makes everything magically work
     (setf cl-opengl-bindings:*gl-get-proc-address* #'glfw:get-proc-address)
     (glfw:set-window-size-callback (cffi:callback resize-window-cb))
     (glfw:set-key-callback (cffi:callback key-press-cb))
     (glfw:set-mouse-pos-callback (cffi:callback mouse-move-cb))
     (glfw:set-mouse-button-callback (cffi:callback mouse-button-cb))
     (glfw:set-mouse-wheel-callback (cffi:callback mouse-wheel-cb))
     (setf *last-mouse-wheel-pos* 0)    ; we track this manually, so reset it
     (init-opengl)
     (funcall setup-fn)
     (dbg :info "(window) Window setup (~sx~s), starting render loop~%" width height))

    ;; this is our main loop (just call the draw fn over and over)
    (when *quit*
      (funcall cleanup-fn)
      (return-from glfw::do-open-window))
    (let* ((time (glfw:get-time))
           (dt (- time *last-time*)))
      (funcall render-fn dt)
      (setf *last-time* time))))

(defun resize-window (width height)
  (setf height (max height 1))
  (setf *perspective-matrix* (m-perspective 45.0 (/ width height) 1.0 6000.0))
  (setf *ortho-matrix* (m-ortho -1.0 1.0 -1.0 1.0 -1.0 1.0))
  (setf *window-width* width
        *window-height* height)
  (when *render-objs* (free-fbos))
  (let ((fbo (getf *render-objs* :fbo1)))
    (when fbo
      (free-fbo fbo)))
  (setf (getf *render-objs* :fbo1) (make-fbo width height :depth-type :tex))
  (gl:viewport 0 0 width height)
  (trigger :window-resize (list width height)))

(def-c-callback resize-window-cb :void ((width :int) (height :int))
  (resize-window width height))

