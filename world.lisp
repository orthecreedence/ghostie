(in-package :ghostie)

(defvar *perspective-matrix* nil)
(defvar *ortho-matrix* nil)
(defvar *view-matrix* nil)
(defvar *game-data* nil)

(defclass world ()
  ((physics :accessor world-physics :initform nil)
   (position :accessor world-position :initform '(0 0 -36))
   (level :accessor world-level :initform nil)
   (draw-meta :accessor world-draw-meta :initform nil))
  (:documentation "Describes a world (physics, current level, and objects."))

(defun create-world (&optional world)
  "Create (or re-initialize) a world object. Sets up physics and some default
   drawing characteristics for the world."
  (dbg :debug "(world) Creating world~%")
  (let ((world (if world world (make-instance 'world))))
    ;; setup physics
    (let ((space (cpw:make-space :gravity-y -9.8d0)))
      (setf (cp-a:space-sleep-time-threshold (cpw:base-c space)) 3d0)
      (setf (cp-a:space-damping (cpw:base-c space)) 0.9d0)
      (setf (world-physics world) space)
      (cp:space-set-default-collision-handler (cpw:base-c space)
                                              (cffi:callback cp-begin)
                                              (cffi:callback cp-pre-solve)
                                              (cffi:callback cp-post-solve)
                                              (cffi:callback cp-separate)
                                              (cffi:null-pointer)))
    (unless (world-draw-meta world)
      (setf (getf (world-draw-meta world) :background) (hex-to-rgb "#222222" :type 'list)
            (getf (world-draw-meta world) :fog-amt) 0.0))
    world))

;;; ----------------------------------------------------------------------------
;;; Game functions
;;; ----------------------------------------------------------------------------
(defun world-game-cleanup (world)
  "Cleanup (ie free) any game objects in the game thread, free the physics world
   and mark the world as empty."
  (dbg :info "(world) Cleaning up game world~%")
  (when (world-level world)
    (level-cleanup (world-level world)))
  (let ((space (world-physics world)))
    (dolist (obj (append (cpw:space-bodies space)
                         (cpw:space-shapes space)
                         (cpw:space-joints space)))
      (cpw:destroy obj))
    (cpw:destroy space))
  (setf (world-physics world) nil
        (world-level world) nil
        (world-draw-meta world) nil)
  world)

(defun game-world-sync (world)
  "This function makes sure all objects in the game thread are synced with their
   physics counterparts.
   
   It also copies some of the world's display properties over to the render
   thread for display."
  (let ((level (world-level world)))
    ;; sync each game object with its physics body
    (dolist (game-object (append (level-objects level)
                                 (level-actors level)))
      (sync-game-object-to-physics game-object :render t))))

(defun step-game-world (world)
  "Move the game world forward by one step. Calculates the physics delta for
   each simulated object"
  (when *quit* (return-from step-game-world nil))
  (trigger :game-step world)
  (let ((space (world-physics world)))
    (cpw:space-step space :dt +dt+)
    (cpw:sync-space-bodies space)
    (dolist (game-object (level-objects (world-level world)))
      (sync-game-object-to-physics game-object))
    (dolist (actor (level-actors (world-level world)))
      (update-actor-state actor))))

(defun world-load-level (world level-name)
  "Load a level into the given world."
  ;; load the current level
  (dbg :notice "(world) Loading level ~a~%" level-name)
  (setf (world-level world) (load-level level-name))
  (init-level-physics-objects world)
  (let ((level-meta (level-meta (world-level world))))
    ;; grab/generate some display/physics characteristics for the level
    (let* ((camera (getf level-meta :camera))
           (gravity (getf level-meta :gravity))
           (iterations (getf level-meta :physics-iterations))
           (background (if (getf level-meta :background)
                           (hex-to-rgb (getf level-meta :background) :type 'list)
                           (hex-to-rgb "#262524" :type 'list)))
           (fog-amt (getf (level-meta (world-level world)) :fog-amt))
           (fog-start (getf (level-meta (world-level world)) :fog-start))
           (fog-end (getf (level-meta (world-level world)) :fog-end))
           (fog-color (if (getf level-meta :fog-color)
                          (hex-to-rgb (getf level-meta :fog-color) :type 'list)
                          background)))
      (when iterations
        (dbg :debug "(world) Setting physics iterations: ~a~%" iterations)
        (cp-f:space-set-iterations (cpw:base-c (world-physics world)) (round iterations)))
      (when gravity
        (setf (cp-a:space-gravity-y (cpw:base-c (world-physics world))) (coerce gravity 'double-float)))
      (setf (getf (world-draw-meta world) :background) background
            (getf (world-draw-meta world) :fog-amt) (if fog-amt fog-amt 0.0)
            (getf (world-draw-meta world) :fog-start) (if fog-start fog-start 60.0)
            (getf (world-draw-meta world) :fog-end) (if fog-end fog-end 160.0)
            (getf (world-draw-meta world) :fog-color) fog-color)
      (when camera
        (setf (world-position world) camera))))

  (let ((meta (copy-tree (world-draw-meta world)))
        (position (copy-tree (world-position world))))
    (in-render (render-world)
      (dbg :info "(world) Copying game world meta to render world.~%")
      (apply #'gl:clear-color (getf meta :background))
      (setf (world-draw-meta render-world) meta
            (world-position render-world) position))))

;;; ----------------------------------------------------------------------------
;;; Render functions
;;; ----------------------------------------------------------------------------
(defun step-render-world (world dt)
  "Runs all queued render items (in the render thread) and draws the world. Also
   lets the game thread know a render happened, as well as syncs game objects to
   their physics bodies."
  (progn
    (trigger :render-step world dt)
    (in-game (game-world)
      (game-world-sync game-world))
    (process-queue world :render)
    (draw-world world)))

(defun world-render-cleanup (world)
  "Cleanup the render thread. Frees any OpenGL objects laying around and makes
   sure any other display objects are properly cleaned up."
  (dbg :info "(world) Cleaning up render world~%")
  (when (world-level world)
    (level-cleanup (world-level world)))
  (setf (world-level world) nil
        (world-draw-meta world) nil)
  (free-gl-assets)
  world)

(defun free-gl-assets ()
  "Free all the GL objects we're using to display our game world."
  (loop for (nil obj) on *game-data* by #'cddr do
    (when (subtypep (type-of obj) 'gl-object)
      (free-gl-object obj)))
  (setf *game-data* nil))

(defun init-render (world)
  "Init the render thread."
  (free-gl-assets)
  (apply #'gl:clear-color (getf (world-draw-meta world) :background))
  ;; this is the quad we render our FBO texture onto
  (setf (getf *game-data* :quad) (make-gl-object :data '(((-1 -1 0) (1 -1 0) (-1 1 0)) ((1 -1 0) (1 1 0) (-1 1 0)))
                                                 :uv-map #(0 0 1 0 0 1 1 1))))

;; TODO: custom pipeline.
;; TODO: move things like fog to game (makes assumptions)
(defun draw-world (world)
  "Draw the render world. This function processes all the shaders and GL objects
   and draws them onto the window."
  (when *quit* (return-from draw-world nil))
  (gl:bind-framebuffer-ext :framebuffer (gl-fbo-fbo (getf *render-objs* :fbo1)))
  (gl:clear :color-buffer-bit :depth-buffer)
  (unless (world-level world)
    (return-from draw-world nil))
  (use-shader :main)
  (set-shader-var #'gl:uniformf "fogAmt" (getf (world-draw-meta world) :fog-amt))
  (set-shader-var #'gl:uniformf "fogStart" (getf (world-draw-meta world) :fog-start))
  (set-shader-var #'gl:uniformf "fogEnd" (getf (world-draw-meta world) :fog-end))
  (when (getf (world-draw-meta world) :fog-color)
    (apply #'set-shader-var (append (list #'gl:uniformf "fogColor") (getf (world-draw-meta world) :fog-color))))
  (setf *view-matrix* (apply #'m-translate (world-position world)))
  (set-shader-matrix "cameraToClipMatrix" *perspective-matrix*)
  (when (world-level world)
    (draw-level (world-level world)))
  (gl:bind-framebuffer-ext :framebuffer 0)
  (let ((fbo (getf *render-objs* :fbo1)))
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (use-shader :dof)
    (set-shader-matrix "cameraToClipMatrix" *ortho-matrix*)
    (set-shader-var #'gl:uniformi "renderTexWidth" 600)
    (set-shader-var #'gl:uniformi "renderTexHeight" 600)
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d (gl-fbo-tex fbo))
    (gl:generate-mipmap-ext :texture-2d)
    (set-shader-var #'gl:uniformi "renderTex" 0)
    (gl:active-texture :texture1)
    (gl:bind-texture :texture-2d (gl-fbo-depth fbo))
    (set-shader-var #'gl:uniformi "depthTex" 1))
  (draw-gl-object (getf *game-data* :quad))
  (use-shader 0))

(defun test-gl-funcs ()
  "IGNORE ME!!"
  ;(gl:clear-color 1 1 1 1)
  (format t "OpenGL version: ~a~%" (gl:get-string :version))
  (format t "Shader version: ~a~%" (gl:get-string :shading-language-version))
  ;(format t "Extensions: ~a~%" (gl:get-string :extensions))
  (format t "Err: ~a~%" (gl:get-error)))

