(in-package :ghostie-demo)

(defun input-key-handler (game render-world dt main-actor)
  (let ((sync-position nil)
        (world (game-world game)))
    (when (key= #\-)
      (setf sync-position t)
      (decf (nth 2 (world-position render-world)) (* (coerce dt 'single-float) 100)))
    (when (key= #\=)
      (setf sync-position t)
      (incf (nth 2 (world-position render-world)) (* (coerce dt 'single-float) 100)))
    (pill-stop main-actor)
    (when (key= glfw:+key-left+)
      (pill-run main-actor -200.4))
    (when (key= glfw:+key-right+)
      (pill-run main-actor 200.4))
    (when (key= glfw:+key-up+)
      (let ((leftp (key= glfw:+key-left+))
            (rightp (key= glfw:+key-right+)))
        (pill-jump main-actor
                   :y 320d0
                   :x (cond (leftp -200d0)
                            (rightp 200d0)
                            (t 0d0)))))
    (when sync-position
      (let ((position (copy-tree (world-position render-world))))
        (setf (world-position world) position)))
    (when (key= #\P)
      (dbg :notice "(actor) Position: ~s~%" (object-position main-actor)))
    (when (key= #\A)
      (let ((x (car (object-position main-actor))))
        (add-box world :x x)))))

(defun input-key-press (game key)
  (when (or (eq (code-char key) #\Q)
            (eq key glfw:+key-esc+))
    (stop-game game)))

(defun input-key-release (game key)
  (let ((world (game-world game)))
    (case (code-char key)
      (#\L
       (ghostie::world-cleanup world)
       (ghostie::create-world world)
       (ghostie::init-render world)
       (ghostie::world-load-level world "trees"))
      (#\B
       (dbg :debug "(bridge) Reloading (and creating) bridge~%")
       (load (format nil "~a/~a/~a/bridge/class"
                     *game-directory*
                     *resource-path*
                     *compound-object-path*))
       (test-bridge))
      (#\C
       (recompile-shaders))
      (#\T
       (ghostie::test-gl-funcs))
      (#\R
       (let* ((camera (getf (level-meta (world-level world)) :camera))
              (camera (if camera camera '(0 0 -36))))
         (dbg :debug "(input) Camera reset: ~s~%" camera)
         (setf (world-position world) (copy-tree camera)))))))

