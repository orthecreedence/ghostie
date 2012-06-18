(in-package :game-level)

(defun mouse-pos ()
  (glfw:get-mouse-pos))

(defun mouse-btn (btn-num)
  (eq (glfw:get-mouse-button btn-num) glfw:+press+))

(defun key-pressed (key state)
  ;; state == glfw:+release+ || glfw:+press+
  (declare (ignore key state)))

(def-c-callback key-pressed-cb :void ((key :int) (state :int))
  (key-pressed key state))

(defmacro key= (key)
  (let ((key (if (characterp key)
                 (char-int key)
                 key)))
    `(eql (glfw:get-key ,key) glfw:+press+)))

(defun key-handler (dt)
  (when (key= #\-)
    (decf (nth 2 *world-position*) (* (coerce dt 'single-float) 10)))
  (when (key= #\=)
    (incf (nth 2 *world-position*) (* (coerce dt 'single-float) 10)))
  (when (key= glfw:+key-up+)
    (decf (nth 1 *world-position*) (* (coerce dt 'single-float) 10)))
  (when (key= glfw:+key-down+)
    (incf (nth 1 *world-position*) (* (coerce dt 'single-float) 10)))
  (when (key= glfw:+key-left+)
    (incf (nth 0 *world-position*) (* (coerce dt 'single-float) 10)))
  (when (key= glfw:+key-right+)
    (decf (nth 0 *world-position*) (* (coerce dt 'single-float) 10)))
  (when (key= #\R)
    (setf *world-position* '(-17.19999 -24.00002 -36.000065))
    (sleep .1))
  (when (key= #\C)
    (recompile-shaders))
  (when (key= #\M)
    (let ((spike (getf *game-data* :spike)))
      (when spike
        (bt:make-thread (lambda ()
                          (loop for d from 0 to 360 do
                                (setf (gl-object-rotation (getf *game-data* :spike)) (list 1 0 0 d))
                                (sleep (/ dt .02))))))))
  (when (key= #\T)
    (test-gl-funcs))
  (when (key= #\L)
    (load-assets))
  (when (or (key= glfw:+key-esc+) (key= #\Q))
    (setf *quit* t)))


