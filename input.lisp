(in-package :game-level)

(defun key-pressed (key state)
  ;; state == glfw:+release+ || glfw:+press+
  (declare (ignore key state)))

;(cffi:defcallback key-pressed-cb :void ((key :int) (state :int))
;  (key-pressed key state))

(defun key-handler (dt)
  (macrolet ((key-is-on (key)
               `(eql (glfw:get-key ,key) glfw:+press+))
             (key= (key &body body)
               `(when (key-is-on ,(if (characterp key)
                                      (char-int key)
                                      key))
                  ,@body))
             (key-fn (fn keys &body body)
               `(when (,fn ,@(loop for k in keys collect (list 'key-is-on
                                                               (if (characterp k)
                                                                   (char-int k)
                                                                   k))))
                  ,@body))
             (key|| (keys &body body)
               `(key-fn or ,keys ,@body))
             (key&& (keys &body body)
               `(key-fn and ,keys ,@body)))
    (key|| (glfw:+key-esc+ #\Q)
      (setf *quit* t))
    (key= #\-
      (decf (nth 2 *world-position*) (* dt 10)))
    (key= #\=
      (incf (nth 2 *world-position*) (* dt 10)))
    (key= glfw:+key-up+
      (decf (nth 1 *world-position*) (* dt 10)))
    (key= glfw:+key-down+
      (incf (nth 1 *world-position*) (* dt 10)))
    (key= glfw:+key-left+
      (incf (nth 0 *world-position*) (* dt 10)))
    (key= glfw:+key-right+
      (decf (nth 0 *world-position*) (* dt 10)))
    (key= #\R
      (setf *world-position* '(-17.19999 -24.00002 -36.000065)))
    (key= #\C
      (recompile-shaders))
    (key= #\M
      (let ((spike (getf *game-data* :spike)))
        (when spike
          (bt:make-thread (lambda ()
                            (loop for d from 0 to 360 do
                                  (setf (gl-object-rotation (getf *game-data* :spike)) (list 1 0 0 d))
                                  (sleep (/ dt .02))))))))
    (key= #\T
      (test-gl-funcs))
    (key= #\L
      (load-assets))))

