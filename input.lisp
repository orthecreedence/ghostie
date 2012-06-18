(in-package :game-level)

(defvar *mouse-buttons* nil)
(defvar *mouse-x* 0)
(defvar *mouse-y* 0)

(defun key-pressed (key state)
  ;; state == glfw:+release+ || glfw:+press+
  (declare (ignore key state)))

(def-c-callback key-pressed-cb :void ((key :int) (state :int))
  (key-pressed key state))

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
      (decf (nth 2 *world-position*) (* (coerce dt 'single-float) 10)))
    (key= #\=
      (incf (nth 2 *world-position*) (* (coerce dt 'single-float) 10)))
    (key= glfw:+key-up+
      (decf (nth 1 *world-position*) (* (coerce dt 'single-float) 10)))
    (key= glfw:+key-down+
      (incf (nth 1 *world-position*) (* (coerce dt 'single-float) 10)))
    (key= glfw:+key-left+
      (incf (nth 0 *world-position*) (* (coerce dt 'single-float) 10)))
    (key= glfw:+key-right+
      (decf (nth 0 *world-position*) (* (coerce dt 'single-float) 10)))
    (key= #\R
      (setf *world-position* '(-17.19999 -24.00002 -36.000065))
      (sleep .1))
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

(defun mouse-pos (x y)
  ;(format t "Mouse: ~ax~a~%" x y)
  (setf *mouse-x* x
        *mouse-y* y))

(def-c-callback mouse-pos-cb :void ((x :int) (y :int))
  (mouse-pos x y))

(defun mouse-button-pressed (btn-num)
  (getf *mouse-buttons* btn-num))

(defun mouse-button (btn action)
  ;(format t "Mouse btn: ~a (~a)~%" btn (if (eq action glfw:+press+) :pressed :released))
  (if (eq action glfw:+press+)
      (setf (getf *mouse-buttons* btn) t)
      (setf (getf *mouse-buttons* btn) nil)))

(def-c-callback mouse-button-cb :void ((btn :int) (action :int))
  (mouse-button btn action))
