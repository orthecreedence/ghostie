(in-package :ghostie)

(defun mouse-pos ()
  "Get the current mouse position"
  (glfw:get-mouse-pos))

(defun key-pressed-p (key)
  "Determine if a key is in a pressed state."
  (let ((key (if (characterp key)
                 (char-int key)
                 key)))
    (eq (glfw:get-key key) glfw:+press+)))

(defun mouse-button-pressed-p (btn-num)
  "Determines if a particular mouse button is pressed."
  (eq (glfw:get-mouse-button btn-num) glfw:+press+))

(defmacro key= (key)
  "Macro that wraps key status testing."
  (let ((key (if (characterp key)
                 (char-int key)
                 key)))
    `(eql (glfw:get-key ,key) glfw:+press+)))

(defun key-press (key state)
  "Triggered when a key is pressed. More or less just forwards the event."
  (enqueue (lambda (w)
             (declare (ignore w))
             (trigger (if (eq state glfw:+press+)
                          :key-press
                          :key-release)
                      key))
           :game))

(defun mouse-move (x y)
  "Triggered when the mouse changes position."
  (enqueue (lambda (w)
             (declare (ignore w))
             (trigger :mouse-position x y))
           :game))

(defun mouse-button (button status)
  "Triggered when a mouse button is pressed/released."
  (let ((state (if (= status glfw:+press+)
                   :press
                   :release)))
    (enqueue (lambda (w)
               (declare (ignore w))
               (trigger :mouse-button button state))
             :game)))

(defvar *last-mouse-wheel-pos* 0
  "Used to track the mouse wheel position.")

(defun mouse-wheel (pos)
  "Triggered when the mouse wheel is moved."
  (let* ((change (- pos *last-mouse-wheel-pos*)))
    (Format t "change: ~S~%" change)
    (setf *last-mouse-wheel-pos* pos)
    (enqueue (lambda (w)
               (declare (ignore w))
               (trigger :mouse-wheel change pos))
             :game)))

(def-c-callback key-press-cb :void ((key :int) (state :int))
  (key-press key state))

(def-c-callback mouse-move-cb :void ((x :int) (y :int))
  (mouse-move x y))                

(def-c-callback mouse-button-cb :void ((button :int) (status :int))
  (mouse-button button status))

(def-c-callback mouse-wheel-cb :void ((pos :int))
  (mouse-wheel pos))

