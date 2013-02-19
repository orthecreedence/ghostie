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

(defun grab-mouse ()
  "Allows the window to hide the mouse and keep it from leaving the window. This
   can be useful if you want the mouse cursor to control various objects in the
   game without showing the native mouse cursor."
  (glfw:disable glfw:+mouse-cursor+))

(defun release-mouse ()
  "Gives the mouse cursor back to the OS (opposite of grab-mouse)."
  (glfw:enable glfw:+mouse-cursor+))

(defmacro key= (key)
  "Macro that wraps key status testing."
  (let ((key (if (characterp key)
                 (char-int key)
                 key)))
    `(eql (glfw:get-key ,key) glfw:+press+)))

(defun key-press (key state)
  "Triggered when a key is pressed. More or less just forwards the event."
  (trigger (if (eq state glfw:+press+)
             :key-press
             :key-release)
           key))

(defun mouse-move (x y)
  "Triggered when the mouse changes position."
  (trigger :mouse-position x y))

(defun mouse-button (button status)
  "Triggered when a mouse button is pressed/released."
  (trigger (if (= status glfw:+press+)
               :mouse-button-press
               :mouse-button-release)
           button))

(defvar *last-mouse-wheel-pos* 0
  "Used to track the mouse wheel position.")

(defun mouse-wheel (pos)
  "Triggered when the mouse wheel is moved."
  (let* ((change (- pos *last-mouse-wheel-pos*)))
    (setf *last-mouse-wheel-pos* pos)
    (trigger (if (< 0 change)
                 :mouse-wheel-up
                 :mouse-wheel-down)
             pos)))

(def-c-callback key-press-cb :void ((key :int) (state :int))
  (key-press key state))

(def-c-callback mouse-move-cb :void ((x :int) (y :int))
  (mouse-move x y))                

(def-c-callback mouse-button-cb :void ((button :int) (status :int))
  (mouse-button button status))

(def-c-callback mouse-wheel-cb :void ((pos :int))
  (mouse-wheel pos))

