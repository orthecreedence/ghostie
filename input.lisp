(in-package :ghostie)

(defun mouse-pos ()
  (glfw:get-mouse-pos))

(defun mouse-btn (btn-num)
  (eq (glfw:get-mouse-button btn-num) glfw:+press+))

(defmacro key= (key)
  (let ((key (if (characterp key)
                 (char-int key)
                 key)))
    `(eql (glfw:get-key ,key) glfw:+press+)))

(defun key-pressed (key state)
  "Triggered when a key is pressed. More or less just forwards the event."
  (enqueue (lambda (w)
             (declare (ignore w))
             (trigger (if (eq state glfw:+press+)
                          :key-press
                          :key-release)
                      key))
           :game))

(def-c-callback key-pressed-cb :void ((key :int) (state :int))
  (key-pressed key state))

(defun key-pressed-p (key)
  "Determine if a key is in a pressed state."
  (let ((key (if (characterp key)
                 (char-int key)
                 key)))
    (eq (glfw:get-key key) glfw:+press+)))
