(in-package :ghostie-demo)

(defun start ()
  (let ((game (ghostie::create-game "trees")))
    (bind :key-release (key)
      (input-key-release game key))

    (bind :key-press (key)
      (input-key-press game key))

    (bind (:render-step 'render-step-key-handler) (world dt)
      (input-key-handler game world dt))))

