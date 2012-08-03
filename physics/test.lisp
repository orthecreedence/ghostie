(in-package :ghostie)

(defun init-physics (world)
  (ode:init-ode-2 0)
  (setf (phx-world-space world) (make-space :hash))
  (make-shape :plane
              (lambda (space-ptr)
                (ode:create-plane space-ptr 0.0 1.0 0.0 0.0))
              :space (phx-world-space world)))

(defun add-random-box (world &key (x (- (random 60.0) 30)) (y (+ (random 10.0) 40)))
  (let* ((phx-world (world-physics world))
         (box-body (make-body phx-world))
         (box-shape (make-shape :box
                                (lambda (space-ptr) (ode:create-box space-ptr 10.0 10.0 +object-thickness+))
                                :space (phx-world-space phx-world))))
    (body-set-mass box-body (lambda (mass-ptr) (ode:mass-set-box mass-ptr 0.1 10.0 10.0 +object-thickness+)))
    (shape-set-body box-shape box-body)
    (ode:body-set-position (phx-obj box-body) x y 0.0)
    (push box-body (phx-world-bodies phx-world))

    (let* ((gl-object (make-gl-object :data (glu-tessellate:tessellate #((-5 -5) (5 -5) (5 5) (-5 5)))))
           (game-object (create-game-object :gl-objects (list gl-object)
                                            :position (list x y 0))))
      (setf (game-object-physics-body game-object) box-body)
      (push game-object (level-objects (world-level world))))))
