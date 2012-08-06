(in-package :ghostie)

(defun add-random-box (world)
  (let ((space (world-physics world))
        (x (- (random 200d0) 100)))
    (let* ((verts #((-2 -2) (-2 2) (2 2) (2 -2)))
           (moment (cpw:moment-for-poly 1 verts 0 0))
           (box-body (cpw:make-body (lambda () (cp:body-new 1d0 moment))))
           (box-shape (cpw:make-shape :poly box-body (lambda (body) (cpw:shape-poly body verts 0 0)))))
      (setf (cp-a:shape-u (cpw:base-c box-shape)) 0.7d0
            (cp-a:shape-e (cpw:base-c box-shape)) 0.3d0)
      (cp:body-set-pos (cpw:base-c box-body) x 100d0)
      (let ((gl-objects (list (make-gl-object :data (glu-tessellate:tessellate verts)
                                              :position '(0 0 0)
                                              :color (hex-to-rgb "#22cc44")))))
        (let ((game-object (make-game-object :type 'game-object
                                             :gl-objects gl-objects
                                             :physics box-body)))
          (cpw:space-add-body space box-body)
          (cpw:space-add-shape space box-shape)
          (sync-game-object-to-physics game-object)
          (push game-object (level-objects (world-level world))))))))
