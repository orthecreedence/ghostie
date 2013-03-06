(in-package :ghostie-demo)

(defactor pill (actor)
  ((feet :accessor pill-feet :initform nil)))

(defmethod load-physics-body ((pill pill) actor-meta)
  (declare (ignore actor-meta))
  (let* ((body (call-next-method))
         (mass (cp-a:body-m (cpw:base-c body))))
    (setf (pill-feet pill) (caddr (cpw:body-shapes body)))
    (in-game (world)
      (let ((space (world-physics world)))
        ;; fix the character's rotation
        (let ((joint (cpw:make-joint (cpw:space-static-body space) body
                                     (lambda (body1 body2)
                                       (cp:damped-rotary-spring-new (cpw:base-c body1) (cpw:base-c body2)
                                                                    0d0 (* mass 240000d0) (* mass 25000d0))))))
          (cpw:space-add-joint space joint))))
    body))

(defun get-object-under-pill (pill)
  (when (and pill (game-object-physics-body pill))
    (let ((feet-shape-c (cpw:base-c (pill-feet pill)))
          (space-c (cpw:base-c (cpw:shape-space (pill-feet pill)))))
      (cffi:with-foreign-object (query 'clipmunk:segment-query-info)
        (let ((body-x (cp-a:body-p-x (cp-a:shape-body feet-shape-c)))
              (body-y (cp-a:body-p-y (cp-a:shape-body feet-shape-c)))
              (shape-offset-x (cp-a:circle-shape-c-x feet-shape-c))
              (shape-offset-y (cp-a:circle-shape-c-y feet-shape-c))
              (shape-radius (cp-a:circle-shape-r feet-shape-c)))
          (let* ((x1 (+ body-x shape-offset-x))
                 (y1 (+ body-y shape-offset-y 2d0 (- shape-radius)))
                 (x2 x1)
                 (y2 (- y1 10)))
            (cp:space-segment-query-first space-c x1 y1 x2 y2 99 (cffi:null-pointer) query)
            (let* ((shape (cp-a:segment-query-info-shape query))
                   (body (unless (cffi:null-pointer-p shape)
                           (cpw:find-body-from-pointer (cp-a:shape-body shape)))))
              (when (and body (not (eql body (game-object-physics-body pill))))
                (let ((n-x (cp-a:segment-query-info-n-x query))
                      (n-y (cp-a:segment-query-info-n-y query)))
                  ;(dbg :debug "cn: ~s~%" (list n-x n-y))
                  (values shape (list n-x n-y)))))))))))

(defun pill-grounded-p (pill)
  (multiple-value-bind (shape contact-normal) (get-object-under-pill pill)
    (when shape
      (when (< (car contact-normal) (+ (cadr contact-normal) 0.3))
        t))))

(defun pill-stop (pill)
  (when (and pill (game-object-physics-body pill))
    (let ((shape-c (cpw:base-c (pill-feet pill))))
      (setf (cp-a:shape-surface_v-x shape-c) 0d0
            (cp-a:shape-surface_v-y shape-c) 0d0))))

(defun pill-run (pill x)
  "Move the character on the HORizonal plane."
  (when (and pill (game-object-physics-body pill))
    (let ((body-c (cpw:base-c (game-object-physics-body pill))))
      (let ((vel (cp-a:body-v-x body-c))
            (y (/ (abs x) 3)))
        (when (< (abs vel) *character-max-run*)
          ;(setf (cp-a:shape-u shape-c) (if (zerop x) 0.4d0 0.1d0))
          (if (pill-grounded-p pill)
              (let ((shape-c (cpw:base-c (pill-feet pill))))
                (cp:body-activate body-c)
                (setf (cp-a:shape-surface_v-x shape-c) (coerce x 'double-float)
                      (cp-a:shape-surface_v-y shape-c) (coerce y 'double-float)))
              (cp:body-apply-impulse body-c
                                     (* 0.02d0 x (cp-a:body-m body-c))
                                     0d0
                                     0d0 0d0)))))))

(defun pill-jump (pill &key (x 0d0) (y 300d0))
  "Make the character jump."
  (when (and pill (game-object-physics-body pill))
    (let* ((body-c (cpw:base-c (game-object-physics-body pill))))
      ;(dbg :debug "v-y: ~s ~s~%" (cp-a:body-v-y body-c) (actor-vel-avg-y pill))
      (when (and (pill-grounded-p pill)
                 (< (abs (cp-a:body-v-y body-c)) 160)
                 (< (abs (actor-vel-avg-y pill)) 160))
        (let* ((vel-x (cp-a:body-v-x body-c))
               (x (* x (- 1 (/ (abs vel-x) *character-max-run*)))))
          (cp:body-apply-impulse body-c
                                 (* (cp-a:body-m body-c) (coerce x 'double-float))
                                 (* (cp-a:body-m body-c) (coerce y 'double-float))
                                 0d0 0d0))))))

