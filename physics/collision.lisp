(in-package :ghostie)

(defparameter *hack-world-global* nil)

(def-c-callback near-callback :void ((data :pointer) (geom1 :pointer) (geom2 :pointer))
  "Callback passed to c-land that gets run every time two collision objects
  (geoms or spaces) are near eachother. It can recursively call itself if there
  are spaces within spaces, which can often happen when setting up hierarchical
  collision system."
  (declare (optimize (speed 3) (safety 1) (space 1)))
  (let ((world *hack-world-global*))
    (if (or (= (ode:geom-is-space geom1) 1) (= (ode:geom-is-space geom2) 1))
        (progn
          (ode:space-collide-2 geom1 geom2 data (cffi:callback near-callback))
          (if (= (ode:geom-is-space geom1) 1)
              (ode:space-collide geom1 data (cffi:callback near-callback)))
          (if (= (ode:geom-is-space geom2) 1)
              (ode:space-collide geom2 data (cffi:callback near-callback))))
        (progn
          (let* ((body1 (ode:geom-get-body geom1))
                 (body1-set (if (cffi:null-pointer-p body1) nil t))
                 (body2 (ode:geom-get-body geom2))
                 (body2-set (if (cffi:null-pointer-p body2) nil t)))
            (when (and body1-set
                       body2-set
                       (= (ode:are-connected-excluding body1 body2 (cffi:foreign-enum-value 'ode:joint-type :+joint-type-contact+)) 1)
                       (eql (body-supertype (gethash body1 *body-lookup*)) :chain)
                       (eql (body-supertype (gethash body2 *body-lookup*)) :chain))
              (return-from near-callback nil))
            (cffi:with-foreign-object (contacts 'ode:contact +max-collision-objects+)
              (let ((num-contacts (ode:collide geom1 geom2 +max-collision-objects+ (ode-a:contact-geom (cffi:mem-aref contacts 'ode:contact)) (cffi:foreign-type-size 'ode:contact))))
                (dotimes (i num-contacts)
                  (let ((surface (ode-a:contact-surface (cffi:mem-aref contacts 'ode:contact i))))
                    (setf (ode-a:surface-parameters-mode surface) +physics-contact-mode+
                          (ode-a:surface-parameters-mu surface) +physics-contact-mu+
                          (ode-a:surface-parameters-mu-2 surface) +physics-contact-mu2+
                          (ode-a:surface-parameters-slip-1 surface) +physics-contact-slip1+
                          (ode-a:surface-parameters-slip-2 surface) +physics-contact-slip2+
                          (ode-a:surface-parameters-soft-erp surface) +physics-contact-erp+
                          (ode-a:surface-parameters-soft-cfm surface) +physics-contact-cfm+
                          (ode-a:surface-parameters-motion-1 surface) +physics-contact-motion1+
                          (ode-a:surface-parameters-motion-2 surface) +physics-contact-motion2+
                          (ode-a:surface-parameters-motion-n surface) +physics-contact-motion-n+
                          (ode-a:surface-parameters-bounce surface) +physics-contact-bounce+
                          (ode-a:surface-parameters-bounce-vel surface) +physics-contact-bounce-min-vel+))
                  (let ((joint-ptr (ode:joint-create-contact (phx-obj world)
                                                             (phx-obj (phx-world-contact-group world))
                                                             (cffi:mem-aref contacts 'ode:contact i))))
                    (ode:joint-attach joint-ptr body1 body2))))))))))

(defun collide (world)
  "Collide the objects in the world. Uses a c callback (near-callback) to get
  the objects that are close to eachother, then loops over those and determines
  whether or not they are contacting.

  near-callback can call itself recursively if the objects that are near each
  other are spaces instead of geoms. This allows hierarchical collision which
  can offer performance benefits if used correctly."
  (unless *hack-world-global*
    (setf *hack-world-global* world))
  (ode:space-collide (phx-obj (phx-world-space world))
                     (cffi:null-pointer)
                     (cffi:callback near-callback)))

