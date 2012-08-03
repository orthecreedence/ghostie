(in-package :ghostie)

(defparameter *joint-types* '(:none :ball :hinge
                              :slider :contact :universal
                              :hinge2 :fixed :null
                              :amotor :lmotor :plane-2d
                              :pr :pu :piston))

(defclass joint (phx-obj)
  ((phx-type :initform :joint)
   (type :accessor joint-type :initarg :type)))

(defun make-joint (world type joint-group)
  "Wrapper around joint creation (and grouping)."
  (let ((joint (make-joint world type (phx-obj joint-group))))
    (add-joint-to-group joint joint-group)
    joint))

(defun make-joint (world type joint-group)
  "Wrapper around joint creation."
  (unless (find type *joint-types*)
    (error "Trying to instantiate joint with bad type: ~A" type))
  (let* ((joint-group (if (cffi:pointerp joint-group) joint-group (cffi:null-pointer)))
         (phx-joint (joint-create world type joint-group)))
    (make-instance 'joint :phx phx-joint :type type)))

;(defmethod make-contact-joint ((world world) group contacts) )

(defun joint-create (world type group-ptr)
  "Wrapper around joint creation in c-land."
  (let ((phx-world (phx-obj world))
        (joint-fn (case type (:ball #'ode:joint-create-ball)
                             (:hinge #'ode:joint-create-hinge)
                             (:slider #'ode:joint-create-slider)
                             (:universal #'ode:joint-create-universal)
                             (:hinge2 #'ode:joint-create-universal)
                             (:fixed #'ode:joint-create-fixed)
                             (:amotor #'ode:joint-create-amotor)
                             (:lmotor #'ode:joint-create-lmotor)
                             (:plane-2d #'ode:joint-create-plane-2d)
                             (:pr #'ode:joint-create-pr)
                             (:pu #'ode:joint-create-pu)
                             (:piston #'ode:joint-create-piston))))
    (unless joint-fn
      (error "Loaded nil joint creation function from type ~a" type))
    (funcall joint-fn phx-world group-ptr)))

(defmethod joint-attach ((joint joint) (body1 body) &optional (body2 0))
  "Attach a joint to two bodies. Although the second body is optional, in most
  cases a joint attaches two bodies."
  (let ((body2-is-body (eql (type-of body2) 'body)))
    (ode:joint-attach (phx-obj joint)
                      (phx-obj body1)
                      (if body2-is-body (phx-obj body2) body2))
    (push joint (body-joints body1))
    (when body2-is-body
      (push joint (body-joints body2)))))

(defmethod joint-destroy ((joint joint))
  "Destroy a joint object."
  (when (phx-destroy joint)
    (ode:joint-destroy (phx-obj joint))))



