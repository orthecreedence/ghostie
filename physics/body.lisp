(in-package :ghostie)

;; stores a reverse lookup (c pointer -> lisp body class) so a body can be
;; pulled out quickly just based on it's physics object pointer. this is mainly
;; so the collision solver can pull information about a body it's processing,
;; such as what type it is.
(defparameter *body-lookup* (make-hash-table :test #'eql))

(defclass body (phx-obj)
  ((phx-type :initform :body)
   (supertype :accessor body-supertype :initarg :supertype :initform nil)
   (joints :accessor body-joints :initarg :joints :initform nil)))

(defun make-body (world &key (constrain-2d t) (supertype nil))
  "Wrapper around body creation."
  (let* ((phx-body (ode:body-create (phx-obj world)))
         (body (make-instance 'body :phx phx-body :supertype supertype)))
    (when constrain-2d
      (joint-attach (make-joint world :plane-2d 0) body (cffi:null-pointer)))
    ;; add body to pointer -> body lookup table
    (setf (gethash phx-body *body-lookup*) body)
    (format t " - created body ~a~%" body)
    body))

(defun body-set-mass (body mass-fn)
  "Create a mass object and assign it into the body using a callback."
  ;; mass is copied into body internally, so no need to keep our mass around
  ;; once it's set into the body
  (cffi:with-foreign-object (mass-ptr 'ode:mass)
    (funcall mass-fn mass-ptr)
    (ode:body-set-mass (phx-obj body) mass-ptr)))

(defun body-constrain-2d (body)
  "Constrain a body's rotation two a two-dimensional axis (ie only rotate on
  the z-axis)."
  (let* ((body-ptr (phx-obj body))
         (angular-vel (ode:body-get-angular-vel body-ptr))
         (quat-ptr (ode:body-get-quaternion body-ptr)))
    (cffi:with-foreign-object (quat :float 4)
      (setf (cffi:mem-aref quat :float 0) (cffi:mem-aref quat-ptr :float)
            (cffi:mem-aref quat :float 1) 0.0
            (cffi:mem-aref quat :float 2) 0.0
            (cffi:mem-aref quat :float 3) (cffi:mem-aref quat-ptr :float 3))
      (let ((quat-len (sqrt (+ (expt (cffi:mem-aref quat :float 0) 2) (expt (cffi:mem-aref quat :float 3) 2)))))
        (setf (cffi:mem-aref quat :float 0) (/ (cffi:mem-aref quat :float 0) quat-len)
              (cffi:mem-aref quat :float 3) (/ (cffi:mem-aref quat :float 3) quat-len))
        (ode:body-set-quaternion body-ptr quat)
        (ode:body-set-angular-vel body-ptr 0.0 0.0 (cffi:mem-aref angular-vel :float 2))))))


(defun body-destroy (body)
  "Destroy a body."
  (when body body
    (when (phx-destroy body)
      ;; remove body from lookup table
      (remhash (phx-obj body) *body-lookup*)
      (ode:body-destroy (phx-obj body)))))

