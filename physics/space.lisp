(in-package :ghostie)

(defparameter *space-types* '(:simple :hash :quad-tree))

;; note that this WAS names "space" but due to CL naming conflicts has been
;; renamed to collision space. the method names and accessors however, are
;; still space-*
(defclass collision-space (phx-obj)
  ((phx-type :initform :space)
   (type :accessor space-type :initarg :type)
   (shapes :accessor space-shapes :initform nil)))

(defun make-space (type)
  "Wrapper around space creation."
  (unless (find type *space-types*)
    (error "Trying to instantiate space with bad type: ~A" type))
  (let* ((phx-obj (space-create type)))
    (make-instance 'collision-space :phx phx-obj :type type)))

(defun space-create (type &optional (curspace (cffi:null-pointer)))
  "Wrapper around space creation in c-land."
  (let ((space-fn (case type (:simple #'ode:simple-space-create)
                             (:hash #'ode:hash-space-create))))
    (funcall space-fn curspace)))

(defmethod space-add-shape ((space collision-space) (shape shape))
  "Add a shape to a space in c-land and track it internally. It's careful about
  double-adding and makes a point to check whether the shape is in that space
  in c-land before adding it."
  (format t " - adding shape of type ~a to space ~a.~%" (shape-type shape) space)
  (unless (find shape (space-shapes space))
    (push shape (space-shapes space)))
  (unless (= (ode:space-query (phx-obj space) (phx-obj shape)) 1)
    (ode:space-add (phx-obj space) (phx-obj shape))))

(defmethod space-remove-shape ((space collision-space) (shape shape))
  "Remove a shape from a c-land space. Remove it in the internal tracking as
  well."
  (ode:space-remove (phx-obj space) (phx-obj shape))
  (setf (space-shapes space) (remove-if (lambda (s) (eql (phx-obj s) (phx-obj shape))) (space-shapes space))))

(defmethod space-destroy ((space collision-space))
  "Destroy a space. Destroys all shapes in that space as well."
  (dolist (s (space-shapes space))
    (phx-destroy s))
  (when (phx-destroy space)
    (ode:space-destroy (phx-obj space))))

