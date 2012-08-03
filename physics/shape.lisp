(in-package :ghostie)

(defparameter *shape-types* '(:sphere :box :plane
                              :capsule :cylinder :ray
                              :convex :triangle-mesh :heightfield))

(defclass shape (phx-obj)
  ((phx-type :initform :shape)
   (type :accessor shape-type :initarg :type)
   (placeable :accessor shape-placeable :initarg :placeable :initform t)
   (body :accessor shape-body :initform nil)))

(defun make-shape (type create-fn &key space)
  "Wraps creation of shapes (geoms). Takes a creat function which instantiates
  the actual c object of the shape (this is mainly because there are too many
  classes, each with its own parameters to make wrappers for)."
  (unless (find type *shape-types*)
    (error "Trying to instantiate space with bad type: ~A" type))
  (let* ((space-ptr (if space (phx-obj space) (cffi:null-pointer)))
         (phx-obj (funcall create-fn space-ptr))
         (placeable (if (eql type :plane) nil t))
         (shape (make-instance 'shape :phx phx-obj :type type :placeable placeable)))
    ;; if we passed a space, add the shape to it. this is smart about 
    ;; duplicates and shit (both in our internal tracking and c-land).
    (when space (space-add-shape space shape))
    shape))

(defun shape-set-body (shape body)
  "Attach a shape to a body."
  (unless (shape-placeable shape)
    (format t "Trying to set body of non-placeable shape. Not going to happen.~%")
    (return-from shape-set-body nil))
  (ode:geom-set-body (phx-obj shape) (phx-obj body))
  (setf (shape-body shape) body))

(defun shape-retrieve-body (shape bodies-list)
  "Given a shape and a list of bodies, find the body that this shape is
  attached to by comparing memory pointers."
  (unless (shape-placeable shape)
    (format t "Trying to get body of non-placeable shape. Not going to happen.~%")
    (return-from shape-retrieve-body nil))
  (let* ((body-ptr (ode:geom-get-body (phx-obj shape)))
         (body (block getbody
                      (dolist (b bodies-list)
                        (let ((ptr-b (phx-obj b)))
                          (when (eql ptr-b body-ptr)
                            (return-from getbody b)))))))
    (when body
      (setf (shape-body shape) body))))

(defun shape-destroy (shape)
  "Destroy the c object for this shape."
  (when (phx-destroy shape)
    (ode:geom-destroy (phx-obj shape))))
