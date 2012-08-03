(in-package :ghostie)

(defclass joint-group (phx-obj)
  ((phx-type :initform :joint-group)
   (joints :accessor joint-group-joints :initform nil)))

(defun make-joint-group ()
  "Wrapper around creation of joint group."
  (let* ((phx-group (ode:joint-group-create 0)))
    (make-instance 'joint-group :phx phx-group)))

(defmethod add-joint-to-group ((joint joint) (group joint-group))
  "Tracks joints being added to a group."
  (when (find joint (joint-group-joints group))
    (return-from add-joint-to-group nil))
  (push joint (joint-group-joints group)))

(defmethod joint-group-empty ((group joint-group))
  "Empty the joint group. This destroys all contained joints (in c land) so
  there's no need to do this manually. Once emptied, the joint group lives
  on."
  (ode:joint-group-empty (phx-obj group))
  (setf (joint-group-joints group) nil))

(defmethod joint-group-destroy ((joint-group joint-group))
  "Destroy a joint group. This destroys all contained joints (in c land) so
  there's no need to destro them directly."
  (when (phx-destroy joint-group)
    (ode:joint-group-destroy (phx-obj joint-group))))

