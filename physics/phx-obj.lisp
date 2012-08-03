(in-package :ghostie)

;; defines a very simple class for tracking whether or not a c pointer/class is
;; still active. once destroyed, it is not allowed to be destroyed again.
(defclass phx-obj ()
  ((phx :accessor phx-obj :initarg :phx)
   (phx-type :accessor phx-obj-type :initform :base)
   (active :accessor phx-obj-active :initform t)))

(defmethod phx-destroy ((obj phx-obj))
  "Doesn't really do anything but track internal state of objects. The point is
  to call this directly before a C physics object is destroyed so it can be 
  tracked whether or not it's active and possibly save exceptions in the 
  foreign library."
  (unless (phx-obj-active obj)
    ;(error "Object of type ~A has already been destroyed" (phx-obj-type obj))
    (format t " - object of type ~a (~a) has already been destroyed.~%" (phx-obj-type obj) obj)
    (return-from phx-destroy nil))
  (format t " - destroying object of type ~a (~a) : ~a~%" (phx-obj-type obj) (if (eql (phx-obj-type obj) :shape) (shape-type obj) "") obj)
  (setf (phx-obj-active obj) nil)
  t)

