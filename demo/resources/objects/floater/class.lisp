(in-package :ghostie-demo)

(defobject floater (dynamic-object)
  ((speed :accessor floater-speed :initform 0d0)
   (last-process :accessor floater-last-process :initform nil)))

(bind (:collision-pre :moving-floater-begin) ((actor actor) (floater floater) arbiter)
  (declare (ignore actor))
  (cond ((< .5 (cadar (cpw:arbiter-normals arbiter)))
         ;; ignore if coming up from bottom
         (setf (cpw:arbiter-ignore-collision arbiter) t))
        ((and (< (cadar (cpw:arbiter-normals arbiter)) -.98)
              (within-limit floater))
         ;; move the floater
         (set-velocity floater 50))))

(bind (:collision-separate :moving-floater-separate) ((actor actor) (floater floater) arbiter)
  (declare (ignore actor arbiter))
  (set-velocity floater 0))

(defmethod load-physics-body ((floater floater) meta)
  "Set up some extra springs."
  (declare (ignore meta))
  (call-next-method)
  ;; TODO: set up springs!!!!
  )

(defun set-velocity (floater vel)
  "Sets the velocity for the floater."
  (let ((vel (coerce (or vel 0) 'double-float))
        (body-c (cpw:base-c (object-physics-body floater))))
    (setf (floater-speed floater) vel)
    (if (eq (getf (object-level-meta floater) :direction) :vertical)
        (setf (cp-a:body-v-y body-c) vel)
        (setf (cp-a:body-v-x body-c) vel))))

(defun inc-position (floater val)
  "Increment the floater's position."
  (let ((val (coerce val 'double-float))
        (body-c (cpw:base-c (object-physics-body floater))))
    (if (eq (getf (object-level-meta floater) :direction) :vertical)
        (setf (cp-a:body-p-y body-c) (+ (cp-a:body-p-y body-c) val))
        (setf (cp-a:body-p-x body-c) (+ (cp-a:body-p-x body-c) val)))))

(defun within-limit (floater)
  "Check if the floater is within its specified limits."
  (let* ((limits (getf (object-level-meta floater) :limit))
         (speed (floater-speed floater))
         (limit-low (- (car limits) 0))
         (limit-high (- (cadr limits) 0)))
    (or (not limits)
        (case (getf (object-level-meta floater) :direction)
          (:vertical
            (< (cadr (object-position floater))
               limit-high))
          (t
            (< (car (object-position floater))
               limit-high))))))

(defmethod process-object ((floater floater))
  (let* ((speed (floater-speed floater))
         (time (get-internal-real-time))
         (last-process (floater-last-process floater))
         (speed (when (and speed
                           last-process
                           (not (= last-process time)))
                  (* (/ (- time last-process) internal-time-units-per-second)
                     speed))))
    (unless (= time (or (floater-last-process floater) 0))
      (setf (floater-last-process floater) time))
    (when speed
      (let ((body-c (cpw:base-c (object-physics-body floater))))
        (if (not (within-limit floater))
            (progn
              (setf (floater-speed floater) nil)
              (set-velocity floater 0))
            (inc-position floater speed))))))

