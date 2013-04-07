(in-package :ghostie-demo)

(defobject platform (dynamic-object)
  ((speed :accessor platform-speed :initform 0d0)
   (last-process :accessor platform-last-process :initform nil)))

(bind (:collision-pre :moving-platform-begin) ((actor actor) (platform platform) arbiter)
  (declare (ignore actor))
  (cond ((< .5 (cadar (cpw:arbiter-normals arbiter)))
         ;; ignore if coming up from bottom
         (setf (cpw:arbiter-ignore-collision arbiter) t))
        ((and (< (cadar (cpw:arbiter-normals arbiter)) -.98)
              (within-limit platform))
         ;; move the platform
         (set-velocity platform 50))))

(bind (:collision-separate :moving-platform-separate) ((actor actor) (platform platform) arbiter)
  (declare (ignore actor arbiter))
  (set-velocity platform 0))

(defun set-velocity (platform vel)
  "Sets the velocity for the platform."
  (let ((vel (coerce (or vel 0) 'double-float))
        (body-c (cpw:base-c (object-physics-body platform))))
    (setf (platform-speed platform) vel)
    (if (eq (getf (object-level-meta platform) :direction) :vertical)
        (setf (cp-a:body-v-y body-c) vel)
        (setf (cp-a:body-v-x body-c) vel))))

(defun inc-position (platform val)
  "Increment the platform's position."
  (let ((val (coerce val 'double-float))
        (body-c (cpw:base-c (object-physics-body platform))))
    (if (eq (getf (object-level-meta platform) :direction) :vertical)
        (setf (cp-a:body-p-y body-c) (+ (cp-a:body-p-y body-c) val))
        (setf (cp-a:body-p-x body-c) (+ (cp-a:body-p-x body-c) val)))))

(defun within-limit (platform)
  "Check if the platform is within its specified limits."
  (let* ((limits (getf (object-level-meta platform) :limit))
         (speed (platform-speed platform))
         (limit-low (- (car limits) 0))
         (limit-high (- (cadr limits) 0)))
    (or (not limits)
        (case (getf (object-level-meta platform) :direction)
          (:vertical
            (< (cadr (object-position platform))
               limit-high))
          (t
            (< (car (object-position platform))
               limit-high))))))

(defmethod process-object ((platform platform))
  (let* ((speed (platform-speed platform))
         (time (get-internal-real-time))
         (last-process (platform-last-process platform))
         (speed (when (and speed
                           last-process
                           (not (= last-process time)))
                  (* (/ (- time last-process) internal-time-units-per-second)
                     speed))))
    (unless (= time (or (platform-last-process platform) 0))
      (setf (platform-last-process platform) time))
    (when speed
      (let ((body-c (cpw:base-c (object-physics-body platform))))
        (if (not (within-limit platform))
            (progn
              (setf (platform-speed platform) nil)
              (set-velocity platform 0))
            (inc-position platform speed))))))

