(in-package :ghostie-demo)

(defobject platform (dynamic-object)
  ((speed :accessor platform-speed :initform nil)
   (last-process :accessor platform-last-process :initform nil)))

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
      (if (<= (cadr (getf (object-level-meta platform) :limit-x))
              (car (game-object-position platform)))
          (setf (platform-speed platform) nil)
          (symbol-macrolet ((pos (cp-a:body-p-x (cpw:base-c (game-object-physics-body platform)))))
            (setf pos (+ pos speed)))))))

(bind (:collision-begin :moving-platform-begin) ((actor actor) (platform platform) arbiter)
  (declare (ignore actor))
  (cond ((< .98 (cadar (cpw:arbiter-normals arbiter)))
         (setf (cpw:arbiter-ignore-collision arbiter) t))
        ((< (cadar (cpw:arbiter-normals arbiter)) -.98)
         ;; move the platform
         (setf (platform-speed platform) 50d0)))
  (dbg :debug "(platform) Player hit platform ~s~%" (cpw:arbiter-normals arbiter)))

(bind (:collision-separate :moving-platform-separate) ((actor actor) (platform platform) arbiter)
  (declare (ignore actor arbiter))
  (setf (platform-speed platform) nil))
