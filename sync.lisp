(in-package :ghostie)

(defparameter *queue-game-to-render* nil)
(defparameter *queue-render-to-game* nil)

(defun init-message-queue ()
  (setf *queue-game-to-render* (make-instance 'jpl-queues:synchronized-queue
                                              :queue (make-instance 'jpl-queues:unbounded-fifo-queue))
        *queue-render-to-game* (make-instance 'jpl-queues:synchronized-queue
                                              :queue (make-instance 'jpl-queues:unbounded-fifo-queue))))

(defun enqueue (fn &optional (queue :render))
  (assert (find queue '(:game :render)))
  (jpl-queues:enqueue fn (case queue
                           (:game *queue-render-to-game*)
                           (:render *queue-game-to-render*))))

(defun process-queue (data queue)
  (let ((queue (case queue
                 (:game *queue-render-to-game*)
                 (:render *queue-game-to-render*))))
    (loop until (jpl-queues:empty? queue) do
      (let ((fn (jpl-queues:dequeue queue)))
        (funcall fn data)))))

