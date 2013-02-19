(in-package :ghostie-util)

(defparameter *queue-game-to-render* nil)
(defparameter *queue-render-to-game* nil)

(defun init-message-queue ()
  (setf *queue-game-to-render* (make-instance 'jpl-queues:synchronized-queue
                                              :queue (make-instance 'jpl-queues:unbounded-fifo-queue))
        *queue-render-to-game* (make-instance 'jpl-queues:synchronized-queue
                                              :queue (make-instance 'jpl-queues:unbounded-fifo-queue))))

(defun enqueue (fn queue)
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

(defmacro run-in-queue (queue world-var &body body)
  "Macro that wraps running code in a specific queue."
  (let ((has-var (car world-var))
        (fake-var (gensym "world")))
    `(enqueue
       (lambda (,(if has-var has-var fake-var))
         ,(unless has-var `(declare (ignore ,fake-var)))
         ,@body)
       ,queue)))

(defmacro in-game (world-var &body body)
  "Run the given body in the game queue, binding the world object to the given
   binding variable."
  `(run-in-queue :game ,world-var ,@body))

(defmacro in-render (world-var &body body)
  "Run the given body in the render queue, binding the world object to the given
   binding variable."
  `(run-in-queue :render ,world-var ,@body))

