(in-package :ghostie)

(defclass compound-object ()
  ((pieces :accessor compound-object-pieces :initarg :pieces :initform nil)
   (joints :accessor compound-object-joints :initarg :joints :initform nil)
   (motors :accessor compound-object-motors :initarg :motors :initform nil)
   (meta :accessor compound-object-meta :initarg :meta :initform nil))
  (:documentation
    "A compound object is one that holds a number of dynamic objects. It is
     generally used for more complex objects that a dynamic-object class can
     describe: a bridge, a rope, a walking eye, etc.
     
     Compound objects generally hold their dynamic object pieces together with
     joints and motors."))

(defgeneric init-compound-object (compound-object)
  (:documentation
    "Create a compound object. Since most compound objects will differ
     significantly, it makes sense to use a method that will allow the app to
     instantiate it however it wants once it's created. This is that method,
     and it is called for each compound object contained by a level."))

(defmethod init-compound-object ((object compound-object))
  (declare (ignore object meta))
  (dbg :warning "(object) Hi, you are creating a compound object using the default method.~%")
  (dbg :warning "(object) You most likely want to define your own create-compound-object method for ~a.~%" (type-of object)))

(defun create-compound-object (level-meta)
  "Creates and loads a compound object given level meta describing the object."
  (let* ((object-type (getf level-meta :type))
         (object-directory (format nil "~a/~a/~a/~a"
                                  (namestring *game-directory*)
                                  *resource-path*
                                  *compound-object-path*
                                  object-type))
         (meta (read-file (format nil "~a/meta.lisp" object-directory))))
    (dbg :debug "(object) Loading compound object ~s~%" (list :type object-type))
    ;; set the object's global meta into the level meta
    (setf level-meta (append level-meta meta))

    ;; load the object's class file, if it has one
    (unless (object-loaded object-type)
      (let ((class-file (format nil "~a/class.lisp" object-directory)))
        (when (probe-file class-file)
          (load class-file))))

    ;; attempt to load the object class
    (let* ((object-symbol (intern (string-upcase object-type) :ghostie))
           (object-class (if (find-class object-symbol nil)
                             object-symbol
                             'compound-object))
           (object (make-instance object-class :meta level-meta)))
      ;; load the object's physics body
      (init-compound-object object)
      object)))

(defun load-compound-objects (level-meta)
  "Loads compound objects in a level defined by that level's meta."
  (let ((objects nil))
    (dolist (compound-meta level-meta)
      (push (create-compound-object compound-meta) objects))
    objects))

