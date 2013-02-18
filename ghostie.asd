(asdf:defsystem ghostie-config
  :author "Andrew Lyon <orthecreedence@gmail.com>"
  :licence "MIT"
  :version "0.2.1"
  :description "Configuration for Ghostie"
  :depends-on (#:chipmunk-wrapper)
  :components
  ((:file "config")))

(asdf:defsystem ghostie-util
  :author "Andrew Lyon <orthecreedence@gmail.com>"
  :licence "MIT"
  :version "0.2.1"
  :description "Utility package for Ghostie"
  :depends-on (#:split-sequence #:cffi #:jpl-queues #:ghostie-config)
  :components
  ((:module lib
	:serial t
	:components
	((:file "util")
	 (:file "sync")
	 (:file "matrix")))))

(asdf:defsystem ghostie-event
  :author "Andrew Lyon <orthecreedence@gmail.com>"
  :licence "MIT"
  :version "0.2.1"
  :description "Event system for Ghostie"
  :depends-on (#:ghostie-config #:ghostie-util)
  :components
  ((:file "event")))

(asdf:defsystem ghostie
  :author "Andrew Lyon <orthecreedence@gmail.com>"
  :licence "MIT"
  :version "0.2.1"
  :description "Ghostie: A platformer engine for CL"
  :depends-on (#:cl-glfw #:cl-opengl #:cl-glu #:bordeaux-threads #:split-sequence #:cl-svg-polygon #:glu-tessellate #:clipmunk #:chipmunk-wrapper #:ghostie-config #:ghostie-util #:ghostie-event)
  :components
  ((:file "package")
   (:module opengl
	:depends-on ("package")
    :serial t
	:components
	((:file "shaders")
	 (:file "fbo")
	 (:file "object")))
   (:file "input" :depends-on ("package" opengl))
   (:file "window" :depends-on ("package" opengl))
   (:file "physics" :depends-on ("package" opengl))
   (:module classes
	:depends-on ("package")
    :serial t
	:components
	((:file "game-object")
	 (:file "actor")
	 (:file "level")))
   (:file "world" :depends-on ("package" opengl classes))
   (:file "game" :depends-on ("world"))))

