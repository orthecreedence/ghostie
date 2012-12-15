(asdf:defsystem ghostie
  :author "Andrew Lyon <andrew@lyonbros.com>"
  :licence "MIT"
  :version "0.1.1"
  :depends-on (#:cl-glfw #:cl-opengl #:cl-glu #:bordeaux-threads #:jpl-queues #:split-sequence #:cl-svg-polygon #:glu-tessellate #:clipmunk #:chipmunk-wrapper)
  :components
  ((:file "package")
   (:file "config" :depends-on ("package"))
   (:module lib
    :depends-on ("config")
	:serial t
	:components
	((:file "util")
	 (:file "sync")
	 (:file "matrix")))
   (:module opengl
	:depends-on (lib)
    :serial t
	:components
	((:file "shaders")
	 (:file "fbo")
	 (:file "object")))
   (:file "input" :depends-on (lib))
   (:file "window" :depends-on (opengl))
   (:module classes
	:depends-on (lib)
    :serial t
	:components
	((:file "game-object")
	 (:file "actor")
	 (:file "level")))
   (:file "world" :depends-on (lib opengl classes))
   (:file "physics" :depends-on (lib classes))
   (:file "main" :depends-on ("world"))))

