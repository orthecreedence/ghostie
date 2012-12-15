(asdf:defsystem ghostie
  :author "Andrew Lyon <andrew@lyonbros.com>"
  :licence "MIT"
  :version "0.1.1"
  :depends-on (#:cl-glfw #:cl-opengl #:cl-glu #:bordeaux-threads #:jpl-queues #:split-sequence #:cl-svg-polygon #:glu-tessellate #:clipmunk #:chipmunk-wrapper)
  :components
  ((:file "package")
   (:file "config" :depends-on ("package"))
   (:file "util" :depends-on ("config"))
   (:file "sync" :depends-on ("util"))
   (:file "matrix" :depends-on ("util"))
   (:module opengl
	:depends-on ("util")
    :serial t
	:components
	((:file "shaders")
	 (:file "fbo")
	 (:file "object")))
   (:file "input" :depends-on ("util"))
   (:file "window" :depends-on (opengl))
   (:module game-classes
	:depends-on ("util")
    :serial t
	:components
	((:file "game-object")
	 (:file "actor")
	 (:file "level")))
   (:file "world" :depends-on ("util" opengl game-classes))
   (:file "physics" :depends-on ("util" game-classes))
   (:file "main" :depends-on ("world"))))

