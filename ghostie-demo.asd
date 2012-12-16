(asdf:defsystem ghostie-demo
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :description "A demo of Ghostie Engine's supreme power."
  :depends-on (#:ghostie)
  :components
  ((:module demo
    :serial t
	:components
	((:file "package")
	 (:file "input")
	 (:file "main")))))
