(asdf:defsystem ghostie-event
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.2.6"
  :description "Event functions for ghostie."
  :depends-on (#:jpl-queues)
  :components
  ((:file "lib/event")))
