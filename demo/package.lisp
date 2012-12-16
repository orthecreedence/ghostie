(defpackage :ghostie-demo
  (:use :cl :ghostie-event :ghostie)
  (:import-from #:ghostie #:key=)
  (:export #:start))
(in-package :ghostie-demo)

;; make sure ghostie knows where to load our assets LOL
(setf ghostie:*game-directory* (asdf:system-relative-pathname :ghostie #P"demo/"))
