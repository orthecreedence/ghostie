(defpackage :ghostie-demo
  (:use :cl :ghostie-event :ghostie)
  (:import-from #:ghostie #:key=)
  (:import-from #:ghostie #:world-physics)
  (:import-from #:ghostie #:world-level)
  (:import-from #:ghostie #:level-objects)
  (:import-from #:ghostie #:dbg)

  (:import-from #:ghostie #:object-position)
  (:import-from #:ghostie #:object-physics-body)


  (:export #:start))
(in-package :ghostie-demo)

;; make sure ghostie knows where to load our assets LOL
(setf ghostie:*game-directory* (asdf:system-relative-pathname :ghostie #P"demo/"))
