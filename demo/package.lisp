(defpackage :ghostie-demo
  (:use :cl :ghostie-event :ghostie)
  (:import-from #:ghostie #:key=)
  (:import-from #:ghostie #:world-physics)
  (:import-from #:ghostie #:world-level)
  (:import-from #:ghostie #:level-objects)
  (:import-from #:ghostie #:object-gl-objects)
  (:import-from #:ghostie #:make-gl-object-from-fake)
  (:import-from #:ghostie #:sync-base-object-to-physics)
  (:import-from #:ghostie #:object-rotation)
  (:import-from #:ghostie #:dbg)
  (:import-from #:ghostie #:make-fake-gl-object)
  (:import-from #:ghostie #:hex-to-rgb)
  (:import-from #:ghostie #:make-base-object)

  (:import-from #:ghostie #:object-position)
  (:import-from #:ghostie #:object-physics-body)
  (:import-from #:ghostie #:object-render-ref)


  (:export #:start))
(in-package :ghostie-demo)

;; make sure ghostie knows where to load our assets LOL
(setf ghostie:*game-directory* (asdf:system-relative-pathname :ghostie #P"demo/"))
