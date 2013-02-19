(defpackage :ghostie-demo
  (:use :cl :ghostie-event :ghostie)
  (:import-from #:ghostie #:key=)
  (:import-from #:ghostie #:world-physics)
  (:import-from #:ghostie #:world-level)
  (:import-from #:ghostie #:level-objects)
  (:import-from #:ghostie #:game-object-gl-objects)
  (:import-from #:ghostie #:make-gl-object-from-fake)
  (:import-from #:ghostie #:sync-game-object-to-physics)
  (:import-from #:ghostie #:game-object-rotation)
  (:import-from #:ghostie #:dbg)
  (:import-from #:ghostie #:make-fake-gl-object)
  (:import-from #:ghostie #:hex-to-rgb)
  (:import-from #:ghostie #:make-game-object)

  (:import-from #:ghostie #:game-object-position)
  (:import-from #:ghostie #:game-object-physics-body)
  (:import-from #:ghostie #:game-object-render-ref)


  (:export #:start))
(in-package :ghostie-demo)

;; make sure ghostie knows where to load our assets LOL
(setf ghostie:*game-directory* (asdf:system-relative-pathname :ghostie #P"demo/"))
