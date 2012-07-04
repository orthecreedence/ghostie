(defpackage :glu-tesselate
  (:use :cl :cffi)
  (:nicknames :tess)
  (:export :tesselate
           :next-contour :end-polygon :delete-tess
           :new-tess :tess-begin-contour :tess-begin-polygon
           :tess-callback :tess-end-contour :tess-end-polygon
           :tess-normal :tess-property :tess-vertex
           :get-tess-property :begin-polygon))
(in-package :glu-tesselate)

(defcfun ("gluNextContour" next-contour) :void (tess :pointer) (type :int))
(defcfun ("gluEndPolygon" end-polygon) :void (tess :pointer))
(defcfun ("gluDeleteTess" delete-tess) :void (tess :pointer))
(defcfun ("gluNewTess" new-tess) :pointer )
(defcfun ("gluTessBeginContour" tess-begin-contour) :void (tess :pointer))
(defcfun ("gluTessBeginPolygon" tess-begin-polygon) :void (tess :pointer) (data :pointer))
(defcfun ("gluTessCallback" tess-callback) :void (tess :pointer) (which :int) (CallBackFunc :pointer))
(defcfun ("gluTessEndContour" tess-end-contour) :void (tess :pointer))
(defcfun ("gluTessEndPolygon" tess-end-polygon) :void (tess :pointer))
(defcfun ("gluTessNormal" tess-normal) :void (tess :pointer) (valueX :double) (valueY :double) (valueZ :double))
(defcfun ("gluTessProperty" tess-property) :void (tess :pointer) (which :int) (data :double))
(defcfun ("gluTessVertex" tess-vertex) :void (tess :pointer) (location :pointer) (data :pointer))

(defcfun ("gluGetTessProperty" get-tess-property) :void (tess :pointer) (which :int) (data :pointer))
(defcfun ("gluBeginPolygon" begin-polygon) :void (tess :pointer))

(defparameter *cur-polygon* nil)
(defparameter *cur-polygon-points* nil)
(defparameter *finished-polygons* nil)

(game-level::def-c-callback tess-begin-cb :void ((prim :int))
  (setf *cur-polygon-points*
        (case prim
          (:triangles

(game-level::def-c-callback tess-vertex-cb :void ((data :pointer))
  (push ((mem-aref data :double 0) 
(game-level::def-c-callback tess-begin :void ((prim :int))

(defun tesselate (points)
  (declare (type vector points))
  (let ((polygon nil)
        (finished-polygons nil)
        (begin (lambda (
  (let* ((tess (new-tess))

