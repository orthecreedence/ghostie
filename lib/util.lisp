(defpackage :ghostie-util
  (:use :cl :ghostie-config)
  (:export #:dbg
           #:flatten-image-data
           #:file-contents
           #:load-points-from-ai
           #:load-triangles-from-ply
           #:hex-to-rgb
           #:read-file
           #:def-c-callback
           #:bit-or
           #:svg-from-polygons

           #:*queue-game-to-render*
           #:*queue-render-to-game*
           #:init-message-queue
           #:enqueue
           #:process-queue
           
           #:id-matrix
           #:mat*
           #:m-perspective
           #:m-ortho
           #:m-rotate
           #:m-scale
           #:m-translate))
(in-package :ghostie-util)

(defun dbg (loglevel &rest format-args)
  (when (<= (getf +log-levels+ loglevel) (getf +log-levels+ *log-level*))
    (apply #'format (append (list t) format-args))))

(defun flatten-image-data (data)
  (let* ((ax (array-dimension data 0))
         (ay (array-dimension data 1))
         (az (array-dimension data 2))
         (vec (make-array (* ax ay az) :element-type '(unsigned-byte 8)))
         (counter 0))
    (dotimes (z az)
      (dotimes (y ay)
        (dotimes (x ax)
          (setf (aref vec counter) (aref data x y z))
          (incf counter))))
    vec))

(defun file-contents (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
      returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun load-points-from-ai (filename &key precision center (scale '(1 1)))
  (let ((points nil)
        (in-point-block nil)
        (file-data (file-contents filename))
        (mult (if precision (expt 10 precision) 1)))
    (loop for line in (split-sequence:split-sequence #\return file-data) do
          (cond ((equal line "1 XR")
                 (setf in-point-block t))
                ((and (equal (string-downcase line) "n") (not (zerop (length points))))
                 (return))
                (in-point-block
                  (let* ((scale-x (car scale))
                         (scale-y (cadr scale))
                         (verts (read-from-string (concatenate 'string "(" line ")")))
                         (verts (list (* scale-x (car verts))
                                      (* scale-y (cadr verts))))
                         (verts (if precision 
                                    (mapcar (lambda (v) (/ (floor (* v mult)) mult)) verts)
                                    verts)))
                    (push (list (car verts)
                                (cadr verts)) points)))))
    (let* ((last (car points))
           (points (reverse points)))
      (let ((points (if (equal (car points) last) (butlast points) points)))
        (if center
            (let* ((x-vals (loop for (x nil) in points collect x))
                   (y-vals (loop for (nil y) in points collect y))
                   (x-offset (/ (+ (apply #'min x-vals) (apply #'max x-vals)) -2))
                   (y-offset (/ (+ (apply #'min y-vals) (apply #'max y-vals)) -2)))
              (values (mapcar (lambda (pt) (list (+ (car pt) x-offset)
                                                 (+ (cadr pt) y-offset))) points)
                      (list x-offset y-offset)))
            (values points '(0 0)))))))

(defun load-triangles-from-ply (filename)
  (let ((in-vertex-block nil)
        (in-index-block nil)
        (verts nil)
        (num-verts nil)
        (triangles nil)
        (vert-c 0)
        (file-data (file-contents filename)))
    (loop for line in (split-sequence:split-sequence #\newline file-data) do
          (cond ((equal line "end_header")
                 (setf in-vertex-block t))
                ((search "element vertex" line)
                 (setf num-verts (read-from-string (subseq line 14)))
                 (setf verts (make-array num-verts :initial-element 0)))
                (in-index-block
                 (unless (equal line "")
                   (let ((index (read-from-string (format nil "(~a)" line))))
                     (if (= (car index) 4)
                         ;; quads, break into triangles
                         (progn (push (list (aref verts (nth 1 index))
                                            (aref verts (nth 2 index))
                                            (aref verts (nth 3 index))) triangles)
                                (push (list (aref verts (nth 3 index))
                                            (aref verts (nth 4 index))
                                            (aref verts (nth 1 index))) triangles))
                         ;; straight triangles. sick brahh
                         (push (list (aref verts (nth 1 index))
                                     (aref verts (nth 2 index))
                                     (aref verts (nth 3 index))) triangles)))))
                (in-vertex-block
                 (setf (aref verts vert-c) (subseq (read-from-string (format nil "(~a)" line)) 0 3))
                 (incf vert-c)
                 (when (= vert-c num-verts)
                   (setf in-vertex-block nil)
                   (setf in-index-block t)))))
    triangles))

(defun hex-to-rgb (hex-str &key (opacity 1.0) type)
  "Turn #a4892c into #(.64 .54 .17 1)"
  (handler-case
    (let ((tmp-str (make-string 2))
          (color (make-array 4 :element-type 'single-float :initial-element 1.0))
          (offset (if (eq (aref hex-str 0) #\#) 1 0)))
      (dotimes (i 3)
        (setf (aref tmp-str 0) (aref hex-str (+ offset (* i 2)))
              (aref tmp-str 1) (aref hex-str (+ offset (* i 2) 1)))
        (setf (aref color i)
              (coerce (/ (parse-integer tmp-str :radix 16) 255) 'single-float)))
      (setf (aref color 3) (coerce (if (stringp opacity)
                                       (read-from-string opacity)
                                       opacity) 'single-float))
      (if type
          (coerce color type)
          color))
    (error (e) (dbg :debug "Hex string -> color error: ~a~%" e) #(0 0 0 1))))

(defun read-file (filename)
  "Read a lisp file and parse it into a lisp datastructure."
  (when (probe-file filename)
    (read-from-string (file-contents filename))))


(defmacro def-c-callback (name &rest args)
  (let ((cffi-name #+(or win32 windows) (list name :convention :stdcall)
                   #-(or win32 windows) name))
    `(cffi:defcallback ,cffi-name ,@args)))

(defmacro bit-or (&rest vals)
  "Perform a logical OR on a set of values. Could probably be a function. Don't
  care, really..."
  (let ((num (length vals)))
    `(cond ((= ,num 1) ,(nth 0 vals))
           ((= ,num 2) (boole boole-ior ,(nth 0 vals) ,(nth 1 vals)))
           ((> ,num 2) (boole boole-ior ,(nth 0 vals) (bit-or ,@(cdr vals)))))))

(defun svg-from-polygons (polygons &key random-colors flip-y displace cols)
  "Given a list of polygons (a polygon is a vector of list point pairs), create
  an SVG string that displays all of the polygons in different colors for easy
  visualization. Great for debugging with *last-triangulation-run-log*."
  (let ((colors '("red" "green" "blue" "orange" "yellow" "red" "pink" "navy"))
        (c 0)
        (displace-cols (if cols cols (round (sqrt (length polygons))))))
    (with-output-to-string (s)
      (format s "<?xml version=\"1.0\" standalone=\"no\"?>
              <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
              <svg with=\"744\" height=\"1052\">~%")
      (dolist (p polygons)
        (let ((color (if random-colors (nth (mod c (length colors)) colors) "#677821"))
              (displace-x (if displace (* displace (mod c displace-cols)) 0))
              (displace-y (if displace (* displace (floor (/ c displace-cols))) 0)))
          (when displace
            (let* ((pt (aref p 0))
                   (x (coerce (car pt) 'single-float))
                   (y (coerce (if flip-y (- (cadr pt)) (cadr pt)) 'single-float))
                   (dx (+ (- displace-x 10) x))
                   (dy (+ (- displace-y 10) y)))
              (format s "<text x=\"~a\" y=\"~a\" font-size=\"12\">~a~a (~a, ~a)</text>~%" dx dy "P" c x y)))
          (incf c)
          (format s "<polygon fill=\"~a\" style=\"opacity: ~a;\" points=\"" color (if displace 1 .3))
          (loop for i from 0 for (x y) across p do
            (let ((x (coerce x 'single-float))
                  (y (coerce y 'single-float)))
              (when (zerop (mod i 3)) (format s "~%      "))
              (format s "~a,~a " (+ x displace-x) (+ (if flip-y (- y) y) displace-y))))
          (format s "\" />~%")
          ;(loop for i from 0 for (x y) across p do
          ;  (format s "<rect x=\"~a\" y=\"~a\" width=\".05\" height=\".05\" fill=\"#000000\" />~%" (+ x displace-x) (+ (if flip-y (- y) y) displace-y)))
          ))
      (format s "</svg>"))))

;(let ((files '("ground.ai" "ground-background.ai" "tree1.ai" "tree2.ai" "tree3.ai" "tree4.ai"))
;      (polygons nil))
;  (dolist (file files)
;    (push (coerce (load-points-from-ai (format nil "resources/levels/trees/~a" file) :precision 2) 'vector)
;          polygons))
;  (with-open-file (f "resources/levels/trees/trees.svg" :direction :output :if-exists :supersede)
;    (format f "~a" (svg-from-polygons polygons :random-colors nil :flip-y t))))
