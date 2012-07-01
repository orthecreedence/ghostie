(in-package :game-level)

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

(defun hex-to-rgb (hex-str)
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
      color)
    (error () #(0 0 0 1))))

(defmacro def-c-callback (name &rest args)
  (let ((cffi-name #+(or win32 windows) (list name :convention :stdcall)
                   #-(or win32 windows) name))
    `(cffi:defcallback ,cffi-name ,@args)))
