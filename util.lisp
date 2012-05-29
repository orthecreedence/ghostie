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

(defun file-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
      returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun load-points-from-ai (filename &key precision)
  (let ((points nil)
        (in-point-block nil)
        (file-data (file-string filename))
        (mult (if precision (expt 10 precision) 1)))
    (loop for line in (split-sequence:split-sequence #\return file-data) do
          (cond ((equal line "1 XR")
                 (setf in-point-block t))
                ((and (equal (string-downcase line) "n") (not (zerop (length points))))
                 (return))
                (in-point-block
                  (let* ((verts (read-from-string (concatenate 'string "(" line ")")))
                         (verts (if precision 
                                    (mapcar (lambda (v) (if (numberp v)
                                                            (/ (floor (* v mult)) mult)
                                                            nil)) verts)
                                    verts)))
                    (push (list (car verts) (cadr verts)) points)))))
    (let* ((last (car points))
           (points (reverse points)))
      (if (equal (car points) last)
          (butlast points)
          points))))

(defun object-leftmost-point (points)
  (reduce (lambda (a b) (min (if (listp a) (car a) a)
                             (if (listp b) (car b) b))) points))

