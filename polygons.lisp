(defparameter r 350)
(defparameter p 0.05)
(defparameter q 0.95)

(defun coord (x y)
  (cons x y))

(defun x (coord)
  (car coord))

(defun y (coord)
  (cdr coord))

(defun round-coord (coord)
  (cons (round (x coord)) (round (y coord))))

(defparameter origin (coord r r))

(defun square (x)
  (* x x))

(defun distance (a b)
  (sqrt (+ (square (- (x a) (x b))) (square (- (y a) (y b))))))

(defun next (coords fst)
  (cond
    ((null (cdr coords))
     (list (coord (+ (* p (x (car coords))) (* q (x fst)))
                  (+ (* p (y (car coords))) (* q (y fst))))))
    (t (cons
         (coord (+ (* p (x (car coords))) (* q (x (cadr coords))))
                (+ (* p (y (car coords))) (* q (y (cadr coords)))))
         (next (cdr coords) fst)))))

(defun polygon (coords)
  (let ((edge-size (distance (car coords) (cadr coords))))
    (if (< edge-size 10.0)
      nil
      (let ((next-coords (next coords (car coords))))
        (append
          (list (list 'begin-path))
          (list (list 'move-to (round-coord (car coords))))
          (append (loop for coord in (cdr coords)
                        collect (list 'line-to (round-coord coord))))
          (list (list 'line-to (round-coord (car coords))))
          (list (list 'stroke))
          (polygon next-coords))
          ))))

(defun polygons (sides)
  (let ((vertice (loop for s from 0 to (- sides 1) collect s))
        (alpha (/ (* 2 pi) sides)))
    (polygon
      (mapcar #'(lambda (i)
                  (coord
                    (+ (x origin) (* r (cos (* i alpha))))
                    (+ (y origin) (* r (sin (* i alpha))))))
              vertice))))

(defun render-instructions (instructions)
  (cond ((null instructions)
         (format t "/* that's it */"))
        ((equal 'stroke (caar instructions))
         (progn
           (format t "ctx.stroke();~%")
           (render-instructions (cdr instructions))))
        ((equal 'fill (caar instructions))
         (progn
           (format t "ctx.fill();~%")
           (render-instructions (cdr instructions))))
        ((equal 'begin-path (caar instructions))
         (progn
           (format t "ctx.beginPath();~%")
           (render-instructions (cdr instructions))))
        ((equal 'end-path (caar instructions))
         (progn
           (format t "ctx.endPath();~%")
           (render-instructions (cdr instructions))))
         ((equal 'move-to (caar instructions))
          (progn
            (format t "ctx.moveTo(~A, ~A);~%"
                    (car (car (cdr (car instructions))))
                    (cdr (car (cdr (car instructions)))))
            (render-instructions (cdr instructions))))
         ((equal 'line-to (caar instructions))
          (progn
            (format t "ctx.lineTo(~A, ~A);~%"
                    (car (car (cdr (car instructions))))
                    (cdr (car (cdr (car instructions)))))
            (render-instructions (cdr instructions))))
         ((equal 'fill-style (caar instructions))
          (progn
            (color (cdar instructions))
            (render-instructions (cdr instructions))))))

(defun render (width height instructions)
  (progn
    (format t "<!DOCTYPE html>~%")
    (format t "<html>~%")
    (format t "<canvas id=\"canvas\" width=\"~A\" height=\"~A\"</canvas>~%" width height)
    (format t "<script>~%")
    (format t "const ctx = document.getElementById(\"canvas\").getContext(\"2d\");~%")
    (render-instructions instructions)
    (format t "</script>~%")
    (format t "</body>~%")
    (format t "</html>~%")))


(defun process ()
  (if (< (length *posix-argv*) 2)
    (error "missing argument: sides")
    (render 1400 700 (polygons (parse-integer (cadr *posix-argv*))))))

(progn
  (process)
  (sb-ext:quit))
