(defparameter *pi* 3.14159265359)
(defparameter *half-pi* (/ *pi* 2))

(defun round-coord (coord)
  (cons (round (car coord)) (round (cdr coord))))

(defun square (origin unit angle factor)
  (defun square-aux (lower-left unit angle factor increment)
  (if (< unit 100.0) 
    nil
    (let* ((perp (+ angle *half-pi*))
           (next-angle (let ((candidate (+ angle increment)))
                         (if (< candidate *pi*) candidate (- candidate *pi*))))

           (lower-right (cons (+ (car lower-left) (* unit (cos angle))) 
                              (+ (cdr lower-left) (* unit (sin angle)))))
           (upper-right (cons (+ (car lower-right) (* unit (cos perp)))
                              (+ (cdr lower-right) (* unit (sin perp)))))
           (upper-left (cons (+ (car lower-left) (* unit (cos perp)))
                             (+ (cdr lower-left) (* unit (sin perp)))))
           (next-origin (cons (+ (car lower-left) (* (* unit factor) (cos angle)))
                              (+ (cdr lower-left) (* (* unit factor) (sin angle))))))
      (append (list
                'moveto (round-coord lower-left)
                'lineto (round-coord lower-right)
                'lineto (round-coord upper-right)
                'lineto (round-coord upper-left)
                'lineto (round-coord lower-left))
              (square-aux next-origin (* unit (- 1.0 factor)) next-angle factor increment)))))
  (square-aux origin unit angle factor (atan factor)))

(defun render-instructions (instructions)
  (cond ((null instructions) nil)
         ((equal 'moveto (car instructions))
          (progn
            (format t "ctx.stroke();~%ctx.moveTo(~A, ~A);~%"
                    (car (cadr instructions))
                    (cdr (cadr instructions)))
            (render-instructions (cddr instructions))))
         ((equal 'lineto (car instructions))
          (progn
            (format t "ctx.lineTo(~A, ~A);~%"
                    (car (cadr instructions))
                    (cdr (cadr instructions)))
            (render-instructions (cddr instructions))))))

(defun render (width height instructions)
  (progn
    (format t "<!DOCTYPE html>~%")
    (format t "<html>~%")
    (format t "<canvas id=\"c\" width=\"~A\" height=\"~A\"</canvas>~%" width height)
    (format t "<script>~%")
    (format t "const ctx = c.getContext(\"2d\");~%")
    (format t "ctx.beginPath();~%")
    (render-instructions instructions)
    (format t "ctx.stroke();~%")
    (format t "</script>~%")
    (format t "</body>~%")
    (format t "</html>~%")))

(render 1000 1000 (square (cons 0 0) 1000.0 0.0 0.010))
(sb-ext:quit)

