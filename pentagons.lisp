(defparameter p (/ (* 2 pi) 5))
(defparameter o (/ 350 2))
(defparameter r (/ 350 2))

;
;
; /* SQUARES: This program draws 50 squares inside each other */
;   main ()
;   { float xA, yA, xB, yB, xC, yC, xD, yD,
;      xxA, yyA, xxB, yyB, xxC, yyC,xxD, yyD, p ,q ;
;      int i;
;      p=0.95; q=1.0-p;
;      xA=2.0; xB=8.0; xC=8.0; xD=2.0;
;      yA=0.5; yB=0.5; yC=6.5; yD=6.5;
;      intgr();
;      for (i=0; i<50; i++)
;      { move(xA, yA);
;         draw(xB, yB); draw(xC, yC); draw(xD, yD); draw(xA, yA);
;         xxA=p*xA+q*xB; yyA=p*yA+q*yB; xxB=p*xB+q*xC; yyB=p*yB+q*yC;
;         xxC=p*xC+q*xD; yyC=p*yC+q*yD; xxD=p*xD+q*xA; yyD=p*yD+q*yA;
;         xA=xxA; xB=xxB; xC=xxC; xD=xxD;
;         yA=yyA; yB=yyB; yC=yyC; yD=yyD;
;      }
;      endgr();
;   }


(defun x (coord)
  (car coord))

(defun y (coord)
  (cdr coord))

(defun coord (x y)
  (cons x y))

(defun round-coord (coord)
  (cons (round (car coord)) (round (cdr coord))))

(defun color (rgb)
  (format t "ctx.fillStyle=\"rgb(~A%,~A%,~A%)\";~%" (car rgb) (cadr rgb) (caddr rgb)))

(defun pentagons (a b c d e p limit)
  (let ((q (- 1 p)))

    (defun pentagon (a b c d e counter)
      (if (>= counter limit)
        nil
        (let ((next-a (coord (+ (* p (x a)) (* q (x b))) (+ (* p (y a)) (* q (y b)))))
              (next-b (coord (+ (* p (x b)) (* q (x c))) (+ (* p (y b)) (* q (y c)))))
              (next-c (coord (+ (* p (x c)) (* q (x d))) (+ (* p (y c)) (* q (y d)))))
              (next-d (coord (+ (* p (x d)) (* q (x e))) (+ (* p (y d)) (* q (y e)))))
              (next-e (coord (+ (* p (x e)) (* q (x a))) (+ (* p (y e)) (* q (y a))))))
          (append (list
                    'begin-path
                    'moveto (round-coord a)
                    'lineto (round-coord b)
                    'lineto (round-coord c)
                    'lineto (round-coord d)
                    'lineto (round-coord e)
                    'lineto (round-coord a)
                    ; 'fillStyle (list 00 00 (- 50 (* (truncate counter 1.0) 1.0)))
                    'stroke
                    )
                  (pentagon next-a next-b next-c next-d next-e (1+ counter))))))
    (pentagon a b c d e 0)))

(defun render-instructions (instructions)
  (cond ((null instructions) nil)
        ((equal 'stroke (car instructions))
         (progn
           (format t "ctx.stroke();~%")
           (render-instructions (cdr instructions))))
        ((equal 'fill (car instructions))
         (progn
           (format t "ctx.fill();~%")
           (render-instructions (cdr instructions))))
        ((equal 'begin-path (car instructions))
         (progn
           (format t "ctx.beginPath();~%")
           (render-instructions (cdr instructions))))
        ((equal 'end-path (car instructions))
         (progn
           (format t "ctx.endPath();~%")
           (render-instructions (cdr instructions))))
         ((equal 'moveto (car instructions))
          (progn
            (format t "ctx.moveTo(~A, ~A);~%"
                    (car (cadr instructions))
                    (cdr (cadr instructions)))
            (render-instructions (cddr instructions))))
         ((equal 'lineto (car instructions))
          (progn
            (format t "ctx.lineTo(~A, ~A);~%"
                    (car (cadr instructions))
                    (cdr (cadr instructions)))
            (render-instructions (cddr instructions))))
         ((equal 'fillStyle (car instructions))
          (progn
            (color (cadr instructions))
            (render-instructions (cddr instructions))))))

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


(render 1400 700 
        (append (pentagons 
                  (coord (+ o (* r (cos (* 0 p)))) (+ o (* r (sin (* 0 p)))))
                  (coord (+ o (* r (cos (* 1 p)))) (+ o (* r (sin (* 1 p)))))
                  (coord (+ o (* r (cos (* 2 p)))) (+ o (* r (sin (* 2 p)))))
                  (coord (+ o (* r (cos (* 3 p)))) (+ o (* r (sin (* 3 p)))))
                  (coord (+ o (* r (cos (* 4 p)))) (+ o (* r (sin (* 4 p)))))
                  0.95 100)
                ))
(sb-ext:quit)

