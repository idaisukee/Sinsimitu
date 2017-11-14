;;; from http://blog.livedoor.jp/tek_nishi/archives/8805061.html

(require 'dash)
(require 's)

(defun Ssm-hsv (r g b)
    "RGB[0.0, 1.0]からHSV[0.0, 1.0]へ変換する (SOURCE:Wikipedia)"
    (let* ((max (max r g b))
              (min (min r g b))
              (h (- max min))
              (s (- max min))
              (v max))
        (if (> h 0.0)
            (cond ((= max r)
                      (progn
                          (setq h (/ (- g b) h))
                          (if (< h 0.0)
                              (setq h (+ 6.0)))))
                ((= max g)
                    (setq h (+ 2.0 (/ (- b r) h))))
                (t
                    (setq h (+ 4.0 (/ (- r g) h))))))
        (setq h (/ h 6.0))
        (if (/= max 0)
            (setq s (/ s max)))
        (list h s v)))


(defun Ssm-rgb (h s v)
    "HSV[0.0, 1.0]からRGB[0.0, 1.0]へ変換する (SOURCE:Wikipedia)"
    (let ((r v)
             (g v)
             (b v))
        (if (> s 0)
            (progn 
                (setq h (* h 6))
                (let* ((i (truncate h))
                          (f (- h i)))
                    (case i
                        (0 (progn
                               (setq g (* g (- 1 (* s (- 1 f)))))
                               (setq b (* b (- 1 s)))))
                        (1 (progn
                               (setq r (* r (- 1 (* s f))))
                               (setq b (* b (- 1 s)))))
                        (2 (progn
                               (setq r (* r (- 1 s)))
                               (setq b (* b (- 1 (* s (- 1 f)))))))
                        (3 (progn
                               (setq r (* r (- 1 s)))
                               (setq g (* g (- 1 (* s f))))))
                        (4 (progn
                               (setq r (* r (- 1 (* s (- 1 f)))))
                               (setq g (* g (- 1 s)))))
                        (5 (progn
                               (setq g (* g (- 1 s)))
                               (setq b (* b (- 1 (* s f))))))))))
        (list r g b)))

(defun Ssm-rgb-hex (h s v)
    (let*
        (
            (rgb (Ssm-rgb h s v)))
        (->> rgb
            (--map (* 256 it))
            (-map 'floor)
            (--map (format "%x" it))
            (s-join "")
            (s-concat "#"))))

(provide 'Sinsimitu)
