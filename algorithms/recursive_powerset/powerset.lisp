; Common Lisp
; real    0m1.764s

(defun powerset (set)
  (if (null set)
    '(())
    (let ((subpowersets (powerset (cdr set))))
      (append (loop for subpowerset in subpowersets
                    collect (cons (car set) subpowerset))
              subpowersets))))

(defun range (n m)
  (loop for i from n to m collect i))


(defparameter *max* 21)
(format t "~a" (length (powerset (range 1 *max*))))