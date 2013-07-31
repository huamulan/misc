;;; Introductory comments are preceded by ";;;"
;;; Function headers are preceded by ";;"
;;; Inline comments are introduced by ";"
;;;

;;
;; Triple the value of a number
;;
(defun triple (X)
  "Compute three times X."  ; Inline comments can
  (* 3 X))                  ; be placed here.

;;
;; Negate the sign of a number
;;
(defun negate (X)
  "Negate the value of X."  ; This is a documentation string.
  (- X))

;;
;; Add function
;;
(defun add(X Y)
  "add value X and Y." ; Inline comments
  (+ X Y))

;;
;; Minus function
;;
(defun minus(X Y)
  "minus value X and Y." ; Inline comments
  (- X Y))

;;
;; sum
;;
(defun sum(n)
  (let ((s 0))
    (dotimes (i n s)
      (incf s i))))

;; Lisp
(defun addn(n)
  #'(lambda (x)
    (+ x n)))

;; hello-world
(defun hello-world()
  (format t "Hello World"))

(defun mylisttest()
  (if (listp '(a b c))
    (+ 1 2)
    (+ 5 6)))

(defun mylisttest2()
  (if (listp 27)
    (+ 1 2)
    (+ 5 6)))

(defun our-third (x)
  (car (cdr (cdr x))))

(defun our-member (obj lst)
  (if (null lst)
    nil
  (if(eql (car lst) obj)
    lst
    (our-member obj (cdr lst)))))

(defun askem (string)
  (format t "~A" string)
  (format t "~%")
  (read))

(defun ask-number()
  (format t "Please enter a number.")
  (let ((val (read)))
    (if (numberp val)
      val
      (ask-number))))

(defun ch2-cn()
  (setf x (list 'a 'b 'c))
  (setf (car x) 'n)
  (format t x)
  )

(defun show-squares(start end)                                         
  (do ((j start (+ j 1)))                                              
    ((> j end) 'done)                                                
  (format t "~A ~A ~%" j (* j j))))

(defun show-squares2 (i end)
  (if (> i end)
    'done
    (progn
      (format t "~A ~A~%" i (* i i))
      (show-squares2 (+ i 1) end))))

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

(defun our-length2 (lst)
  (if (null lst)
    0
    (+ (our-length2 (cdr lst)) 1)))

(defun our-listp (x)
  (or (null x) (consp x)))

(defun our-atom (x)
  (not (consp x)))

(defun our-equal (x y)
  (or (eql x y)
    (and (consp x)
      (consp y)
      (our-equal (car x) (car y))
      (our-equal (cdr x) (cdr y)))))

(defun our-copy-list (lst)
  (if (atom lst)
    lst
    (cons (car lst) (our-copy-list (cdr lst)))))

;run-length encoding
(defun compress (x)
  (if (consp x)
    (compr (car x) 1 (cdr x))
    x))

(defun compr (elt n lst)
  (if (null lst)
    (list (n-elts elt n))
    (let ((next (car lst)))
      (if (eql next elt)
        (compr elt (+ n 1) (cdr lst))
        (cons (n-elts elt n)
          (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if(> n 1)
    (list n elt)
    elt))

;uncompress
(defun uncompress (lst)
  (if (null lst)
    nil
    (let ((elt (car lst))
      (rest (uncompress (cdr lst))))
    (if (consp elt)
      (append (apply #'list-of elt)
        rest)
      (cons elt rest)))))

(defun list-of (n elt)
  (if (zerop n)
    nil
    (cons elt (list-of (- n 1) elt))))

(defun our-nthcdr (n lst)
  (if (zerop n)
    lst
    (our-nthcdr (- n 1) (cdr lst))))

(defun our-map-func ()
  (mapcar #'(lambda (x) (+ x 10))
    '(1 2 3))
  (mapcar #'list
    '(a b c)
    '(1 2 3 4))
  (maplist #'(lambda (x) x)
    '(a b c)))

(defun our-copy-tree (tr)
  (if (atom tr)
    tr
    (cons (our-copy-tree (car tr))
          (our-copy-tree (cdr tr)))))