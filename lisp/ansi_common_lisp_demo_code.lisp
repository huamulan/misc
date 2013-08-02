;; Triple the value of a number
(defun triple (X)
  "Compute three times X."  ; Inline comments can
  (* 3 X))                  ; be placed here.

;; Negate the sign of a number
(defun negate (X)
  "Negate the value of X."  ; This is a documentation string.
  (- X))

;; Add function
(defun add(X Y)
  "add value X and Y." ; Inline comments
  (+ X Y))

;; Minus function
(defun minus(X Y)
  "minus value X and Y." ; Inline comments
  (- X Y))

;; sum
(defun sum(n)
  (let ((s 0))
    (dotimes (i n s)
      (incf s i))))

;; lambda
(defun addn(n)
  #'(lambda (x)
    (+ x n)))

;; hello-world
(defun hello-world()
  (format t "Hello World"))

;; mylisttest
(defun mylisttest()
  (if (listp '(a b c))
    (+ 1 2)
    (+ 5 6)))

;; mylisttest2
(defun mylisttest2()
  (if (listp 27)
    (+ 1 2)
    (+ 5 6)))

;; our-third
(defun our-third (x)
  (car (cdr (cdr x))))

;; our-member
(defun our-member (obj lst)
  (if (null lst)
    nil
  (if(eql (car lst) obj)
    lst
    (our-member obj (cdr lst)))))

;; askem
(defun askem (string)
  (format t "~A" string)
  (format t "~%")
  (read))

;; ask-number
(defun ask-number()
  (format t "Please enter a number.")
  (let ((val (read)))
    (if (numberp val)
      val
      (ask-number))))

;; ch2-cn
(defun ch2-cn()
  (setf x (list 'a 'b 'c))
  (setf (car x) 'n)
  (format t x)
  )

;; show-squares
(defun show-squares(start end)                                         
  (do ((j start (+ j 1)))                                              
    ((> j end) 'done)                                                
  (format t "~A ~A ~%" j (* j j))))

;; show-squares2
(defun show-squares2 (i end)
  (if (> i end)
    'done
    (progn
      (format t "~A ~A~%" i (* i i))
      (show-squares2 (+ i 1) end))))

;; our-length
(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

;; our-length2
(defun our-length2 (lst)
  (if (null lst)
    0
    (+ (our-length2 (cdr lst)) 1)))

;; our-listp
(defun our-listp (x)
  (or (null x) (consp x)))

;; our-atom
(defun our-atom (x)
  (not (consp x)))

;; our-equal
(defun our-equal (x y)
  (or (eql x y)
    (and (consp x)
      (consp y)
      (our-equal (car x) (car y))
      (our-equal (cdr x) (cdr y)))))

;; our-copy-list
(defun our-copy-list (lst)
  (if (atom lst)
    lst
    (cons (car lst) (our-copy-list (cdr lst)))))

;run-length encoding
(defun compress (x)
  (if (consp x)
    (compr (car x) 1 (cdr x))
    x))

;; compr
(defun compr (elt n lst)
  (if (null lst)
    (list (n-elts elt n))
    (let ((next (car lst)))
      (if (eql next elt)
        (compr elt (+ n 1) (cdr lst))
        (cons (n-elts elt n)
          (compr next 1 (cdr lst)))))))

;; n-elts
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

;; list-of
(defun list-of (n elt)
  (if (zerop n)
    nil
    (cons elt (list-of (- n 1) elt))))

;; our-nthcdr
(defun our-nthcdr (n lst)
  (if (zerop n)
    lst
    (our-nthcdr (- n 1) (cdr lst))))

;; our-map-func
(defun our-map-func ()
  (mapcar #'(lambda (x) (+ x 10))
    '(1 2 3))
  (mapcar #'list
    '(a b c)
    '(1 2 3 4))
  (maplist #'(lambda (x) x)
    '(a b c)))

;; our-copy-tree
(defun our-copy-tree (tr)
  (if (atom tr)
       tr
       (cons (our-copy-tree (car tr))
             (our-copy-tree (cdr tr)))))

;; our-subst
(defun our-subst (new old tree)
    (if (eql tree old)
        new
        (if (atom tree)
            tree
            (cons (our-subst new old (car tree))
                  (our-subst new old (cdr tree))))))

;; len
(defun len (lst)
    (if (null lst)
        0
	(+ (len (cdr lst)) 1)))

;; our-member-if
(defun our-member-if (fn lst)
  (and (consp lst)
       (if (funcall fn (car lst))
           lst
           (our-member-if fn (cdr lst)))))

(union '(a b c) '(c b s))
(intersection '(a b c) '(b b c))
(set-difference '(a b c d e) '(b e))

(length '(a b c))
(subseq '(a b c d) 1 3)
(subseq '(a b c d) 1)
(reverse '(a b c))

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
         (let ((mid (/ len 2)))
           (equal (subseq s 0 mid)
                  (reverse (subseq s mid)))))))

(sort '(0 2 1 3 8) #'>)
(sort '(0 2 1 3 8) #'<)

(defun nthmost (n lst)
  (nth (- n 1)
    (sort (copy-list lst) #'>)))

(every #'oddp '(1 3 5))
(some #'evenp '(1 2 3 4))

(let ((x (car lst)))
  (setf lst (cdr lst))
  x)

(setf x '(b))
(push 'a x)
x
(setf y x)
(pop x)
x

(defun our-reverse (lst)
  (let ((acc nil))
    (dolist (elt lst)
      (push elt acc))
    acc))

'(a . (b . (c . nil)))
'(a b c)

(cons 'a (cons 'b (cons 'c 'd)))
(setf trans '((+ . "add") (- . "subtract")))
(assoc '+ trans)
(assoc '* trans)
(assoc '- trans)

(defun our-assoc (key alist)
  (and (consp alist)
       (let ((pair (car alist)))
        (if (eql key (car pair))
            pair
            (our-assoc key (cdr alist))))))

(setf min '((a b c) (b c) (c d)))

;; Shortest Path
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (reverse path)
              (bfs end
                   (append (cdr queue)
                           (new-paths path node net))
                   net))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))

(eq 3 3)
