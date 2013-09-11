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

(setf arr (make-array '(2 3) :initial-element nil))
(aref arr 0 0)
(setf (aref arr 0 0) 'b)
(aref arr 0 0)
;;#2a((b nil nil) (nil nil nil))

(setf vec (make-array 4 :initial-element nil))
(vector "a" 'b 3)

;; Binary Search
(defun bin-search (obj vec)
  (let ((len (length vec)))
    (and (not (zerop len))
         (finder obj vec 0 (- len 1)))))

(defun finder (obj vec start end)
  (format t "~A~%" (subseq vec start (+ end 1)))
  (let ((range (- end start)))
    (if (zerop range)
      (if (eql obj (aref vec start))
        obj
        nil)
      (let ((mid (+ start (round (/ range 2)))))
        (let ((obj2 (aref vec mid)))
          (if (< obj obj2)
            (finder obj vec start (- mid 1))
            (if (> obj obj2)
              (finder obj vec (+ mid 1) end)
              obj)))))))

(sort "elbow" #'char<)
(aref "abc" 1)
(char "abc" 1)

(let ((str (copy-seq "Merlin")))
  (setf (char str 3) #\k)
  str)

(equal "fred" "fred")
(equal "fred" "Fred")
(string-equal "fred" "Fred")

(format nil "~A or ~A" "truth" "dare")
(format t "~A or ~A" "truth" "dare")
(concatenate 'string "not " "to worry")

;; We have implement func mirror before.
(mirror? "abba")
(elt '(a b c) 1)

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
         (do ((forward 0 (+ forward 1))
              (back (- len 1) (- back 1)))
           ((or (> forward back)
                (not (eql (elt s forward)
                          (elt s back))))
            (> forward back))))))
(mirror? "abccba")

(position #\a "fantasia")
(position #\a "fantasia" :start 3 :end 5)

(position #\a "fantasia" :from-end t)

(position 'a '((c d) (a b)) :key #'car)

(position 'a '((c b) (c d) (a b)) :key #'car)

(position '(a b) '((a b) (c d)))
(position '(a b) '((a b) (c d)) :test #'equal)
(position '(a b) '(a b) :test #'eql)
(position 3 '(1 0 7 5) :test #'<)

(defun second-word (str)
  (let ((p1 (+ (position #\  str) 1)))
    (subseq str p1 (position #\  str :start p1))))
(second-word "Form follows function")

(position-if #'oddp '(2 3 4 5))
(position-if #'oddp '(2 2 3 4))

(find #\a "cat")
(find-if #'characterp "ham")

(find-if #'(lambda (x)
             (eql (car x) 'complete))
         lst)

(find 'complete lst :key #'car)
(remove-duplicates "abracadabra")
(reduce #'fn '(a b c d))
(fn (fn (fn 'a 'b) 'c) 'd)
(reduce #'intersection '((b r a d 's) (b a d) (c a t)))

;; parse tokens
(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
      (let ((p2 (position-if #'(lambda (c)
                                 (not (funcall test c)))
                             str :start p1)))
        (cons (subseq str p1 p2)
              (if p2
                (tokens str test p2)
                nil)))
      nil)))

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\ ))))

(tokens "ab12 3cde.f" #'alpha-char-p 0)
(tokens "ab12 3cde.f gh" #'constituent 0)

;; parse date
(defun parse-date (str)
  (let ((toks (tokens str #'constituent 0)))
    (list (parse-integer (first toks))
          (parse-month (second toks))
          (parse-integer (third toks)))))

(defconstant month-names
             #("jan" "feb" "mar" "apr" "may" "jun"
               "jul" "aug" "sep" "oct" "nov" "dec"))

(defun parse-month (str)
  (let ((p (position str month-names
                     :test #'string-equal)))
    (if p
      (+ p 1)
      nil)))

(parse-date "16 Aug 1980")

;; parse integer
(defun read-integer (str)
  (if (every #'digit-char-p str)
    (let ((accum 0))
      (dotimes (pos (length str))
        (setf accum (+ (* accum 10)
                       (digit-char-p (char str pos)))))
      accum)
    nil))

(read-integer "12 34 5")

;; structures
(defun block-height (b) (svref b 0))

(defstruct point
  x
  y)

(setf p (make-point :x 0 :y 0))
(point-x p)
p

(defstruct polemic
  (type (progn
          (format t "What kind of polemic was it?")
          (read)))
  (effect nil))
(make-polemic)

(defstruct (point (:conc-name p)
                  (:print-function print-point))
  (x 0)
  (y 0))
(defun print-point (p stream depth)
  (format stream "#<~A, ~A>" (px p) (py p)))

(make-point)

;; Binary Search Tree(BST)
(defstruct (node (:print-function
                   (lambda (n s d)
                     (format s "#<~A>" (node-elt n)))))
  elt (l nil) (r nil))

(defun bst-insert (obj bst <)
  (if (null bst)
    (make-node :elt obj)
    (let ((elt (node-elt bst)))
      (if (eql obj elt)
        bst
        (if (funcall < obj elt)
          (make-node
            :elt elt
            :l (bst-insert obj (node-l bst) <)
            :r (node-r bst))
          (make-node
            :elt elt
            :r (bst-insert obj (node-r bst) <)
            :l (node-l bst)))))))

(defun bst-find (obj bst <)
  (if (null bst)
    nil
    (let ((elt (node-elt bst)))
      (if (eql obj elt)
        bst
        (if (funcall < obj elt)
          (bst-find obj (node-l bst) <)
          (bst-find obj (node-r bst) <))))))

(defun bst-min (bst)
  (and bst
       (or (bst-min (node-l bst)) bst)))

(defun bst-max (bst)
  (and bst
       (or (bst-max (node-r bst)) bst)))

(setq str (copy-seq "0123456789"))
(elt str 6)

(setf (elt str 0) #\#)
str

(setf nums nil)
(dolist (x '(5 8 4 2 1 9 6 7 3))
  (setf nums (bst-insert x nums #'<)))

;; Hash Table
(setf ht (make-hash-table))
(gethash 'color ht)
(setf (gethash 'color ht) 'red)
(gethash 'color ht)
(setf (gethash 'shape ht) 'spherical
	  (gethash 'size ht) 'giant)
(maphash #' (lambda (k v)
			  (format t "~A = ~A~%" k v))
			ht)

;; Chapter 5
;; progn
(progn
  (format t "a")
  (format t "b")
  (+ 11 12))

;; block
(block head
  (format t "Here we go.")
  (return-from head 'idea)
  (format t "We'll never see this."))

(block nil
  (return 27))

(dolist (x '(a b c d e))
  (format t "~A " x)
  (if (eql x 'c)
	  (return 'done)))

(defun read-integer (str)
  (let ((accum 0))
	(dotimes (pos (length str))
	  (let ((i (digit-char-p (char str pos))))
		(if i
			(setf accum (+ (* accum 10) i))
			(return-from read-integer nil))))
	accum))

(tagbody
   (setf x 0)
   top
   (setf x (+ x 1))
   (format t "~A " x)
   (if (< x 10) (go top)))

(let ((x 7)
	  (y 2))
  (format t "Number")
  (+ x y))

((lambda (x) (+ x 1)) 3)

;;------------------
(let ((x 2)
	  (y (+ x 1)))
  (+ x y))
((lambda (x y) (+ x y)) 2
 (+ x 1))

;;------------------
(let* ((x 2)
	   (y (+ x 1)))
  (+ x y))

(let ((x 1))
  (let ((y (+ x 1)))
	(+ x y)))

(destructuring-bind (w (x y) . z) '(a (b c) d e)
  (list w x y z))

;; Conditionals
(when (oddp that)
  (format t "Hmm, that's odd.")
  (+ that 1))
;; It's equal to the following
(if (oddp that)
	(progn
	  (format t "Hmm, that's odd.")
	  (+ that 1)))

(defun our-member (obj lst)
  (if (atom lst)
	  nil
	  (if (eql (car lst) obj)
		  lst
		  (our-member obj (cdr lst)))))

(defun our-member (obj lst)
  (cond ((atom lst) nil)
		((eql (car lst) obj) lst)
		(t (our-member obj (cdr lst)))))

(cond (99))

(defun month-length (mon)
  (case mon
	((jan mar may jul aug oct dec) 31)
	((apr jun sept nov) 30)
	(feb (if(leap-year) 29 28))
	(otherwise "unknown month")))

;; Iteration
(defun show-squares (start end)
  (do ((i start (+ i 1)))
	  ((> i end) 'done)
	(format t "~A ~A~%" i (* i i))))

(let ((x 'a))
  (do ((x 1 (+ x 1))
	   (y x x))
	  ((> x 5))
	(format t "(~A ~A)  " x y)))

(do* ((x 1 (+ x 1))
	  (y x x))
	 ((> x 5))
  (format t "(~A ~A)" x y))

(dolist (x '(a b c d) 'done)
  (format t "~A " x))

(dotimes (x 5 x)
  (format t "~A " x))

(mapc #' (lambda (x y)
		   (format t "~A ~A  " x y))
		 '(hip flip slip)
		 '(hop flop slop))

(values 'a nil (+ 2 4))
((lambda () ((lambda () (values 1 2)))))

(values)
(let ((x (values)))
  x)

;;; multiple-value-bind
(multiple-value-bind (x y z) (values 1 2 3)
  (list x y z))
(multiple-value-bind (x y z) (values 1 2)
  (list x y z))
(multiple-value-bind (s m h) (get-decoded-time)
  (format t "~A:~A:~A" h m s))
(multiple-value-call #'+ (values 1 2 3))
(multiple-value-list (values 'a 'b 'c))

;;; Aborts
(defun super ()
  (catch 'abort
	(sub)
	(format t "We'll never see this.")))
(defun sub ()
  (throw 'abort 99))

(progn
  (error "Oops!")
  (format t "After the error."))

;;; Example: Date Arithmetic
(setf mon '(31 28 31 30 31 30 31 31 30 31 30 31))
(apply #'+ mon)
(setf nom (reverse mon))
(setf sums (maplist #'(lambda (x)
						(apply #'+ x))
					nom))

(defconstant month
    #(0 31 59 90 120 151 181 212 243 273 304 334 365))
(defconstant yzero 2000)
(defun leap? (y)
  (and (zerop (mod y 4))
	   (or (zerop (mod y 400))
		   (not (zerop (mod y 100))))))

;;; Chapter 10
;;; Functions
(fboundp '+)
(fboundp '-)
(symbol-function '+)
(setf (symbol-function 'add2)
	    #'(lambda (x) (+ x 2)))
(add2 1)

(defun add3 (x) (+ x 2))
(add3 1)


(defun primo (lst) (car lst))
(defun (setf primo) (val lst)
  (setf (car lst) val))
(let ((x (list 'a 'b 'c)))
  (setf (primo x) 480)
  x)

(defun foo (x)
  "Implements an enhanced paradigm of diversity"
  x)
(documentation 'foo 'function)

;; Local Functions
(labels ((add10 (x) (+ x 10))
		 (consa (x) (cons 'a x)))
  (consa (add10 3)))

(labels ((len (lst)
		   (if (null lst)
			   0
			   (+ (len (cdr lst)) 1))))
  (len '(a b c)))

;; iterator
(do ((x a (b x))
	 (y c (d y)))
	((test x y) (z x y))
  (f x y))
;; equal to
(labels ((rec (x y)
		   (cond ((test x y)
				  (z x y))
				 (t
				  (f x y)
				  (rec (b x) (d y))))))
  (rec a c))

;;; Parameter Lists
(defun our-funcall (fn &rest args)
  (apply fn args))

;;; Optional parameters
(defun philosoph (thing &optional property)
  (list thing 'is property))
(philosoph 'death)

(defun philosoph (thing &optional (property 'fun))
  (list thing 'is property))
(philosoph 'death)

;;; Keyword parameters
(defun keylist (a &key x y z)
  (list a x y z))
(keylist 1 :y 2)

(defun our-adjoin (obj lst &rest args)
  (if (apply #'member obj lst args)
	  lst
	  (cons obj lst)))

;;; Utilities
(defun single? (lst)
  (and (consp lst) (null (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun map-int (fn n)
  (let ((acc nil))
	(dotimes (i n)
	  (push (funcall fn i) acc))
	(nreverse acc)))

(defun filter (fn lst)
  (let ((acc nil))
	(dotimes (x lst)
	  (let ((val (funcall fn x)))
		(if val (push val acc))))
	(nreverse acc)))

(defun most (fn lst)
  (if (null lst)
	  (values nil nil)
	  (let* ((wins (car lst))
			 (max (funcall fn wins)))
		(dolist (obj (cdr lst))
		  (let ((score (funcall fn obj)))
			(when (> score max)
			  (setf wins obj
					max score))))
		(values wins max))))

;;; Test utilities
(single? '(a))
(append1 '(a b c) 'd)
(map-int #'identity 10)
(map-int #'(lambda (x) (random 100))
		 10)
(filter #'(lambda (x)
			(and (evenp x) (+ x 10)))
		'(1 2 3 4 5 6 7))
(most #'length '((a b) (a b c) (a)))

;;; Closures
(defun combiner (x)
  (typecase x
	(number #'+)
	(list #'append)
	(t #'list)))
(defun combine (&rest args)
  (apply (combiner (car args))
		 args))
;;; Test combine
(combine 2 3)
(combine '(a b) '(c d))

(setf fn (let ((i 3))
		   #'(lambda (x) (+ x i))))
(funcall fn 2)

(defun add-to-list (num lst)
  (mapcar #'(lambda (x)
			  (+ x num))
		  lst))
(defun make-adder (n)
  #'(lambda (x)
	  (+ x n)))
(setf add3 (make-adder 3))
(funcall add3 2)
(setf add27 (make-adder 27))
(funcall add27 2)

(let ((counter 0))
  (defun reset ()
	(setf counter 0))
  (defun stamp ()
	(setf counter (+ counter 1))))
(list (stamp) (stamp) (reset) (stamp))

;;; complement
(mapcar (complement #'oddp)
		'(1 2 3 4 5 6))
;;; our-complement
(defun our-complement (f)
  #'(lambda (&rest args)
	  (not (apply f args))))
(mapcar (our-complement #'evenp)
		'(1 2 3 4 5 6))

;;; Function Builders
;;; Dylan is the combination for Common Lisp and Scheme.
(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
	#'(lambda (&rest args)
		(reduce #'(lambda (v f) (funcall f v))
				rest
				:initial-value (apply fn1 args)))))
(defun disjoin (fn &rest fns)
  (if (null fns)
	  fn
	  (let ((disj (apply #'disjoin fns)))
		#'(lambda (&rest args)
			(or (apply fn args) (apply disj args))))))
(defun conjoin (fn &rest fns)
  (if (null fns)
	  fn
	  (let ((conj (apply #'conjoin fns)))
		#'(lambda (&rest args)
			(and (apply fn args) (apply conj args))))))
(defun curry (fn &rest args)
  #'(lambda (&rest args2)
	  (apply fn (append args args2))))
(defun rcurry (fn &rest args)
  #'(lambda (&rest args2)
	  (apply fn (append args2 args))))
(defun always (x) #'(lambda (&rest args) x))

(compose #'a #'b #'c)
#'(lambda (&rest args) (a (b (apply #'c args))))
(mapcar (compose #'list #'round #'sqrt)
				 '(4 9 16 25))

(mapcar (disjoin #'integerp #'symbolp)
		'(a "a" 2 3))
(mapcar (conjoin #'integerp #'symbolp)
		'(a "a" 2 3))
(curry #'+ 3)
(rcurry #'+ 3)
(funcall (curry #'- 3) 2)
(funcall (rcurry #'- 3) 2)

;;; Dynamic Scope
(let ((x 10))
  (defun foo ()
	x))
(let ((x 20)) (foo))

(let ((x 10))
  (defun foo ()
	(declare (special x))
	x))
(let ((x 20))
  (declare (special x))
  (foo))

(setf x 30)
(foo)

(let ((*print-base* 16))
  (print 32))

;;; Compilation
(defun foo (x) (+ x 1))
(compiled-function-p #'foo)
(compile 'foo)

(defun make-adder (x) (+ x 100))
(compile 'make-adder)
(compiled-function-p (make-adder 2))

;; 6.9 Using Recursion
(defun fib (n)
  (if (<= n 1)
	  1
	  (+ (fib (- n 1))
		 (fib (- n 2)))))
(fib 10)

(defun fib (n)
  (do ((i n (- i 1))
	   (f1 1 (+ f1 f2))
	   (f2 1 f1))
	  ((<= i 1) f1)))
(fib 10)

;;; Chapter 7. IO
(setf path (make-pathname :name "myfile"))

