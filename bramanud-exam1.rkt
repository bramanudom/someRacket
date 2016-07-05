#lang racket

;; Budnampet Ramanudom

;; **********************************************************************
;; This is the starter file for the CS251 Spring 2016 Take-Home Exam 1
;; **********************************************************************

;; **********************************************************************
;; These helper functions are used throughout the exam
;; **********************************************************************

(define (forall? pred xs)
  (if (null? xs)
      #t
      (and (pred (car xs))
           (forall? pred (cdr xs)))))

(define (exists? pred xs)
  (if (null? xs)
      #f
      (or (pred (car xs))
          (exists? pred (cdr xs)))))

(define (find pred not-found xs)
  (if (null? xs)
      not-found
      (if (pred (car xs))
          (car xs)
          (find pred not-found (cdr xs)))))

(define (zip xs ys)
  (if (or (null? xs) (null? ys))
      null
      (cons (cons (car xs) (car ys))
            (zip (cdr xs) (cdr ys)))))

(define (foldr-ternop ternop null-value xs)
  (if (null? xs)
      null-value
      (ternop (first xs)
              (rest xs)
              (foldr-ternop ternop null-value (rest xs)))))

(define (genlist next done? seed)
  (if (done? seed)
      null
      (cons seed (genlist next done? (next seed)))))

(define (iterate next done? finalize state)
  (if (done? state)
      (finalize state)
      (iterate next done? finalize (next state))))

(define (iterate-apply next done? finalize state)
  (if (apply done? state)
      (apply finalize state)
      (iterate-apply next done? finalize (apply next state))))

;; **********************************************************************
;; Problem 1: Conjunction Junction
;; **********************************************************************

(define (and-fun a b) (if a b #f))

(define (between x lo hi)
  (and (<= lo x) (<= x hi)))

(define (first-positive? nums)
  (and (not (null? nums))
       (> (first nums) 0)))

 (define (all-negative? nums)
   (foldr (and-fun) #t (map (λ (n) (< n 0)) nums)))

;; **********************************************************************
;; Problem 2: It's a Factor!
;; **********************************************************************

(define (least-divisor-rec num)
  (let ((limit (ceiling (sqrt num))))
    (define (search-for-divisor candidate)
      (if (> candidate limit)
          num
          (if (divisible-by? num candidate)
              candidate
              (search-for-divisor (+ candidate 2)))))
    (if (divisible-by? num 2)
        2
        (search-for-divisor 3))))

(define (divisible-by? num divisor)
  (= (remainder num divisor) 0))

(define (factors-rec num)
  (let ((factor (least-divisor-rec num)))
    (if (= factor num)
        (list factor)
        (cons factor (factors-rec (quotient num factor))))))

;; ----------------------------------------------------------------------
;; Problem 2a: Put your definition of hamming? here


(define (hamming? num)
  (and (integer? num)
       (> num 0)
       (or (= num 1)
           (forall? (λ (n) (or (= n 2)(= n 3)(= n 5)))
            (factors-rec num))
            )))

;; ----------------------------------------------------------------------
;; Problem 2b: Put your definition of least-divisor-find here

(define (least-divisor-find num)
  (find   (λ (divisor) (divisible-by? num divisor))
          num 
          (if (divisible-by? num 2) 
              (list 2)  
              (range 3 (+ (sqrt num) 1) 2))))

;; ----------------------------------------------------------------------
;; Problem 2c: Put your definition of factors-genlist here


(define (factors-genlist num)
  (map second
       (genlist
        (λ (n&f)
          (list (quotient (car n&f) (least-divisor-rec (car n&f)))
                (least-divisor-rec
                 (quotient (car n&f)(least-divisor-rec (car n&f)))) ))
        (λ (n&f) (= (car n&f) 1) )
        (list num (least-divisor-rec num)) )))

;; ----------------------------------------------------------------------
;; Problem 2d: Put your definition of factors-iterate-apply here

(define (factors-iterate-apply num)
  (iterate-apply
   (λ (num fact)
     (list
      (quotient num (least-divisor-rec num))
      (cons (least-divisor-rec num) fact)))
   (λ (num fact) (= 1 num ))
   (λ (num fact) (reverse fact))
   (list num null)))

;; **********************************************************************
;; Problem 3: Mysterious Composition 
;; **********************************************************************

(define (mystery nums)
  (foldr max 0
         (filter (λ (n) (> n 0))
                 (map (λ (pair) (* (car pair) (cdr pair)))
                      ((λ (ns) (zip ns (rest ns)))
                       (cons 0 nums)) ))))

;; ----------------------------------------------------------------------
;; Problem 3c: Put your definition of mystery-foldl here

(define (mystery-foldl nums)
  (cdr (foldl (λ (frst valueSoFar)
                (let ( [newProduct (* frst (car valueSoFar))]
                       [oldProduct (cdr valueSoFar)])
                  (cons frst
                        (if (>= newProduct oldProduct)
                            newProduct
                            oldProduct))))
              (cons 0 0)
              nums)) )

;; ----------------------------------------------------------------------
;; Problem 3e: Here are the helper functions

   (define (id x) x)
   (define (o f g) (λ (x) (f (g x))))
   (define (o-all fun-list) (foldr o id fun-list))
   (define (flip2 binop) (λ (x y) (binop y x)))
   (define (curry2 binop) (λ (x) (λ (y) (binop x y))))
   (define (curry3 ternop) (λ (x) (λ (y) (λ (z) (ternop x y z)))))
   (define (pair-dup x) (cons x x))
   (define (pair-apply f g) (λ (pair) (cons (f (car pair)) (g (cdr pair)))))
   (define (unpair-apply binop) (λ (pair) (binop (car pair) (cdr pair))))

#| (define (mystery nums)
  (foldr max 0
         (filter (λ (n) (> n 0))
                 (map (λ (pair) (* (car pair) (cdr pair)))
                      ((λ (ns) (zip ns (rest ns)))
                       (cons 0 nums)) )))) |#

 ;FIRST STEP: USING 0-ALL


 (define mystery-composed1
  (o-all (list
          (λ (a) (foldr max 0 a))
          (λ (b) (filter (λ (n) (> n 0)) b))
          (λ (num) (map (λ (pair) (* (car pair) (cdr pair)) )
                      ((λ (ns) (zip ns (rest ns)))
                       (cons 0 num)) ))
          )))
 



; SECOND STEP: REMOVING TOP-LEVEL λ

 #| (define mystery-composed
  (o-all (list
          (((curry3 foldr) max) 0)
          ((curry2 filter)((curry2 >)0) )
          ((curry2 map)( (unpair-apply *)
                         ;; still having issues generating the zipped list
                         ;; but I do think I'm getting closer
                         ((λ (ns) (zip ns (rest ns)))
                          ((curry2 cons) 0)))) ))) |#
                  





;; **********************************************************************
;; Problem 4: Folding
;; **********************************************************************

;; ----------------------------------------------------------------------
;; Problem 4b: Put your definition of unzip here

(define (unzip pairs)
   (foldr (λ (frst subres) (list
                            (cons (car frst) (first subres))
                            (cons (cdr frst) (second subres))))
          (list null null)
          pairs))

;; ----------------------------------------------------------------------
;; Problem 4c: Put your definition of subsets here


;; please note that this definition of subsets is *almost* there in that all the necessary terms
;; are there but they repeat too many times most likely due to using subres twice (since subres is the recursive part of foldr)
(define (subsets set)
  (foldr (λ (frst subres)
           (append
            (cons (list frst) subres)
            (map (λ (sublist) (cons frst sublist)) subres)))              
         (list(list))
         set))

;; **********************************************************************
;; Problem 5:Down and Up Recursion
;; **********************************************************************

;; ----------------------------------------------------------------------
;; Problem 5a: Put your definition of down-and-up-helper here

(define (down-and-up nums)
  (down-and-up-helper 0 nums))

(define (down-and-up-helper sumSoFar ns)
    (if (null? ns)
       (list null sumSoFar null)
         (let {[ restHelper
                 (down-and-up-helper
                  (+ sumSoFar (first ns))
                  (rest ns) )]}
           (list
            (cons (* (+ sumSoFar (first ns)) (first ns)) (first restHelper))
            (second restHelper)
            (cons (divisible-by? (second restHelper) (first ns)) (third restHelper))))))

;; ----------------------------------------------------------------------
;; Problem 5b: Put your definition of down-and-up-foldLR below:

(define (foldLR combineL state combineR nullfun xs)
  (if (null? xs)
      (nullfun state)
      (let ((next-state (combineL (car xs) state)))
        (combineR (first xs)
                  next-state
                  (foldLR combineL next-state combineR nullfun (rest xs))))))

; now works correctly! 
(define (down-and-up-foldLR nums)
  (foldLR  (λ (carxs st) (+ carxs st)  )
           0
           (λ (a b c) (list (* a  b) c (divisible-by? b a))) 
           (λ (st) (id st))
           nums))


;; ----------------------------------------------------------------------
;; Problem 5c: Put your definition of my-foldl, my-foldr, and my-foldr-ternop below

(define (my-foldl combine state xs)
  (foldLR   combine
            state
            (λ (a b c) (id c))
            (λ (input) (id input))
          xs))

; now works correctly!
(define (my-foldr combine nullval xs)
  (foldLR  (λ (carXS st) (id st))
           nullval
           (λ (firstEl nextSt recCall) (combine firstEl recCall))
           (λ (finalSt) (id finalSt))
           xs))

#| (define (foldr-ternop ternop null-value xs) ; standard definition of foldr-ternop from PS3
  (if (null? xs) 
      null-value
      (ternop (car xs)
              (cdr xs)
              (foldr-ternop ternop null-value (cdr xs))))) |#

;; this is producing the right terms, but are excess terms (most likely due to
;; the lambda function for combineL)
(define (my-foldr-ternop ternop nullval xs)
  (foldLR   (λ (carXS st) (id st))
            nullval
            (λ (firstEl nextSt recCall)(ternop firstEl recCall recCall))
            (λ (finalSt) (id finalSt))
          xs))

;; **********************************************************************
;; Losing your Marbles
;; **********************************************************************

;; ----------------------------------------------------------------------
;; Define your marbles function here:

#| ------ Please see notes in google doc ------- |#



;; ------------------------------------------------------------



    