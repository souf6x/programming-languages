#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below



(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))



(define (list-nth-mod xs n)
  (cond [(> 0 n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))


(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1))))) 


(define funny-number-stream
  (letrec ([f (lambda (x) (cons x
                                (lambda () (if (= (remainder (+ (abs x) 1) 5) 0)
                                               (f (- (+ (abs x) 1) (* (+ (abs x) 1) 2)))
                                               (f (+ (abs x) 1))))))])
    (lambda () (f 1))))


(define dan-then-dog
  (lambda () (cons "dan.jpg" (lambda () (cons "dog.jpg" dan-then-dog)))))




(define (stream-add-zero s)
  (lambda () (cons
              (cons 0 (car (s)))
              (stream-add-zero (cdr (s))))))


(define (cycle-lists xs ys)
  (letrec ([ks xs]
           [js ys]
           [f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                                (lambda ()
                                  (cond [(null? ks) (cycle-lists xs (cdr js))]
                                        [(null? js) (cycle-lists (cdr ks) ys)]
                                        [(and (null? ks) (null? js)) (cycle-lists xs ys)]
                                        [#t (f (+ n 1))]))))])
    (lambda () (f 0))))

(define x #f)

(define (vector-assoc v vec)
  (letrec  ([f (lambda (n)
                 (cond [(equal? n (vector-length vec)) #f]
                       [(if (pair? (vector-ref vec n))
                            (equal? (car (vector-ref vec n)) v)
                            (equal? (vector-ref vec n) v))
                        (vector-ref vec n)]
                       [#t (f (+ n 1))]))])
    (f 0)))


(define (cached-assoc xs n)
  (letrec ([vec (make-vector n #f)]
           [pos 0])
    (lambda (v)  (if (vector-assoc v vec)
                     (let ([ass (assoc v xs)])
                       (begin
                         (vector-set! vec pos ass)
                         (set! pos (remainder (+ pos 1) n))
                         ass))
                     (assoc v xs)))))
  

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([e e1])
       (letrec ([smelly (lambda (h)
                          (if
                              (<= e h)
                              #t
                              (begin e2 (smelly (+ h 1)))))])
         (smelly e2)))]))
                           
