#lang racket
(require rnrs/bytevectors-6)
(require test-engine/racket-tests)
(require racket/list)
(require (for-syntax syntax/parse))
(require racket/fixnum)

(define-syntax cat
  (syntax-rules ()
    [(cat (str . args))
     (format str . args)]
    [(cat (str . args) (str2 . args2) ...)
     (string-append
      (cat (str . args))
      (cat (str2 . args2) ...))]))

(define-syntax orz
  (syntax-rules ()
    [(orz . body)
     (error (cat . body))]))

(define-syntax fun
  (lambda (stx)
    (syntax-parse stx
      #:literals (->)
      [(fun [v ... -> r ...] ...)
       (syntax
        (match-lambda** [(v ...) (begin r ...)] ...))])))

(check-expect
    ((fun
       [1 x -> x x]
       [_ _ -> 'fun 'fun])
     1 2)
    2)

(define-syntax just-fun
  (syntax-rules ()
    [(just-fun argument-list . body)
     ((fun . body) . argument-list)]))

(check-expect
    (just-fun [1 2]
      [1 x -> x x])
    2)

(define-syntax note
  (syntax-rules ()
    [(note . body)
     (void)]))

(define file->string
  (lambda (file)
    (call-with-input-file file
      (lambda (file-port)
        (read-string (file-size file) file-port)))))

(define-syntax try
  (syntax-rules (try:help)
    [(try body key function)
     (with-handlers
         ([(lambda (exception)
             (equal? (list '<exception> 'key) exception))
           (lambda (exception)
             function)])
       body)]))

(define-syntax back-to-last-try
  (syntax-rules ()
    [(back-to-last-try key
       e ...)
     (begin
       e ...
       (raise (list '<exception> 'key)))]))

(define name-table (make-hash))
(define name-table-get
  (lambda (name)
    (hash-ref name-table name 'unnamed)))
(define name-table-set
  (lambda (name value)
    (hash-set! name-table name value)))
(define name-table-print
  (lambda ()
    (hash-map
     name-table
     (lambda (name value)
       (display
        (cat ("- ~a\n~a\n" name value)))))
    (void)))

(define-syntax prolog
  (syntax-rules ()
    [(prolog . body)
     (prolog-help (quote body))]))

(define prolog-help
  (lambda (body)
    (map compile-predicate
         (map exp->syntax-tree body))))

(define exp->predicate-formal-term
  (lambda (exp)
    `(predicate-formal-term:
       (predicate-head:
         ,(vector (car exp)
                  (length (cdr exp))))
       (argument-vector:
         ,(list->vector (cdr exp))))))

(note
  (exp->predicate-formal-term
   '(append (cons :head1 :tail1)
            :list2
            (cons :head1 :tail2)))
  '(predicate-formal-term:
     (predicate-head: #(append 3))
     (argument-vector:
       #((cons :head1 :tail1)
         :list2
         (cons :head1 :tail2)))))

(define exp->syntax-tree
  (lambda (exp)
    (define exp-list:antecedent
      (just-fun [(member '<- exp)]
        [`(<- . ,tail) -> tail]
        [#f -> '()]))
    (define exp:succedent
      (just-fun [(member '<- exp)]
        [#f -> exp]
        [rest-exp
         -> (take exp (- (length exp)
                         (length rest-exp)))]))
    `(predicate:
       (predicate-head:
         ,(vector (car exp:succedent)
                  (length (cdr exp:succedent))))
       (sequent:
         (succedent:
           ,(vector
             (exp->predicate-formal-term exp:succedent)))
         (antecedent:
           ,(list->vector
             (map exp->predicate-formal-term
                  exp-list:antecedent)))))))

(check-expect
  (exp->syntax-tree
   '(append (cons :head1 :tail1) :list2 (cons :head1 :tail2) <-
            (append :tail1 :list2 :tail2)))
  '(predicate:
     (predicate-head: #(append 3))
     (sequent:
       (succedent:
         #((predicate-formal-term:
             (predicate-head: #(append 3))
             (argument-vector:
               #((cons :head1 :tail1)
                 :list2
                 (cons :head1 :tail2))))))
       (antecedent:
         #((predicate-formal-term:
             (predicate-head: #(append 3))
             (argument-vector:
               #(:tail1
                 :list2
                 :tail2))))))))

(check-expect
  (exp->syntax-tree
   '(h :x <-
       (f :x)
       (g :x)))
  '(predicate:
     (predicate-head: #(h 1))
     (sequent:
       (succedent:
         #((predicate-formal-term:
             (predicate-head: #(h 1))
             (argument-vector:
               #(:x)))))
       (antecedent:
         #((predicate-formal-term:
             (predicate-head: #(f 1))
             (argument-vector:
               #(:x)))
           (predicate-formal-term:
             (predicate-head: #(g 1))
             (argument-vector:
               #(:x))))))))

(check-expect
  (exp->syntax-tree
   '(g a))
  '(predicate:
     (predicate-head: #(g 1))
     (sequent:
       (succedent:
         #((predicate-formal-term:
             (predicate-head: #(g 1))
             (argument-vector:
               #(a)))))
       (antecedent:
         #()))))

(define compile-predicate
  (lambda (syntax-tree)
    (just-fun [syntax-tree]
      [`(predicate:
          (predicate-head: ,predicate-head)
          ,sequent)
       -> (just-fun [(name-table-get predicate-head)]
            ['unnamed
             -> (name-table-set
                 predicate-head
                 `(<sequent-vector>
                   ,(vector sequent)))]
            [`(<sequent-vector> ,sequent-vector)
             -> (name-table-set
                 predicate-head
                 `(<sequent-vector>
                   ,(vector-append
                     sequent-vector
                     (vector sequent))))])]
      [else
       -> (orz ("- compile-predicate\n")
               ("  can not compile syntax-tree:\n~a\n"
                syntax-tree))])))

(define goal-stack-size 1024)
(define goal-stack (make-vector goal-stack-size))
(define goal-stack-pointer 0)

(define variable-stack-size (* 1024 8))
(define variable-stack (make-vector variable-stack-size))
(define variable-stack-pointer 0)

(module+ main
  (void))

(module+ test
  (display
   (cat ("\n")
        ("- testing little-prolog o.o\n")
        ("\n")))
  (test))