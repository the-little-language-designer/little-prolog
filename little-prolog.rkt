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
(define name-table-report
  (lambda ()
    (hash-map
     name-table
     (lambda (name value)
       (display
        (cat ("- ~a\n~a\n" name value)))))
    (void)))

(define-syntax define-knowledge
  (syntax-rules ()
    [(define-knowledge . body)
     (map compile-predicate
          (map exp->predicate (quote body)))]))

(define exp->formal-predicate-term
  (lambda (exp)
    `(formal-predicate-term:
       (predicate-head:
         ,(vector (car exp)
                  (length (cdr exp))))
       (argument-vector:
         ,(list->vector (cdr exp))))))

(note
  (exp->formal-predicate-term
   '(append (cons :head1 :tail1)
            :list2
            (cons :head1 :tail2)))
  '(formal-predicate-term:
     (predicate-head: #(append 3))
     (argument-vector:
       #((cons :head1 :tail1)
         :list2
         (cons :head1 :tail2)))))

(define exp->predicate
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
             (exp->formal-predicate-term exp:succedent)))
         (antecedent:
           ,(list->vector
             (map exp->formal-predicate-term
                  exp-list:antecedent)))))))

(check-expect
  (exp->predicate
   '(append (cons :head1 :tail1) :list2 (cons :head1 :tail2) <-
            (append :tail1 :list2 :tail2)))
  '(predicate:
     (predicate-head: #(append 3))
     (sequent:
       (succedent:
         #((formal-predicate-term:
             (predicate-head: #(append 3))
             (argument-vector:
               #((cons :head1 :tail1)
                 :list2
                 (cons :head1 :tail2))))))
       (antecedent:
         #((formal-predicate-term:
             (predicate-head: #(append 3))
             (argument-vector:
               #(:tail1
                 :list2
                 :tail2))))))))

(check-expect
  (exp->predicate
   '(h :x <-
       (f :x)
       (g :x)))
  '(predicate:
     (predicate-head: #(h 1))
     (sequent:
       (succedent:
         #((formal-predicate-term:
             (predicate-head: #(h 1))
             (argument-vector:
               #(:x)))))
       (antecedent:
         #((formal-predicate-term:
             (predicate-head: #(f 1))
             (argument-vector:
               #(:x)))
           (formal-predicate-term:
             (predicate-head: #(g 1))
             (argument-vector:
               #(:x))))))))

(check-expect
  (exp->predicate
   '(g a))
  '(predicate:
     (predicate-head: #(g 1))
     (sequent:
       (succedent:
         #((formal-predicate-term:
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
                 (vector sequent))]
            [(? vector? sequent-vector)
             -> (name-table-set
                 predicate-head
                 (vector-append sequent-vector
                                (vector sequent)))]
            [else -> (orz ("- compile-predicate\n")
                          ("  name-table-get else:\n")
                          ("  ~a\n" else))])]
      [else
       -> (orz ("- compile-predicate\n")
               ("  can not compile syntax-tree:\n~a\n"
                syntax-tree))])))

(define variable-area-size (* 1024 8))
(define variable-area (make-vector variable-area-size))
(define variable-area-pointer 0)

(define variable-area-set
  (lambda (address value)
    (vector-set! variable-area address value)))

(define variable-area-get
  (lambda (address)
    (vector-ref variable-area address)))

(define variable-new-address
  (lambda ()
    (if (< variable-area-pointer variable-area-size)
      (let ([return-value variable-area-pointer])
        (set! variable-area-pointer
              (+ variable-area-pointer 1))
        return-value)
      ;; round-buffer
      (let ([return-value 0])
        (set! variable-area-pointer 1)
        return-value))))

(define variable-area-report
  (lambda ()
    (define loop
      (lambda (cursor)
        (when (< cursor variable-area-pointer)
          (just-fun [(variable-area-get cursor)]
            [(vector name predicate-head determinacy value)
             -> (display
                 (cat ("~a :~a ~a\n" cursor predicate-head name)
                      ("  ~a\n"
                       (if (equal? determinacy 0)
                         "<unbound>"
                         value))))])
          (loop (+ 1 cursor)))))
    (loop 0)))

(define goal-stack-size 1024)
(define goal-stack (make-vector goal-stack-size))
(define goal-stack-pointer 0)

(define goal-stack-push
  (lambda (goal)
    (vector-set! goal-stack goal-stack-pointer goal)
    (set! goal-stack-pointer (+ goal-stack-pointer 1))))

(define goal-stack-pop
  (lambda ()
    (set! goal-stack-pointer (- goal-stack-pointer 1))
    (vector-ref goal-stack goal-stack-pointer)))

(define formal-and-vector->and-vector
  (lambda (formal-and-vector)
    (define variable-record (make-hash))
    (define variable-record-get
      (lambda (name)
        (hash-ref variable-record name 'unnamed)))
    (define variable-record-set
      (lambda (name value)
        (hash-set! variable-record name value)))
    (define formal-variable?
      (lambda (x)
        (and (symbol? x)
             (let* ([str (symbol->string x)]
                    [len (string-length str)])
               (and (equal? ":" (substring str 0 1))
                    (not
                     (equal? ":" (substring str (- len 1)))))))))
    (define formal-predicate-term->predicate-term
      (lambda (formal-predicate-term)
        (define current-predicate-head
          (just-fun [formal-predicate-term]
            [`(formal-predicate-term:
                (predicate-head:
                  ,predicate-head)
                (argument-vector:
                  ,argument-vector))
             -> predicate-head]
            [else
             -> (orz ("- formal-predicate-term->predicate-term\n")
                     ("  current-predicate-head"))]))
        (define formal-term->term
          (fun
            [(cons head tail)
             -> (cons (formal-term->term head)
                      (formal-term->term tail))]
            [(? formal-variable? formal-variable)
             -> `(<variable>
                  ,(just-fun [(variable-record-get formal-variable)]
                     ['unnamed
                      -> (let ([address (variable-new-address)])
                           (variable-record-set formal-variable address)
                           (variable-area-set address
                                              (vector formal-variable
                                                      current-predicate-head
                                                      0 ;; determinacy
                                                      0))
                           address)]
                     [address -> address]))]
            [else -> else]))
        (just-fun [formal-predicate-term]
          [`(formal-predicate-term:
              (predicate-head:
                ,predicate-head)
              (argument-vector:
                ,argument-vector))
           -> `(predicate-term:
                 (predicate-head:
                   ,predicate-head)
                 (argument-vector:
                   ,(vector-map formal-term->term
                      argument-vector)))]
          [else
           -> (orz ("- formal-predicate-term->predicate-term"))])))
    (vector-map formal-predicate-term->predicate-term
      formal-and-vector)))

(note
  (formal-and-vector->and-vector
   #((formal-predicate-term:
       (predicate-head: #(append 3))
       (argument-vector:
         #((cons :head1 :tail1)
           :list2
           (cons :head1 :tail2))))))
  #((predicate-term:
      (predicate-head: #(append 3))
      (argument-vector:
        #((cons (<variable> 0) (<variable> 1))
          (<variable> 2)
          (cons (<variable> 0) (<variable> 3)))))))

(define and-vector->and-meta-vector
  (lambda (and-vector)
    (vector-map
        (fun
          [`(predicate-term:
              (predicate-head: ,predicate-head)
              (argument-vector: ,argument-vector))
           -> (vector
               0 ;; or-cursor
               null ;; undo-record
               (just-fun [(name-table-get predicate-head)]
                 ['unnamed
                  -> (orz ("- and-vector->and-meta-vector\n")
                          ("  name-table-get unnamed:\n")
                          ("  ~a\n" predicate-head))]
                 [(? vector? sequent-vector)
                  -> sequent-vector]
                 [else -> (orz ("- and-vector->and-meta-vector\n")
                               ("  name-table-get else:\n")
                               ("  ~a\n" else))]))]
          [else
           -> (orz ("- and-vector->and-meta-vector\n")
                   ("  not predicate-term:\n")
                   ("  ~a\n" else))])
      and-vector)))

(define goal-stack-interpreter
  (lambda ()
    `()))

(define-syntax query
  (syntax-rules ()
    [(query . body)
     (let ([and-vector
            (formal-and-vector->and-vector
             (list->vector
              (map exp->formal-predicate-term (quote body))))])
       (goal-stack-push
        (vector
         0 ;; and-cursor
         and-vector
         (and-vector->and-meta-vector and-vector)))

       ;; (goal-stack-interpreter)
       )]))

(module+ main
  (void))

(module+ test
  (display
   (cat ("\n")
        ("- testing little-prolog o.o\n")
        ("\n")))
  (test))
