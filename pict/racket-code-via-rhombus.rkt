#lang racket/base
(require (for-syntax racket/base)
         racket/unit
         racket/class
         racket/draw
         texpict/code
         (only-in pict/code
                  current-code-line-sep
                  current-code-font
                  get-current-code-font-size)
         rhombus/parse
         (prefix-in rhm:
                    (lib "pict/main.rhm"))
         (prefix-in rhm:
                    (lib "pict/text.rhm"))
         (prefix-in rhm:
                    (lib "draw/main.rhm"))
         (prefix-in rhm:
                    (lib "pict/rhombus.rhm"))
         (only-in rhombus
                  is_a
                  |.|
                  |#'|
                  #%call
                  with
                  [= rhm:=])
         rhombus/private/version-case)

(provide code)

(define-syntax (meta-when-unit-available stx)
  (syntax-case stx ()
    [(_ avail not-avail)
     (if (identifier-binding #'code-wrt-pict@)
         #'avail
         #'not-avail)]))

(meta-when-unit-available
 (begin
   (define-unit rhombus-pict-for-code@
     (import)
     (export pict-for-code^)
     (define (pict-convertible? v)
       (rhombus-expression (group v is_a rhm:Pict)))
     (define (pict-width v)
       (rhombus-expression (group rhm:Pict (op |.|) width (parens (group v)))))
     (define (pict-height v)
       (rhombus-expression (group rhm:Pict (op |.|) height (parens (group v)))))
     (define-syntax-rule (make-append combine align-kw align rhm-extra ...)
       (letrec ([proc (case-lambda
                        [(a) a]
                        [(a b)
                         (if (number? a)
                             b
                             (rhombus-expression
                              (group combine (parens (group align-kw (block (group (op |#'|) align)))
                                                     (group a) (group b) rhm-extra ...))))]
                        [(a b c)
                         (if (number? a)
                             (rhombus-expression
                              (group combine (parens (group align-kw (block (group (op |#'|) align)))
                                                     (group #:sep (block (group a)))
                                                     (group b) (group c) rhm-extra ...)))
                             (proc (proc a b) c))]
                        [(a b c . ds)
                         (if (number? a)
                             (apply proc a (proc a b c) ds)
                             (apply proc (proc a b) c ds))])])
         proc))
     (define vl-append (make-append rhm:stack #:horiz left))
     (define hbl-append (make-append rhm:beside #:vert baseline))
     (define htl-append (make-append rhm:beside #:vert topline))
     (define code-vl-append (make-append rhm:stack #:horiz left))
     (define code-hbl-append (make-append rhm:beside #:vert baseline (group #:attach (block (group (op |#'|) paragraph)))))
     (define code-htl-append (make-append rhm:beside #:vert topline (group #:attach (block (group (op |#'|) paragraph)))))
     (define lt-superimpose (make-append rhm:overlay #:vert top (group #:horiz (block (group (op |#'|) left)))))
     (define cc-superimpose (make-append rhm:overlay #:vert center))
     (define (lt-find a b)
       (rhombus-expression (group rhm:Find (parens (group b)) (op |.|) in (parens (group a)))))
     (define (colorize p c)
       (let ([c (if (is-a? c color%)
                    (rhombus-expression (group rhm:Color (op |.|) from_handle (parens (group c))))
                    c)])
         (rhombus-expression (group rhm:Pict (op |.|) colorize (parens (group p) (group c))))))
     (define (launder p)
       (rhombus-expression (group rhm:Pict (op |.|) launder (parens (group p)))))
     (define (ghost p)
       (rhombus-expression (group rhm:Pict (op |.|) ghost (parens (group p)))))
     (define (refocus p q)
       (rhombus-expression (group rhm:Pict (op |.|) launder (parens (group p) (group q)))))
     (define inset
       (case-lambda
         [(p amt)
          (rhombus-expression (group rhm:Pict (op |.|) pad (parens (group p) (group amt))))]
         [(p h v)
          (rhombus-expression (group rhm:Pict (op |.|) pad (parens (group p)
                                                                   (group #:horiz (block (group h)))
                                                                   (group #:vert (block (group v))))))]
         [(p l t r b)
          (rhombus-expression (group rhm:Pict (op |.|) pad (parens (group p)
                                                                   (group #:left (block (group l)))
                                                                   (group #:top (block (group t)))
                                                                   (group #:right (block (group r)))
                                                                   (group #:bottom (block (group b))))))]))
     (define (text content style size)
       (let ([content (string->immutable-string content)]
             [style (let loop ([style style])
                      (cond
                        [(symbol? style)
                         (rhombus-expression (group rhm:Font (parens (group #:kind (block (group style))))))]
                        [(is-a? style font%)
                         (rhombus-expression (group rhm:Font (op |.|) from_handle (parens (group style))))]
                        [else
                         (case (car style)
                           [(bold)
                            (let ([w (car style)]
                                  [f (loop (cdr style))])
                              (rhombus-expression (group f with (parens (group weight (op rhm:=) w)))))]
                           [(italic)
                            (let ([w (car style)]
                                  [f (loop (cdr style))])
                              (rhombus-expression (group f with (parens (group style (op rhm:=) w)))))]
                           [(subscript) (loop (cdr style))]
                           [(superscript) (loop (cdr style))]
                           [else
                            (error 'unsupported "not yet supported: ~s" style)])]))]
             [make-text (let loop ([style style])
                          (cond
                            [(symbol? style) rhm:text]
                            [(is-a? style font%) rhm:text]
                            [else
                             (case (car style)
                               [(subscript) (lambda (c #:font f)
                                              (parameterize ([rhm:current_font f])
                                                (rhm:subscript c)))]
                               [(superscript) (lambda (c #:font f)
                                                (parameterize ([rhm:current_font f])
                                                  (rhm:superscript c)))]
                               [else (loop (cdr style))])]))])
         (rhombus-expression (group make-text (parens (group content) (group #:font (block (group style))))))))
     (define blank
       (case-lambda
         [()
          (rhombus-expression (group rhm:blank (parens)))]
         [(w h)
          (rhombus-expression (group rhm:blank (parens (group #:width (block (group w)))
                                                       (group #:height (block (group h))))))]
         [(w h a d)
          (rhombus-expression (group rhm:blank (parens (group #:width (block (group w)))
                                                       (group #:height (block (group h)))
                                                       (group #:ascent (block (group a)))
                                                       (group #:descent (block (group d))))))])))

   (define (current-font-size) ((get-current-code-font-size)))

   (define-values/invoke-unit/infer
     (export code^)
     (link code-wrt-pict@
           rhombus-pict-for-code@))

   (current-code-tt (lambda (str)
                      ((rhm:current_rhombus_tt) (string->immutable-string str))))

   (define (typeset-code-via-rhombus p)
     (typeset-code p))

   (define-code code typeset-code-via-rhombus))
 (begin
   (require pict/code)
   (provide code)))
