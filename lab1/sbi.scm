#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.4 2018-04-11 16:31:36-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))
        ))
        (path->string basepath)
    )
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit) ;; usage exit
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename) ;; takes filename and makes a function that readslist from input 
    (let ((inputfile (open-input-file filename))) ;; open filename and let the input be assigned to inputfile
         (if (not (input-port? inputfile)) ;; if it is not a valid inputfile, print the open failiure
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile))) ;; let program be equal to the reading of inputfile
                  (close-input-port inputfile) ;; close-input-port 
                         program)))) ;; return program

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

(define( label-table-caller program)
    (map (lambda (line) (label-table-make line)) program))

(define (main arglist) ;; main function takes argument list
    (if (or (null? arglist) (not (null? (cdr arglist)))) ;; checks if argument list is null or if there is more than one argument
        (usage-exit)
        (let* ((sbprogfile (car arglist)) ;; let the argument list be set to sbprogfile
               (program (readlist-from-inputfile sbprogfile))) ;; lets sbprogfile to be read and then assigned into program
                ;(write-program-by-line sbprogfile program) ;; program is written line line 
    ;(printstuff program))))
    (loopthroughprogram program))))


(define *variable-table* (make-hash))
(define *label-table* (make-hash))
(define *function-table* (make-hash))
(define (symbol-get key)
        (hash-ref *function-table* key))
(define (symbol-put! key value)
        (hash-set! *function-table* key value))

(for-each
    (lambda (pair)
            (symbol-put! (car pair) (cadr pair)))
    `(

        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (/       ,(lambda (x y) (/ (+ x 0.0) (+ y 0.0))))
        (%       ,(lambda (x y) (- x (* (/ x y) y))))
        (+       ,+)
        (^       ,expt)
        (-       ,-)
        (*       ,*)
        (abs    ,abs)
        (acos   ,acos)
        (asin   ,asin)
        (atan   ,atan)
        (ceil   ,ceiling)
        (cos    ,cos)
        (exp    ,exp)
        (floor  ,floor)
        (log    ,log)
        (log10  ,(lambda (x) (/ (log x) (log 10.0))))
        (round  ,round)
        (sin    ,sin)
        (sqrt   ,sqrt)
        (tan    ,tan)
        (trunc  ,truncate)

     )) 

;(define (printstuff program)
;   ( printf(cdar(program)))
;)


;(define (loopthroughprogram program)
;   (cond (( null? (cdr(car program)))
; (printf"This line just has a number"))
; (if ((null? (cdr(car(cdr(car(program))))))
;   (printf"there is only one label")))
;
; ((hash-has-key?(cdar(program)))
;   (printf"This line has a key"))
; (else(printf"it is a line"))
;   )
;)
           
(define (checkCases program)
    (when (not (null?  program))
       (cond
           ((null? (cdr(car program)))
          ;(printf "case 1  \n")
           (checkCases (cdr program)))
  
           ((pair? (cadr(car program)))
          ;(printf "case 3 \n")
           (checkCases (cdr program)))
  
           ((symbol? (cadr(car program)))  
       (cond 
           ((null? (cddar program))
           ;(printf "case 2 \n")
           (checkCases (cdr program)))
        (else(void);printf "case 4 \n")
            (checkCases (cdr program))))))
  
    )
)


(define (loopthroughprogram program)
(when (not (null?  program))
   (cond
        ((null? (cdr(car program)))
        ;(printf "case 1  \n")
        (loopthroughprogram (cdr program)))

        ((pair? (cadr(car program)))
        ;(printf "case 3 \n")
        (checkStatement(cadar program));added
        (loopthroughprogram (cdr program)))

        ((symbol? (cadr(car program)))
          (cond
            ((null? (cddar program))
        ;(printf "case 2 \n")
        ;add check here to see if it is the end of the program
        ;when(not (null? (car program))
          (hash-set! *label-table* (cadar program) program)

          ;;(display(hash-ref! *label-table* (cadar program) program))
          (loopthroughprogram (cdr program)))
          (else(void) ;printf "case 4 \n")
            (checkStatement(cadadr program));added
          (hash-set! *label-table* (cadr program) program )
          ;;(display(hash-ref! *label-table* (cadr program) program))
          (loopthroughprogram (cdr program))))))
        
    )
)       

(define (evalexpr expr)
   (cond ((number? expr) expr)
         ((symbol? expr) (hash-ref *variable-table* expr #f))
         ((string? expr) expr)
         ((pair? expr)   (apply (hash-ref *function-table* (car expr))
                                (map evalexpr (cdr expr))))
         (else #f)
    )
)



(define (checkStatement line)
  (if(eqv? (car line) 'print) 
  (begin (printStatement (cdr line))
  (newline))
  (void)
    )
  (if(eqv? (car line) 'let)
    (letStatement (cdr line)) 
    (void)
    )
  (if(eqv? (car line) 'dim)
    (dimStatement (cdr line))
    (void) 
    )
  (if(eqv? (car line) 'goto)
    (gotoStatement (cdr line)) 
    (void)
    )
  (if(eqv? (car line) 'if)
    (ifStatement (cdr line)) 
    (void)
    )
  (if(eqv? (car line) 'input)
    (inputStatement (cdr line)) 
    (void)
  )
)
  

(define (letStatement statement)
  (when (not (null?  statement))
    (if(pair? '())
      (void)
      (hash-set! *variable-table* (car statement) (evalexpr(cadr statement)))
    )
  )
)


(define (printStatement statement)
  (when (not (null?  statement))
    (display(evalexpr(car statement)))
    (printStatement(cdr statement))
    )
)

(define (dimStatement statement)
  (when (not (null?  statement))
    (display(evalexpr(car statement)))
    (printStatement(cdr statement))
    )
)

(define (gotoStatement statement)
  (when (not (null?  statement))
    (display(evalexpr(car statement)))
    (printStatement(cdr statement))
    )
)

(define (ifStatement statement)
  (when (not (null?  statement))
    (display(evalexpr(car statement)))
    (printStatement(cdr statement))
    )
)

(define (inputStatement statement)
  (when (not (null?  statement))
    (display(evalexpr(car statement)))
    (printStatement(cdr statement))
    )
)


(when (terminal-port? *stdin*)
      (main (vector->list (current-command-line-arguments))))