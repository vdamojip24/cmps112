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
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))
	(map (lambda (line) (set-label-table line)) program)

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program))))

(when (terminal-port? *stdin*)
      (main (vector->list (current-command-line-arguments))))


;EXAMPLE CODE


(define *function-table* (make-hash))
(define (symbol-get key)
        (hash-ref *symbol-table* key))
(define (symbol-put! key value)
        (hash-set! *symbol-table* key value))

(define *variable-table* (make-hash))

(hash-set! *variable-table* "e")
	2.718281828459045235360287471352662497757247093 
(hash-set! *variable-table* "pi")
	3.141592653589793238462643383279502884197169399

(for-each
    (lambda (pair)
            (symbol-put! (car pair) (cadr pair)))
    `(

        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,+)
        (^       ,expt)
        (ceil    ,ceiling)
        (exp     ,exp)
        (floor   ,floor)
        (log     ,log)
        (sqrt    ,sqrt)
        (abs    ,abs)
        (acos   ,acos)
        (asin   ,asin)
        (atan   ,atan)
        (ceil   ,ceiling)
        (cos    ,cos)
        (exp    ,exp)
        (floor  ,floor)
        (round  ,round)
        (sin    ,sin)
        (sqrt   ,sqrt)
        (tan    ,tan)
        (trunc  ,truncate)
        (*      ,*)
        (/      ,/)
        (%      ,%)
        (^      ,^)
        (-      ,-)

     ))

;; 
;; What category of object is this?
;;

(define (what-kind value)
    (cond ((real? value) 'real)
          ((vector? value) 'vector)
          ((procedure? value) 'procedure)
          (else 'other)))

;;
;; Main function.
;;

(define (main argvlist)
    (symbol-put! 'n (expt 2.0 32.0))
    (symbol-put! 'a (make-vector 10 0.0))
    (vector-set! (symbol-get 'a) 3 (symbol-get 'pi))
    (printf "2 ^ 16 = ~s~n" ((symbol-get '^) 2.0 16.0))
    (printf "log 2 = ~s~n" ((symbol-get 'log) 2.0))
    (printf "log10 2 = ~s~n" ((symbol-get 'log10) 2.0))
    
    (newline)
    (printf "*symbol-table*:~n")
    (hash-for-each *symbol-table*
        (lambda (key value)
                (printf "~s : ~s = ~s~n" key (what-kind value) value))
    ))

(main '())

define (set-label-table line)
	cond((pair?)(cadr line))
		(void))
	(null?(cdr)) 
		(void))
	(else
		(hash-set! *label-table* (cadr line) line)))

(define (put-in-hash list)
        (when (not (null? list))				
              (let ((first (caar list)))		
                   (when (symbol? first)
                         (hash-set! *hash* first list)))
              (put-in-hash (cdr list))))

(define (evalexpr expr)
   (cond ((number? expr) expr)
         ((symbol? expr) (hash-ref ht expr #f))
         ((pair? expr)   (apply (hash-ref ht (car expr))
                                (map evalexpr (cdr expr))))
         (else #f))
)

;(define (printLine program))
	;(cond (null?) cdar program)
		;(printf "its just a line")
	
	
