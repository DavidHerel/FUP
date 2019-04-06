;#lang scheme

;;MIstakes is when you jumop out of procedure -> it will go back and kill it..we have to add 1 to limit
;but how -> when we jump out of procedure lets add 1 to limit
;should work now yeeah


;function will apply fn on 1d array
(define (apply-at fn list pos)
  (cond ((= pos 0) (cons (fn (car list)) (cdr list)))
        (#t (cons (car list) (apply-at fn (cdr list) (- pos 1))))
  )
)

;function will apply fn on 2d array       
(define (apply-2D fn x y list-of-lists)
  (apply-at (lambda (line)
              (apply-at fn line x)
             ) list-of-lists y)
)

;find n-field
(define (find-n-block position acc coord)
  (cond
    ((= acc coord) (car position))
    (#t (find-n-block (cdr position) (+ acc 1) coord))
  )
)

;get X and Y coord (middle element in position)
(define (coordXY position) (cadr position))

;takes first element from XY coords (Xs)
(define (coordX position) (car (coordXY position)))

;take second element from XY cords (Y)
(define (coordY position) (cadr(coordXY position)))

;get maze in 2D (first element in position)
(define (2D-maze position) (car position))

;get orientation (last element in positiion)
(define (orientation position) (caddr position))

;get procedure name
(define (get-proc-name program)
 (cadr (car program)))

;north block
(define (find-north-block position)
  (find-n-block (find-n-block (2D-maze position) 0 (- (coordY position) 1)) 0 (coordX position))
)

;south block
(define (find-south-block position)
  (find-n-block (find-n-block (2D-maze position) 0 (+ (coordY position) 1)) 0 (coordX position))
)
;east block
(define (find-east-block position)
  (find-n-block (find-n-block (2D-maze position) 0 (coordY position)) 0 (+ (coordX position) 1))
)

;west block
(define (find-west-block position)
  (find-n-block (find-n-block (2D-maze position) 0 (coordY position)) 0 (- (coordX position) 1))
)

;now position
(define (find-curr-block position)
  (find-n-block (find-n-block (2D-maze position) 0 (coordY position)) 0 (coordX position))
)

;increase x
(define (increase x)
  (+ x 1))

;decrerase x
(define (decrease x)
  (- x 1))

;if is nort?
(define (north? position)
  (eqv? (orientation position) 'north)
)

;if is south?
(define (south? position)
  (eqv? (orientation position) 'south)
)

;east?
(define (east? position)
  (eqv? (orientation position) 'east)
)

;west?
(define (west? position)
  (eqv? (orientation position) 'west)
)

;if there is mark
(define (mark? position) 
   (> (find-curr-block position) 0)
)


;if there is wall
(define (wall? position)
  (cond    
    ((north? position) 
     (eqv? 'w (find-north-block position)))
    ((south? position)
     (eqv? 'w (find-south-block position)))
    ((east? position)
     (eqv? 'w (find-east-block position)))
    ((west? position)
     (eqv? 'w (find-west-block position)))
    )
)

;kills last element in array
(define (kill-last array)
  (reverse (cdr (reverse array))))

;make step
(define (step position steps-made)
  (cond
    ((wall? position)
        (list (kill-last steps-made) position))
    ((north? position)
       (list (2D-maze position) (list (coordX position) (- (coordY position) 1)) (orientation position)))
    ((west? position)
       (list (2D-maze position) (list (- (coordX position) 1) (coordY position)) (orientation position)))
    ((east? position)
       (list (2D-maze position) (list (+ (coordX position) 1) (coordY position)) (orientation position)))
    ((south? position)
        (list (2D-maze position) (list (coordX position) (+ (coordY position) 1)) (orientation position)))
    )
)

;(require racket/trace)
;(trace step)

;turn left
(define (turn-left position)
  (cond    
    ((north? position)
       (list (2D-maze position) (coordXY position) 'west))
    ((west? position)
       (list (2D-maze position) (coordXY position) 'south))
    ((east? position)
       (list (2D-maze position) (coordXY position) 'north))
    ((south? position)
        (list (2D-maze position) (coordXY position) 'east))
    )
)


;(require racket/trace)
;(trace turn-left)

;decrease mark by 1 and rerun position
(define (get-mark position steps-made)
  (if (mark? position)
  (list (apply-2D decrease (coordX position) (coordY position) (2D-maze position)) (coordXY position) (orientation position))
    (list (kill-last steps-made) position)))

;incrase mark by 1 and return position
(define (put-mark position)
  (list (apply-2D increase (coordX position) (coordY position) (2D-maze position)) (coordXY position) (orientation position)))


;basic simulate
(define (simulate position expr program limit threshold)
  (input-to-simulate-basic position expr program limit threshold)
)

;we will upgrade simulate to put there this 
(define (input-to-simulate-basic position expr program limit threshold)
  (cond ((list? expr) (simulate-basic '() position expr program limit threshold))
      (#t (simulate-basic '() position (list expr) program limit threshold))
  )
)

;lets do onlyy normal moves,
; if there are procuderes, call procedures
(define (simulate-basic steps-made position expr program limit threshold)
  (if (null? expr) (list steps-made position)
      (let ((move (car expr)))
        (cond
          ((< (cadddr threshold) (n-steps (make-list steps-made) 0)) (list steps-made position))
          ((eqv? 'put-mark move) (simulate-basic (append steps-made '(put-mark)) (put-mark position) (cdr expr) program limit))
          ((eqv? 'turn-left move) (simulate-basic (append steps-made '(turn-left)) (turn-left position) (cdr expr) program limit))
          ((eqv? 'step move) (if (wall? position) (list steps-made position) (simulate-basic (append steps-made '(step)) (step position steps-made) (cdr expr) program limit)))
          ((eqv? 'get-mark move) (if (mark? position) (list steps-made position) (simulate-basic (append steps-made '(get-mark)) (get-mark position steps-made) (cdr expr) program limit)))
          (#t (prepare-procedure steps-made position expr program (- limit 1) move program threshold))
         )
       )
   )
)

(define (make-list l)
  (if (list? l) l (list l)))

(define (get-proc-command program)
  (caddr (car program)))
;(cadr (car program))

;function to prepare procedures
;get element by element and call do-procedure
(define (prepare-procedure steps-made position expr program limit curr-proc temp-program threshold)
  (if (and (eqv? (get-proc-name temp-program) curr-proc) (eqv? (car (car program)) 'procedure)) (do-procedure steps-made position program limit (append (make-list (get-proc-command temp-program)) '(kill-it) (cdr expr)) threshold)
      (prepare-procedure steps-made position expr  program limit curr-proc (cdr temp-program) threshold)))

;get it to list
(define (to-list n)
  (if (list? n) n (list n))
)

;(require racket/trace)
;
;(trace get-proc-name)


;takes procedure list and make it right
(define (do-procedure steps-made position program limit expr threshold)
    (cond
      ;terminate dat shit
     ((< (cadddr threshold) (n-steps (make-list steps-made) 0)) (list steps-made position))
    ((or (= (+ limit 1) 0) (null? expr)) (list steps-made position))
     ;IF and (...)
     ((eqv? (car expr) 'if)
      
         (if
           (or (and (north? position) (eqv? (cadr expr) 'north?)) (and (wall? position) (eqv? (cadr expr) 'wall?))
                     (and (mark? position) (eqv? (cadr expr) 'mark?)))
           ;if true
            (do-procedure steps-made position program limit (append (to-list (car (cdr (cdr expr)))) (to-list (cddddr expr))) threshold)
            ;else 
            (do-procedure steps-made position program limit (append (to-list (car (cdr (cdr (cdr expr))))) (to-list (cddddr expr))) threshold)
         )
     )
     ((list? (car expr)) ;(write "jsem tu")
      (if 
           (or (and (north? position) (eqv? (cadr (car expr)) 'north?)) (and (wall? position) (eqv? (cadr (car expr)) 'wall?))
                     (and (mark? position) (eqv? (cadr (car expr)) 'mark?)))
           ;if true
           (do-procedure steps-made position program limit (append (to-list (car (cdr (cdr (car expr))))) (cdr (cdddar (to-list expr))) (to-list (cdr expr))) threshold)
            ;else 
           (do-procedure steps-made position program limit (append (to-list (cadr (cdr (cdr (car expr))))) (cdr (cdddar (to-list expr))) (to-list (cdr expr))) threshold)
         )
      )
     ((eqv? 'put-mark (car expr)) (do-procedure (append steps-made '(put-mark)) (put-mark position) program limit (cdr expr) threshold))
     ((eqv? 'turn-left (car expr)) (do-procedure (append steps-made '(turn-left)) (turn-left position) program limit (cdr expr) threshold))
     ((eqv? 'step (car expr)) (if (wall? position) (list steps-made position) (do-procedure (append steps-made '(step)) (step position steps-made) program limit (cdr expr) threshold)))
     ;need to increment dat -> or we will fail -> we will jump there again
     ((eqv? 'kill-it (car expr)) (do-procedure steps-made position program (+ limit 1) (cdr expr) threshold))
     ((eqv? 'get-mark (car expr)) (if (mark? position) (do-procedure (append steps-made '(get-mark)) (get-mark position steps-made) program limit (cdr expr) threshold) (list steps-made position)))

     ;another procedure
     (#t (prepare-procedure steps-made position expr program (- limit 1) (car expr) program threshold))
  )
)


;(require racket/trace)
;(trace simulate-basic)


;(require racket/trace)
;(trace find-curr-block)
;(trace wall?)
;(trace get-procedures)
;(trace do-procedure)


;-----Lets make HW 02------


;flatten function
(define (my-flatten l)
  (cond ((null? l) l)
        ((list? (car l)) (append ( my-flatten (car l)) (my-flatten (cdr l))))
        (#t (cons (car l) (my-flatten (cdr l))))
  )
)

;prgs - procedures
;pairs - pairs of mazes - first is normal and second is how it should look
;threshold - if something is higher than threshold -> cancel program and forget him
;stacksize - is limit
(define (evaluate prgs pairs threshold stack_size)
 (real-eval prgs pairs threshold stack_size '()) 
)

;start doing procedures on maze pairs
;what i want -> do 1 procedure on all mazes -> then compare -> then sum
;then second procedure on pairs

;return output
;else -> calls it on itself with thinner prgs and bigger output
(define (real-eval prgs pairs threshold stack_size output)
  (if (null? prgs) (merge-sort (kill-threshold output '()))
      (real-eval (cdr prgs) pairs threshold stack_size
                 (append output (list (do-prg prgs pairs threshold stack_size (car prgs) '()))))

  )
)

;do procedure start - on all mazes
;return mazes -> return outputs from hw01 in list
;a co misto outputu vracet uz tu state - manhatten distance apod a ten program ktery to je
;to bude best.tedy misto outputu volat fci co to vse spocitat
(define (do-prg prgs pairs threshold stack_size one-proc output)
  ;this returns me first hw01
  (if (null? pairs) (list (sum-states output '(0 0 0 0) threshold) one-proc)
  (do-prg prgs (cdr pairs) threshold stack_size one-proc (append output (list (calc-states (simulate (caar pairs) 'start one-proc stack_size threshold) (cadar pairs) one-proc))))
  )
)

(define (kill-threshold l1 output)
  (cond
    ((null? l1) output)
    ((null? (caar l1)) (kill-threshold (cdr l1) output))
    (#t (kill-threshold (cdr l1) (append output (list (car l1)))))
   )
)

;it will count states
(define (sum-states proc-mazes output-sum threshold)
  (cond
        ;if there is threshold
        ((or (> (car output-sum) (car threshold))
             (> (cadr output-sum) (cadr threshold))
             (> (caddr output-sum) (caddr threshold))
             (> (car (cdddr output-sum)) (cadddr threshold)))
              '())
        ((null? proc-mazes) output-sum)
        ((or (> (caar proc-mazes) (car threshold))
             (> (cadar proc-mazes) (cadr threshold))
             (> (caddar proc-mazes) (caddr threshold))
             (> (car (cdddar proc-mazes)) (cadddr threshold)))
              '())
        
      ;else
      (#t (sum-states (cdr proc-mazes)
                  (list (+ (caar proc-mazes) (car output-sum))
                        (+ (cadar proc-mazes) (cadr output-sum))
                        (+ (caddar proc-mazes) 0)
                        (+ (car (cdddar proc-mazes)) (cadddr output-sum))
                  ) threshold))
  )
)

;func that will calculate all states - manhatten apod. and return them with proc
(define (calc-states hw1-out sec-pair proc)
  (list 
         (manhattan-dist (caadr hw1-out) (car sec-pair))
         (robot-dist (cdadr hw1-out) (cdr sec-pair))
         (prog-len (my-flatten proc) 0)
         (n-steps (car hw1-out) 0)
         proc
         
   )
)

;merge sort -> lets sort it and if there is empty list -> ya know what to do -> get rid of him
(define (split list)
  (cond ((null? list) (cons '() '()))
        ((null? (cdr list)) (cons list '()))
        (#t (let ((rest (split (cddr list))))
              (cons (cons (car list) (car rest))
                    (cons (cadr list) (cdr rest)))))))

(define (merge a b)
  (cond ((null? a) b)
        ((null? b) a)
        ;here we will add some change to compare whole list not just 1 number maan
        ((compare-list (caar a) (caar b) 4) (cons (car a) (merge (cdr a) b)))
        (#t (cons (car b) (merge a (cdr b))))))

(define (compare-list l1 l2 acc)
  (cond
    ((= acc 1) #t)
    ;lets do sec crit
    ((= (car l1) (car l2)) (compare-list (cdr l1) (cdr l2) (- acc 1)))
    ((< (car l1) (car l2)) #t)
    (#t #f)
   )
)

(define (merge-sort list)
  (cond ((null? list) '())
        ((null? (cdr list)) list)
        ;if empty list
        (#t (let* ((s (split list))
                   (a (merge-sort (car s)))
                   (b (merge-sort (cdr s))))
              (merge a b)))))


;now compare mazes and return value
(define (manhattan-dist first-pair sec-pair)
  (real-mt-dist (my-flatten first-pair) (my-flatten sec-pair) 0)
)

(define (real-mt-dist first-pair sec-pair acc)
  (cond
    ((null? first-pair) acc)
    ((eqv? (car first-pair) 'w) (real-mt-dist (cdr first-pair) (cdr sec-pair) acc))
    (#t (real-mt-dist (cdr first-pair) (cdr sec-pair) (+ acc (abs (- (car first-pair) (car sec-pair))))))
  )
)

;compare robot distance
(define (robot-dist first-pair sec-pair)
  (+ (abs (- (caar first-pair) (caar sec-pair))) (abs (- (cadar first-pair) (cadar sec-pair)))
     (if (eqv? (cadr first-pair) (cadr sec-pair)) 0 1))
)

  

;calculate length of program -> except if and procedure
(define (prog-len proc acc)
  (cond
    ((null? proc) acc)
    ((eqv? (car proc) 'procedure) (prog-len (cdr proc) acc))
    ((eqv? (car proc) 'if) (prog-len (cdr proc) acc))
    (#t (prog-len (cdr proc) (+ acc 1)))
  )
)

;steps of program made
(define (n-steps steps-made acc)
  (if (null? steps-made) acc (n-steps (cdr steps-made) (+ acc 1)))
)

;(require racket/trace)
;(trace real-eval)
;(trace do-prg)
;(trace simulate)
;(trace calc-states)
;(trace do-procedure)

