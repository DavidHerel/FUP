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
(define (simulate position expr program limit)
  (input-to-simulate-basic position expr program limit)
)

;we will upgrade simulate to put there this 
(define (input-to-simulate-basic position expr program limit)
  (cond ((list? expr) (simulate-basic '() position expr program limit))
      (#t (simulate-basic '() position (list expr) program limit))
  )
)

;lets do onlyy normal moves,
; if there are procuderes, call procedures
(define (simulate-basic steps-made position expr program limit)
  (if (null? expr) (list steps-made position)
      (let ((move (car expr)))
        (cond
          ((eqv? 'put-mark move) (simulate-basic (append steps-made '(put-mark)) (put-mark position) (cdr expr) program limit))
          ((eqv? 'turn-left move) (simulate-basic (append steps-made '(turn-left)) (turn-left position) (cdr expr) program limit))
          ((eqv? 'step move) (if (wall? position) (list steps-made position) (simulate-basic (append steps-made '(step)) (step position steps-made) (cdr expr) program limit)))
          ((eqv? 'get-mark move) (if (mark? position) (list steps-made position) (simulate-basic (append steps-made '(get-mark)) (get-mark position steps-made) (cdr expr) program limit)))
          (#t (prepare-procedure steps-made position expr program (- limit 1) move program))
         )
       )
   )
)

(define (get-proc-command program)
  (caddr (car program)))
;(cadr (car program))

;function to prepare procedures
;get element by element and call do-procedure
(define (prepare-procedure steps-made position expr program limit curr-proc temp-program)
  (if (and (eqv? (get-proc-name temp-program) curr-proc) (eqv? (car (car program)) 'procedure)) (do-procedure steps-made position program limit (append (get-proc-command temp-program) '(kill-it) (cdr expr)))
      (prepare-procedure steps-made position expr  program limit curr-proc (cdr temp-program))))

;get it to list
(define (to-list n)
  (if (list? n) n (list n))
)

;(require racket/trace)
;
;(trace get-proc-name)


;takes procedure list and make it right
(define (do-procedure steps-made position program limit expr)
    (cond
      ;terminate dat shit
    ((or (= (+ limit 1) 0) (null? expr)) (list steps-made position))
     ;IF and (...)
     ((eqv? (car expr) 'if)
      
         (if
           (or (and (north? position) (eqv? (cadr expr) 'north?)) (and (wall? position) (eqv? (cadr expr) 'wall?))
                     (and (mark? position) (eqv? (cadr expr) 'mark?)))
           ;if true
            (do-procedure steps-made position program limit (append (to-list (car (cdr (cdr expr)))) (to-list (cddddr expr))))
            ;else 
            (do-procedure steps-made position program limit (append (to-list (car (cdr (cdr (cdr expr))))) (to-list (cddddr expr))))
         )
     )
     ((list? (car expr)) ;(write "jsem tu")
      (if 
           (or (and (north? position) (eqv? (cadr (car expr)) 'north?)) (and (wall? position) (eqv? (cadr (car expr)) 'wall?))
                     (and (mark? position) (eqv? (cadr (car expr)) 'mark?)))
           ;if true
           (do-procedure steps-made position program limit (append (to-list (car (cdr (cdr (car expr))))) (cdr (cdddar (to-list expr))) (to-list (cdr expr))))
            ;else 
           (do-procedure steps-made position program limit (append (to-list (cadr (cdr (cdr (car expr))))) (cdr (cdddar (to-list expr))) (to-list (cdr expr))))
         )
      )
     ((eqv? 'put-mark (car expr)) (do-procedure (append steps-made '(put-mark)) (put-mark position) program limit (cdr expr)))
     ((eqv? 'turn-left (car expr)) (do-procedure (append steps-made '(turn-left)) (turn-left position) program limit (cdr expr) ))
     ((eqv? 'step (car expr)) (if (wall? position) (list steps-made position) (do-procedure (append steps-made '(step)) (step position steps-made) program limit (cdr expr))))
     ;need to increment dat -> or we will fail -> we will jump there again
     ((eqv? 'kill-it (car expr)) (do-procedure steps-made position program (+ limit 1) (cdr expr)))
     ((eqv? 'get-mark (car expr)) (if (mark? position) (do-procedure (append steps-made '(get-mark)) (get-mark position steps-made) program limit (cdr expr)) (list steps-made position)))

     ;another procedure
     (#t (prepare-procedure steps-made position expr program (- limit 1) (car expr) program))
  )
)


;(require racket/trace)
;(trace simulate-basic)


;(require racket/trace)
;(trace find-curr-block)
;(trace wall?)
;(trace get-procedures)
;(trace do-procedure)


