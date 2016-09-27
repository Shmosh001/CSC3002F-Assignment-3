;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functional Programming Assignment --- Fixing The World    ;;
;; 25/3/15                                                   ;;
;; Osher Shuman
;; SHMOSH001
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;-------------------HELPER FUNCTIONS----------------------
;; ;print function for debugging purposes
(define (print . args)
  (cond ((not (null? args))
        (display (car args))
        (apply print (cdr args)))
  )
)

;; ;gets nth index of 0-indexed list. Can use list-ref instead
(define (index lst idx)
    (if (null? lst)
        lst
        (if (= idx 0)
            (car lst)
            (index (cdr lst) (- idx 1))
        )
    )
)
;; ;TESTS
;; ; (print (= 1 (index '(1 2 3 4 5) 0)) "\n")
;; ; (print (= 4 (index '(1 2 3 4 5) 3)) "\n")
;; ; (print (not (= 1 (index '(1 2 3 4 5) 2))) "\n")
;; ; (print (not (= 0 (index '(1 2 3 4 5) 0))) "\n")

;; ;checks if an item is in a list
;; You might want to do a more efficient version of this.
;;
(define (in item lst)
    (if (null? lst)
        #f
        (if (equal? item (car lst))
            #t
            (in item (cdr lst))
        )
    )
)

(define (position item lst n)
    (if (equal? item (car lst))
        n
        (position item (cdr lst) n)
    )
    
)
;; ;TESTS
;; ; (print (in 1 '(1 2 3)) "\n")
;; ; (print (in 2 '(1 2 3)) "\n")
;; ; (print (not (in 4 '(1 2 3))) "\n")
;; ; (print (in '(1 2) '((1 2) (3 4) 5)) "\n")

;; ;helper function for finding the length of a list
(define (lengthHelper n lst)
    (if (null? lst)
        n
        (lengthHelper (+ n 1) (cdr lst))
    )
)

;; ;finds length of a list
(define (length lst)
    (lengthHelper 0 lst)
)
;; ;TESTS
;; ; (print (= 4 (length '(1 2 3 4))) "\n")
;; ; (print (= 1 (length '(1))) "\n")
;; ; (print (= 2 (length '((1 2) (3 4)))) "\n")
;; ; (print (not (= 4 (length '(1 2 3 4 5)))) "\n")
;; ;-----------------------------------------------------------


;---------------------SOLVED STATES------------------------
;solved states of a 2x2x2 rubiks cube
(define solvedStates
    '(
        ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
        ((3 1) (1 1) (4 1) (2 1) (7 3) (5 3) (8 3) (6 3))
        ((4 1) (3 1) (2 1) (1 1) (8 3) (7 3) (6 3) (5 3))
        ((2 1) (4 1) (1 1) (3 1) (6 3) (8 3) (5 3) (7 3))

        ((5 5) (1 6) (7 5) (3 6) (6 5) (2 6) (8 5) (4 6))
        ((7 5) (3 6) (8 5) (4 6) (5 5) (1 6) (6 5) (2 6))
        ((8 5) (4 6) (6 5) (2 6) (7 5) (3 6) (5 5) (1 6))
        ((6 5) (2 6) (5 5) (1 6) (8 5) (4 6) (7 5) (3 6))

        ((2 5) (6 6) (4 5) (8 6) (1 5) (5 6) (3 5) (7 6))
        ((4 5) (8 6) (3 5) (7 6) (2 5) (6 6) (1 5) (5 6))
        ((3 5) (7 6) (1 5) (5 6) (4 5) (8 6) (2 5) (6 6))
        ((1 5) (5 6) (2 5) (6 6) (3 5) (7 6) (4 5) (8 6))

        ((7 1) (8 1) (5 1) (6 1) (3 3) (4 3) (1 3) (2 3))
        ((5 1) (7 1) (6 1) (8 1) (1 3) (3 3) (2 3) (4 3))
        ((6 1) (5 1) (8 1) (7 1) (2 3) (1 3) (4 3) (3 3))
        ((8 1) (6 1) (7 1) (5 1) (4 3) (2 3) (3 3) (1 3))

        ((3 2) (4 2) (7 4) (8 4) (1 2) (2 2) (5 4) (6 4))
        ((1 2) (3 2) (5 4) (7 4) (2 2) (4 2) (6 4) (8 4))
        ((2 2) (1 2) (6 4) (5 4) (4 2) (3 2) (8 4) (7 4))
        ((4 2) (2 2) (8 4) (6 4) (3 2) (1 2) (7 4) (5 4))

        ((5 2) (6 2) (1 4) (2 4) (7 2) (8 2) (3 4) (4 4))
        ((7 2) (5 2) (3 4) (1 4) (8 2) (6 2) (4 4) (2 4))
        ((8 2) (7 2) (4 4) (3 4) (6 2) (5 2) (2 4) (1 4))
        ((6 2) (8 2) (2 4) (4 4) (5 2) (7 2) (1 4) (3 4))
    )
)
;; ;-----------------------------------------------------


;; ;---------------------QUESTION 1.1-----------------------
;; ;helper function for rotating the cube. Recalculates the various orientations
;; ;of the sub-cubes
(define (recalculateOrientation orientation axis)
    (cond
        [(= axis 0)
            (if (> orientation 4)
                orientation
                (if(= orientation 4)
                    1
                    (+ orientation 1)
                )
            )
        ]
        [(= axis 1)
            (if (or (= orientation 1) (= orientation 3))
                orientation
                (cond
                    [(= orientation 2) 6]
                    [(= orientation 4) 5]
                    [(= orientation 5) 2]
                    [(= orientation 6) 4]
                )
            )
        ]
        [(= axis 2)
            (if (or (= orientation 2) (= orientation 4))
                orientation
                (cond
                    [(= orientation 1) 5]
                    [(= orientation 3) 6]
                    [(= orientation 5) 3]
                    [(= orientation 6) 1]
                )
            )
        ]
    )
)


;; ;TESTS
;; ; (print (= 2 (recalculateOrientation 1 0)) "\n")
;; ; (print (= 5 (recalculateOrientation 5 0)) "\n")
;; ; (print (= 1 (recalculateOrientation 1 1)) "\n")
;; ; (print (= 6 (recalculateOrientation 2 1)) "\n")
;; ; (print (= 5 (recalculateOrientation 1 2)) "\n")
;; ; (print (= 2 (recalculateOrientation 2 2)) "\n")

;rotations are performed using the left hand rule
;rotates left 4 cubes along x axis
(define (rotateX ispositive state)
	(cond 
        [(equal? ispositive #t) 
            (list(list
                (list (car (index state 4)) (recalculateOrientation (car (cdr (index state 4))) 0))
                (index state 1)
                (list (car (index state 0)) (recalculateOrientation (car (cdr (index state 0))) 0))
                (index state 3)
                (list (car (index state 6)) (recalculateOrientation (car (cdr (index state 6))) 0))
                (index state 5)
                (list (car (index state 2)) (recalculateOrientation (car (cdr (index state 2))) 0))
                (index state 7)
                                               
            ))
        ]

        [(equal? ispositive #f)

            ;rotate 3 times is equivalent to rotating once in the negative direction.
            (rotateX #t (index (rotateX #t (index (rotateX #t state) 0)) 0))
            
        ]
        
    )


)

;rotates bottom 4 cubes along y axis
(define (rotateY ispositive state)
   	(cond 
        [(equal? ispositive #t) 
            (list(list
                (index state 0)
                (index state 1)
                (index state 2)
                (index state 3)
                (list (car (index state 5)) (recalculateOrientation (car (cdr (index state 5))) 1))
                (list (car (index state 7)) (recalculateOrientation (car (cdr (index state 7))) 1))
                (list (car (index state 4)) (recalculateOrientation (car (cdr (index state 4))) 1))
                (list (car (index state 6)) (recalculateOrientation (car (cdr (index state 6))) 1))
                
                                               
            ))

        ]

        [(equal? ispositive #f)

            (rotateY #t (index (rotateY #t (index (rotateY #t state) 0)) 0))
            
        ]
        
    )

)

;rotates back 4 cubes along z axis
(define (rotateZ ispositive state)
	(cond 
        [(equal? ispositive #t) 
            (list (list
                (list (car (index state 1)) (recalculateOrientation (car (cdr (index state 1))) 2))
                (list (car (index state 5)) (recalculateOrientation (car (cdr (index state 5))) 2))
                (index state 2)
                (index state 3)
                (list (car (index state 0)) (recalculateOrientation (car (cdr (index state 0))) 2))
                (list (car (index state 4)) (recalculateOrientation (car (cdr (index state 4))) 2))
                (index state 6)
                (index state 7)
                
                                               
            ))

        ]

        [(equal? ispositive #f)

            ;rotate 3 times is equivalent to rotating once in the negative direction.
            (rotateZ #t (index (rotateZ #t (index (rotateZ #t state) 0)) 0))
            
        ]
        
    )
)







;; ;helper for rotate function
(define (rotateHelper char state)
    (cond
        [(char=? char #\x) (car (rotateX #t state))]
        [(char=? char #\X) (car (rotateX #f state))]
        [(char=? char #\y) (car (rotateY #t state))]
        [(char=? char #\Y) (car (rotateY #f state))]
        [(char=? char #\z) (car (rotateZ #t state))]
        [(char=? char #\Z) (car (rotateZ #f state))]
    )
)

;; ;parses a string for rotations
(define (rotate rotations state)
    (if (= (string-length rotations) 0)
        state
        (rotate (substring rotations 1 (string-length rotations)) (rotateHelper (string-ref rotations 0) state))
    )
)
;; ;TESTS
;; ; (print (equal? (rotate "x" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))) "\n")
;; ; (print (equal? (rotate "xyz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateZ #t (car (rotateY #t (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
;; ; (print (equal? (rotate "xXx" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #t (car (rotateX #f (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
;; ; (print (equal? (rotate "yXz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateZ #t (car (rotateX #f (car (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
;; ; (print (not (equal? (rotate "xXy" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #f (car (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)))))))) "\n")
;; ;------------------------------------------------------------





;-----------------------QUESTION 1.2-----------------------
;generates the successor states of the current given rubiks cube state
(define (generateSuccessorStates state prevMoves) 
    
     (list 
            (list 
            (car (rotateX #t state))
            (car (rotateX #f state))
            (car (rotateY #t state))
            (car (rotateY #f state))
            (car (rotateZ #t state))
            (car (rotateZ #f state))
            )
        
        
        
        ;creates a list of the individual characters of the string
        (list 
            (append prevMoves (list "x"))
            (append prevMoves (list "X"))
            (append prevMoves (list "y"))
            (append prevMoves (list "Y"))
            (append prevMoves (list "z"))
            (append prevMoves (list "Z"))
        
        ))

)

;; ;TESTS
;; ; (print (equal? (generateSuccessorStates '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())
;; ;         (list
;; ;             (list
;; ;                 (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateX #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateY #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateZ #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;             )
;; ;             '(("x") ("X") ("y") ("Y") ("z") ("Z"))
;; ;         )
;; ;     )
;; ; "\n")get


;-----------------------------QUESTION 2.1--------------------------

;finds all the states at a specific depth
(define (genStates n state moves)
    (cond
            [(not(equal? n 0))

            (list
                (removeLoop n n (getStates n state))
                (removeLoop n n (getMoves n moves))
            )

            ]

            [(equal? n 0)
                (list (list state) (list moves))
            ]


    )


)

;calls removeBrackets method n-1 times 
(define (removeLoop n t lst)
    (cond 
        [(not (equal? t 1))
        
            (removeLoop n (- t 1) (removeBrackets lst n 0 '()))
        
        ]

        [(equal? t 1)
            lst
        ]
    )

)

;removes extra brackets
(define (removeBrackets lst n t newlst)
    
    (cond 
        [(not(equal? t (expt 6 (- n 1))))
            (removeBrackets lst n (+ t 1) (append newlst 
                (list  
                    (index (index lst t) 0)
                    (index (index lst t) 1)
                    (index (index lst t) 2)
                    (index (index lst t) 3)
                    (index (index lst t) 4)
                    (index (index lst t) 5)   
                )))

        ]

        [(equal? t (expt 6 (- n 1)))
            newlst


        ]

    )

)

;gets the list of states
(define (getStates n state)
    (cond
        [(not(equal? n 0))
             
            (list 
                ;rotate in each direction for the given state to get all possible outcome states. This repeats till n = 0
                (getStates (- n 1) (index (rotateX #t state) 0))
                (getStates (- n 1) (index (rotateX #f state) 0))
                (getStates (- n 1) (index (rotateY #t state) 0))
                (getStates (- n 1) (index (rotateY #f state) 0))
                (getStates (- n 1) (index (rotateZ #t state) 0))
                (getStates (- n 1) (index (rotateZ #f state) 0))
            )
        ]

        [(equal? n 0)
            ;returns state
            state
        ]

    )

)

;gets the list of moves
(define (getMoves n moves)
    (cond
        [(not(equal? n 0))
            (list
                ;obtains the moves for each rotation
                (getMoves (- n 1) (addMove "x" moves))
                (getMoves (- n 1) (addMove "X" moves))
                (getMoves (- n 1) (addMove "y" moves))
                (getMoves (- n 1) (addMove "Y" moves))
                (getMoves (- n 1) (addMove "z" moves))
                (getMoves (- n 1) (addMove "Z" moves))
            )

        ]

        [(equal? n 0)
            
           moves
        ]

    )

)

;helper function to make a apend to a list of moves
(define (addMove m moves)
    (cond
    
        [(equal? m "x")
            (append moves (list "x"))
        ]

        [(equal? m "X")
            (append moves (list "X"))            
        ]

        [(equal? m "y")
            (append moves (list "y"))            
        ]

        [(equal? m "Y")
            (append moves (list "Y"))            
        ]

        [(equal? m "z")
            (append moves (list "z"))
        ]

        [(equal? m "Z")
            (append moves (list "Z"))
        ]
    
    )
    




)
;----------------------------------------------------------


;---------------------------QUESTION 3.1-----------------------
;Solves a rubiks cube using breadth first search. Can solve up to roughly 7 moves.
(define (solveCube solved initial n)
    ;'("Z", "Y", "X") ;;; *TODO* ;;;
    (if (equal? (in initial solved) #t)
        '()
        (helperSolveCube solved initial n (genStates n initial '()))
    )
)

;solveCube helper method. Receives genState so the method genStates does not get called twice
(define (helperSolveCube solved initial n genState)
    (if (equal? (helpFindState solved initial genState n 0) #t) ;checks if solved state is found
                (helpFindMove solved genState 0) ;finds the move          
                (solveCube solved initial (+ n 1))
    )


)

;checks list for state
(define (helpFindState solved states genState n p)
    (if (equal? p (length (car genState)))
        #f
        (if (equal? (in (index (car genState) p) solved) #t)
            #t
            (helpFindState solved states genState n (+ p 1))
        )
        
    )               
)

;checks list for corresponding move
(define (helpFindMove solved genState p)
    (if (equal? p (length (car genState)))
        '()
        (if (equal? (in (index (car genState) p) solved) #t) ;checks for the position of the state in solvedStates and then searches the move list in the same position to get the corresponding moves
            (index (car (cdr genState)) p) 
            (helpFindMove solved genState (+ p 1))
        )
        
    )               
)




;---------------------------------------------------------------------
;TESTS
; (print (equal? '("Z" "Y" "X") (solveCube solvedStates (rotate "xyz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0)) "\n")
; (print (equal? '("X") (solveCube solvedStates (rotate "x" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0)) "\n")
;---------------------------------------------------------------------
