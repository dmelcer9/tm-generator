#lang racket


(modifier firstWordBegin)
(modifier firstWordEnd)
(modifier secondWordEnd)
(char czero "0")
(char cone "1")
(char cplus "+")
(char cblank "_")

(define (move-items-right-until replInit condition)
  (subroutine
   ((prevChar (allChars) replInit))
   (while (not (condition prevChar))
          (swap prevChar)
          (move right))))

(define move-right-until-blank
  (subroutine
   ()
   (while (not (cblank? head))
          (move right))))

(define move-first-right-and-mark-ends
  (subroutine
   ()
   (move-items-right-until (firstWordBegin (czero)) cplus?)
   (move left)
   (mark firstWordEnd)
   move-right-until-blank
   (move left)
   (mark secondWordEnd)))

(define (dequals x)
  (λ (y) (equals x y)))

; Start at end of second word
(define addition
  (subroutine
   ((sumCarry (list "0" "1" "2") "0"))
   (while (not (firstWordEnd? head)) ; If we finish adding up everything but the last carry, we end up at end of first word
          ((cond-head
            (cone? ((cond-var sumCarry
                              ((dequals "0") ((set-var sumCarry "1")))
                              ((dequals "1") ((set-var sumCarry "2")))))))
           mark-and-move-to-corresponding-in-first-word
           (cond-head
            (czero? ((cond-var sumCarry ; 0 + 0 = 00 (no action needed)
                               ((dequals "1") ((set-var sumCarry "0") ; 0 + 1 = 01
                                               (change-base-letter cone)))
                               ((dequals "2") ((set-var sumCarry "1")))))) ; 0 + 2 = 10
            (cone? ((cond-var sumCarry  ; 1 + 0 = 01 (no action needed)
                              ((dequals "1") ((set-var sumCarry "1") ; 1 + 1 = 10
                                              (change-base-letter czero)))
                              ((dequals "2") ((set-var sumCarry "1"))))))) ; 1 + 2 = 11 (leave the one here and carry a 1 to next time)
           mark-and-move-to-corresponding-in-second-word))
   move-to-beginning
   (cond-var sumCarry
             ((dequals "1") ((change-base-letter cone))))))

(define prog
  (subroutine
   ()
   move-first-right-and-mark-ends
   addition
   cleanup
   halt))
          