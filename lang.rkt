#lang racket

; List-of TChar
(define base-chars '())

; List-of [TChar -> TChar]
(define modifiers '())

; List-of String
(define modifiers-raw '())

(define state 0)

(struct tchar [base modifiers] #:transparent)

; TODO order for printing

; TChar -> String
(define (pretty-tchar tc)
  (string-append (tchar-base tc) (foldr (λ (mod reststr)
                                          (if (member mod (tchar-modifiers tc))
                                              
                                              (string-append reststr mod)
                                              reststr))
                                        ""
                                        modifiers-raw)))

; [X] [Lo X] -> [Lo [Lo X]]
(define (powerlist s)
  (cond
    [(empty? s) '(())]
    [(cons? s) (begin
                 (define rst (powerlist (rest s)))
                 (append rst (map (λ (x) (cons (first s) x)) rst)))]))
    

(define (all-tc)
  (map (λ (x) (foldr (λ (modifier ch) (modifier ch)) (first x) (second x))) ;; Apply all modifiers
       (cartesian-product base-chars (powerlist modifiers)))) ;; Get all combinations of chars and modifiers

(define-for-syntax (add-sym stx str)
  (datum->syntax stx (string->symbol (string-append (symbol->string (syntax->datum stx)) str))))

(define-syntax (modifier stx)
  (syntax-case stx ()
    [(modifier name val)
     #`(begin (define name (λ (tc) (if (list? (member val (tchar-modifiers tc))) tc (tchar (tchar-base tc) (cons val (tchar-modifiers tc))))))
              (set! modifiers (cons name modifiers))
              (set! modifiers-raw (cons val modifiers-raw))
              (define (#,(add-sym #'name "?") tc)
                (list? (member val (tchar-modifiers tc)))))]))

(define-syntax (char stx)
  (syntax-case stx ()
    [(char name val)
     #`(begin (define name (tchar val '()))
              (set! base-chars (cons name base-chars))
              (define (#,(add-sym #'name "?") tc)
                (string=? (tchar-base tc) val)))]))

#;(define-syntax (while stx)
    (syntax-case stx ()
      [(while )]))

; A State is a Number

; -> State
(define (next-state)
  (set! state (add1 state))
  state)

(char czero "0")
(char cone "1")
;(char cplus "+")
(char cblank "_")
;(modifier Test "*")





; A Direction is one of:
; - "left"
; - "right"
; - "stay"
(define left "left")
(define right "right")
(define stay "stay")

; A Condition is a (list [Value Context -> Bool] [List-of Instruction])

; A SubVar is a (sv Symbol [List-of Value] Value)
(struct sv [name possible current])

; An Instruction is one of:
; - (while Condition [List-of Instruction])
(struct while [condition body])
; - (move Direction)
(struct move [direction])
; - (swap Symbol) ; NOTE: Assumes possible ID values are equal to (allChars)
(struct swap [id])
; - (mark Modifier)
(struct mark [modifier])
; - (tcond [List-of Condition])
(struct tcond [conditions])
; - (set-var Symbol Value)
(struct set-var [id val])
; - (subroutine [List-of SubVar] [List-of Instruction])
(struct sub [subvars instrs])



#|
; - (cond-head [List-of Condition])
(struct cond-head [conditions])
; - (cond-var ID [List-of Condition])
(struct cond-var [id conditions])
|#
  
; A Rule is:
; (rule State Value State Value Direction)
(struct rule [from-state read to-state write move] #:transparent)

; Rule -> String
(define (pretty-rule rule)
  (string-append
   (number->string (rule-from-state rule))
   " "
   (pretty-tchar (rule-read rule))
   " -> "
   (number->string (rule-to-state rule))
   " "
   (pretty-tchar (rule-write rule))
   " "
   (rule-move rule)))

(define (display-rules rules)
  (begin
    (map (compose displayln pretty-rule) rules)
    (void)))
  
; A ContextDesc is:
; (ctxd Symbol [List-of Value])
(struct ctxd [name possible] #:transparent)
; values is all possible values, like a SubVar

; A ContextItem is:
; (ctxi Symbol Value)
(struct ctxi [name value] #:transparent)
; Represents a single item in a context

; [Lo ContextItem] Symbol Value -> [Lo ContextItem]
; NOTE: Assumes s is in list exactly once
(define (set-context loci s v)
  (map (λ (ci) (if (symbol=? (ctxi-name ci) s)
                   (ctxi s v)
                   ci)) loci))

; [Lo ContextItem] Symbol -> Value
(define (get-context loci s)
  (ctxi-value (first (filter (λ (x) (symbol=? (ctxi-name x) s)) loci))))

; SubVar [Lo ContextItem] -> [Lo ContextItem]
(define (add-sv-to-cis subv loc)
  (cons (ctxi (sv-name subv) (sv-current subv)) loc))

; SubVar [Lo ContextDesc] -> [Lo ContextDesc]
(define (add-sv-to-cds subv loc)
  (cons (ctxd (sv-name subv) (sv-possible subv)) loc))

; ContextDesc -> [Lo ContextItem]
(define (get-all-possible-ctx-instances cd)
  (map (λ (it) (ctxi (ctxd-name cd) it))
       (ctxd-possible cd)))

; [List-of ContextDesc] -> [Lo '(Context State)]
(define (gen-all-ctx-combinations ctx)
  (cond [(empty? ctx) '(())]
        [else (begin
                (define rst-combs (gen-all-ctx-combinations (rest ctx)))
                (apply append
                       (map
                        (λ (fstval)
                          (map (λ (rstcomb) (cons fstval rstcomb)) rst-combs))
                        (get-all-possible-ctx-instances (first ctx)))))]))

(module+ test
  (require rackunit)

  (check-equal? (get-all-possible-ctx-instances (ctxd 'T '(1 2))) (list (ctxi 'T 1) (ctxi 'T 2)))
  
  (check-equal? (gen-all-ctx-combinations '()) '(()))
  (check-equal? (gen-all-ctx-combinations `(,(ctxd 'T '(1 2))))  `((,(ctxi 'T 1)) (,(ctxi 'T 2))))
  )

; [List-of ContextDesc] -> [Context -> State]
(define (gen-states-for-contexts contexts)
  (define ctxt-combs (gen-all-ctx-combinations contexts))
  (define ctxt-states (map (λ (cc) (list cc (next-state))) ctxt-combs))
  (λ (ctx)
    (begin
      (define context-matches (filter (λ (y) (equal? (first y) ctx)) ctxt-states))
      (define first-match (first context-matches))
      (define match-state (second first-match))
      match-state)))

(define empty-context '())

(define init-state (gen-states-for-contexts empty-context))
(define halt-state (gen-states-for-contexts empty-context))

; compile-instr-list: [List-of ContextDesc] Instruction [Context -> State] [Context -> State] -> [List-of Rule]
(define (compile-instr-list contexts instrs before-state after-state)
  (define all-ctx-tc (cartesian-product (gen-all-ctx-combinations contexts) (all-tc)))
  (define context first)
  (define tc second)
  (cond
    [(empty? instrs) ; Just jump to after-state
     (map
      (λ (val) (rule (before-state (context val)) (tc val) (after-state (context val)) (tc val) stay))
      all-ctx-tc)]
    [(empty? (rest instrs)) ; Go to after-state through the only instruction here
     (compile-instr contexts (first instrs) before-state after-state)]
    [(cons? instrs) ; Create a temp state, first goes from before to temp-state, rest goes from temp to after.
     (define after-first-state (gen-states-for-contexts contexts))
     (define first-instrs (compile-instr contexts (first instrs) before-state after-first-state))
     (define rest-instrs (compile-instr-list contexts (rest instrs) after-first-state after-state))
     (append first-instrs rest-instrs)]))
    

; compile-instr: [List-of ContextDesc] Instruction [Context -> State] [Context -> State] -> [List-of Rule]
(define (compile-instr contexts instr before-state after-state)
  ; Cartesian product: [List-of '([List-of ContextItem], TChar)]
  (define all-ctx-tc (cartesian-product (gen-all-ctx-combinations contexts) (all-tc)))
  (define context first)
  (define tc second)
  (cond
    [(while? instr)
     (begin
       (define while-succ-state (gen-states-for-contexts contexts)) ; The state that the while loop starts in, given the current local variables
       ; How to get while-succ state to go into for other contexts?
       ; We end up at a different before-state depending on the context after the body finishes
       ; Nested while loops- will this while-succ-state
       (define cond-rules (map (λ (val) (rule
                                         (before-state (context val))
                                         (tc val)
                                         (if ((while-condition instr) (tc val) (context val)) (while-succ-state (context val)) (after-state (context val)))
                                         (tc val)
                                         stay))
                               all-ctx-tc))
       ;; TODO context for before state?
       (define body-rules (compile-instr-list contexts (while-body instr) while-succ-state before-state)) ; After body is complete, go back to initial state, cond-rules will then apply
       ;; Possible cases- body doesn't change local vars, no problem
       ;; Body introduces local var, changes, goes out of scope anyways so we don't care
       ;; Body modifies var in outside scope- Inner context will be different.
       ;; When it is done running, it goes back to before-state, which already handles different contexts
       ;; If the local variables change in a nested while, it may need to go back to while-succ-state, but a different "real" state (so that is also Context->State)
       (append cond-rules body-rules))]
    [(move? instr) ; Move without editing, output rule for each possible val on tape
     (map (λ (val) (rule (before-state (context val)) (tc val) (after-state (context val)) (tc val) (move-direction instr))) all-ctx-tc)]
    [(swap? instr)
     (map
      (λ (val)
        (begin
          (define ctx (context val))
          (define tch (tc val))
          (rule (before-state ctx)
                tch
                (after-state (set-context ctx (swap-id instr) tch)) ; Set the variable to be what is at the head
                (get-context ctx (swap-id instr)) ; Set head to be at variable
                stay)))
      all-ctx-tc)]
    [(mark? instr)
     (map (λ (val) (rule (before-state (context val)) (tc val) (after-state (context val)) ((mark-modifier instr) (tc val)) stay)) all-ctx-tc)]
    [(tcond? instr)
     (begin
       ; Compile all rules for all contexts
       ; [List-of '([Context -> State] [Value Context -> Bool] [List-of Rule])]
       (define conds-compiled (map (λ (condition) (begin
                                                    (define rule-start (gen-states-for-contexts contexts))
                                                    (define rule-compiled (compile-instr-list contexts (second condition) rule-start after-state))
                                                    (list rule-start (first condition) rule-compiled))) (tcond-conditions instr)))
       (define rules-compiled (apply append (map third conds-compiled)))
       ; Go to start state for each rule
       (define go-into-rules
         (map (λ (val)
                (begin
                  (define ctx (context val))
                  (define tch (tc val))
                  (define matching-rules
                    (filter
                     (λ (condition) ((second condition) tch ctx))
                     conds-compiled))
                  (if (empty? matching-rules)
                      (rule (before-state ctx) tch (after-state ctx) tch stay) ; No rules match, just leave context same and go to after-state
                      (local ; A rule matches. Go into the state and let the rule take over
                        ((define rule-to-follow (first matching-rules))
                         (define state-before-rule (first rule-to-follow)))
                        (rule (before-state ctx) tch (state-before-rule ctx) tch stay)))))
              all-ctx-tc))
       (append go-into-rules rules-compiled))]
    [(set-var? instr)
     (map (λ (val)
            (begin
              (define ctx (context val))
              (define tch (tc val))
              (define new-ctx (set-context ctx (set-var-id instr) (set-var-val instr)))
              (rule (before-state ctx) tch (after-state new-ctx) tch stay)))
          all-ctx-tc)]
    [(sub? instr)
     (begin
       (define newvars (sub-subvars instr))
       (define instrs (sub-instrs instr))
       (define new-contexts (foldr add-sv-to-cds contexts newvars))
       (define new-start (gen-states-for-contexts new-contexts))
       (define new-end (gen-states-for-contexts new-contexts))
       (define instrs-compiled (compile-instr-list new-contexts instrs new-start new-end))
       (define all-new-ctx-tc (cartesian-product (gen-all-ctx-combinations new-contexts) (all-tc)))
       (define instrs-map-into  ; As we enter the context, add all the variables as their defaults
         (map 
          (λ (val)
            (begin
              (define ctx (context val))
              (define tch (tc val))
              (define ct2  (foldr add-sv-to-cis ctx newvars))
              (rule (before-state ctx) tch (new-start ct2) tch stay)))
          all-ctx-tc))
       (define instrs-map-out  ; As we exit the context, pop the variables
         (map
          (λ (val)
            (begin
              (define ctx (context val))
              (define tch (tc val))
              (rule (new-end ctx) tch (after-state (foldr (λ (x y) (rest y)) ctx newvars)) tch stay)))
          all-new-ctx-tc))
       (append instrs-map-into instrs-compiled instrs-map-out))]))
                  
    
          
              
     
                                             
