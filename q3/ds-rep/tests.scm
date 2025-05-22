(module tests mzscheme
  
  (provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "-(44,33)" 11)
  
      ;; nested arithmetic
      (nested-arith-left "-(-(44,33),22)" -11)
      (nested-arith-right "-(55, -(22,11))" 44)
  
      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "-(x,1)" 9)
      (test-var-3 "-(1,x)" -9)
      
      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "-(x,foo)" error)
  
      ;; simple conditionals
      (if-true "if zero?(0) then 3 else 4" 3)
      (if-false "if zero?(1) then 3 else 4" 4)
      
      ;; test dynamic typechecking
      (no-bool-to-diff-1 "-(zero?(0),1)" error)
      (no-bool-to-diff-2 "-(1,zero?(0))" error)
      (no-int-to-if "if 1 then 2 else 3" error)

      ;; make sure that the test and both arms get evaluated
      ;; properly. 
      (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
      (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)
      
      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
      (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)

      ;; simple let
      (simple-let-1 "let x = 3 in x" 3)

      ;; make sure the body and rhs get evaluated
      (eval-let-body "let x = 3 in -(x,1)" 2)
      (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)

      ;; check nested let and shadowing
      (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
      (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
      (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)

      ;; simple applications
      (apply-proc-in-rator-pos "(proc(x=0) -(x,1)  x=30)" 29)
      (apply-simple-proc "let f = proc (x=0) -(x,1) in (f x=30)" 29)
      (let-to-proc-1 "(proc(f=0)(f x=30)  f=proc(x=0)-(x,1))" 29)


      (nested-procs "((proc (x=0) proc (y=0) -(x,y)  x=5) y=6)" -1)
      (nested-procs2 "let f = proc(x=0) proc (y=0) -(x,y) in ((f x=-(10,5)) y=6)"
        -1)
      
;;       (y-combinator-1 "
;; let fix =  proc (f=0)
;;             let d = proc (x=0) proc (z=0) ((f f=(x x=x)) z=z)
;;             in proc (n=0) ((f f=(d x=d)) z=n)
;; in let
;;     t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
;; in let times4 = (fix t4m)
;;    in (times4 3)" 12)
      (proc-named-args-1 "let p = proc(x=1,y= 2) -(x,y) in (p)" -1)

      (proc-named-args-2 "let p = proc(x=1,y=2) -(x,y) in (p  x=  5)" 3)

      (proc-named-args-3 "let p = proc(x=1 , y=2) -(x,y) in (p x=5,y=-1)" 6)

      (proc-named-args-4 "let p = proc(x=1, y=2) -(x,y) in (p y=-1, x=5)" 6)
      
      ;; Given y a value twice
      (proc-named-args-error-1 "let p = proc(x=1, y=2) -(x,y) in (p y=-1,y=5)" error) 
      
      ;; Given the parameter a, even though it is not defined in the proc
      (proc-named-args-error-1 "let p = proc(x=1,y=2,z=1) -(x,y) in (p x=-1,y=5,a=5)" error) 
      
      ;; Defined a function with x two times
      (proc-named-args-error-2 "let p = proc(x=1,y=2,x=3) -(x,y) in (p x=-1,y=5)" error)

      ;; Given a function too many arguments
      (proc-named-args-error-3 "let p = proc(x=1,y=2) -(x,y) in (p x=-1,y=5,z=5)" error)
      
      (proc-2-named-args-1 "
let p = proc(a=10,b=20,c=30) 
        -(c, -(a,b))
in
    (p)" 40)

    (proc-2-named-args-2 "
let p = proc(a=10,b=20,c=30) 
        -(c, -(a,b))
in
    (p b=50)" 70)

    (proc-2-named-args-3 "
let p = proc(a=10,b=20,c=30) 
        -(c, -(a,b))
in
    (p x=50)" error) ;; Because x is not defined in the proc

      (proc-2-named-args-4 "
let p = proc(a=10,b=20,c=30) 
        -(c, -(a,b))
in 
   (p c=100, a=35)" 85)

      

      ))
  )