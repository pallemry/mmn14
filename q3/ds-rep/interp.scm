(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the PROC language, using the data structure
  ;; representation of procedures.

  ;; The \commentboxes are the latex code for inserting the rules into
  ;; the code in the book. These are too complicated to put here, see
  ;; the text, sorry. 

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        ;; (proc-exp (var body)
        ;;   (proc-val (procedure var body env)))

        ;; (call-exp (rator rand)
        ;;   (let ((proc (expval->proc (value-of rator env)))
        ;;         (arg (value-of rand env)))
        ;;     (apply-procedure proc arg)))

        (proc-multi-named-exp (named-vars body)
          (proc-val (procedure named-vars body env)))
        
        (call-multi-named-exp (rator named-vars)
          (let ((proc (expval->proc (value-of rator env))))
            (apply-procedure-named proc named-vars)))

        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc1 val)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of body (extend-env var val saved-env))))))

  ;; has-duplicate-ids? : NamedVarList -> Bool
  ;; checks for duplicate ids in a list of named variables
  (define (extract-ids named-vars)
  (map (lambda (nv)
         (cases named-var nv
           (a-named-var (id val) id)))
       named-vars))
  
  (define (has-duplicate-ids? named-vars)
  (letrec ([check-dups
            (lambda (ids)
              (cond
                [(null? ids) #f]
                [(member (car ids) (cdr ids)) #t]
                [else (check-dups (cdr ids))]))])
    (check-dups (extract-ids named-vars))))

  (define (all-ids-exist? origin-vars supplied-vars)
  (let ([origin-ids (extract-ids origin-vars)]
        [supplied-ids (extract-ids supplied-vars)])
    (letrec ([check-all
              (lambda (ids)
                (cond
                  [(null? ids) #t]
                  [(member (car ids) origin-ids) (check-all (cdr ids))]
                  [else #f]))])
      (check-all supplied-ids))))

  (define extend-env-named
    (lambda (named-vars saved-env)
      (if (null? named-vars)
        saved-env
        (let ((first-var (car named-vars)))
          (cases named-var first-var
            (a-named-var (var val)
              (extend-env-named (cdr named-vars)
                (extend-env var (value-of val saved-env) saved-env))))))))

  (define apply-procedure-named
    (lambda (proc1 vals)
      (cases proc proc1
        (procedure (params body saved-env)
          (if (> (length vals) (length params))
            (eopl:error 'apply-procedure-named "Too many named arguments")
          (if (has-duplicate-ids? vals)
            (eopl:error 'apply-procedure-named "Duplicate named arguments")
          (if (not (all-ids-exist? params vals))
            (eopl:error 'apply-procedure-named "Some named arguments do not exist in the function defintion")
          (let* ((env-proc (extend-env-named params saved-env))
                 (new-env (extend-env-named vals env-proc)))
            (value-of body new-env)))))))))

  )
