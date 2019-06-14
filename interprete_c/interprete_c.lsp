(defun pertenece (elemento lista)
    (cond
        ((null lista) nil)
        ((eq elemento (car lista)) T)
        (T (pertenece elemento (cdr lista)))
    )
)

(defun eliminar_de_mem (nombre mem)
    (cond
        ((null mem) nil)
        ((eq (car mem) nombre) (eliminar_de_mem nombre (cddr mem)))
        (T (append (list (car mem) (cadr mem)) (eliminar_de_mem nombre (cddr mem))))
    )
)

(defun asignar (nombre valor mem)
    (append (eliminar_de_mem nombre mem) (list nombre valor))
)

(defun es_operador (expr)
    (pertenece expr '(+ - * / %  && || ! == != <= < > >=))
)

(defun peso (operador)
    (cond 
        ((pertenece operador '(* / %)) 1)
        ((pertenece operador '(+ -)) 2)
        ((pertenece operador '(< <= > >=)) 3)
        ((pertenece operador '(== !=)) 4)
        ((eq operador '&&) 5)
        ((eq operador '||) 6)
    )
)

(defun operar (operador operando1 operando2)
    ; (print (list operando1 operador operando2))
    (cond
        ((eq operador '+) (+ operando1 operando2))
        ((eq operador '-) (- operando1 operando2))
        ((eq operador '*) (* operando1 operando2))
        ((eq operador '-) (- operando1 operando2))
        ((eq operador '%) (% operando1 operando2))
        ((eq operador '<) (if (< operando1 operando2) 1 0))
        ((eq operador '>) (if (> operando1 operando2) 1 0))
        ((eq operador '<=) (if (<= operando1 operando2) 1 0))
        ((eq operador '>=) (if (>= operando1 operando2) 1 0))
        ((eq operador '==) (if (eq operando1 operando2) 1 0))
        ((eq operador '!=) (if (eq operando1 operando2) 0 1))
        ((eq operador '&&) (if (and (eq operando1 1) (eq operando2 1)) 1 0))
        ((eq operador '||) (if (and (eq operando1 0) (eq operando2 0)) 0 1))
    )
)

(defun buscar (var mem)
    (cond
        ((null mem) 'ERROR_VARIABLE_NO_DECLARADA)
        ((eq var (car mem)) (cadr mem))
        (T (buscar var (cddr mem)))
    )
)

(defun asigvar (asigs mem)
    (cond
        ((null asigs) mem)
        ((eq (cadr asigs) '=) (asigvar (cdddr asigs) (cons (car asigs) (cons (caddr asigs) mem))))
        (T (asigvar (cdr asigs) (cons (car asigs) (cons 0 mem))))
    )
)

(defun simbolo (sim)
    (cond
        ((or (eq '+= sim) (eq '++ sim)) '+)
        ((or (eq '-= sim) (eq '-- sim)) '-)
        ((eq '*= sim) '*)
        ((eq '/= sim) '/)
        ((eq '%= sim) '%)
    )
)

(defun valor (expr mem &optional (operadores nil) (operandos nil))
    (print (list 'VALOR expr mem operadores operandos))
    (if (and (atom expr) (not (null expr))) (if (numberp expr) expr (buscar expr mem))
        (if (null expr) 
            (if (null operadores) 
                (car operandos)
                (valor expr mem (cdr operadores) (cons (operar (car operadores) (cadr operandos) (car operandos)) (cddr operandos)))
            )
            (if (listp (car expr))
                (valor (cdr expr) mem operadores (cons (valor (car expr) mem) operandos))
                (if (es_operador (car expr))
                    (if (null operadores)
                        (valor (cdr expr) mem (list (car expr)) operandos)
                        (if (< (peso (car operadores)) (peso (car expr)))
                            (valor (cdr expr) mem (cons (car expr) operadores) operandos)
                            (valor (cdr expr) mem (cdr operadores) (cons (operar (car expr) (cadr operandos) (car operandos)) (cddr operandos)))
                        )
                    )
                    ;es operando
                    (valor (cdr expr) mem operadores (cons (valor (car expr) mem) operandos))
                )
            )
        )
    )
)

(defun ejec (prg ent mem &optional (sal nil))
    (print (list 'EJEC (car prg) ent mem sal))
    (if (null prg)
        (reverse sal)
        (cond
            ((eq (caar prg) 'printf) (ejec (cdr prg) ent mem (cons (valor (cdar prg) mem) sal)))
            ((eq (caar prg) 'scanf) (ejec (cdr prg) (cdr ent) (asignar (cadar prg) (car ent) mem) sal))
            ((eq (cadar prg) '=) (ejec (cdr prg) ent (asignar (caar prg) (valor (cddar prg) mem) mem) sal))
            ((pertenece (nth 1 (car prg)) '(+= -= *= /= %= ++ --)) (ejec (cons (append (list (caar prg) '= (caar prg) (simbolo (nth 1 (car prg)))) (if (eq (length (car prg)) 2) '(1) (cddar prg))) (cdr prg)) ent mem sal))
            ((pertenece (caar prg) '(++ --)) (ejec (cons (reverse (car prg)) (cdr prg)) ent mem sal))
            ((eq (caar prg) 'if) (if (not (eq (valor (nth 1 (car prg)) mem) 0)) 
                                        (ejec (append (nth 2 (car prg)) (cdr prg)) ent mem sal)
                                        (if (eq (length (car prg)) 5) 
                                            (ejec (append (nth 4 (car prg)) (cdr prg)) ent mem sal)
                                            (ejec (cdr prg) ent mem sal)
                                        )
                                    )
            )
            ((eq (caar prg) 'while) (if (eq (valor (nth 1 (car prg)) mem) 0) 
                                        (ejec (cdr prg) ent mem sal)
                                        (ejec (append (nth 2 (car prg)) prg) ent mem sal)
                                     )
            )
        )
    )
)

(defun run (prg ent &optional (mem nil))
    (if (null prg) 
        "NO HAY PROGRAMA"
        (if (eq (caar prg) 'int)
            (run (cdr prg) ent (asigvar (cdar prg) mem))
            (if (eq (caar prg) 'main)
                (ejec (cadar prg) ent mem)
                "ERROR - NO HAY MAIN"
            )
        )
    )
)

; PRUEBAS
; Factorial de 5 - FUNCIONA
;
; (print (run '( (int n fact = 1)
;                 (main (
;                     (scanf n)
;                     (if (n < 0 )
;                         ((printf "no existe fact de nro negativo" ))
;                         else (
;                             (while (n > 1) ( 
;                                 (fact = fact * n)
;                                 (n -- )
;                                 )
;                             )
;                             (printf fact)
;                             )
;                     )
;                 )
;                 )
;             )
;             '(5)
;         )
; )

; Printf Basico - FUNCIONA
;
; (print (RUN '( (int a = 2 b = 3)
;                (main (
;                       (printf a)
;                      )
;                )
;              )
;              () 
;         )
; )

; Variable no declarada - NO FUNCIONA
;
; (print (RUN '( (int z = 2)
; (main (
; (printf b)
; )
; )
; ) () ) )

; Prueba no devuelve nada - FUNCIONA
; (print (RUN '( (int a = 6)
;                (main (
;                        (if (a == 2)
;                            ( (printf (a + 1)) )
;                        )
;                      )
;                )
;                ) 
;             () 
;         )
; )

; If y printf - FUNCIONA
; (print (RUN '( (int a = 2)
;                (main (
;                        (if (a == 2)
;                             ( (printf (a + 1)) )
;                        )
;                      )
;                )
;             )
;             () 
;         )
; )

; Scanf y printf - FUNCIONA
; (print (RUN '( (int a = 2 b)
;                (main (
;                         (scanf b)
;                         (a = b + 3)
;                         (printf a)
;                      )
;                )
;             ) 
;             '(5) 
;         )
; )

; Varios printf complejos - FUNCIONA
; (print (RUN '( (int a = 2 b)
;                (main (
;                         (a = (a + 1) * 4)
;                         (b -= 5)
;                         (a += 3)
;                         (printf a)
;                         (scanf a)
;                         (printf a)
;                         (printf b)
;                      )
;                )
;             ) 
;             '(6) 
;         )
; )

; (print (RUN '( (int x y p = 10)
;                (int r)
;                (main ( 
;                         (x = p + 10)
;                         (p ++)
;                         (++ x)
;                         (x *= p - 4)
;                         (if (x < p) ( 
;                                         (printf x + p)
;                                         (scanf y)
;                                     )
;                          else ( 
;                                 (x = x * 6)
;                                 (printf p * p)
;                               )
;                         )
;                         (while (x > p * 10) (
;                                                (printf x + p)
;                                                (scanf y)
;                                                (printf y)
;                                                (x -= y)
;                                             )
;                         )
;                      )
;                )
;             ) 
;             '(700 100) 
;         )
; )

(print (valor '(4 * 2 / 1 + 3 * 2) nil))