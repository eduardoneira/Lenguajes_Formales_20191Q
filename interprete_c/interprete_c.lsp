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

(defun variables (mem)
    (if (null mem) 
        nil
        (cons (car mem) (variables (cddr mem)))
    )
)

(defun palabras_reservadas ()
    '(if while printf scanf else ++ -- += -= *= /= %= = + - * / % && || ! == != <= < > >=)
)

(defun es_operador (expr)
    (pertenece expr '(+ - * / %  && || ! == != <= < > >=))
)

(defun peso (operador)
    (cond 
        ((pertenece operador '(* / %)) 6)
        ((pertenece operador '(+ -)) 5)
        ((pertenece operador '(< <= > >=)) 4)
        ((pertenece operador '(== !=)) 3)
        ((eq operador '&&) 2)
        ((eq operador '||) 1)
    )
)

(defun operar (operador operando1 operando2)
    ; (print (list operando1 operador operando2))
    (cond
        ((eq operador '+) (+ operando1 operando2))
        ((eq operador '-) (- operando1 operando2))
        ((eq operador '*) (* operando1 operando2))
        ((eq operador '/) (/ operando1 operando2))
        ((eq operador '%) (% operando1 operando2))
        ((eq operador '<) (if (< operando1 operando2) 1 0))
        ((eq operador '>) (if (> operando1 operando2) 1 0))
        ((eq operador '<=) (if (<= operando1 operando2) 1 0))
        ((eq operador '>=) (if (>= operando1 operando2) 1 0))
        ((eq operador '==) (if (eq operando1 operando2) 1 0))
        ((eq operador '!=) (if (eq operando1 operando2) 0 1))
        ((eq operador '&&) (if (and (eq operando1 1) (eq operando2 1)) 1 0))
        ((eq operador '||) (if (or (eq operando1 1) (eq operando2 1)) 1 0))
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
        ((eq (car asigs) 'function) (cons (cadr asigs) (cons (cddr asigs) mem)))
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

(defun funciones (mem)
    (cond
        ((null mem) nil)
        ((listp (cadr mem)) (cons (car mem) (funciones (cddr mem))))
        (T (funciones (cddr mem)))
    )
)

; ampliar_mem (X Y Z) (1 2 3) nil => (x 1 y 2 z 3)
(defun ampliar_mem (parametros valores mem)
    (if (null parametros) 
        mem
        (ampliar_mem (cdr parametros) (cdr valores) (asignar (car parametros) (car valores) mem))
    )
)

; ejecutar (restar 2) (X 2)
(defun ejecutar_funcion (funcion mem)
    (car (ejec (cadr (buscar (car funcion) mem)) 
            nil 
            (ampliar_mem (car (buscar (car funcion) mem)) (mapcar (lambda (x) (valor x mem)) (cdr funcion)) mem)
        )
    )
)

; valor (x + 1) (x 2) nil nil
; valor x (x 2) => 2
; valor (+ 1) (x 2) nil (2)
; valor (1) (x 2) (+) (2)
; valor 1 (x 2) => 1
; valor nil (x 2) (+) (1 2)
; valor nil (x 2) nil (3) => 3

;valor ((restar 2)) (RESTAR ((X) ((X -= 1) (RETURN X))) FACT 1 N 0) 
(defun valor (expr mem &optional (operadores nil) (operandos nil))
    ; (print (list 'VALOR expr mem operadores operandos))
    (if (and (atom expr) (not (null expr))) 
        (if (numberp expr) expr (buscar expr mem))
        (if (null expr) 
            (if (null operadores) 
                (car operandos)
                (valor expr mem (cdr operadores) (cons (operar (car operadores) (cadr operandos) (car operandos)) (cddr operandos)))
            )
            (if (listp (car expr))
                (if (pertenece (caar expr) (funciones mem))
                    (valor (cdr expr) mem operadores (cons (ejecutar_funcion (car expr) mem) operandos))
                    (valor (cdr expr) mem operadores (cons (valor (car expr) mem) operandos))
                )
                (if (es_operador (car expr))
                    (if (null operadores)
                        (valor (cdr expr) mem (list (car expr)) operandos)
                        (if (< (peso (car operadores)) (peso (car expr)))
                            (valor (cdr expr) mem (cons (car expr) operadores) operandos)
                            (valor (cdr expr) mem (cons (car expr) (cdr operadores)) (cons (operar (car operadores) (cadr operandos) (car operandos)) (cddr operandos)))
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
    ; (print (list 'EJEC (car prg) ent mem sal))
    (if (null prg)
        (reverse sal)
        (cond
            ((eq (caar prg) 'printf) (ejec (cdr prg) ent mem (cons (valor (cdar prg) mem) sal)))
            ((eq (caar prg) 'scanf) (ejec (cdr prg) (cdr ent) (asignar (cadar prg) (car ent) mem) sal))
            ((eq (cadar prg) '=) (ejec (cdr prg) ent (asignar (caar prg) (valor (cddar prg) mem) mem) sal))
            ((pertenece (nth 1 (car prg)) '(+= -= *= /= %= ++ --)) (ejec (cons (list (caar prg) '= (caar prg) (simbolo (nth 1 (car prg))) (if (eq (length (car prg)) 2) 1 (valor (cddar prg) mem))) (cdr prg)) ent mem sal))
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
            ((eq (caar prg) 'return) (ejec nil ent mem (list (valor (cdar prg) mem))))
            (T (ejec (cdr prg) ent mem sal))
        )
    )
)

(defun todas_las_variables_validas (prg palabras_validas)
    (cond
        ((null prg) T)
        ((atom prg) (or (pertenece prg palabras_validas) (stringp prg) (numberp prg)))
        (T (reduce (lambda (x y) (and x y)) (mapcar (lambda (x) (todas_las_variables_validas x palabras_validas)) prg)))
    )
)

(defun run (prg ent &optional (mem nil))
    (cond 
        ((null prg) '(ERROR_NO_HAY_PROGRAMA))
        ((eq (caar prg) 'int) (run (cdr prg) ent (asigvar (cdar prg) mem)))
        ((eq (caar prg) 'function) (run (cdr prg) ent (asigvar (car prg) mem)))
        ((eq (caar prg) 'main) (if (todas_las_variables_validas (cadar prg) (append (palabras_reservadas) (variables mem)))
                                   (ejec (cadar prg) ent mem)
                                   '(ERROR_VARIABLE_NO_DECLARADA)
                               )
        )
        (T '(ERROR_INSTRUCCION_DESCONOCIDA))
    )
)

; PRUEBAS
; Factorial de 5 - FUNCIONA
(print (run '( (int n fact = 1)
                (function restar (x) ((x -= 1) (return x)))
                (main (
                    (scanf n)
                    (printf (restar 2))
                    (if (n < 0 )
                        ((printf "no existe fact de nro negativo" ))
                        else (
                            (while (n > 1) ( 
                                (fact = fact * n)
                                (n = (restar n))
                                )
                            )
                            (printf fact)
                            )
                    )
                )
                )
            )
            '(10)
        )
)

; Printf Basico - FUNCIONA
; (print (RUN '( (int a = 2 b = 3)
;                (main (
;                       (printf a)
;                      )
;                )
;              )
;              () 
;         )
; )

; Variable no declarada - FUNCIONA
; (print (RUN '( (int z = 2)
;                (main (
;                         (printf b)
;                      )
;                )
;              ) 
;             () 
;         )
; )

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

; While - FUNCIONA
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

; Variable no declarada complejo - FUNCIONA
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
;                                 (x = b * 6)
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

; Error programa vacio - FUNCIONA
; (print (run nil nil))

; Error instruccion desconocida - FUNCIONA
; (print (run '((int n = 0) (algo)) nil))