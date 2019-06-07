(defun asigvar (asigs mem)
	(cond
		((null asigs) nil)
		((eq (cadr asigs) '=) (asigvar (cdddr asigs) (cons (car asigs) (cons (caddr asigs) mem))))
		(T (asigvar (cdr asigs) (cons (car asigs) (cons nil mem))))
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
	(if (atom expr) (if (numberp expr) expr (buscar expr mem))
		(if (null expr) 
			(if (null operadores) 
				(car operandos)
				(valor expr mem (cdr operadores) (cons (operar (car operadores) (cadr operandos) (car operandos)) (cddr operandos)))
			)
			(if (es_operador (car expr))
				(if (null operadores)
					(valor (cdr expr) mem (list (car expr)) operandos)
					(if (< (peso (car operadores)) (peso (car expr)))
						(valor (cdr expr) mem (cons (car expr) operadores) operandos)
						(valor (cdr exp) mem (cdr operadores) (cons (operar (car expr) (cadr operandos) (car operandos)) (cddr operandos)))
					)
				)
				;es operando
				(valor (cdr expr) mem operadores (cons (valor (car expr) mem) operandos))
			)
		)
	)
)

(defun ejec (prg ent mem &optional (sal nil))
	(if (null prg)
		(reverse sal)
		(cond
			((eq (caar prg) 'printf) (ejec (cdr prg) ent mem (cons (valor (cdar prg) mem) sal)))
			((eq (caar prg) 'scanf) (ejec (cdr prg) (cdr ent) (asignar (cadar prg) (car ent) mem) sal))
			((eq (cadar prg) '=) (ejec (cdr prg) ent (asignar (caar prg) (valor (cddar prg) mem) mem) sal))
			((pert (nth 1 (car prg)) '(+= -= *= /= %= ++ --)) (ejec (cons (list (caar prg) '= (caar prg) (simbolo (nth 1 (car prg))) (if (eq (length (car prg)) 2) 1 (cddar prg))) (cdr prg)) ent mem sal))
			((pert (nth 1 (car prg)) '(++ --)) (ejec (cons (reverse (car prg)) (cdr prg)) ent mem sal))
			((eq (caar prg) 'if) (if (not (eq (valor (nth 1 (car prg)) mem)) 0) 
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