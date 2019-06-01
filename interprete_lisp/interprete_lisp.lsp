(defun buscar (var amb)
	(cond
		((null amb) var)
		((eq var (car amb)) (cadr amb))
		(T (buscar var (cddr amb)))
	)
)

;lae -> lista_de_argumentos_evaluados
(defun aplicar (fn lae amb)
	(if (atom fn)
		(cond
			((eq fn 'car) (car (car lae)))
			((eq fn 'car) (cdr (car lae)))
			((eq fn 'cons) (cons (car lae) (cadr lae)))
			;más funciones primitivas, al final va funciones definidas en el ambiente
			(T (aplicar (buscar fn amb) lae amb))
		)
		;solo puede ser un lambda si es una lista
		(evaluar (caddr fn) (ampliar_amb (cadr fn) lae amb))
	)
)

(defun evaluar (exp amb)
	(if (atom exp) (if (null exp) 
					   nil
					   (if (numberp exp) exp (buscar exp amb))
				   )
		(cond 
			((eq (car exp) 'QUOTE) (cadr exp))
			((eq (car exp) 'and) (if (null (evaluar (cadr exp) amb)) nil (evaluar (caddr exp) amb)))
			((eq (car exp) 'or) (if (eq T (evaluar (cadr exp) amb)) T (evaluar (caddr exp) amb)))
			((eq (car exp) 'if) (if (null (evaluar (cadr exp) amb)) (evaluar (cadddr exp) amb) (evaluar (caddr exp) amb)))
			((eq (car exp) 'cond) (if (eq T (evaluar (caadr exp) amb)) (evaluar (cadadr exp) amb) (evaluar (cons 'cond (cddr exp)) amb)))
			; Agregar el cond
			; Si vamos a hacer el my_eval de la cabeza hay que agregar al ambiente las funciones primitivas (evaluar (car exp) amb)
			(T (aplicar (car exp) (mapcar #'(lambda (x) (evaluar x amb)) (cdr exp))) amb)
		)
	)
)

;PRUEBAS

;(print (evaluar '2 nil))
;(print (evaluar 'nil nil))
;(print (evaluar 't nil))
;(print (evaluar 'A '(A 2)))
(print (evaluar 'B '(A 2 B 10)))