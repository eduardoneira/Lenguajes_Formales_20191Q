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
		(MY_EVAL (nth 2 fn) (ampliar_amb (nth 1 fn) lae amb))
	)
)

(defun MY_EVAL (exp amb)
	(if (atom exp) (if (null exp) 
					   nil
					   (if (numberp exp) exp (buscar exp amb))
				   )
		(cond 
			((eq (car exp) 'QUOTE) (cadr exp))
			((eq (car exp) 'and) (if (null (MY_EVAL (cadr exp) amb)) nil (MY_EVAL (caddr exp) amb)))
			((eq (car exp) 'or) (if (eq T (MY_EVAL (cadr exp) amb)) T (MY_EVAL (caddr exp) amb)))
			((eq (car exp) 'if) (if (null (MY_EVAL (cadr exp) amb)) (MY_EVAL (cadddr exp) amb) (MY_EVAL (caddr exp) amb)))
			; Agregar el cond
			; Si vamos a hacer el my_eval de la cabeza hay que agregar al ambiente las funciones primitivas
			(T (aplicar (MY_EVAL (car exp) amb) (mapcar #'(lambda (x) (MY_EVAL x amb)) (cdr exp))) amb)
		)
	)
)

