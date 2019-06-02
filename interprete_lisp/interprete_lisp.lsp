(defun pertenece (elemento lista)
	(cond
		((null lista) nil)
		((eq elemento (car lista)) T)
		(T (pertenece elemento (cdr lista)))
	)
)

(defun agrupar (A B) 
	(if (null A)
		nil
		(append (list (car A) (car B)) (agrupar (cdr A) (cdr B)))
	)
)

(defun eliminar_de_amb (params amb)
	(cond
		((null amb) nil)
		((pertenece (car amb) params) (eliminar_de_amb params (cddr amb)))
		(T (append (list (car amb) (cadr amb)) (eliminar_de_amb params (cddr amb))))
	)
)

(defun ampliar_amb (params valores amb)
	(append (eliminar_de_amb params amb) (agrupar params valores))
)

(defun buscar (var amb)
	(cond
		((null amb) var)
		((eq var (car amb)) (cadr amb))
		(T (buscar var (cddr amb)))
	)
)

(defun aplicar (fn lae amb)
	(if (atom fn)
		(cond
			((eq fn 'car) (caar lae))
			((eq fn 'cdr) (cdar lae))
			((eq fn 'cons) (cons (car lae) (cadr lae)))
			((eq fn 'list) lae)
			;Relaciones
			((eq fn 'eq) (eq (car lae) (cadr lae)))
			((eq fn '<) (eq (car lae) (cadr lae)))
			((eq fn '>) (eq (car lae) (cadr lae)))
			;Aritmeticas
			((eq fn '+) (+ (car lae) (cadr lae)))
			((eq fn '-) (- (car lae) (cadr lae)))
			((eq fn '*) (* (car lae) (cadr lae)))
			((eq fn '/) (/	 (car lae) (cadr lae)))
			;Mapcar y reduce
			((eq fn 'mapcar) (mapcar (lambda (x) (aplicar (car lae) (list x) amb)) (cadr lae)))
			;Funciones definidas en el ambiente
			(T (aplicar (buscar fn amb) lae amb))
		)
		;Si es una lista la fn, entonces es una lambda
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
			((eq (car exp) 'lambda) exp)
			; Si vamos a hacer el evaluar de la cabeza hay que agregar al ambiente las funciones primitivas (evaluar (car exp) amb)
			(T (aplicar (car exp) (mapcar (lambda (x) (evaluar x amb)) (cdr exp)) amb))
		)
	)
)

;PRUEBAS

;PASAN
;(print (evaluar '2 nil))
;(print (evaluar 'nil nil))
;(print (evaluar 't nil))
;(print (evaluar 'A '(A 2)))
;(print (evaluar 'B '(A 2 B 10)))
;(print (evaluar '(quote A) nil))
;(print (evaluar '(quote 1) nil))
;(print (evaluar '(quote (car a)) nil))
;(print (evaluar '(quote ((2 3) (4 5))) nil))
;(print (evaluar '(and (or t nil) t) nil))
;(print (evaluar '(and (or t nil) (or nil nil)) nil))
;(print (evaluar '(or (or t nil) (or nil nil )) nil))
;(print (evaluar '(car (list a 2 3)) '(a 100)))
;(print (evaluar '(cdr (list a b c)) '(a 100 b 99 c 98)))
;(print (evaluar '((lambda (x) (* x 2)) 2) nil))
;(print (evaluar '((lambda (x y) (+ (* x 2) y)) 2 4) nil))
;(print (evaluar '(lambda (x) (* x 2)) nil))
;(print (evaluar '(mapcar (lambda (x) (cons x (cdr '(3 4 5)))) '(1 2 3)) nil))
;(print (evaluar '(mapcar 'car (quote ( (2 3) (4 5 )))) nil))
;(print (evaluar '(fact 5) '(fact (lambda (n) (if (eq n 0) 1 (* n (fact (- n 1))))))))
;(print (evaluar '(mapcar 'fact (quote ( 2 3 4 5 )))
;				'(fact (lambda (n) (if (eq n 0) 1 (* n (fact (- n 1))))))))