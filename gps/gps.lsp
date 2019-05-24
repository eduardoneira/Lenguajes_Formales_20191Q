( defun pertenece (elemento lista)
	(cond
		((null lista) nil)
		((eq elemento (car lista)) T)
		(T (pertenece elemento (cdr lista)))
	)
)

( defun aristas (nodo grafo)
	(let ((entrada_grafo (car grafo)))
    	(if (eq nodo (car entrada_grafo))
    		(cadr entrada_grafo)
    		(aristas nodo (cdr grafo))
    	)
    )
)

( defun eliminar (e L)
	( cond
		((null L) nil)
		((eq e (car L)) (eliminar e (cdr L)))
		(T (cons (car L) (eliminar e (cdr L))))
	)
)

( defun diferencia (A B)
	(cond
		((null B) A)
		(T (diferencia (eliminar (car B) A) (cdr B)))
	)
)

(defun breadth (adyacentes final grafo recorrido)
	(let ((camino (dfs (car adyacentes) final grafo recorrido)))
		(cond
			((null adyacentes) nil) 
			((eq final (car (last camino))) camino)
			(T (breadth (cdr adyacentes) final grafo recorrido))
		)
	)
)

( defun dfs (actual final grafo recorrido)
	(let ((aristas_actual (aristas actual grafo)))
		(let ((adyacentes (diferencia aristas_actual recorrido)))
			(cond 
				((null adyacentes) nil)
				((pertenece final aristas_actual) (append recorrido (list actual final)))
				(T (breadth adyacentes final grafo (append recorrido (list actual))))
			)
		)
	)
)


(setq grafo '((a (b f)) (b (a c)) (c (b d)) (d (c n e)) (e (d)) (f (g))(g (h)) (h (i l)) (i (m j)) (j (k)) (k (o))(l (b f)) (m (l c)) (n (j m)) (o (e n))))
(print (dfs 'a 'k grafo '()))

#| 
(setq grafo '((a (b c)) (b (a c d)) (c (a b d)) (d (b c e)) (e (d))))
(defun GPS (i f grafo dicc &optional (tray (list(list i))))
	(dfs i f grafo '())
)
 |#