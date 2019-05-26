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

( defun caminos (actual final grafo recorrido)
	(let ((adyacentes (diferencia (aristas actual grafo) recorrido)) (recorrido_actualizado (append recorrido (list actual))))
		(cond
			((eq final (car (last recorrido_actualizado))) recorrido_actualizado)
			((null adyacentes) nil)
			(T (mapcar #'(lambda (x) (caminos x final grafo recorrido_actualizado)) adyacentes))
		)
	)
)

( defun limpiar_caminos (L)
	(cond
		((null L) nil)
		((not (listp (car L))) (list L))
		((listp L) (append (limpiar_caminos (car L)) (limpiar_caminos (cdr L))))
	)
)

(setq grafo '((a (b f)) (b (a c)) (c (b d)) (d (c n e)) (e (d)) (f (g))(g (h)) (h (i l)) (i (m j)) (j (k)) (k (o))(l (b f)) (m (l c)) (n (j m)) (o (e n))))
(print (limpiar_caminos (caminos 'a 'b grafo '())))

#| 
(defun GPS (i f grafo dicc &optional (tray (list(list i))))
(setq grafo '((a (b c)) (b (a c d)) (c (a b d)) (d (b c e)) (e (d))))
	(dfs i f grafo '())
)
 |#