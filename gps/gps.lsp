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
	( cond
		((null B) A)
		(T (diferencia (eliminar (car B) A) (cdr B)))
	)
)


( defun dfs (nodo_actual nodo_final grafo nodos_recorridos)
	( if (eq nodo_actual nodo_final)

	)
)

(defun GPS (i f grafo dicc &optional (tray (list(list i))))
	(dfs i f grafo '())
)



( defun aristas (nodo grafo)
	(let ((entrada_grafo (car grafo)))
		(print nodo )
		(print entrada_grafo)
    )
)


(setq grafo '((a (b f)) (b (a c)) (c (b d)) (d (c n e)) (e (d)) (f (g))(g (h)) (h (i l)) (i (m j)) (j (k)) (k (o))(l (b f)) (m (l c)) (n (j m)) (o (e n))))

(aristas 'd grafo)

