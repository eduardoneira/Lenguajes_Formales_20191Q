(defun pertenece (elemento lista)
	(cond
		((null lista) nil)
		((eq elemento (car lista)) T)
		(T (pertenece elemento (cdr lista)))
	)
)

(defun aristas (nodo grafo)
	(let ((entrada_grafo (car grafo)))
    	(if (eq nodo (car entrada_grafo))
    		(cadr entrada_grafo)
    		(aristas nodo (cdr grafo))
    	)
    )
)

(defun eliminar (e L)
	( cond
		((null L) nil)
		((eq e (car L)) (eliminar e (cdr L)))
		(T (cons (car L) (eliminar e (cdr L))))
	)
)

(defun diferencia (A B)
	(cond
		((null B) A)
		(T (diferencia (eliminar (car B) A) (cdr B)))
	)
)

(defun todos_los_caminos (actual final grafo recorrido)
	(let ((adyacentes (diferencia (aristas actual grafo) recorrido)) (recorrido_actualizado (append recorrido (list actual))))
		(cond
			((eq final (car (last recorrido_actualizado))) recorrido_actualizado)
			((null adyacentes) nil)
			(T (mapcar #'(lambda (x) (todos_los_caminos x final grafo recorrido_actualizado)) adyacentes))
		)
	)
)

(defun limpiar_caminos (L)
	(cond
		((null L) nil)
		((not (listp (car L))) (list L))
		((listp L) (append (limpiar_caminos (car L)) (limpiar_caminos (cdr L))))
	)
)

(defun minima_distancia (caminos)
	(reduce #'(lambda (x y) (if (< (length x) (length y)) (length x) (length y))) caminos)
)

(defun seleccionar_caminos_minimos (caminos distancia)
	(cond 
		((null caminos) nil)
		((> (length (car caminos)) distancia) (seleccionar_caminos_minimos (cdr caminos) distancia))
	)	(T (cons (car caminos) (seleccionar_caminos_minimos (cdr caminos) distancia)))
)

; TODO: Quedarse con todos los minimos
(defun elegir_camino (caminos)
	(seleccionar_caminos_minimos caminos (minima_distancia caminos))
)

(defun id_a_interseccion (id diccionario)
	(if (eq id (caar diccionario))
		(cadar diccionario)
		(id_a_interseccion id (cdr diccionario))
	)
)

(defun misma_calle (a b)
	(if (pertenece (car a) b) 
		(car a) 
		(cadr a)
	)
)

(defun camino_a_calles (interseccion  intersecciones)
	(if (null intersecciones) 
		nil
		(cons (misma_calle interseccion (car intersecciones)) (camino_a_calles (car intersecciones) (cdr intersecciones)))	
	)
)

(defun crear_set (L)
	(reduce #'(lambda (x y) (if (pertenece y x) x (append x (list y)))) (append '(nil) L))
)

(defun contar (e L)
	(reduce #'+ (mapcar #'(lambda (x) (if (eq x e) 1 0)) L))
)

(defun contar_elemento (x)
	(let ((e (car x)))
		(list e (contar e (cadr x)))
	)
)

(defun agrupar_por_calle (calles)
	(mapcar #'contar_elemento (mapcar #'(lambda (x) (list x calles)) (crear_set calles)))
)

(defun auxiliar_escribir_camino_largo(actual calles_agrupadas)
	(format t "RECORRER ~D CUADRA(S) POR ~A Y DOBLAR EN ~A.~%" (cadr actual) (car actual) (caadr calles_agrupadas))
	(escribir_camino_largo (cdr calles_agrupadas))
)

(defun escribir_camino_largo (calles_agrupadas)
	(let ((actual (car calles_agrupadas)))
		(if (eq 1 (length calles_agrupadas))
			(format t "RECORRER ~D CUADRA(S) POR ~A HASTA LLEGAR A DESTINO." (cadr actual) (car actual))
			(auxiliar_escribir_camino_largo actual calles_agrupadas)
		)
	)
)

(defun escribir_camino (camino diccionario)
	( let ((intersecciones (mapcar #'(lambda (x) (id_a_interseccion x diccionario)) camino)))
		(if (eq 1 (length intersecciones))
			(print "YA TE ENCUENTRAS EN EL DESTINO.")
			(escribir_camino_largo (agrupar_por_calle (camino_a_calles (car intersecciones) (cdr intersecciones))))
		) 
	)
)

;usuario manda intersecciones, no nodos
(defun GPS (i f grafo dicc &optional (tray (list(list i))))
	(escribir_camino (elegir_camino (limpiar_caminos (todos_los_caminos i f grafo '()))) diccionario)
)

(setq grafo '((a (b f)) (b (a c)) (c (b d)) (d (c n e)) (e (d)) (f (g))(g (h)) (h (i l)) (i (m j)) (j (k)) (k (o))(l (b f)) (m (l c)) (n (j m)) (o (e n))))

(setq diccionario '(
(a (PaseoColon Independencia))
(b (PaseoColon Chile))
(c (PaseoColon Mexico ))
(d (PaseoColon Venezuela))
(e (PaseoColon Belgrano))
(f (Independencia Balcarce))
(g (Independencia Defensa))
(h (Defensa Chile))
(i (Defensa Mexico))
(j (Defensa Venezuela))
(k (Defensa Belgrano ))
(l (Balcarce Chile ))
(m (Balcarce Mexico))
(n (Balcarce Venezuela))
(o (Balcarce Belgrano))
) )

(GPS 'a 'g grafo diccionario)