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
	(cond
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
			((eq final (car (last recorrido_actualizado))) (list recorrido_actualizado))
			((null adyacentes) nil)
			(T (reduce #'append (mapcar (lambda (x) (todos_los_caminos x final grafo recorrido_actualizado)) adyacentes)))
		)
	)
)

(defun minima_distancia (caminos)
	(length (reduce (lambda (x y) (if (< (length x) (length y)) x y)) caminos))
)

(defun seleccionar_caminos_minimos (caminos distancia)
	(cond 
		((null caminos) nil)
		((> (length (car caminos)) distancia) (seleccionar_caminos_minimos (cdr caminos) distancia))
		(T (cons (car caminos) (seleccionar_caminos_minimos (cdr caminos) distancia)))
	)
)

(defun elegir_caminos (caminos)
	(if (null caminos)
		nil
		(seleccionar_caminos_minimos caminos (minima_distancia caminos))
	)
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
	(reduce (lambda (x y) (if (pertenece y x) x (append x (list y)))) (cons nil L))
)

(defun contar (e L)
	(reduce #'+ (mapcar (lambda (x) (if (eq x e) 1 0)) L))
)

(defun contar_elemento (x)
	(let ((e (car x)))
		(list e (contar e (cadr x)))
	)
)

(defun agrupar_por_calle (calles)
	(mapcar #'contar_elemento (mapcar (lambda (x) (list x calles)) (crear_set calles)))
)

(defun escribir_camino (calles_agrupadas)
	(let ((actual (car calles_agrupadas)))
		(cond 
			((eq 1 (length calles_agrupadas)) (format t "RECORRER ~D CUADRA(S) POR ~A HASTA LLEGAR A DESTINO.~%~%" (cadr actual) (car actual)))
			(T (format t "RECORRER ~D CUADRA(S) POR ~A Y DOBLAR EN ~A.~%" (cadr actual) (car actual) (caadr calles_agrupadas))
				(escribir_camino (cdr calles_agrupadas))
			)
		)
	)
)

(defun formatear_y_escribir_camino (camino diccionario)
	(let ((intersecciones (mapcar (lambda (x) (id_a_interseccion x diccionario)) camino)))
		(escribir_camino (agrupar_por_calle (camino_a_calles (car intersecciones) (cdr intersecciones))))
	)
)

(defun escribir_caminos (caminos diccionario)
	(cond
		((null (car caminos)) (format t "NO HAY CAMINOS POSIBLES.~%")) 
		((eq 1 (length (car caminos))) (format t "YA TE ENCUENTRAS EN EL DESTINO.~%"))
		(T (format t "HAY ~D CAMINOS POSIBLES.~%~%" (length caminos)) (mapcar (lambda (x) (formatear_y_escribir_camino x diccionario)) caminos))
	) 
)

(defun misma_interseccion (A B)
	(cond
		((eq (car A) (car B)) (eq (cadr A) (cadr B)))
		((eq (car A) (cadr B)) (eq (cadr A) (car B)))
		(T nil)
	)
)

(defun interseccion_a_nodo (interseccion diccionario)
	(cond
		((null diccionario) (format t "LA INTERSECCION ENTRE ~A Y ~A NO EXISTE.~%" (car interseccion) (cadr interseccion)))
		((misma_interseccion interseccion (cadar diccionario)) (caar diccionario))
		(T (interseccion_a_nodo interseccion (cdr diccionario)))
	)
)

(defun GPS (i f grafo dicc)
	(let ((nodo_inicial (interseccion_a_nodo i diccionario)) (nodo_final (interseccion_a_nodo f diccionario)))
		(if (or (null nodo_inicial) (null nodo_final))
			nil
			(escribir_caminos (elegir_caminos (todos_los_caminos nodo_inicial nodo_final grafo '())) diccionario)
		) 
	)
)

; PRUEBAS
; (setq grafo '((a (b f)) (b (a c)) (c (b d)) (d (c n e)) (e (d)) (f (g))(g (h)) (h (i l)) (i (m j)) (j (k)) (k (o))(l (b f)) (m (l c)) (n (j m)) (o (e n)) (p nil)))

; (setq diccionario '(
; (a (PaseoColon Independencia))
; (b (PaseoColon Chile))
; (c (PaseoColon Mexico))
; (d (PaseoColon Venezuela))
; (e (PaseoColon Belgrano))
; (f (Independencia Balcarce))
; (g (Independencia Defensa))
; (h (Defensa Chile))
; (i (Defensa Mexico))
; (j (Defensa Venezuela))
; (k (Defensa Belgrano))
; (l (Balcarce Chile))
; (m (Balcarce Mexico))
; (n (Balcarce Venezuela))
; (o (Balcarce Belgrano))
; (p (calle separada))
; ) )

; (GPS '(PaseoColon Independencia) '(Balcarce Belgrano) grafo diccionario)

; (GPS '(PaseoColon Independencia) '(Independencia PaseoColon) grafo diccionario)

; (GPS '(Balcarce Mexico) '(calle separada) grafo diccionario)

; (GPS '(Balcarce Mexico) '(calle falsa) grafo diccionario)