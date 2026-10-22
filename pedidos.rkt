#lang racket
;; =====================================================================
;; 1. CONSTANTES DEL NEGOCIO
;; =====================================================================
(define TIPOS-CLIENTE '("Normal" "Frecuente" "Premium"))
(define MEDIOS-PAGO   '("Efectivo" "Tarjeta" "PSE" "Transferencia"))
(define TIPOS-ENVIO   '("Normal" "Express"))
(define ESTADOS-PAGO  '("Aprobado" "Pendiente" "Rechazado"))

;; Se usan fracciones exactas (19/100 en vez de 0.19) para que los
;; cálculos de dinero no tengan errores de redondeo de punto flotante.
(define DESCUENTO-FRECUENTE      5/100)
(define DESCUENTO-PREMIUM        10/100)
(define UMBRAL-DESCUENTO-VOLUMEN 1000000)
(define DESCUENTO-VOLUMEN        3/100)
(define RECARGO-EXPRESS          40/100)
(define IMPUESTO-GENERAL         19/100)
(define IMPUESTO-ALIMENTOS       5/100)
(define UMBRAL-PREMIUM-EXPRESS   2000000)


;; =====================================================================
;; 2. REPRESENTACIÓN DE LOS DATOS
;; ---------------------------------------------------------------------
;; Un PEDIDO es una lista de 15 campos, en este orden:
;;  0 número   1 cédula   2 nombre   3 ciudad   4 categoría
;;  5 código   6 producto 7 cantidad 8 precio   9 inventario
;; 10 tipo-cliente  11 medio-pago  12 tipo-envío  13 distancia
;; 14 estado-pago
;; Cada campo se lee con una función "accesora" basada en list-ref,
;; así el resto del código habla de (p-cantidad p) y no de posiciones.
;; =====================================================================
(define (p-numero p)       (list-ref p 0))
(define (p-cedula p)       (list-ref p 1))
(define (p-nombre p)       (list-ref p 2))
(define (p-ciudad p)       (list-ref p 3))
(define (p-categoria p)    (list-ref p 4))
(define (p-codigo p)       (list-ref p 5))
(define (p-producto p)     (list-ref p 6))
(define (p-cantidad p)     (list-ref p 7))
(define (p-precio p)       (list-ref p 8))
(define (p-inventario p)   (list-ref p 9))
(define (p-tipo-cliente p) (list-ref p 10))
(define (p-medio-pago p)   (list-ref p 11))
(define (p-tipo-envio p)   (list-ref p 12))
(define (p-distancia p)    (list-ref p 13))
(define (p-estado-pago p)  (list-ref p 14))

;; Una LIQUIDACIÓN es una lista nueva que envuelve al pedido original
;; junto con sus valores calculados:
;; (pedido subtotal desc-cliente desc-volumen comercial despacho impuesto total)
(define (l-pedido l)       (first l))
(define (l-subtotal l)     (second l))
(define (l-desc-cliente l) (third l))
(define (l-desc-volumen l) (list-ref l 3))
(define (l-comercial l)    (list-ref l 4))
(define (l-despacho l)     (list-ref l 5))
(define (l-impuesto l)     (list-ref l 6))
(define (l-total l)        (list-ref l 7))
(define (l-descuentos l)   (+ (l-desc-cliente l) (l-desc-volumen l)))

;; Atajos para leer datos del pedido desde una liquidación
(define (l-numero l)       (p-numero (l-pedido l)))
(define (l-cedula l)       (p-cedula (l-pedido l)))
(define (l-ciudad l)       (p-ciudad (l-pedido l)))
(define (l-categoria l)    (p-categoria (l-pedido l)))
(define (l-tipo-cliente l) (p-tipo-cliente (l-pedido l)))
(define (l-medio-pago l)   (p-medio-pago (l-pedido l)))
(define (l-codigo l)       (p-codigo (l-pedido l)))
(define (l-cantidad l)     (p-cantidad (l-pedido l)))

;; Un PAGO es una lista: (número-pedido valor-recibido)
(define (pago-numero pg) (first pg))
(define (pago-valor pg)  (second pg))

;; Un GRUPO (resultado de agrupar) es: (clave cantidad-registros suma)
(define (g-clave g)    (first g))
(define (g-cantidad g) (second g))
(define (g-suma g)     (third g))

;; Una CONCILIACIÓN es: (número valor-facturado valor-recibido estado)
(define (c-numero c)    (first c))
(define (c-facturado c) (second c))
(define (c-recibido c)  (third c))
(define (c-estado c)    (list-ref c 3))


;; =====================================================================
;; 3. ENTRADA POR TECLADO
;; =====================================================================

;; Muestra un mensaje y devuelve la línea escrita por el usuario.
;; 'any acepta finales de línea de Windows y Linux. Si no hay más
;; entrada (fin de archivo) devuelve "" para no romper el programa.
(define (leer-texto mensaje)
  (display mensaje)
  (let ([linea (read-line (current-input-port) 'any)])
    (if (string? linea) linea "")))

;; Lee un número. Si lo escrito no es número, string->number da #f
;; y la validación posterior lo marca como inconsistencia.
(define (leer-numero mensaje)
  (string->number (leer-texto mensaje)))

;; Lee un entero >= 0 y vuelve a preguntar mientras sea inválido
;; (equivalente a un "while" usando let loop).
(define (leer-cantidad mensaje)
  (let loop ([n (leer-numero mensaje)])
    (if (and (number? n) (>= n 0) (= n (round n)))
        n
        (loop (leer-numero "  Valor inválido, escriba un entero >= 0: ")))))

;; let* garantiza que los campos se pidan en orden, uno tras otro.
(define (leer-pedido i)
  (printf "\n--- Pedido ~a ---\n" i)
  (let* ([numero     (leer-numero "Número de pedido: ")]
         [cedula     (leer-texto  "Cédula del cliente: ")]
         [nombre     (leer-texto  "Nombre del cliente: ")]
         [ciudad     (leer-texto  "Ciudad: ")]
         [categoria  (leer-texto  "Categoría del producto: ")]
         [codigo     (leer-texto  "Código del producto: ")]
         [producto   (leer-texto  "Nombre del producto: ")]
         [cantidad   (leer-numero "Cantidad solicitada: ")]
         [precio     (leer-numero "Precio unitario: ")]
         [inventario (leer-numero "Cantidad disponible en inventario: ")]
         [tipo       (leer-texto  "Tipo de cliente (Normal/Frecuente/Premium): ")]
         [medio      (leer-texto  "Medio de pago (Efectivo/Tarjeta/PSE/Transferencia): ")]
         [envio      (leer-texto  "Tipo de envío (Normal/Express): ")]
         [distancia  (leer-numero "Distancia del despacho (km): ")]
         [estado     (leer-texto  "Estado del pago (Aprobado/Pendiente/Rechazado): ")])
    (list numero cedula nombre ciudad categoria codigo producto
          cantidad precio inventario tipo medio envio distancia estado)))

;; Construye la colección de pedidos de forma recursiva.
(define (leer-pedidos n)
  (let loop ([i 1])
    (if (> i n)
        '()
        (cons (leer-pedido i) (loop (+ i 1))))))

(define (leer-pago i)
  (printf "\n--- Pago ~a ---\n" i)
  (let* ([numero (leer-numero "Número de pedido: ")]
         [valor  (leer-numero "Valor recibido por la empresa: ")])
    (list numero valor)))

(define (leer-pagos n)
  (let loop ([i 1])
    (if (> i n)
        '()
        (cons (leer-pago i) (loop (+ i 1))))))


;; =====================================================================
;; 4. FUNCIONES AUXILIARES GENÉRICAS
;; =====================================================================

(define (positivo? x) (and (number? x) (> x 0)))

;; member devuelve una sublista o #f; lo convertimos en #t / #f.
(define (pertenece? valor permitidos)
  (if (member valor permitidos) #t #f))

;; Suma el resultado de aplicar f a cada elemento: (apply + (map f lst))
(define (sumar f lst)
  (apply + (map f lst)))

;; Elimina repetidos conservando el orden de primera aparición.
(define (unicos lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (unicos (filter (lambda (x) (not (equal? x (car lst))))
                            (cdr lst))))))

;; Devuelve el elemento con mayor (valor elemento). Lista no vacía.
(define (maximo-por valor lst)
  (if (null? (cdr lst))
      (car lst)
      (let ([mejor-del-resto (maximo-por valor (cdr lst))])
        (if (>= (valor (car lst)) (valor mejor-del-resto))
            (car lst)
            mejor-del-resto))))

;; Une una lista de textos separándolos con "; " (recursivo)
(define (unir-textos textos)
  (cond [(null? textos) ""]
        [(null? (cdr textos)) (car textos)]
        [else (string-append (car textos) "; " (unir-textos (cdr textos)))]))

;; Redondea a pesos enteros para mostrar
(define (pesos x) (round x))


;; =====================================================================
;; 5. VALIDACIÓN (inconsistencias)
;; ---------------------------------------------------------------------
;; Cada regla produce un mensaje si falla o #f si se cumple; filter
;; se queda solo con los mensajes. Pedido válido = lista vacía.
;; =====================================================================
(define (motivos-inconsistencia p)
  (filter string?
          (list
           (if (positivo? (p-numero p)) #f "número de pedido no positivo")
           (if (string=? (p-cedula p) "") "cédula vacía" #f)
           (if (positivo? (p-cantidad p)) #f "cantidad solicitada <= 0")
           (if (positivo? (p-precio p)) #f "precio unitario <= 0")
           (if (and (number? (p-inventario p)) (>= (p-inventario p) 0))
               #f "inventario negativo")
           (if (string=? (p-categoria p) "") "categoría vacía" #f)
           (if (pertenece? (p-medio-pago p) MEDIOS-PAGO) #f "medio de pago no permitido")
           (if (pertenece? (p-tipo-cliente p) TIPOS-CLIENTE) #f "tipo de cliente no permitido")
           (if (pertenece? (p-tipo-envio p) TIPOS-ENVIO) #f "tipo de envío no permitido")
           (if (pertenece? (p-estado-pago p) ESTADOS-PAGO) #f "estado de pago no permitido")
           ;; Regla adicional: sin distancia válida no se puede cobrar el despacho
           (if (and (number? (p-distancia p)) (>= (p-distancia p) 0))
               #f "distancia no numérica o negativa"))))

(define (valido? p) (null? (motivos-inconsistencia p)))

(define (pago-valido? pg)
  (and (positivo? (pago-numero pg))
       (number? (pago-valor pg))
       (>= (pago-valor pg) 0)))


;; =====================================================================
;; 6. DUPLICADOS (mismo número de pedido + mismo código de producto)
;; ---------------------------------------------------------------------
;; "vistos" es una lista nueva que crece en cada llamada recursiva con
;; las claves ya encontradas; nada se modifica.
;; =====================================================================
(define (clave p) (list (p-numero p) (p-codigo p)))

;; Primera ocurrencia de cada clave (lo que se procesa)
(define (quitar-duplicados lst vistos)
  (cond [(null? lst) '()]
        [(member (clave (car lst)) vistos)
         (quitar-duplicados (cdr lst) vistos)]
        [else
         (cons (car lst)
               (quitar-duplicados (cdr lst) (cons (clave (car lst)) vistos)))]))

;; Ocurrencias repetidas (para saber cuántos duplicados hubo)
(define (extraer-duplicados lst vistos)
  (cond [(null? lst) '()]
        [(member (clave (car lst)) vistos)
         (cons (car lst) (extraer-duplicados (cdr lst) vistos))]
        [else
         (extraer-duplicados (cdr lst) (cons (clave (car lst)) vistos))]))


;; =====================================================================
;; 7. SELECCIÓN COMERCIAL (pago e inventario)
;; =====================================================================
(define (pago-aprobado? p) (string=? (p-estado-pago p) "Aprobado"))

;; No hay despachos parciales: el inventario debe cubrir todo
(define (inventario-suficiente? p) (<= (p-cantidad p) (p-inventario p)))

(define (facturable? p)
  (and (pago-aprobado? p) (inventario-suficiente? p)))


;; =====================================================================
;; 8. LIQUIDACIÓN DE UN PEDIDO (transformación)
;; =====================================================================
(define (porcentaje-cliente tipo)
  (case tipo
    [("Frecuente") DESCUENTO-FRECUENTE]
    [("Premium")   DESCUENTO-PREMIUM]
    [else 0]))

(define (costo-base-despacho km)
  (cond [(<= km 10) 8000]
        [(<= km 30) 15000]
        [else (+ 25000 (* 1000 (- km 30)))]))

(define (costo-despacho p)
  (let ([base (costo-base-despacho (p-distancia p))])
    (if (string=? (p-tipo-envio p) "Express")
        (* base (+ 1 RECARGO-EXPRESS))
        base)))

(define (tasa-impuesto p)
  (if (string=? (p-categoria p) "Alimentos")
      IMPUESTO-ALIMENTOS
      IMPUESTO-GENERAL))

;; Ambos descuentos se calculan sobre el subtotal ORIGINAL y luego se restan.
(define (liquidar p)
  (let* ([subtotal     (* (p-cantidad p) (p-precio p))]
         [desc-cliente (* subtotal (porcentaje-cliente (p-tipo-cliente p)))]
         [desc-volumen (if (>= subtotal UMBRAL-DESCUENTO-VOLUMEN)
                           (* subtotal DESCUENTO-VOLUMEN)
                           0)]
         [comercial    (- subtotal desc-cliente desc-volumen)]
         [despacho     (costo-despacho p)]
         [impuesto     (* (+ comercial despacho) (tasa-impuesto p))]
         [total        (+ comercial despacho impuesto)])
    (list p subtotal desc-cliente desc-volumen comercial despacho impuesto total)))


;; =====================================================================
;; 9. ORDENAMIENTO (inserción recursiva, de mayor a menor valor final)
;; =====================================================================
(define (insertar-desc l ordenada)
  (cond [(null? ordenada) (list l)]
        [(>= (l-total l) (l-total (car ordenada))) (cons l ordenada)]
        [else (cons (car ordenada) (insertar-desc l (cdr ordenada)))]))

(define (ordenar-desc lst)
  (if (null? lst)
      '()
      (insertar-desc (car lst) (ordenar-desc (cdr lst)))))


;; =====================================================================
;; 10. AGRUPACIÓN
;; ---------------------------------------------------------------------
;; Función de orden superior: recibe cómo obtener la clave y cómo
;; obtener el valor a sumar. Sirve para ciudad, categoría, tipo de
;; cliente, medio de pago, cliente (cédula) y código de producto.
;; =====================================================================
(define (agrupar-por obtener-clave obtener-valor lst)
  (map (lambda (k)
         (let ([grupo (filter (lambda (e) (equal? (obtener-clave e) k)) lst)])
           (list k (length grupo) (sumar obtener-valor grupo))))
       (unicos (map obtener-clave lst))))


;; =====================================================================
;; 11. VERIFICACIONES SOBRE LO FACTURADO
;; =====================================================================
(define (liquidacion-consistente? l)
  (and (positivo? (l-subtotal l))
       (>= (l-descuentos l) 0)
       (< (l-descuentos l) (l-subtotal l))
       (positivo? (l-comercial l))
       (positivo? (l-despacho l))
       (positivo? (l-impuesto l))
       (positivo? (l-total l))
       (= (l-total l) (+ (l-comercial l) (l-despacho l) (l-impuesto l)))))

(define (todos-consistentes? lst)
  (null? (filter (lambda (l) (not (liquidacion-consistente? l))) lst)))

(define (premium-express-alto? l)
  (and (string=? (l-tipo-cliente l) "Premium")
       (string=? (p-tipo-envio (l-pedido l)) "Express")
       (> (l-total l) UMBRAL-PREMIUM-EXPRESS)))


;; =====================================================================
;; 12. CONCILIACIÓN DE PAGOS
;; ---------------------------------------------------------------------
;; Se concilia por número de pedido: el valor facturado de un número es
;; la suma de sus líneas facturadas y el valor recibido es la suma de
;; los pagos con ese número.
;; =====================================================================
(define (conciliar-numero n facturados pagos)
  (let* ([facturado (pesos (sumar l-total
                                  (filter (lambda (l) (equal? (l-numero l) n)) facturados)))]
         [recibido  (sumar pago-valor
                           (filter (lambda (pg) (equal? (pago-numero pg) n)) pagos))]
         [estado    (cond [(zero? facturado)          "Sin pedido facturado"]
                          [(= recibido facturado)     "Conciliado"]
                          [(< recibido facturado)     "Saldo pendiente"]
                          [else                       "Excedente"])])
    (list n facturado recibido estado)))

(define (conciliar facturados pagos)
  (map (lambda (n) (conciliar-numero n facturados pagos))
       (unicos (append (map l-numero facturados) (map pago-numero pagos)))))

(define (con-estado estado conciliacion)
  (filter (lambda (c) (string=? (c-estado c) estado)) conciliacion))


;; =====================================================================
;; 13. REPORTES (solo muestran; no modifican nada)
;; =====================================================================
(define (titulo texto)
  (displayln "")
  (displayln "==============================================================")
  (displayln texto)
  (displayln "=============================================================="))

(define (mostrar-pedido p)
  (printf "  #~a | ~a | ~a (~a) | ~a x ~a | ~a | ~a\n"
          (p-numero p) (p-producto p) (p-nombre p) (p-cedula p)
          (p-cantidad p) (p-precio p) (p-ciudad p) (p-estado-pago p)))

(define (mostrar-lista-pedidos lst)
  (if (null? lst)
      (displayln "  (ninguno)")
      (for-each mostrar-pedido lst)))

(define (mostrar-inconsistencias invalidos)
  (if (null? invalidos)
      (displayln "  (ninguna)")
      (for-each (lambda (p)
                  (printf "  #~a | ~a -> ~a\n"
                          (p-numero p) (p-producto p)
                          (unir-textos (motivos-inconsistencia p))))
                invalidos)))

(define (mostrar-rechazados-inventario lst)
  (if (null? lst)
      (displayln "  (ninguno)")
      (for-each (lambda (p)
                  (printf "  #~a | ~a | solicitado: ~a, disponible: ~a\n"
                          (p-numero p) (p-producto p) (p-cantidad p) (p-inventario p)))
                lst)))

(define (mostrar-liquidacion l)
  (let ([p (l-pedido l)])
    (printf "  #~a ~a (~a, ~a, envío ~a, ~a km)\n"
            (p-numero p) (p-producto p) (p-tipo-cliente p)
            (p-categoria p) (p-tipo-envio p) (p-distancia p))
    (printf "     Subtotal: $~a | Desc. cliente: $~a | Desc. volumen: $~a\n"
            (pesos (l-subtotal l)) (pesos (l-desc-cliente l)) (pesos (l-desc-volumen l)))
    (printf "     Comercial: $~a | Despacho: $~a | Impuesto: $~a | TOTAL: $~a\n"
            (pesos (l-comercial l)) (pesos (l-despacho l))
            (pesos (l-impuesto l)) (pesos (l-total l)))))

(define (mostrar-grupos nombre grupos)
  (printf "\n  Por ~a:\n" nombre)
  (for-each (lambda (g)
              (printf "    ~a: ~a pedido(s), $~a\n"
                      (g-clave g) (g-cantidad g) (pesos (g-suma g))))
            grupos))

(define (mostrar-conciliacion c)
  (printf "  #~a | facturado: $~a | recibido: $~a | ~a\n"
          (c-numero c) (c-facturado c) (c-recibido c) (c-estado c)))


;; =====================================================================
;; 14. INFORME CONSOLIDADO
;; ---------------------------------------------------------------------
;; Cada let* crea una colección nueva a partir de la anterior:
;; pedidos -> válidos -> únicos -> facturables -> liquidados -> ordenados
;; =====================================================================
(define (generar-informe pedidos pagos)
  (let* (;; --- Validación y duplicados ---
         [validos      (filter valido? pedidos)]
         [invalidos    (filter (lambda (p) (not (valido? p))) pedidos)]
         [unicos-ped   (quitar-duplicados validos '())]
         [duplicados   (extraer-duplicados validos '())]
         ;; --- Selección comercial ---
         [sin-aprobar  (filter (lambda (p) (not (pago-aprobado? p))) unicos-ped)]
         [rechazados   (filter (lambda (p) (and (pago-aprobado? p)
                                                (not (inventario-suficiente? p))))
                               unicos-ped)]
         [facturables  (filter facturable? unicos-ped)]
         ;; --- Transformación y ordenamiento ---
         [liquidados   (map liquidar facturables)]
         [ordenados    (ordenar-desc liquidados)]
         [n-fact       (length liquidados)]
         ;; --- Acumulación ---
         [total-bruto     (sumar l-subtotal liquidados)]
         [total-desc      (sumar l-descuentos liquidados)]
         [total-despacho  (sumar l-despacho liquidados)]
         [total-impuestos (sumar l-impuesto liquidados)]
         [total-final     (sumar l-total liquidados)]
         [promedio        (if (zero? n-fact) 0 (/ total-final n-fact))]
         ;; --- Agrupación ---
         [por-ciudad     (agrupar-por l-ciudad l-total liquidados)]
         [por-categoria  (agrupar-por l-categoria l-total liquidados)]
         [por-tipo       (agrupar-por l-tipo-cliente l-total liquidados)]
         [por-medio      (agrupar-por l-medio-pago l-total liquidados)]
         [por-cliente    (agrupar-por l-cedula l-total liquidados)]
         [por-codigo     (agrupar-por l-codigo l-cantidad liquidados)]
         ;; --- Conciliación ---
         [pagos-validos  (filter pago-valido? pagos)]
         [conciliacion   (conciliar liquidados pagos-validos)]
         [conciliados    (con-estado "Conciliado" conciliacion)]
         [pendientes     (con-estado "Saldo pendiente" conciliacion)]
         [excedentes     (con-estado "Excedente" conciliacion)]
         [sin-pedido     (con-estado "Sin pedido facturado" conciliacion)]
         [diferencias    (filter (lambda (c) (not (string=? (c-estado c) "Conciliado")))
                                 conciliacion)])

    ;; ---------------- 1. Registros recibidos ----------------
    (titulo "1. REGISTROS RECIBIDOS")
    (printf "  Total de registros recibidos: ~a\n" (length pedidos))
    (mostrar-lista-pedidos pedidos)

    ;; ---------------- 2. Inconsistencias ----------------
    (titulo "2. INCONSISTENCIAS")
    (printf "  Registros válidos: ~a | Registros inválidos: ~a\n"
            (length validos) (length invalidos))
    (mostrar-inconsistencias invalidos)

    ;; ---------------- 3. Duplicados ----------------
    (titulo "3. DUPLICADOS (mismo número de pedido y código de producto)")
    (printf "  Registros repetidos encontrados: ~a | Registros únicos: ~a\n"
            (length duplicados) (length unicos-ped))
    (mostrar-lista-pedidos duplicados)

    ;; ---------------- 4. Rechazados ----------------
    (titulo "4. PEDIDOS RECHAZADOS POR INVENTARIO (pago aprobado)")
    (mostrar-rechazados-inventario rechazados)
    (displayln "\n  Pedidos sin pago aprobado (Pendiente / Rechazado):")
    (mostrar-lista-pedidos sin-aprobar)

    ;; ---------------- 5. Facturados ----------------
    (titulo "5. PEDIDOS FACTURADOS (de mayor a menor valor)")
    (printf "  Cantidad de pedidos facturados: ~a\n" n-fact)
    (if (null? ordenados)
        (displayln "  (ninguno)")
        (for-each mostrar-liquidacion ordenados))

    ;; ---------------- 6. Valores económicos ----------------
    (titulo "6. VALORES ECONÓMICOS DE LA JORNADA")
    (printf "  Total bruto antes de descuentos: $~a\n" (pesos total-bruto))
    (printf "  Total descontado:                $~a\n" (pesos total-desc))
    (printf "  Total cobrado por despachos:     $~a\n" (pesos total-despacho))
    (printf "  Total generado en impuestos:     $~a\n" (pesos total-impuestos))
    (printf "  TOTAL DEFINITIVO FACTURADO:      $~a\n" (pesos total-final))

    ;; ---------------- 7. Resúmenes por agrupación ----------------
    (titulo "7. RESÚMENES POR AGRUPACIÓN")
    (mostrar-grupos "ciudad" por-ciudad)
    (mostrar-grupos "categoría" por-categoria)
    (mostrar-grupos "tipo de cliente" por-tipo)
    (displayln "\n  Pedidos procesados por medio de pago:")
    (for-each (lambda (g) (printf "    ~a: ~a pedido(s)\n" (g-clave g) (g-cantidad g)))
              por-medio)
    (displayln "\n  Unidades a descontar del inventario por código:")
    (for-each (lambda (g) (printf "    ~a: ~a unidad(es)\n" (g-clave g) (g-suma g)))
              por-codigo)
    (if (null? por-categoria)
        (displayln "\n  Categoría con mayor facturación: no hay facturación")
        (let ([mejor (maximo-por g-suma por-categoria)])
          (printf "\n  Categoría con mayor facturación: ~a ($~a)\n"
                  (g-clave mejor) (pesos (g-suma mejor)))))
    (if (null? por-cliente)
        (displayln "  Cliente con mayor valor acumulado: no hay facturación")
        (let* ([mejor  (maximo-por g-suma por-cliente)]
               [nombre (p-nombre (l-pedido (first (filter (lambda (l)
                                                            (equal? (l-cedula l) (g-clave mejor)))
                                                          liquidados))))])
          (printf "  Cliente con mayor valor acumulado: ~a, cédula ~a ($~a en ~a pedido(s))\n"
                  nombre (g-clave mejor) (pesos (g-suma mejor)) (g-cantidad mejor))))

    ;; ---------------- 8. Conciliación ----------------
    (titulo "8. CONCILIACIÓN DE PAGOS")
    (printf "  Pagos registrados: ~a | Pagos con datos inválidos (ignorados): ~a\n"
            (length pagos) (- (length pagos) (length pagos-validos)))
    (for-each mostrar-conciliacion conciliacion)
    (printf "\n  Total de pedidos conciliados:        ~a\n" (length conciliados))
    (printf "  Total de dinero pendiente:           $~a\n"
            (sumar (lambda (c) (- (c-facturado c) (c-recibido c))) pendientes))
    (printf "  Total de excedentes recibidos:       $~a\n"
            (sumar (lambda (c) (- (c-recibido c) (c-facturado c))) excedentes))
    (printf "  Dinero recibido sin pedido facturado: $~a\n"
            (sumar c-recibido sin-pedido))
    (printf "  Números de pedido con diferencias:   ~a\n" (map c-numero diferencias))

    ;; ---------------- 9. Métricas generales ----------------
    (titulo "9. MÉTRICAS GENERALES DE LA JORNADA")
    (if (null? ordenados)
        (displayln "  No hubo pedidos facturados.")
        (begin
          (printf "  Pedido de mayor valor: #~a ($~a)\n"
                  (l-numero (first ordenados)) (pesos (l-total (first ordenados))))
          (printf "  Pedido de menor valor: #~a ($~a)\n"
                  (l-numero (car (reverse ordenados)))
                  (pesos (l-total (car (reverse ordenados)))))
          (printf "  Promedio por pedido facturado: $~a\n" (pesos promedio))))
    (printf "  ¿Todos los facturados son positivos y consistentes?: ~a\n"
            (if (todos-consistentes? liquidados) "Sí" "No"))
    (printf "  ¿Existe un pedido Premium + Express mayor a $2.000.000?: ~a\n"
            (if (null? (filter premium-express-alto? liquidados)) "No" "Sí"))
    (printf "  Pedidos por encima del promedio: ~a\n"
            (map l-numero (filter (lambda (l) (> (l-total l) promedio)) ordenados)))
    (if (>= n-fact 5)
        (printf "  Top 5 pedidos de mayor valor: ~a\n"
                (map (lambda (i) (l-numero (list-ref ordenados i))) '(0 1 2 3 4)))
        (displayln "  Top 5: no aplica (hay menos de cinco pedidos facturados)"))
    (printf "  Porcentaje de registros que terminaron en venta: ~a%\n"
            (if (null? pedidos)
                0
                (exact->inexact (/ (round (* 10000 (/ n-fact (length pedidos)))) 100))))))


;; =====================================================================
;; 15. PROGRAMA PRINCIPAL
;; =====================================================================
(define (main)
  (displayln "=== SISTEMA DE GESTIÓN, CONCILIACIÓN Y LIQUIDACIÓN DE PEDIDOS ===")
  (let* ([n       (leer-cantidad "¿Cuántos pedidos se van a registrar? ")]
         [pedidos (leer-pedidos n)]
         [m       (leer-cantidad "\n¿Cuántos pagos se van a registrar? ")]
         [pagos   (leer-pagos m)])
    (generar-informe pedidos pagos)))

(main)
