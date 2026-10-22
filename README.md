# Sistema integral de gestión, conciliación y liquidación de pedidos

Práctica de **Paradigmas de Programación** resuelta en **Racket** con el paradigma funcional.

El programa recibe por teclado los pedidos de una jornada y los pagos recibidos, y genera un informe consolidado con:

1. Registros recibidos
2. Inconsistencias (con el motivo de cada una)
3. Duplicados
4. Pedidos rechazados por inventario (y pedidos sin pago aprobado)
5. Pedidos facturados, ordenados de mayor a menor valor
6. Valores económicos de la jornada
7. Resúmenes por ciudad, categoría, tipo de cliente, medio de pago y código de producto
8. Conciliación de pagos
9. Métricas generales

## Archivos

| Archivo | Contenido |
|---|---|
| `pedidos.rkt` | Programa completo |
| `datos-prueba.txt` | 11 pedidos y 6 pagos de ejemplo que prueban todos los casos |
| `Explicacion-Practica-Racket.pdf` | Explicación de la arquitectura, las funciones usadas y un ejemplo numérico |

## Cómo ejecutarlo

**En DrRacket**

1. Abrir `pedidos.rkt` (la primera línea es `#lang racket`, así que no hay que elegir lenguaje).
2. Presionar **Run**.
3. Escribir los datos en la consola. También se puede copiar todo el contenido de `datos-prueba.txt` y pegarlo en la caja de entrada: cada línea responde una pregunta.

**Desde la terminal**

```bash
racket pedidos.rkt                     # escribir los datos a mano
racket pedidos.rkt < datos-prueba.txt  # usar los datos de prueba
```

## Orden en que se piden los datos

Primero la cantidad de pedidos y luego, por cada pedido:

`número, cédula, nombre, ciudad, categoría, código, producto, cantidad, precio unitario, inventario, tipo de cliente, medio de pago, tipo de envío, distancia (km), estado del pago`

Después la cantidad de pagos y, por cada pago: `número de pedido, valor recibido`.

## Reglas de negocio implementadas

- **Inconsistente** si: número ≤ 0, cédula vacía, cantidad ≤ 0, precio ≤ 0, inventario negativo, categoría vacía, o medio de pago / tipo de cliente / tipo de envío / estado de pago fuera de los valores permitidos. Se agregó además *distancia no numérica o negativa*, porque sin ella no se puede cobrar el despacho.
- **Duplicado**: mismo número de pedido y mismo código de producto. Se procesa solo la primera ocurrencia.
- **Facturable**: pago `Aprobado` e inventario ≥ cantidad (no hay despachos parciales).
- **Descuentos** (sobre el subtotal original): Frecuente 5 %, Premium 10 %, y 3 % adicional si el subtotal ≥ $1.000.000.
- **Despacho**: ≤ 10 km $8.000; ≤ 30 km $15.000; > 30 km $25.000 + $1.000 por km adicional. Express +40 %.
- **Impuesto** sobre (valor comercial + despacho): 19 %, o 5 % para `Alimentos`.
- **Conciliación** por número de pedido: conciliado si el valor recibido = valor definitivo (redondeado a pesos); menor → saldo pendiente; mayor → excedente.

Los datos originales nunca se modifican: cada etapa crea listas nuevas (`filter`, `map`, recursión) y no se usa `set!`.
