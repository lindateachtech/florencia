# detecta_columnas_repetidas() muestra el mensaje con una copia

    Code
      detecta_columnas_repetidas(datos)
    Message
      ! Problema: Columnas con contenido repetido.
      i Detalle: La columna 3 repite exactamente el contenido de la columna 1.
      i Solucion: Revisa si los datos se repiten y deja solo una columna.

# detecta_columnas_repetidas() muestra el mensaje con varias copias

    Code
      detecta_columnas_repetidas(datos)
    Message
      ! Problema: Columnas con contenido repetido.
      i Detalle: Las columnas 2, 3 repiten exactamente el contenido de la columna 1.
      i Solucion: Revisa si los datos se repiten y deja solo una columna.

# detecta_columnas_repetidas() muestra el mensaje con varios grupos

    Code
      detecta_columnas_repetidas(datos)
    Message
      ! Problema: Columnas con contenido repetido.
      i Detalle: Las columnas 1, 3 tienen copias en otras columnas.
      i Solucion: Revisa si los datos se repiten y deja solo una columna.

# detecta_columnas_repetidas() aborta con entradas no validas

    Code
      detecta_columnas_repetidas(1:3)
    Condition
      Error in `detecta_columnas_repetidas()`:
      x `datos` debe ser un data.frame o tibble.
      i Recibiste un objeto de clase <integer>.

---

    Code
      detecta_columnas_repetidas(data.frame())
    Condition
      Error in `detecta_columnas_repetidas()`:
      x `datos` no tiene columnas.
      i No hay columnas que comparar en un data.frame vacio.

---

    Code
      detecta_columnas_repetidas(data.frame(a = integer(0)))
    Condition
      Error in `detecta_columnas_repetidas()`:
      x `datos` no tiene filas.
      i Sin filas no hay contenido que comparar entre columnas.

