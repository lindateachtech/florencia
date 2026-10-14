# detecta_columnas_vacias imprime el mensaje en singular

    Code
      detecta_columnas_vacias(ventas_ec)
    Message
      ! Problema: Columna vacia.
      i Detalle: La columna 2 no contiene ningun valor, todas sus celdas estan vacias.
      i Solucion: Elimina las columnas sin datos.

# detecta_columnas_vacias imprime el mensaje en plural

    Code
      detecta_columnas_vacias(datos)
    Message
      ! Problema: Columna vacia.
      i Detalle: Las columnas 2, 4 no contienen ningun valor, todas sus celdas estan vacias.
      i Solucion: Elimina las columnas sin datos.

# detecta_columnas_vacias falla si la entrada no es un data.frame

    Code
      detecta_columnas_vacias(1:3)
    Condition
      Error in `detecta_columnas_vacias()`:
      x `datos` debe ser un data.frame o tibble.
      i Recibiste un objeto de clase <integer>.

# detecta_columnas_vacias falla si no hay columnas

    Code
      detecta_columnas_vacias(data.frame())
    Condition
      Error in `detecta_columnas_vacias()`:
      x `datos` no tiene columnas.
      i No hay columnas que revisar en un data.frame vacio.

# detecta_columnas_vacias falla si no hay filas

    Code
      detecta_columnas_vacias(datos)
    Condition
      Error in `detecta_columnas_vacias()`:
      x `datos` no tiene filas.
      i Sin filas, todas las columnas parecerian vacias.

