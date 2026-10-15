# detecta_columnas_anuales imprime el mensaje en plural

    Code
      detecta_columnas_anuales(datos)
    Message
      ! Problema: Nombres de columnas en formato anual.
      i Detalle: Las columnas 2, 3, 4 tienen un anio en el nombre: 2019, 2020, 2021.
      i Solucion: Pasa las columnas de anios a dos columnas, una con el anio y otra con el valor.

# detecta_columnas_anuales falla si la entrada no es un data.frame

    Code
      detecta_columnas_anuales(1:3)
    Condition
      Error in `detecta_columnas_anuales()`:
      x `datos` debe ser un data.frame o tibble.
      i Recibiste un objeto de clase <integer>.

# detecta_columnas_anuales falla si no hay columnas

    Code
      detecta_columnas_anuales(data.frame())
    Condition
      Error in `detecta_columnas_anuales()`:
      x `datos` no tiene columnas.
      i No hay nombres de columna que revisar en un data.frame vacio.

# detecta_columnas_anuales muestra solo el anio en el mensaje

    Code
      detecta_columnas_anuales(datos)
    Message
      ! Problema: Nombres de columnas en formato anual.
      i Detalle: Las columnas 2, 3, 4 tienen un anio en el nombre: ventas_2019, T1 2020, X2021.
      i Solucion: Pasa las columnas de anios a dos columnas, una con el anio y otra con el valor.

