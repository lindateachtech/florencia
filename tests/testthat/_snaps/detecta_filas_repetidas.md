# mensaje en consola con dos grupos

    Code
      detecta_filas_repetidas(datos)
    Message
      ! Problema: Filas repetidas.
      i Detalle: Las filas 1, 2, 3, 5 y 6 están repetidas.
      i Solución: Revisa si es un registro cargado dos veces. Si lo es, conserva una sola copia y elimina las demas.

# error cuando la entrada no es un data.frame

    Code
      detecta_filas_repetidas("datos")
    Condition
      Error in `detecta_filas_repetidas()`:
      x `datos` debe ser un data.frame o tibble.
      i Recibiste un objeto de clase <character>.

# error cuando no tiene columnas

    Code
      detecta_filas_repetidas(data.frame())
    Condition
      Error in `detecta_filas_repetidas()`:
      x `datos` no tiene columnas.
      i No hay filas que comparar en un data.frame sin columnas.

# error cuando no tiene filas

    Code
      detecta_filas_repetidas(datos)
    Condition
      Error in `detecta_filas_repetidas()`:
      x `datos` no tiene filas.
      i Una tabla sin filas suele indicar un problema al cargar el archivo.

