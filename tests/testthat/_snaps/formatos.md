# detecta_encabezado imprime el mensaje esperado

    Code
      detecta_encabezado(cervecero)
    Message
      ! Problema: Posible encabezado desplazado.
      i Detalle: El encabezado real del conjunto de datos parece estar desplazado. El encabezado real parece estar en la fila 1.
      i Solucion: Promueve el encabezado real a nombres de columna.

# detecta_encabezado rechaza un input que no es data.frame

    Code
      detecta_encabezado("no soy un data.frame")
    Condition
      Error in `detecta_encabezado()`:
      x `datos` debe ser un data.frame o tibble.
      i Recibiste un objeto de clase <character>.

# detecta_encabezado rechaza un data.frame sin columnas

    Code
      detecta_encabezado(data.frame())
    Condition
      Error in `detecta_encabezado()`:
      x `datos` no tiene columnas.
      i No hay encabezado que revisar en un data.frame vacio.

# detecta_encabezado avisa si no puede localizar la fila del encabezado

    Code
      detecta_encabezado(datos)
    Message
      ! Problema: Posible encabezado desplazado.
      i Detalle: El encabezado real del conjunto de datos parece estar desplazado. No se pudo localizar la fila del encabezado real.
      i Solucion: Promueve el encabezado real a nombres de columna.

