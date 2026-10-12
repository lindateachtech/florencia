# detecta_encabezado imprime el mensaje esperado

    Code
      detecta_encabezado(cervecero)
    Message
      ! Problema: Posible encabezado desplazado.
      i Detalle: El encabezado real del conjunto de datos parece estar desplazado, debido a que la mayoria de las columnas no tienen nombre.
      i Solucion recomendada: Usa `corrige_encabezado()` para promover la fila correcta a nombres de columna.

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

