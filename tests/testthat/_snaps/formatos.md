# detecta_encabezados imprime el mensaje esperado

    Code
      detecta_encabezados(cervecero)
    Message
      ! Problema: Posible encabezado desplazado.
      i Detalle: El encabezado real del conjunto de datos parece estar desplazado, debido a que la mayoria de las columnas no tienen nombre.
      i Solucion recomendada: Usa `corrige_encabezados()` para promover la fila correcta a nombres de columna.

# detecta_encabezados rechaza un input que no es data.frame

    Code
      detecta_encabezados("no soy un data.frame")
    Condition
      Error in `detecta_encabezados()`:
      x `datos` debe ser un data.frame o tibble.
      i Recibiste un objeto de clase <character>.

