# detecta_subencabezados imprime el mensaje en singular

    Code
      detecta_subencabezados(datos)
    Message
      ! Problema: Subencabezado incrustado.
      i Detalle: La fila 1 funciona como subencabezado dentro de los datos.
      i Solucion: Pasa el subencabezado a una columna propia y elimina las filas de titulo.

# detecta_subencabezados imprime el mensaje en plural

    Code
      detecta_subencabezados(ventas_ec)
    Message
      ! Problema: Subencabezado incrustado.
      i Detalle: Las filas 1, 5, 10 funcionan como subencabezados dentro de los datos.
      i Solucion: Pasa el subencabezado a una columna propia y elimina las filas de titulo.

# detecta_subencabezados falla si la entrada no es un data.frame

    Code
      detecta_subencabezados(1:3)
    Condition
      Error in `detecta_subencabezados()`:
      x `datos` debe ser un data.frame o tibble.
      i Recibiste un objeto de clase <integer>.

# detecta_subencabezados falla si hay menos de dos columnas

    Code
      detecta_subencabezados(data.frame(a = 1:3))
    Condition
      Error in `detecta_subencabezados()`:
      x `datos` debe tener al menos dos columnas.
      i Recibiste un data.frame con 1 columna.

# detecta_subencabezados falla si no hay filas

    Code
      detecta_subencabezados(datos)
    Condition
      Error in `detecta_subencabezados()`:
      x `datos` no tiene filas.
      i No hay subencabezados que buscar sin filas.

