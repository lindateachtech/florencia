# el mensaje en consola explica un solo valor inconsistente

    Code
      invisible(detecta_categorias_inconsistentes(datos))
    Message
      ! Problema: Un mismo valor escrito de formas distintas.
      i Detalle: En la columna 1 (provincia) hay un valor escrito de 3 formas distintas: "Guayas", "guayas ", "GUAYAS".
      i Solucion: Elige una sola forma de escribir el valor y usala en todas las filas.

# el mensaje en consola resume varios valores inconsistentes

    Code
      invisible(detecta_categorias_inconsistentes(datos))
    Message
      ! Problema: Un mismo valor escrito de formas distintas.
      i Detalle: Hay 3 valores escritos de formas distintas (columnas 1, 2).
      i Solucion: Elige una sola forma de escribir el valor y usala en todas las filas.

# avisa con un error cuando la entrada no sirve

    Code
      detecta_categorias_inconsistentes("texto")
    Condition
      Error in `detecta_categorias_inconsistentes()`:
      x `datos` debe ser un data.frame o tibble.
      i Recibiste un objeto de clase <character>.

---

    Code
      detecta_categorias_inconsistentes(data.frame())
    Condition
      Error in `detecta_categorias_inconsistentes()`:
      x `datos` no tiene columnas.
      i No hay valores que revisar en un data.frame sin columnas.

---

    Code
      detecta_categorias_inconsistentes(data.frame(a = character(0)))
    Condition
      Error in `detecta_categorias_inconsistentes()`:
      x `datos` no tiene filas.
      i No hay valores que comparar en un data.frame sin filas.

