#' Detecta un encabezado desplazado
#'
#' Revisa si los nombres de columna de un conjunto de datos parecen
#' provenir de una fila de título incrustada en el archivo original, o
#' de una fila vacía, en vez de ser los nombres de columna reales.
#'
#' @param datos Un data.frame o tibble a revisar.
#'
#' @details
#' La función calcula qué proporción de las columnas tiene un nombre
#' real (ni vacío ni `NA`). Si menos de la mitad de las columnas tiene
#' nombre, se considera que el encabezado está desplazado. Este corte
#' es una heurística fija, no un valor ajustable y sirve tanto para
#' tablas anchas con un título de pocas celdas en la primera fila como
#' para tablas con una sola celda de título. Una tabla con exactamente
#' la mitad de columnas con nombre se considera sana.
#'
#' @return De forma invisible, un data.frame con las columnas `fila`,
#'   `columna`, `contenido`, `problema`, `detalle`, `importancia` y
#'   `solucion_recomendada`, útil para uso programático, por ejemplo,
#'   desde una función que combine varios diagnósticos, o desde pruebas
#'   con testthat. `fila` es la primera fila del data.frame con al menos
#'   el 50 % de celdas con contenido, candidata a encabezado real, y `NA`
#'   si no se localiza ninguna. `columna` queda en `NA`, y `contenido`
#'   reúne los nombres de columna válidos, separados por `" | "`. Si no
#'   se detecta el problema, el data.frame tiene 0 filas. Al llamarla
#'   directamente, la función también imprime un mensaje legible en
#'   consola indicando si se detectó o no el problema.
#'
#' @examples
#' detecta_encabezado(cervecero)
#'
#' @export
detecta_encabezado <- function(datos) {
  if (!is.data.frame(datos)) {
    cli::cli_abort(c(
      "x" = "{.arg datos} debe ser un data.frame o tibble.",
      "i" = "Recibiste un objeto de clase {.cls {class(datos)}}."
    ))
  }

  if (ncol(datos) == 0) {
    cli::cli_abort(c(
      "x" = "{.arg datos} no tiene columnas.",
      "i" = "No hay encabezado que revisar en un data.frame vacio."
    ))
  }

  # encabezado del conjunto de datos
  nombres <- names(datos)
  con_nombre <- nombres != "" & !is.na(nombres)

  # si al menos la mitad tienen nombre, devuelve el esquema sin filas
  if (mean(con_nombre) >= 0.5) {
    cli::cli_inform(c("v" = "No se detecto un encabezado desplazado."))
    return(invisible(data.frame(
      fila = integer(0),
      columna = integer(0),
      contenido = character(0),
      problema = character(0),
      detalle = character(0),
      importancia = character(0),
      solucion_recomendada = character(0)
    )))
  }

  # primera fila con al menos el 50 % de celdas con contenido
  celdas <- as.matrix(datos)

  # TRUE si la celda no es NA ni texto en blanco
  llena <- !is.na(celdas) & trimws(celdas) != ""

  # posición de la primera fila que cumple >=0.5 de nombres
  fila <- which(rowMeans(llena) >= 0.5)[1]

  # texto que indica la posición del encabezado real
  localizacion <- if (is.na(fila)) {
    "No se pudo localizar la fila del encabezado real."
  } else {
    paste0("El encabezado real parece estar en la fila ", fila, ".")
  }

  # valores del esquema de salida si se encuentra encabezado desplazado
  problema <- "Posible encabezado desplazado."
  detalle <- paste(
    "El encabezado real del conjunto de datos parece estar desplazado.",
    localizacion
  )
  importancia <- paste(
    "Si el encabezado esta mal ubicado, absolutamente todo lo demas queda",
    "afectado porque no se puede referenciar ninguna columna por su nombre",
    "real, y los calculos que dependen de tipos de datos van a fallar."
  )
  solucion_recomendada <- paste(
    "Promueve el encabezado real a nombres de columna."
  )
  contenido <- paste(nombres[con_nombre], collapse = " | ")

  # mensaje en la consola si se detectan errores
  cli::cli_inform(c(
    "!" = "{.strong Problema:} {problema}",
    "i" = "{.strong Detalle:} {detalle}",
    "i" = "{.strong Solucion:} {solucion_recomendada}"
  ))

  # data.frame de problemas
  invisible(data.frame(
    fila = fila,
    columna = NA_integer_,
    contenido = contenido,
    problema = problema,
    detalle = detalle,
    importancia = importancia,
    solucion_recomendada = solucion_recomendada
  ))
}
