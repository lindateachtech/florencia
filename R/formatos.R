#' Detecta encabezados desplazados
#'
#' Revisa si los nombres de columna de un conjunto de datos parecen
#' provenir de una fila de título incrustada en el archivo original, o
#' de una fila vacía, en vez de ser los nombres de columna reales.
#'
#' @param datos Un data.frame o tibble a revisar.
#'
#' @details
#' Esta función asume que estamos trabajando con conjuntos de datos de
#' varias columnas, como los que se generan normalmente para análisis de
#' datos. Si se aplica a una tabla genuinamente pequeña (dos o
#' tres columnas en total), puede marcar un falso positivo, ya que
#' la lógica de detección se basa en cuántas columnas tienen nombre,
#' sin considerar el total de columnas del conjunto de datos.
#'
#' @return De forma invisible, un data.frame con las columnas `fila`,
#'   `problema`, `detalle`, `importancia` y `solucion_recomendada`, útil
#'   para uso programático, por ejemplo, desde una función que
#'   combine varios diagnósticos, o desde pruebas con testthat. Al
#'   llamarla directamente, la función también imprime un mensaje
#'   legible en consola indicando si se detectó o no el problema.
#'
#' @examples
#' detecta_encabezados(cervecero)
#'
#' @export
detecta_encabezados <- function(datos) {
  if (!is.data.frame(datos)) {
    cli::cli_abort(c(
      "x" = "{.arg datos} debe ser un data.frame o tibble.",
      "i" = "Recibiste un objeto de clase {.cls {class(datos)}}."
    ))
  }

  nombres <- names(datos)
  nombres_reales <- sum(nombres != "" & !is.na(nombres))

  if (nombres_reales > 2) {
    cli::cli_inform(c("v" = "No se detectaron encabezados desplazados."))
    return(invisible(data.frame(
      fila = integer(0),
      problema = character(0),
      detalle = character(0),
      importancia = character(0),
      solucion_recomendada = character(0)
    )))
  }

  problema <- "Posible encabezado desplazado."
  detalle <- paste(
    "El encabezado real del conjunto de datos parece estar",
    "desplazado, debido a que la mayoria de las columnas no",
    "tienen nombre."
  )
  importancia <- paste(
    "Si el encabezado esta mal ubicado, absolutamente todo lo",
    "demas queda afectado: no se puede referenciar ninguna",
    "columna por su nombre real, y los calculos que dependen de",
    "tipos de datos van a fallar."
  )
  solucion_recomendada <- paste(
    "Usa corrige_encabezados() para promover la fila correcta a",
    "nombres de columna."
  )

  cli::cli_inform(c(
    "!" = "{.strong Problema:} {problema}",
    "i" = "{.strong Detalle:} {detalle}",
    "i" = paste(
      "{.strong Solucion recomendada:} Usa {.fn corrige_encabezados}",
      "para promover la fila correcta a nombres de columna."
    )
  ))

  invisible(data.frame(
    fila = NA_integer_,
    problema = problema,
    detalle = detalle,
    importancia = importancia,
    solucion_recomendada = solucion_recomendada
  ))
}
