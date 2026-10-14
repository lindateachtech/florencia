#' Detecta subencabezados incrustados en los datos
#'
#' Revisa si el conjunto de datos contiene subencabezados incrustados, es
#' decir, filas que no son registros sino etiquetas que nombran al grupo de
#' filas que las siguen. Es un patrón habitual en tablas pensadas para leerse
#' y no para analizarse, donde la información de agrupación se guarda en una
#' fila propia en lugar de en una columna.
#'
#' @param datos Un data.frame o tibble con al menos dos columnas.
#'
#' @details
#' Una fila se considera subencabezado si tiene un único valor, situado en la
#' primera columna, y el resto de sus celdas están vacías. Una celda se
#' considera vacía si es `NA` o si contiene solo texto en blanco (cadena vacía
#' o espacios).
#'
#' La función aborta si `datos` no es un data.frame, si tiene menos de 2
#' columnas o si no tiene filas.
#'
#' @return Un data.frame que se devuelve de forma invisible, con una fila por
#'   cada subencabezado y siete columnas:
#'   * `fila`: posición de la fila en `datos` (sin contar el encabezado).
#'   * `columna`: siempre `1`, que es donde está el valor.
#'   * `contenido`: texto del subencabezado.
#'   * `problema`: nombre corto del problema.
#'   * `detalle`: explicación de lo que se encontró.
#'   * `importancia`: por qué importa.
#'   * `solucion_recomendada`: acción recomendada.
#'
#' Si no hay subencabezados, devuelve un data.frame con las mismas siete
#' columnas y cero filas.
#'
#' @examples
#' detecta_subencabezados(ventas_ec)
#'
#' @export
detecta_subencabezados <- function(datos) {
  if (!is.data.frame(datos)) {
    cli::cli_abort(c(
      "x" = "{.arg datos} debe ser un data.frame o tibble.",
      "i" = "Recibiste un objeto de clase {.cls {class(datos)}}."
    ))
  }
  # relacionada a la lógica de detección
  if (ncol(datos) < 2) {
    cli::cli_abort(c(
      "x" = "{.arg datos} debe tener al menos dos columnas.",
      "i" = "Recibiste un data.frame con {ncol(datos)} columna{?s}."
    ))
  }
  if (nrow(datos) == 0) {
    cli::cli_abort(c(
      "x" = "{.arg datos} no tiene filas.",
      "i" = "No hay subencabezados que buscar sin filas."
    ))
  }

  # revisión de datos en forma de matriz
  celdas <- as.matrix(datos)

  # matriz de TRUE/FALSE
  llena <- !is.na(celdas) & trimws(celdas) != ""

  # condiciones para detectar subencabezados
  es_subencabezado <- rowSums(llena) == 1 & llena[, 1]
  filas <- unname(which(es_subencabezado))

  # si no se encuentran problemas devuelve el esquema sin filas
  if (length(filas) == 0) {
    cli::cli_inform(c("v" = "No se detectaron subencabezados incrustados."))
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

  # si se encuentran problemas, devuelve estos valores
  # fila, columna, contenido, problema, importancia,
  # solucion recomendada, detalle
  fila <- filas
  columna <- 1L
  contenido <- unname(as.character(celdas[filas, 1]))
  problema <- "Subencabezado incrustado."
  importancia <- paste(
    "Los registros no tienen una columna que indique su grupo, dependen de",
    "la fila de titulo que tienen encima. Al ordenar o filtrar la tabla",
    "pierden su grupo y no se puede analizar por grupo."
  )
  solucion_recomendada <-
    "Pasa el subencabezado a una columna propia y elimina las filas de titulo."

  detalle <- paste0(
    "La fila ",
    filas,
    " funciona como subencabezado dentro de los datos."
  )

  if (length(filas) == 1) {
    detalle_consola <- paste0(
      "La fila ",
      filas,
      " funciona como subencabezado dentro de los datos."
    )
  } else {
    posiciones <- paste(filas, collapse = ", ")
    detalle_consola <- paste0(
      "Las filas ",
      posiciones,
      " funcionan como subencabezados dentro de los datos."
    )
  }

  # mensaje en la consola si se detectan errores
  cli::cli_inform(c(
    "!" = "{.strong Problema:} {problema}",
    "i" = "{.strong Detalle:} {detalle_consola}",
    "i" = "{.strong Solucion:} {solucion_recomendada}"
  ))

  # data.frame de problemas
  invisible(data.frame(
    fila = fila,
    columna = columna,
    contenido = contenido,
    problema = problema,
    detalle = detalle,
    importancia = importancia,
    solucion_recomendada = solucion_recomendada
  ))
}
