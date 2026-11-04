#' Detecta columnas repetidas
#'
#' Revisa si alguna columna repite exactamente el contenido de otra columna
#' anterior de la tabla, lo que indica que la misma información está guardada
#' dos veces o más.
#'
#' @param datos Un data.frame o tibble.
#'
#' @details
#' Solo se compara el contenido de las columnas, no sus nombres. La
#' comparación es exacta y sin normalizar: `""` no es igual a `NA`, y el
#' texto `"10"` no es igual al número `10`. Los `NA` en las mismas posiciones
#' si cuentan como iguales.
#'
#' Las columnas iguales forman un grupo, la primera es la original y las
#' demás son sus copias. Cada grupo se informa en una sola fila, y las copias
#' no se vuelven a comparar con otras columnas.
#'
#' Las columnas totalmente vacías no se tienen en cuenta, porque ya las
#' detecta [detecta_columnas_vacias()].
#'
#' @return Un data.frame que se devuelve de forma invisible, con una fila por
#'   cada columna original que tiene copias y siete columnas:
#'   * `fila`: siempre `NA`, porque el problema afecta a la columna completa.
#'   * `columna`: posición de la columna original, la primera de su grupo.
#'   * `contenido`: nombre de la columna original. Si no tiene nombre, se
#'   escribe `"sin nombre"`.
#'   * `problema`: nombre corto del problema.
#'   * `detalle`: explicación de lo que se encontró, con las posiciones de
#'   todas las copias.
#'   * `importancia`: por qué importa.
#'   * `solucion_recomendada`: acción recomendada.
#'
#'   Si no detecta columnas repetidas, devuelve un data.frame con las mismas
#'   siete columnas y cero filas.
#'
#' @examples
#' datos <- data.frame(
#'   ventas = c(10, 20, 30),
#'   region = c("Costa", "Sierra", "Oriente"),
#'   total = c(10, 20, 30)
#' )
#' detecta_columnas_repetidas(datos)
#'
#' @export
detecta_columnas_repetidas <- function(datos) {
  # debe ser un data.frame
  if (!is.data.frame(datos)) {
    cli::cli_abort(c(
      "x" = "{.arg datos} debe ser un data.frame o tibble.",
      "i" = "Recibiste un objeto de clase {.cls {class(datos)}}."
    ))
  }

  # debe tener columnas
  if (ncol(datos) == 0) {
    cli::cli_abort(c(
      "x" = "{.arg datos} no tiene columnas.",
      "i" = "No hay columnas que comparar en un data.frame vacio."
    ))
  }

  # debe tener filas
  if (nrow(datos) == 0) {
    cli::cli_abort(c(
      "x" = "{.arg datos} no tiene filas.",
      "i" = "Sin filas no hay contenido que comparar entre columnas."
    ))
  }

  # las columnas vacias se ignoran, pero las posiciones se mantienen
  # las de la tabla original
  # se usa detecta_columnas_vacias() sin mostrar su mensaje en consola
  vacias <- suppressMessages(detecta_columnas_vacias(datos))$columna
  candidatas <- setdiff(seq_len(ncol(datos)), vacias)

  # cada candidata se compara con las anteriores que no son copias
  # y se guarda la primera que sea identica
  copias <- integer(0)
  originales <- integer(0)
  for (k in seq_along(candidatas)) {
    actual <- candidatas[k]
    previas <- setdiff(candidatas[seq_len(k - 1)], copias)
    for (previa in previas) {
      if (identical(datos[[previa]], datos[[actual]])) {
        copias <- c(copias, actual)
        originales <- c(originales, previa)
        break
      }
    }
  }

  # si no se encuentran problemas devuelve el esquema sin filas
  if (length(copias) == 0) {
    cli::cli_inform(c(
      "v" = "No se detectaron columnas repetidas."
    ))
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

  # nombres para mostrar
  nombres <- names(datos)
  nombres[is.na(nombres) | nombres == ""] <- "sin nombre"

  # una fila por cada columna original que tiene copias
  grupos <- sort(unique(originales))

  # valores del esquema de salida si se detectan columnas repetidas
  fila <- NA_integer_
  columna <- grupos
  contenido <- nombres[grupos]
  problema <- "Columnas con contenido repetido."
  importancia <- paste(
    "Si los mismos datos se repiten, las sumas, promedios y",
    "conteos que usen las columnas repetidas cuentan el doble.",
    "Ademas, si una de las dos se corrige despues, quedan",
    "contradiciendose."
  )
  solucion_recomendada <-
    "Revisa si los datos se repiten y deja solo una columna."

  # detalle del data.frame, con todas las copias de cada original
  detalle <- vapply(
    grupos,
    function(original) {
      de_este_grupo <- copias[originales == original]
      if (length(de_este_grupo) == 1) {
        paste0(
          "La columna ",
          de_este_grupo,
          " repite exactamente el contenido de la columna ",
          original,
          "."
        )
      } else {
        paste0(
          "Las columnas ",
          paste(de_este_grupo, collapse = ", "),
          " repiten exactamente el contenido de la columna ",
          original,
          "."
        )
      }
    },
    character(1)
  )

  # detalle de la consola
  if (length(grupos) == 1) {
    detalle_consola <- detalle
  } else {
    detalle_consola <- paste0(
      "Las columnas ",
      paste(grupos, collapse = ", "),
      " tienen copias en otras columnas."
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
