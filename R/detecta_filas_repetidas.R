#' Detecta filas repetidas
#'
#' Revisa si hay filas idénticas en la tabla, es decir, filas que repiten
#' exactamente los mismos valores en todas sus columnas.
#'
#' @param datos Un data.frame o tibble.
#'
#' @details
#' Se comparan todas las celdas de cada fila. Los nombres de las columnas no
#' intervienen. La comparación es exacta, entre `"Ana"` y `"ana "` son valores
#' distintos. Dos `NA` en la misma posición cuentan como iguales.
#'
#' Las filas totalmente vacías (todas sus celdas en `NA` o en texto en blanco)
#' no se consideran repetidas. Las posiciones que se informan se refieren a la
#' tabla original.
#'
#' @return Un data.frame que se devuelve de forma invisible, con una fila por
#'   cada grupo de filas repetidas y siete columnas:
#'   * `fila`: posición de la primera fila del grupo.
#'   * `columna`: siempre `NA`, porque el problema es de toda la fila.
#'   * `contenido`: valores de las tres primeras columnas de la fila repetida,
#'     separados por `" | "`.
#'   * `problema`: nombre corto del problema.
#'   * `detalle`: posiciones de todas las filas del grupo.
#'   * `importancia`: por qué importa.
#'   * `solucion_recomendada`: acción recomendada.
#'
#'   Si no detecta filas repetidas, devuelve un data.frame con las mismas siete
#'   columnas y cero filas.
#'
#' @examples
#' datos <- data.frame(
#'   region = c("Costa", "Sierra", "Costa"),
#'   producto = c("A", "B", "A"),
#'   ventas = c(10, 20, 10)
#' )
#' detecta_filas_repetidas(datos)
#'
#' @export
detecta_filas_repetidas <- function(datos) {
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
      "i" = "No hay filas que comparar en un data.frame sin columnas."
    ))
  }

  # debe tener filas
  if (nrow(datos) == 0) {
    cli::cli_abort(c(
      "x" = "{.arg datos} no tiene filas.",
      "i" = "Una tabla sin filas suele indicar un problema al cargar el archivo."
    ))
  }

  # filas totalmente vacias (todas sus celdas en NA o en blanco)
  es_vacia <- function(x) is.na(x) | trimws(as.character(x)) == ""
  vacias <- do.call(cbind, lapply(datos, es_vacia))
  posiciones <- which(rowSums(!vacias) > 0)

  # clave por fila donde cada valor se cambia por un codigo (NA incluido)
  codigos <- lapply(datos[posiciones, , drop = FALSE], function(x) {
    match(x, unique(x))
  })
  clave <- do.call(paste, c(unname(codigos), sep = "-"))

  # grupos de dos o mas filas identicas, ordenados por su primera fila
  grupos <- split(posiciones, clave)
  grupos <- grupos[lengths(grupos) >= 2]
  primeras <- vapply(grupos, function(g) g[1], integer(1))
  orden <- order(primeras)
  grupos <- unname(grupos[orden])
  primeras <- unname(primeras[orden])

  # si no se encuentran problemas devuelve el esquema sin filas
  if (length(grupos) == 0) {
    cli::cli_inform(c(
      "v" = "No se detectaron filas repetidas."
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

  # textos fijos
  problema <- "Filas repetidas."
  importancia <- paste(
    "Una fila repetida cuenta dos veces el mismo registro.",
    "Las sumas, los promedios y los conteos salen inflados,",
    "y nadie lo ve a simple vista."
  )
  solucion_recomendada <- paste(
    "Revisa si es un registro cargado dos veces.",
    "Si lo es, conserva una sola copia y elimina las demas."
  )

  # detalle del data.frame, una frase por grupo
  unir <- function(x) {
    paste0(paste(x[-length(x)], collapse = ", "), " y ", x[length(x)])
  }
  detalle <- vapply(
    grupos,
    function(g) {
      paste0(
        "Las filas ",
        unir(g),
        " son iguales: todas sus celdas coinciden."
      )
    },
    character(1)
  )

  # contenido: las 3 primeras columnas (o las que haya), NA como vacio
  n_col <- min(3, ncol(datos))
  contenido <- vapply(
    primeras,
    function(i) {
      valores <- vapply(
        datos[i, seq_len(n_col), drop = FALSE],
        as.character,
        character(1)
      )
      valores[is.na(valores)] <- ""
      paste(valores, collapse = " | ")
    },
    character(1)
  )

  # mensaje en la consola, todas las posiciones de todos los grupos
  filas <- cli::cli_vec(
    sort(unlist(grupos)),
    style = list("vec-sep" = ", ", "vec-last" = " y ", "vec-sep2" = " y ")
  )
  cli::cli_inform(c(
    "!" = "{.strong Problema:} {problema}",
    "i" = "{.strong Detalle:} Las filas {filas} est\u00e1n repetidas.",
    "i" = "{.strong Soluci\u00f3n:} {solucion_recomendada}"
  ))

  # data.frame de problemas
  invisible(data.frame(
    fila = primeras,
    columna = NA_integer_,
    contenido = contenido,
    problema = problema,
    detalle = detalle,
    importancia = importancia,
    solucion_recomendada = solucion_recomendada
  ))
}
