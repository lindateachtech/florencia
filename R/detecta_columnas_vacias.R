#' Detecta columnas completamente vacías
#'
#' Revisa si el conjunto de datos tiene columnas sin ningún valor. Es habitual
#' en tablas de Excel donde se deja una columna como separador estético, o
#' donde un borde o un formato hacen que una columna parezca parte de la tabla.
#'
#' @param datos Un data.frame o tibble.
#'
#' @details
#' Una celda se considera vacía si es `NA` o si contiene solo texto en blanco
#' (cadena vacía o espacios). Una columna se marca únicamente si todas sus
#' celdas están vacías. Se mira el contenido, no el nombre, una columna con
#' encabezado pero sin datos también se detecta.
#'
#' La función aborta si `datos` no es un data.frame, si no tiene columnas o
#' si no tiene filas, porque en este último caso todas las columnas
#' parecerían vacías.
#'
#' @return Un data.frame que se devuelve de forma invisible, con una fila por
#'   cada columna vacía y siete columnas:
#'   * `fila`: siempre `NA`, porque el problema afecta a la columna entera.
#'   * `columna`: posición de la columna vacía.
#'   * `contenido`: nombre de la columna (puede ser `""` si no tiene nombre).
#'   * `problema`: nombre corto del problema.
#'   * `detalle`: explicación de lo que se encontró.
#'   * `importancia`: por qué importa. No se imprime en consola.
#'   * `solucion_recomendada`: acción recomendada.
#'
#'   Si no hay columnas vacías, devuelve un data.frame con las mismas siete
#'   columnas y cero filas.
#'
#' @examples
#' detecta_columnas_vacias(ventas_ec)
#'
#' @export
detecta_columnas_vacias <- function(datos) {
  if (!is.data.frame(datos)) {
    cli::cli_abort(c(
      "x" = "{.arg datos} debe ser un data.frame o tibble.",
      "i" = "Recibiste un objeto de clase {.cls {class(datos)}}."
    ))
  }
  if (ncol(datos) == 0) {
    cli::cli_abort(c(
      "x" = "{.arg datos} no tiene columnas.",
      "i" = "No hay columnas que revisar en un data.frame vacio."
    ))
  }
  if (nrow(datos) == 0) {
    cli::cli_abort(c(
      "x" = "{.arg datos} no tiene filas.",
      "i" = "Sin filas, todas las columnas parecerian vacias."
    ))
  }

  # revisión de datos en forma de matriz
  celdas <- as.matrix(datos)

  # matriz de TRUE/FALSE
  llena <- !is.na(celdas) & trimws(celdas) != ""

  # posiciones de las columnas sin contenido
  vacias <- unname(which(colSums(llena) == 0))

  # si no se encuentran problemas devuelve el esquema sin filas
  if (length(vacias) == 0) {
    cli::cli_inform(c("v" = "No se detectaron columnas vacias."))
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

  # valores del esquema de salida si se encuentran columnas vacías
  problema <- "Columna vacia."
  importancia <- paste(
    "Una columna vacia no aporta informacion al analisis.",
    "Los calculos por columna pueden devolver NA y, si es un espacio",
    "decorativo, ocupa una posicion que desplaza al resto."
  )
  solucion_recomendada <- "Elimina las columnas sin datos."

  # detalle de cada fila del data.frame
  detalle <- paste0(
    "La columna ",
    vacias,
    " no contiene ningun valor, todas sus celdas estan vacias."
  )

  # detalle de la consola
  if (length(vacias) == 1) {
    detalle_consola <- detalle
  } else {
    posiciones <- paste(vacias, collapse = ", ")
    detalle_consola <- paste0(
      "Las columnas ",
      posiciones,
      " no contienen ningun valor, todas sus celdas estan vacias."
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
    fila = NA_integer_,
    columna = vacias,
    contenido = names(datos)[vacias],
    problema = problema,
    detalle = detalle,
    importancia = importancia,
    solucion_recomendada = solucion_recomendada
  ))
}
