#' Detecta nombres de columnas en formato anual
#'
#' Revisa si los nombres de las columnas contienen un año de cuatro dígitos
#' (por ejemplo `2020`, `ventas_2020` o `T1 2020`), lo que indica una tabla en
#' formato ancho en la que cada año ocupa su propia columna.
#'
#' @param datos Un data.frame o tibble.
#'
#' @details
#' Solo se miran los nombres de las columnas, no su contenido. Una columna se
#' considera anual si su nombre, sin espacios en los extremos, contiene un año
#' de cuatro dígitos entre 1900 y 2099 que no tenga otro dígito pegado. Si el
#' nombre tiene más de un año, se usa el primero. No se detectan los años de
#' dos dígitos (como `'20`). Solo se detecta si hay dos o más columnas anuales.
#'
#' Si el encabezado está desplazado (ver [detecta_encabezado()]), los años no
#' estarán en los nombres de columna y esta función no los verá.
#'
#' @return Un data.frame que se devuelve de forma invisible, con una fila por
#'   cada columna anual y siete columnas:
#'   * `fila`: siempre `NA`, porque el problema está en el encabezado.
#'   * `columna`: posición de la columna anual.
#'   * `contenido`: nombre de la columna (por ejemplo `"2020"` o `"ventas_2020"`).
#'   * `problema`: nombre corto del problema.
#'   * `detalle`: explicación de lo que se encontró.
#'   * `importancia`: por qué importa.
#'   * `solucion_recomendada`: acción recomendada.
#'
#'   Si no detecta columnas en formato anual, devuelve un data.frame con las
#'   mismas siete columnas y cero filas.
#'
#' @examples
#' datos <- data.frame(
#'   region = c("Costa", "Sierra"),
#'   `2019` = c(10, 20),
#'   `2020` = c(12, 22),
#'   `2021` = c(24, 26),
#'   check.names = FALSE
#' )
#' detecta_columnas_anuales(datos)
#'
#' @export
detecta_columnas_anuales <- function(datos) {
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
      "i" = "No hay nombres de columna que revisar en un data.frame vacio."
    ))
  }

  # nombres sin espacios en los extremos
  nombres <- names(datos)
  limpios <- trimws(nombres)

  # anio de 4 digitos (1900-2099) sin otro digito pegado
  patron <- "(?<![0-9])(19|20)[0-9]{2}(?![0-9])"
  es_anual <- grepl(patron, limpios, perl = TRUE)
  columnas <- unname(which(es_anual))

  # si no se encuentran problemas devuelve el esquema sin filas
  if (length(columnas) < 2) {
    cli::cli_inform(c(
      "v" = "No se detectaron columnas con anios en el nombre."
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

  # valores del esquema de salida si se detectan columnas anuales
  fila <- NA_integer_
  columna <- columnas
  contenido <- nombres[columnas]
  problema <- "Nombres de columnas en formato anual."
  importancia <- paste(
    "Cada anio es una columna distinta.",
    "Con este esquema, el anio no es un dato que se pueda filtrar,",
    "agrupar ni graficar."
  )
  solucion_recomendada <- paste(
    "Pasa las columnas de anios a dos columnas,",
    "una con el anio y otra con el valor."
  )

  # detalle del data.frame
  detalle <- paste0(
    "La columna ",
    columnas,
    " tiene un anio en el nombre (",
    contenido,
    ")."
  )

  # detalle de la consola
  posiciones <- paste(columnas, collapse = ", ")
  anios <- paste(contenido, collapse = ", ")
  detalle_consola <- paste0(
    "Las columnas ",
    posiciones,
    " tienen un anio en el nombre: ",
    anios,
    "."
  )

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
