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

  nombres <- names(datos)
  con_nombre <- nombres != "" & !is.na(nombres)

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

  # Primera fila con al menos el 50 % de celdas con contenido
  celdas <- as.matrix(datos)
  llena <- !is.na(celdas) & trimws(celdas) != ""
  fila <- which(rowMeans(llena) >= 0.5)[1]

  localizacion <- if (is.na(fila)) {
    "No se pudo localizar la fila del encabezado real."
  } else {
    paste0("El encabezado real parece estar en la fila ", fila, ".")
  }

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

  cli::cli_inform(c(
    "!" = "{.strong Problema:} {problema}",
    "i" = "{.strong Detalle:} {detalle}",
    "i" = "{.strong Solucion:} {solucion_recomendada}"
  ))

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
#' La función no marca una columna que tenga aunque sea un solo valor, por
#' ejemplo una celda con una nota olvidada. Tampoco distingue entre una
#' columna vacía por error y una que está vacía de forma legítima, como un
#' campo opcional que nadie rellenó. Avisa y la decisión es de quien revisa.
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

  celdas <- as.matrix(datos)
  llena <- !is.na(celdas) & trimws(celdas) != ""
  vacias <- unname(which(colSums(llena) == 0))

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

  problema <- "Columna vacia."
  importancia <- paste(
    "Una columna vacia no aporta informacion al analisis.",
    "Los calculos por columna pueden devolver NA y, si es un espacio",
    "decorativo, ocupa una posicion que desplaza al resto."
  )
  solucion_recomendada <- "Elimina las columnas sin datos."

  # Detalle de cada fila del data.frame
  detalle <- paste0(
    "La columna ",
    vacias,
    " no contiene ningun valor, todas sus celdas estan vacias."
  )

  # Detalle de la consola
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

  cli::cli_inform(c(
    "!" = "{.strong Problema:} {problema}",
    "i" = "{.strong Detalle:} {detalle_consola}",
    "i" = "{.strong Solucion:} {solucion_recomendada}"
  ))

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
