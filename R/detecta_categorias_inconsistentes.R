#' Detecta categorías escritas de formas distintas
#'
#' Revisa las columnas de texto y busca valores que son el mismo, pero están
#' escritos de formas distintas, por ejemplo `"Guayas"`, `"guayas "` y
#' `"GUAYAS"`. Es un problema habitual en datos que se teclean a mano, porque
#' para el análisis cada forma es una categoría diferente.
#'
#' @param datos Un data.frame o tibble.
#'
#' @details
#' Para cada columna de texto (los factores se tratan como texto) se ignoran
#' los `NA` y las celdas en blanco. De cada valor distinto se calcula una clave
#' de comparación, se pasa a minúsculas, se quitan los espacios del inicio y del
#' final, los espacios internos se reducen a uno y se quitan las tildes
#' (`á é í ó ú ü`). La `ñ` se respeta. Los valores que comparten clave forman
#' un grupo, y solo se informa de los grupos con dos o más formas escritas
#' distintas. Los valores originales no se modifican.
#'
#' Se revisan todas las columnas de texto, sin importar cuántas categorías
#' tengan. Las columnas numéricas, de fechas y lógicas no se revisan.
#'
#' Solo detecta diferencias de mayúsculas, espacios y tildes. No detecta
#' errores de tecleo (`"Pinchincha"` frente a `"Pichincha"`) ni sinónimos
#' (`"Costa"` y `"Litoral"`). Quitar las tildes puede unir palabras que sí son
#' distintas, como `"si"` y `"sí"`, sin embargo, la función avisa y la persona
#' decide. La tabla de tildes solo cubre el español.
#'
#' @return Un data.frame que se devuelve de forma invisible, con una fila por
#'   cada grupo de formas distintas y siete columnas:
#'   * `fila`: primera fila de la tabla donde aparece cualquiera de las formas
#'     del grupo.
#'   * `columna`: posición de la columna.
#'   * `contenido`: las formas entre comillas con su número de filas, de más a
#'     menos frecuente.
#'   * `problema`: nombre corto del problema.
#'   * `detalle`: explicación de lo que se encontró.
#'   * `importancia`: por qué importa.
#'   * `solucion_recomendada`: acción recomendada.
#'
#'   Las filas van ordenadas por columna y, dentro de cada una, por `fila`. Si
#'   no detecta categorías inconsistentes, devuelve un data.frame con las mismas
#'   siete columnas y cero filas.
#'
#' @examples
#' datos <- data.frame(
#'   provincia = c("Loja", "Azuay", "Guayas", "guayas ", "Guayas", "GUAYAS"),
#'   ventas = c(10, 20, 30, 40, 50, 60)
#' )
#' detecta_categorias_inconsistentes(datos)
#'
#' @export
detecta_categorias_inconsistentes <- function(datos) {
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
      "i" = "No hay valores que revisar en un data.frame sin columnas."
    ))
  }

  # debe tener filas
  if (nrow(datos) == 0) {
    cli::cli_abort(c(
      "x" = "{.arg datos} no tiene filas.",
      "i" = "No hay valores que comparar en un data.frame sin filas."
    ))
  }

  # solo se revisan las columnas de texto
  es_texto <- vapply(datos, is.character, logical(1)) |
    vapply(datos, is.factor, logical(1))

  # codigos Unicode de las vocales con tilde (y u con dieresis), primero
  # mayusculas y luego minusculas, y de la enie mayuscula. La tabla es
  # explicita y solo cubre el espanol. Se trabaja con codigos y no con
  # chartr(), iconv() o tolower() para que el resultado sea igual en cualquier
  # sistema e idioma
  con_tilde <- c(
    0xC1L,
    0xC9L,
    0xCDL,
    0xD3L,
    0xDAL,
    0xDCL,
    0xE1L,
    0xE9L,
    0xEDL,
    0xF3L,
    0xFAL,
    0xFCL,
    0xD1L
  )
  sin_tilde <- c(
    97L,
    101L,
    105L,
    111L,
    117L,
    117L,
    97L,
    101L,
    105L,
    111L,
    117L,
    117L,
    0xF1L
  )

  # grupos de formas distintas encontrados: un elemento por grupo
  grupo_columna <- integer(0)
  grupo_fila <- integer(0)
  grupo_formas <- list()
  grupo_filas <- list()

  for (j in unname(which(es_texto))) {
    # filas con valor (se ignoran los NA)
    x <- as.character(datos[[j]])
    con_valor <- which(!is.na(x))
    if (length(con_valor) == 0) {
      next
    }
    valores <- x[con_valor]

    # formas distintas, cuantas filas tiene cada una y donde aparece primero
    formas <- unique(valores)
    posicion <- match(valores, formas)
    filas <- tabulate(posicion, nbins = length(formas))
    primera <- con_valor[match(seq_along(formas), posicion)]

    # clave de comparacion de cada forma distinta (el original no se toca)
    claves <- character(length(formas))
    for (i in seq_along(formas)) {
      forma <- formas[i]

      # codigos Unicode del texto; solo el texto declarado latin1 necesita
      # conversion
      if (Encoding(forma) == "latin1") {
        forma <- enc2utf8(forma)
      }
      codigos <- utf8ToInt(forma)

      # si no es UTF-8 valido, la clave es el texto tal cual
      if (anyNA(codigos)) {
        claves[i] <- forma
        next
      }

      # espacio duro (160), tabuladores y saltos de linea (9 a 13) a espacio
      codigos[codigos %in% c(9L:13L, 160L)] <- 32L

      # tramos de espacios a uno solo y sin espacios en los extremos
      if (length(codigos) > 0) {
        es_espacio <- codigos == 32L
        repetido <- es_espacio & c(FALSE, es_espacio[-length(es_espacio)])
        codigos <- codigos[!repetido]
      }
      if (length(codigos) > 0 && codigos[1] == 32L) {
        codigos <- codigos[-1]
      }
      if (length(codigos) > 0 && codigos[length(codigos)] == 32L) {
        codigos <- codigos[-length(codigos)]
      }

      # tildes a vocales sin tilde, y enie mayuscula a enie minuscula
      posicion_tilde <- match(codigos, con_tilde)
      tiene_tilde <- !is.na(posicion_tilde)
      codigos[tiene_tilde] <- sin_tilde[posicion_tilde[tiene_tilde]]

      # mayusculas ASCII a minusculas
      es_mayuscula <- codigos >= 65L & codigos <= 90L
      codigos[es_mayuscula] <- codigos[es_mayuscula] + 32L

      claves[i] <- intToUtf8(codigos)
    }

    # se descartan las celdas en blanco (clave vacia)
    conservar <- claves != ""
    formas <- formas[conservar]
    filas <- filas[conservar]
    primera <- primera[conservar]
    claves <- claves[conservar]

    # grupos de formas con la misma clave, en orden de aparicion; solo
    # interesan los que tienen dos o mas formas escritas distintas
    indices <- split(seq_along(formas), factor(claves, levels = unique(claves)))
    indices <- unname(indices[lengths(indices) >= 2])

    for (indice in indices) {
      # formas de mas a menos filas (si empatan, por primera aparicion)
      indice <- indice[order(-filas[indice], primera[indice])]
      grupo_columna <- c(grupo_columna, j)
      grupo_fila <- c(grupo_fila, min(primera[indice]))
      grupo_formas[[length(grupo_formas) + 1]] <- formas[indice]
      grupo_filas[[length(grupo_filas) + 1]] <- filas[indice]
    }
  }

  # si no se encuentran problemas devuelve el esquema sin filas
  if (length(grupo_formas) == 0) {
    cli::cli_inform(c(
      "v" = "No se detectaron valores escritos de formas distintas."
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

  # orden del resultado: por columna y, dentro de cada una, por fila
  orden <- order(grupo_columna, grupo_fila)
  columna <- grupo_columna[orden]
  fila <- grupo_fila[orden]
  grupo_formas <- grupo_formas[orden]
  grupo_filas <- grupo_filas[orden]
  n_formas <- lengths(grupo_formas)

  # nombre de la columna, o "sin nombre"
  nombres <- names(datos)[columna]
  nombres[is.na(nombres) | !nzchar(trimws(nombres))] <- "sin nombre"

  # formas con su numero de filas, por ejemplo "Guayas" (6 filas)
  formas_con_filas <- character(length(grupo_formas))
  for (g in seq_along(grupo_formas)) {
    formas_con_filas[g] <- paste0(
      '"',
      grupo_formas[[g]],
      '" (',
      grupo_filas[[g]],
      ifelse(grupo_filas[[g]] == 1, " fila", " filas"),
      ")",
      collapse = ", "
    )
  }

  # valores del esquema de salida si se detectan problemas
  contenido <- formas_con_filas
  problema <- "Un mismo valor escrito de formas distintas."
  importancia <- paste(
    "Al contar, filtrar o graficar por esta columna,",
    "el mismo valor queda repartido en varias categorias",
    "y los totales salen mas bajos de lo real."
  )
  solucion_recomendada <- paste(
    "Elige una sola forma de escribir el valor",
    "y usala en todas las filas."
  )

  # detalle del data.frame
  detalle <- paste0(
    "La columna ",
    columna,
    " (",
    nombres,
    ") tiene un mismo valor escrito de ",
    n_formas,
    " formas distintas: ",
    formas_con_filas,
    "."
  )

  # detalle de la consola
  if (length(grupo_formas) == 1) {
    formas <- paste0('"', grupo_formas[[1]], '"', collapse = ", ")
    detalle_consola <- paste0(
      "En la columna ",
      columna,
      " (",
      nombres,
      ") hay un valor escrito de ",
      n_formas,
      " formas distintas: ",
      formas,
      "."
    )
  } else {
    posiciones <- unique(columna)
    detalle_consola <- paste0(
      "Hay ",
      length(grupo_formas),
      " valores escritos de formas distintas (",
      ifelse(length(posiciones) == 1, "columna ", "columnas "),
      paste(posiciones, collapse = ", "),
      ")."
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
