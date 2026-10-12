#' Datos de mercado de la industria cervecera (sin procesar)
#'
#' Datos aproximados/estimados de mercado de la industria cervecera por
#' país, entregados por el área de negocio, sin corregir.
#'
#' @format Un data frame con 221 filas y 10 columnas. La fila de título del
#'   informe quedó como encabezado, por lo que los nombres de columna reales
#'   (`PAIS`, `Densidad Horeca`, `Precio barril`, entre otros) aparecen como
#'   la primera fila de datos, y nueve columnas no tienen nombre asignado.
#'
#' @source Datos de mercado con valores aproximados/estimados, con fines
#'   ilustrativos, estructurados tal como suelen llegar desde áreas de
#'   negocio para su análisis.
"cervecero"

#' Ventas anuales de una distribuidora de cerveza en Ecuador (sin procesar)
#'
#' Ventas anuales, en miles de USD, de una distribuidora de cerveza
#' artesanal en ciudades de Ecuador, de 2021 a 2024. Los datos son ficticios
#' y se entregan sin corregir, con problemas de formato intencionales.
#'
#' @format Un data frame con 12 filas y 6 columnas, todas de tipo texto:
#' \describe{
#'   \item{`Region / Ciudad`}{Nombre de la region o de la ciudad.}
#'   \item{(sin nombre)}{Columna totalmente vacia, usada como separador.}
#'   \item{`2021`, `2022`, `2023`, `2024`}{Ventas de cada anio, en miles de USD.}
#' }
#' Problemas incluidos a proposito: una columna vacia, filas de region
#' (`Costa`, `Sierra`, `Amazonia`) mezcladas con filas de ciudad, y los anios
#' como columnas (formato ancho).
#'
#' @details El archivo Excel original, que conserva formato visual que
#'   `read_excel()` descarta (negrita y color de celda), se instala en
#'   `extdata/ventas_ec.xlsx`. Se localiza con
#'   `system.file("extdata", "ventas_ec.xlsx", package = "florencia")`.
#'
#' @source Datos ficticios creados con fines ilustrativos.
"ventas_ec"
