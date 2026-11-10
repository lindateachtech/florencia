columnas_esquema <- c(
  "fila",
  "columna",
  "contenido",
  "problema",
  "detalle",
  "importancia",
  "solucion_recomendada"
)

test_that("detecta dos filas identicas", {
  datos <- data.frame(a = c("x", "y", "x"), b = c(1, 2, 1))
  res <- suppressMessages(detecta_filas_repetidas(datos))
  expect_equal(nrow(res), 1)
  expect_equal(res$fila, 1)
  expect_named(res, columnas_esquema)
})

test_that("una tabla sana devuelve 0 filas", {
  datos <- data.frame(
    a = c("x", "y", "x"),
    b = c(1, 2, 1),
    c = c("m", "n", "m"),
    d = c("p", "q", "r")
  )
  res <- suppressMessages(detecta_filas_repetidas(datos))
  expect_equal(nrow(res), 0)
  expect_named(res, columnas_esquema)
})

test_that("devuelve un hallazgo por cada grupo", {
  datos <- data.frame(
    a = c("x", "y", "x", "z", "y", "x"),
    b = c(1, 2, 1, 3, 2, 1)
  )
  res <- suppressMessages(detecta_filas_repetidas(datos))
  expect_equal(nrow(res), 2)
  expect_equal(res$fila, c(1, 2))
  expect_match(res$detalle[1], "1, 3 y 6", fixed = TRUE)
  expect_match(res$detalle[2], "2 y 5", fixed = TRUE)
})

test_that("las filas totalmente vacias no se marcan", {
  datos <- data.frame(
    a = c(NA, "x", "", NA, "x", " "),
    b = c(NA, 1, NA, NA, 1, NA)
  )
  res <- suppressMessages(detecta_filas_repetidas(datos))
  expect_equal(nrow(res), 1)
  expect_equal(res$fila, 2)
  expect_match(res$detalle, "2 y 5", fixed = TRUE)
})

test_that("filas identicas con NA en las mismas posiciones se marcan", {
  datos <- data.frame(a = c(NA, "y", NA), b = c(1, 2, 1))
  res <- suppressMessages(detecta_filas_repetidas(datos))
  expect_equal(nrow(res), 1)
  expect_equal(res$fila, 1)
})

test_that("mensaje en consola con dos grupos", {
  datos <- data.frame(
    a = c("x", "y", "x", "z", "y", "x"),
    b = c(1, 2, 1, 3, 2, 1)
  )
  expect_snapshot(detecta_filas_repetidas(datos))
})

test_that("error cuando la entrada no es un data.frame", {
  expect_snapshot(detecta_filas_repetidas("datos"), error = TRUE)
})

test_that("error cuando no tiene columnas", {
  expect_snapshot(detecta_filas_repetidas(data.frame()), error = TRUE)
})

test_that("error cuando no tiene filas", {
  datos <- data.frame(a = character(0), b = numeric(0))
  expect_snapshot(detecta_filas_repetidas(datos), error = TRUE)
})
