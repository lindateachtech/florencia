test_that("detecta_columnas_repetidas() detecta una columna copia", {
  datos <- data.frame(
    a = 1:3,
    b = c("x", "y", "z"),
    c = 1:3
  )
  resultado <- suppressMessages(detecta_columnas_repetidas(datos))

  expect_equal(nrow(resultado), 1)
  expect_named(
    resultado,
    c(
      "fila",
      "columna",
      "contenido",
      "problema",
      "detalle",
      "importancia",
      "solucion_recomendada"
    )
  )
  expect_true(is.na(resultado$fila))
  expect_equal(resultado$columna, 1L)
  expect_equal(resultado$contenido, "a")
  expect_equal(
    resultado$detalle,
    "La columna 3 repite exactamente el contenido de la columna 1."
  )
})

test_that("detecta_columnas_repetidas() devuelve 0 filas en una tabla sana", {
  datos <- data.frame(
    a = 1:3,
    b = c("x", "y", "z"),
    c = c(TRUE, FALSE, TRUE)
  )
  resultado <- suppressMessages(detecta_columnas_repetidas(datos))

  expect_equal(nrow(resultado), 0)
  expect_named(
    resultado,
    c(
      "fila",
      "columna",
      "contenido",
      "problema",
      "detalle",
      "importancia",
      "solucion_recomendada"
    )
  )
})

test_that("detecta_columnas_repetidas() agrupa las copias por columna original", {
  datos <- data.frame(
    a = 1:3,
    b = 1:3,
    c = 1:3,
    d = c("x", "y", "z"),
    e = c("x", "y", "z")
  )
  resultado <- suppressMessages(detecta_columnas_repetidas(datos))

  expect_equal(nrow(resultado), 2)
  expect_equal(resultado$columna, c(1L, 4L))
  expect_equal(resultado$contenido, c("a", "d"))
  expect_equal(
    resultado$detalle,
    c(
      "Las columnas 2, 3 repiten exactamente el contenido de la columna 1.",
      "La columna 5 repite exactamente el contenido de la columna 4."
    )
  )
})

test_that("detecta_columnas_repetidas() detecta aunque los nombres sean distintos", {
  datos <- data.frame(
    ventas = c(10, 20, 30),
    total = c(10, 20, 30)
  )
  resultado <- suppressMessages(detecta_columnas_repetidas(datos))

  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$columna, 1L)
  expect_equal(resultado$contenido, "ventas")
})

test_that("detecta_columnas_repetidas() trata los NA en las mismas posiciones como iguales", {
  datos <- data.frame(
    a = c(1, NA, 3),
    b = c(1, NA, 3),
    c = c(1, 2, NA)
  )
  resultado <- suppressMessages(detecta_columnas_repetidas(datos))

  expect_equal(nrow(resultado), 1)
  expect_equal(
    resultado$detalle,
    "La columna 2 repite exactamente el contenido de la columna 1."
  )
})

test_that("detecta_columnas_repetidas() ignora las columnas vacias", {
  datos <- data.frame(
    a = 1:3,
    b = NA,
    c = NA,
    d = "",
    e = ""
  )
  resultado <- suppressMessages(detecta_columnas_repetidas(datos))

  expect_equal(nrow(resultado), 0)
})

test_that("detecta_columnas_repetidas() no marca texto y numero con la misma informacion", {
  datos <- data.frame(
    a = 1:3,
    b = c("1", "2", "3")
  )
  resultado <- suppressMessages(detecta_columnas_repetidas(datos))

  expect_equal(nrow(resultado), 0)
})

test_that("detecta_columnas_repetidas() no aborta con una sola columna", {
  datos <- data.frame(a = 1:3)
  resultado <- suppressMessages(detecta_columnas_repetidas(datos))

  expect_equal(nrow(resultado), 0)
})

test_that("detecta_columnas_repetidas() funciona con nombres de columna duplicados", {
  datos <- data.frame(
    a = 1:3,
    a = 1:3,
    b = c("x", "y", "z"),
    check.names = FALSE
  )
  resultado <- suppressMessages(detecta_columnas_repetidas(datos))

  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$columna, 1L)
  expect_equal(resultado$contenido, "a")
  expect_equal(
    resultado$detalle,
    "La columna 2 repite exactamente el contenido de la columna 1."
  )
})

test_that("detecta_columnas_repetidas() mantiene las posiciones de la tabla original", {
  datos <- data.frame(
    a = 1:3,
    b = NA,
    c = 1:3
  )
  resultado <- suppressMessages(detecta_columnas_repetidas(datos))

  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$columna, 1L)
  expect_equal(
    resultado$detalle,
    "La columna 3 repite exactamente el contenido de la columna 1."
  )
})

test_that("detecta_columnas_repetidas() resume muchas copias en una sola fila", {
  datos <- data.frame(
    a = 1:3,
    c1 = 1:3,
    c2 = 1:3,
    c3 = 1:3,
    c4 = 1:3,
    c5 = 1:3,
    c6 = 1:3
  )
  resultado <- suppressMessages(detecta_columnas_repetidas(datos))

  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$columna, 1L)
  expect_equal(
    resultado$detalle,
    paste(
      "Las columnas 2, 3, 4, 5, 6, 7 repiten exactamente",
      "el contenido de la columna 1."
    )
  )
})

test_that("detecta_columnas_repetidas() ordena las filas por posicion de la original", {
  datos <- data.frame(
    a = 1:3,
    b = c("x", "y", "z"),
    c = c("x", "y", "z"),
    d = 1:3
  )
  resultado <- suppressMessages(detecta_columnas_repetidas(datos))

  expect_equal(resultado$columna, c(1L, 2L))
})

test_that("detecta_columnas_repetidas() muestra el mensaje con una copia", {
  datos <- data.frame(
    a = 1:3,
    b = c("x", "y", "z"),
    c = 1:3
  )
  expect_snapshot(detecta_columnas_repetidas(datos))
})

test_that("detecta_columnas_repetidas() muestra el mensaje con varias copias", {
  datos <- data.frame(
    a = 1:3,
    c1 = 1:3,
    c2 = 1:3
  )
  expect_snapshot(detecta_columnas_repetidas(datos))
})

test_that("detecta_columnas_repetidas() muestra el mensaje con varios grupos", {
  datos <- data.frame(
    a = 1:3,
    b = 1:3,
    c = c("x", "y", "z"),
    d = c("x", "y", "z")
  )
  expect_snapshot(detecta_columnas_repetidas(datos))
})

test_that("detecta_columnas_repetidas() aborta con entradas no validas", {
  expect_snapshot(detecta_columnas_repetidas(1:3), error = TRUE)
  expect_snapshot(detecta_columnas_repetidas(data.frame()), error = TRUE)
  expect_snapshot(
    detecta_columnas_repetidas(data.frame(a = integer(0))),
    error = TRUE
  )
})
