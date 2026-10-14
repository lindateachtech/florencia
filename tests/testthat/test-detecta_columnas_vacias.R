test_that("detecta_columnas_vacias detecta la columna vacia de ventas_ec", {
  resultado <- detecta_columnas_vacias(ventas_ec)

  expect_equal(nrow(resultado), 1)
  expect_equal(
    colnames(resultado),
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
  expect_equal(resultado$columna, 2L)
  expect_true(is.na(resultado$fila))
  expect_equal(resultado$problema, "Columna vacia.")
})

test_that("detecta_columnas_vacias no marca falsos positivos en una tabla sana", {
  datos <- data.frame(
    pais = c("Ecuador", "Peru"),
    precio = c(10, 20),
    volumen = c(5, 8)
  )
  resultado <- detecta_columnas_vacias(datos)

  expect_s3_class(resultado, "data.frame")
  expect_equal(nrow(resultado), 0)
  expect_equal(ncol(resultado), 7)
})

test_that("detecta_columnas_vacias devuelve una fila por cada columna vacia", {
  datos <- data.frame(
    a = 1:3,
    b = NA,
    c = 4:6,
    d = NA_character_
  )
  resultado <- detecta_columnas_vacias(datos)

  expect_equal(nrow(resultado), 2)
  expect_equal(resultado$columna, c(2L, 4L))
})

test_that("detecta_columnas_vacias detecta una columna con nombre pero sin datos", {
  datos <- data.frame(
    pais = c("Ecuador", "Peru"),
    precio = c(NA_real_, NA_real_)
  )
  resultado <- detecta_columnas_vacias(datos)

  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$columna, 2L)
  expect_equal(resultado$contenido, names(datos)[2])
})

test_that("detecta_columnas_vacias no marca una columna con un solo valor", {
  datos <- data.frame(
    a = 1:3,
    b = c(NA, NA, "x")
  )
  resultado <- detecta_columnas_vacias(datos)

  expect_equal(nrow(resultado), 0)
})

test_that("detecta_columnas_vacias trata el texto en blanco como vacio", {
  datos <- data.frame(
    a = 1:3,
    b = c("", " ", "   ")
  )
  resultado <- detecta_columnas_vacias(datos)

  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$columna, 2L)
})

test_that("detecta_columnas_vacias imprime el mensaje en singular", {
  expect_snapshot(detecta_columnas_vacias(ventas_ec))
})

test_that("detecta_columnas_vacias imprime el mensaje en plural", {
  datos <- data.frame(a = 1:3, b = NA, c = 4:6, d = NA)
  expect_snapshot(detecta_columnas_vacias(datos))
})

test_that("detecta_columnas_vacias falla si la entrada no es un data.frame", {
  expect_snapshot(detecta_columnas_vacias(1:3), error = TRUE)
})

test_that("detecta_columnas_vacias falla si no hay columnas", {
  expect_snapshot(detecta_columnas_vacias(data.frame()), error = TRUE)
})

test_that("detecta_columnas_vacias falla si no hay filas", {
  datos <- data.frame(a = character(0), b = character(0))
  expect_snapshot(detecta_columnas_vacias(datos), error = TRUE)
})
