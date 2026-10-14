test_that("detecta_subencabezados detecta los grupos de ventas_ec", {
  resultado <- detecta_subencabezados(ventas_ec)

  expect_equal(nrow(resultado), 3)
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
  expect_equal(resultado$fila, c(1L, 5L, 10L))
  expect_equal(resultado$columna, c(1L, 1L, 1L))
  expect_equal(resultado$contenido, c("Costa", "Sierra", "Amazonia"))
  expect_equal(unique(resultado$problema), "Subencabezado incrustado.")
})

test_that("detecta_subencabezados no marca falsos positivos en una tabla sana", {
  datos <- data.frame(
    pais = c("Ecuador", "Peru"),
    precio = c(10, 20)
  )
  resultado <- detecta_subencabezados(datos)

  expect_s3_class(resultado, "data.frame")
  expect_equal(nrow(resultado), 0)
  expect_equal(ncol(resultado), 7)
})

test_that("detecta_subencabezados detecta un solo subencabezado", {
  datos <- data.frame(
    region = c("Costa", "Guayaquil", "Machala"),
    ventas = c(NA, 10, 20)
  )
  resultado <- detecta_subencabezados(datos)

  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$fila, 1L)
  expect_equal(resultado$contenido, "Costa")
})

test_that("detecta_subencabezados trata el texto en blanco como vacio", {
  datos <- data.frame(
    a = c("Costa", "Guayaquil"),
    b = c(" ", "10"),
    c = c("", "20")
  )
  resultado <- detecta_subencabezados(datos)

  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$fila, 1L)
})

test_that("detecta_subencabezados no marca un valor fuera de la primera columna", {
  datos <- data.frame(
    a = c(NA, "x"),
    b = c("Costa", "y"),
    c = c(NA, "z")
  )
  resultado <- detecta_subencabezados(datos)

  expect_equal(nrow(resultado), 0)
})

test_that("detecta_subencabezados no marca una fila completamente vacia", {
  datos <- data.frame(
    a = c(NA, "x"),
    b = c(NA, "y")
  )
  resultado <- detecta_subencabezados(datos)

  expect_equal(nrow(resultado), 0)
})

test_that("detecta_subencabezados no marca una fila con dos o mas valores", {
  datos <- data.frame(
    a = c("Costa", "x"),
    b = c("Sierra", "y"),
    c = c(NA, "z")
  )
  resultado <- detecta_subencabezados(datos)

  expect_equal(nrow(resultado), 0)
})

test_that("detecta_subencabezados imprime el mensaje en singular", {
  datos <- data.frame(
    region = c("Costa", "Guayaquil", "Machala"),
    ventas = c(NA, 10, 20)
  )
  expect_snapshot(detecta_subencabezados(datos))
})

test_that("detecta_subencabezados imprime el mensaje en plural", {
  expect_snapshot(detecta_subencabezados(ventas_ec))
})

test_that("detecta_subencabezados falla si la entrada no es un data.frame", {
  expect_snapshot(detecta_subencabezados(1:3), error = TRUE)
})

test_that("detecta_subencabezados falla si hay menos de dos columnas", {
  expect_snapshot(detecta_subencabezados(data.frame(a = 1:3)), error = TRUE)
  expect_error(detecta_subencabezados(data.frame()), "al menos dos columnas")
})

test_that("detecta_subencabezados falla si no hay filas", {
  datos <- data.frame(a = character(0), b = character(0))
  expect_snapshot(detecta_subencabezados(datos), error = TRUE)
})
