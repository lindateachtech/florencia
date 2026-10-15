# data.frame de texto con los nombres de columna dados
tabla_con_nombres <- function(nombres, filas = 2) {
  datos <- as.data.frame(
    matrix("x", nrow = filas, ncol = length(nombres)),
    stringsAsFactors = FALSE
  )
  names(datos) <- nombres
  datos
}

test_that("detecta_columnas_anuales detecta tres columnas con anio", {
  datos <- tabla_con_nombres(c("region", "2019", "2020", "2021"))
  resultado <- suppressMessages(detecta_columnas_anuales(datos))

  expect_equal(nrow(resultado), 3)
  expect_equal(resultado$fila, rep(NA_integer_, 3))
  expect_equal(resultado$columna, 2:4)
  expect_equal(resultado$contenido, c("2019", "2020", "2021"))
  expect_equal(
    unique(resultado$problema),
    "Nombres de columnas en formato anual."
  )
  expect_equal(
    resultado$detalle[1],
    "La columna 2 tiene un anio en el nombre (2019)."
  )
})

test_that("detecta_columnas_anuales devuelve 0 filas en una tabla sana", {
  datos <- tabla_con_nombres(c("region", "ventas", "costo"))
  resultado <- suppressMessages(detecta_columnas_anuales(datos))

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

test_that("detecta_columnas_anuales detecta nombres con X delante", {
  datos <- tabla_con_nombres(c("region", "X2020", "X2021"))
  resultado <- suppressMessages(detecta_columnas_anuales(datos))

  expect_equal(resultado$columna, 2:3)
  expect_equal(resultado$contenido, c("X2020", "X2021"))
})

test_that("detecta_columnas_anuales ignora espacios en los extremos", {
  datos <- tabla_con_nombres(c("region", " 2020 ", "2021 "))
  resultado <- suppressMessages(detecta_columnas_anuales(datos))

  expect_equal(resultado$columna, 2:3)
})

test_that("detecta_columnas_anuales detecta el anio dentro del texto", {
  datos <- tabla_con_nombres(
    c("region", "ventas_2019", "T1 2020", "X2021", "ventas_2022_v2")
  )
  resultado <- suppressMessages(detecta_columnas_anuales(datos))

  expect_equal(resultado$columna, 2:5)
  expect_equal(
    resultado$contenido,
    c("ventas_2019", "T1 2020", "X2021", "ventas_2022_v2")
  )
})

test_that("detecta_columnas_anuales no marca una sola columna con anio", {
  datos <- tabla_con_nombres(c("region", "ventas_2020", "ventas"))
  resultado <- suppressMessages(detecta_columnas_anuales(datos))

  expect_equal(nrow(resultado), 0)
})

test_that("detecta_columnas_anuales no marca numeros que no son anios", {
  datos <- tabla_con_nombres(
    c("1850", "2100", "300", "12345", "codigo_20201")
  )
  resultado <- suppressMessages(detecta_columnas_anuales(datos))

  expect_equal(nrow(resultado), 0)
})

test_that("detecta_columnas_anuales no marca anios de dos digitos", {
  datos <- tabla_con_nombres(c("'20", "'21", "T1", "T2"))
  resultado <- suppressMessages(detecta_columnas_anuales(datos))

  expect_equal(nrow(resultado), 0)
})

test_that("detecta_columnas_anuales acepta una tabla sin filas", {
  datos <- tabla_con_nombres(c("region", "2020", "2021"), filas = 0)
  resultado <- suppressMessages(detecta_columnas_anuales(datos))

  expect_equal(nrow(resultado), 2)
})

test_that("detecta_columnas_anuales acepta una tabla con una sola columna", {
  datos <- tabla_con_nombres("2020")
  resultado <- suppressMessages(detecta_columnas_anuales(datos))

  expect_equal(nrow(resultado), 0)
})

test_that("detecta_columnas_anuales imprime el mensaje en plural", {
  datos <- tabla_con_nombres(c("region", "2019", "2020", "2021"))
  expect_snapshot(detecta_columnas_anuales(datos))
})

test_that("detecta_columnas_anuales falla si la entrada no es un data.frame", {
  expect_snapshot(detecta_columnas_anuales(1:3), error = TRUE)
})

test_that("detecta_columnas_anuales falla si no hay columnas", {
  expect_snapshot(detecta_columnas_anuales(data.frame()), error = TRUE)
})

test_that("detecta_columnas_anuales muestra solo el anio en el mensaje", {
  datos <- tabla_con_nombres(c("region", "ventas_2019", "T1 2020", "X2021"))
  expect_snapshot(detecta_columnas_anuales(datos))
})
