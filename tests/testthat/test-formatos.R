test_that("detecta_encabezado detecta un encabezado desplazado", {
  resultado <- suppressMessages(detecta_encabezado(cervecero))

  expect_s3_class(resultado, "data.frame")
  expect_equal(
    colnames(resultado),
    c("fila", "problema", "detalle", "importancia", "solucion_recomendada")
  )
  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$problema, "Posible encabezado desplazado.")
})

test_that("detecta_encabezado no marca falso positivo en datos sanos", {
  datos_sanos <- data.frame(pais = "Ecuador", precio = 10, volumen = 100)
  resultado <- suppressMessages(detecta_encabezado(datos_sanos))

  expect_s3_class(resultado, "data.frame")
  expect_equal(nrow(resultado), 0)
})

test_that("detecta_encabezado detecta pocos nombres en una tabla ancha", {
  datos_ancho <- as.data.frame(matrix("x", nrow = 3, ncol = 40))
  names(datos_ancho) <- c("Informe", "Fecha", "Paginas", rep("", 37))
  resultado <- suppressMessages(detecta_encabezado(datos_ancho))

  expect_equal(nrow(resultado), 1)
})

test_that("detecta_encabezado considera sana la mitad exacta con nombre", {
  datos_mitad <- data.frame(a = 1, b = 2)
  names(datos_mitad) <- c("a", "")
  resultado <- suppressMessages(detecta_encabezado(datos_mitad))

  expect_equal(nrow(resultado), 0)
})


test_that("detecta_encabezado imprime el mensaje esperado", {
  expect_snapshot(detecta_encabezado(cervecero))
})

test_that("detecta_encabezado rechaza un input que no es data.frame", {
  expect_snapshot(detecta_encabezado("no soy un data.frame"), error = TRUE)
})

test_that("detecta_encabezado rechaza un data.frame sin columnas", {
  expect_snapshot(detecta_encabezado(data.frame()), error = TRUE)
})
