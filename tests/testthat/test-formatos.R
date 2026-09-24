test_that("detecta_encabezados detecta un encabezado desplazado", {
  resultado <- suppressMessages(detecta_encabezados(cervecero))

  expect_s3_class(resultado, "data.frame")
  expect_equal(
    colnames(resultado),
    c("fila", "problema", "detalle", "importancia", "solucion_recomendada")
  )
  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$problema, "Posible encabezado desplazado.")
})

test_that("detecta_encabezados no marca falso positivo en datos sanos", {
  datos_sanos <- data.frame(pais = "Ecuador", precio = 10, volumen = 100)
  resultado <- suppressMessages(detecta_encabezados(datos_sanos))

  expect_s3_class(resultado, "data.frame")
  expect_equal(nrow(resultado), 0)
})

test_that("detecta_encabezados imprime el mensaje esperado", {
  expect_snapshot(detecta_encabezados(cervecero))
})

test_that("detecta_encabezados rechaza un input que no es data.frame", {
  expect_snapshot(detecta_encabezados("no soy un data.frame"), error = TRUE)
})
