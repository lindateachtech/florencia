# ---- detecta_encabezado() ----

test_that("detecta_encabezado detecta un encabezado desplazado con cervecero", {
  resultado <- suppressMessages(detecta_encabezado(cervecero))

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
  expect_equal(resultado$problema, "Posible encabezado desplazado.")
  expect_equal(
    resultado$contenido,
    "Datos Sectoriales de la industria cervecera"
  )
  expect_equal(resultado$fila, 1L)
})

test_that("detecta_encabezado no marca falso positivo en datos sanos", {
  datos_sanos <- data.frame(pais = "Ecuador", precio = 10, volumen = 100)
  resultado <- suppressMessages(detecta_encabezado(datos_sanos))

  expect_s3_class(resultado, "data.frame")
  expect_equal(nrow(resultado), 0)
  expect_equal(ncol(resultado), 7)
})

test_that("detecta_encabezado detecta pocos nombres en una tabla ancha", {
  datos_ancho <- as.data.frame(matrix("x", nrow = 3, ncol = 40))
  names(datos_ancho) <- c("Informe", "Fecha", "Paginas", rep("", 37))
  resultado <- suppressMessages(detecta_encabezado(datos_ancho))

  expect_equal(nrow(resultado), 1)
})

test_that("detecta_encabezado localiza el encabezado real bajo titulo, fecha y paginas", {
  datos <- as.data.frame(matrix(NA_character_, nrow = 5, ncol = 5))
  names(datos) <- c("Informe de ventas", rep("", 4))
  datos[1, 1] <- "Fecha: 2026-01-01"
  datos[2, 1] <- "Pagina 1"
  datos[3, ] <- c("pais", "precio", "volumen", "anio", "region")
  datos[4, ] <- c("Ecuador", "10", "100", "2025", "Costa")
  datos[5, ] <- c("Peru", "12", "80", "2025", "Sierra")

  resultado <- suppressMessages(detecta_encabezado(datos))

  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$fila, 3L)
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

test_that("detecta_encabezado devuelve fila NA si ninguna fila llega al 50 %", {
  datos <- as.data.frame(matrix(NA_character_, nrow = 3, ncol = 5))
  names(datos) <- c("Informe de ventas", rep("", 4))
  datos[1, 1] <- "Fecha: 2026-01-01"
  datos[2, 1] <- "Pagina 1"
  datos[3, 1] <- "Confidencial"

  resultado <- suppressMessages(detecta_encabezado(datos))

  expect_equal(nrow(resultado), 1)
  expect_true(is.na(resultado$fila))
})

test_that("detecta_encabezado avisa si no puede localizar la fila del encabezado", {
  datos <- as.data.frame(matrix(NA_character_, nrow = 3, ncol = 5))
  names(datos) <- c("Informe de ventas", rep("", 4))
  datos[1, 1] <- "Fecha: 2026-01-01"
  datos[2, 1] <- "Pagina 1"
  datos[3, 1] <- "Confidencial"

  expect_snapshot(detecta_encabezado(datos))
})

# ---- detecta_columnas_vacias() ----

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
