detecta <- function(datos) {
  suppressMessages(detecta_categorias_inconsistentes(datos))
}

columnas_esquema <- c(
  "fila",
  "columna",
  "contenido",
  "problema",
  "detalle",
  "importancia",
  "solucion_recomendada"
)

test_that("detecta un valor escrito de formas distintas y respeta el esquema", {
  # "Guayas" aparece por primera vez en la fila 3. "guayas " y "GUAYAS" empatan
  # con 1 fila y deben salir en orden de aparicion.
  datos <- data.frame(
    provincia = c(
      "Loja",
      "Azuay",
      "Guayas",
      "Loja",
      "guayas ",
      "Guayas",
      "GUAYAS",
      "Guayas"
    )
  )

  resultado <- detecta(datos)

  expect_s3_class(resultado, "data.frame")
  expect_named(resultado, columnas_esquema)
  expect_equal(nrow(resultado), 1L)
  expect_equal(resultado$fila, 3L)
  expect_equal(resultado$columna, 1L)
  expect_equal(
    resultado$contenido,
    '"Guayas" (3 filas), "guayas " (1 fila), "GUAYAS" (1 fila)'
  )
})

test_that("unifica tildes y espacios, y respeta la enie", {
  # \u00ed = i con tilde, \u00a0 = espacio duro, \u00f1 = enie, \u00d1 = ENIE
  datos <- data.frame(
    a = c("Manab\u00ed", "Manabi", "Loja"),
    b = c("Santa  Elena", "Santa\u00a0Elena", "Loja"),
    c = c("a\u00f1o", "ano", "A\u00d1O")
  )

  resultado <- detecta(datos)

  # un grupo por columna
  expect_equal(resultado$columna, 1:3)
  expect_equal(
    resultado$contenido[1],
    '"Manab\u00ed" (1 fila), "Manabi" (1 fila)'
  )
  # "ano" no se une a "a\u00f1o" ni a "A\u00d1O"
  expect_equal(
    resultado$contenido[3],
    '"a\u00f1o" (1 fila), "A\u00d1O" (1 fila)'
  )
})

test_that("no marca lo que no es un problema", {
  sano <- data.frame(provincia = c("Guayas", "Loja", "Guayas"), n = 1:3)
  errata <- data.frame(provincia = c("Pichincha", "Pichincha", "Pinchincha"))
  vacios <- data.frame(x = c("Guayas", NA, "", "  ", "Guayas"))
  no_texto <- data.frame(
    n = c(1, 1, 2),
    fecha = as.Date(c("2024-01-01", "2024-01-01", "2024-01-02")),
    logico = c(TRUE, TRUE, FALSE)
  )

  for (datos in list(sano, errata, vacios, no_texto)) {
    resultado <- detecta(datos)
    expect_equal(nrow(resultado), 0L)
    expect_named(resultado, columnas_esquema)
  }
})

test_that("ordena por columna y fila, y revisa los factores como texto", {
  datos <- data.frame(
    tipo = factor(c("Urbano", "urbano", "Rural", "Rural")),
    n = 1:4,
    provincia = c("Loja", "Azuay", "LOJA", "azuay")
  )

  resultado <- detecta(datos)

  expect_equal(resultado$columna, c(1L, 3L, 3L))
  expect_equal(resultado$fila, c(1L, 1L, 2L))
})

test_that("el mensaje en consola explica un solo valor inconsistente", {
  datos <- data.frame(provincia = c("Guayas", "guayas ", "GUAYAS", "Loja"))

  expect_snapshot(invisible(detecta_categorias_inconsistentes(datos)))
})

test_that("el mensaje en consola resume varios valores inconsistentes", {
  datos <- data.frame(
    provincia = c("Guayas", "guayas ", "Loja", "LOJA"),
    zona = c("Norte", "norte", "Sur", "Sur")
  )

  expect_snapshot(invisible(detecta_categorias_inconsistentes(datos)))
})

test_that("avisa con un error cuando la entrada no sirve", {
  expect_snapshot(detecta_categorias_inconsistentes("texto"), error = TRUE)
  expect_snapshot(detecta_categorias_inconsistentes(data.frame()), error = TRUE)
  expect_snapshot(
    detecta_categorias_inconsistentes(data.frame(a = character(0))),
    error = TRUE
  )
})
