## code to prepare `ventas_ec` dataset goes here

library(readxl)

ventas_ec <- read_excel(
  "data-raw/ventas_ec.xlsx",
  .name_repair = "minimal",
  col_types = "text"
)

usethis::use_data(ventas_ec, overwrite = TRUE)

# Copia del .xlsx original para ejemplos que leen el archivo (tidyxl)
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
file.copy(
  "data-raw/ventas_ec.xlsx",
  "inst/extdata/ventas_ec.xlsx",
  overwrite = TRUE
)
