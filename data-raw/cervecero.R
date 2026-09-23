## code to prepare `cervecero` dataset

library(readxl)

cervecero <- read_excel(
  "data-raw/datos_cerveza.xlsx",
  sheet = "cervecero",
  .name_repair = "minimal",
  col_types = "text"
)

usethis::use_data(cervecero, overwrite = TRUE)
