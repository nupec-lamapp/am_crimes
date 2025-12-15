library(testthat)

test_that("crimes_classificados.csv tem colunas essenciais", {
  arq <- file.path("data", "processed", "crimes_classificados.csv")
  skip_if_not(file.exists(arq), "crimes_classificados.csv ainda não existe")

  df <- readr::read_csv(arq, show_col_types = FALSE)
  expect_true(all(c("portal", "data_publicacao", "titulo", "url") %in% names(df)))
  expect_true(all(c("categoria", "tipo_principal", "gravidade", "crime_violento") %in% names(df)))
})

