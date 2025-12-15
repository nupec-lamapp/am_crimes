library(testthat)
library(xml2)

# Garante que as funções definidas no script de scraping estejam disponíveis
source("scripts/01_scraping.R")

test_that("get_domain extrai dominio corretamente", {
  expect_equal(get_domain("https://www.acritica.com/policia"), "https://www.acritica.com")
  expect_equal(get_domain("http://exemplo.com/path"), "http://exemplo.com")
  expect_equal(get_domain("semprotocolo.com"), "")
})

test_that("extrair_data_publicacao reconhece data em dd/mm/aaaa", {
  html <- "<html><body>Publicado em 10/11/2024</body></html>"
  doc  <- xml2::read_html(html)
  d    <- extrair_data_publicacao(doc)
  expect_equal(d, as.Date("2024-11-10"))
})

test_that("eh_crime_violento identifica alguns padroes", {
  expect_true(eh_crime_violento("Homem é morto a tiros em Manaus"))
  expect_true(eh_crime_violento("Mulher é vítima de estupro em bairro X"))
  expect_false(eh_crime_violento("Polícia investiga furto de celular em loja"))
})
