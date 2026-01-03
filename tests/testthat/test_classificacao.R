library(testthat)

test_that("classificador identifica tentativa de homicidio", {
  res <- classificar_crime_completo("Suspeito de tentativa de homicidio e preso")
  expect_equal(res$categoria, "Crime Letal Violento")
  expect_match(res$tipo_principal, "Tentativa de Homic")
})

test_that("classificador identifica homicídio e feminicídio", {
  res1 <- classificar_crime_completo("Homem é morto a tiros em Manaus")
  expect_equal(res1$categoria, "Crime Letal Violento")
  expect_equal(res1$tipo_principal, "Homicídio")

  res2 <- classificar_crime_completo("Mulher é vítima de feminicídio em casa")
  expect_equal(res2$tipo_principal, "Feminicídio")
  expect_equal(res2$gravidade, "extrema")
})

test_that("classificador diferencia violência sexual", {
  res <- classificar_crime_completo("Adolescente denuncia tentativa de estupro em Manaus")
  expect_match(res$tipo_principal, "Tentativa de Estupro")
})

test_that("funções auxiliares enriquecem variáveis demográficas", {
  idade <- extrair_idade("Homem de 32 anos é preso por roubo")
  expect_equal(idade, 32)
  expect_equal(classificar_faixa_etaria(idade), "30-59 (adulto)")

  expect_equal(extrair_genero("Mulher é vítima de agressão"), "feminino")
})
