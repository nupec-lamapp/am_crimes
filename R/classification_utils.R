############################################################
# Funções utilitárias de classificação e enriquecimento
# Compartilhadas entre scripts do pipeline e testes
############################################################

suppressPackageStartupMessages({
  library(stringr)
  library(stringi)
  library(purrr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

normalizar_texto <- function(texto) {
  if (is.null(texto)) return("")
  texto <- as.character(texto)
  texto[is.na(texto)] <- ""
  texto <- tolower(texto)
  texto <- stringi::stri_trans_general(texto, "Latin-ASCII")
  stringr::str_trim(texto)
}

extrair_genero <- function(titulo) {
  txt <- tolower(titulo %||% "")
  if (str_detect(txt, "mulher|menina|garota|moca|idosa|mae|filha|esposa")) return("feminino")
  if (str_detect(txt, "homem|menino|rapaz|garoto|idoso|pai|filho")) return("masculino")
  "indefinido"
}

extrair_idade <- function(titulo) {
  txt <- str_to_lower(titulo %||% "")
  m <- str_match(txt, "([0-9]{1,2})\\s*anos")
  if (is.na(m[2])) return(NA_integer_)
  as.integer(m[2])
}

classificar_faixa_etaria <- function(idade) {
  if (is.na(idade)) return("idade não informada")
  if (idade <= 11) return("0-11 (criança)")
  if (idade <= 17) return("12-17 (adolescente)")
  if (idade <= 29) return("18-29 (jovem)")
  if (idade <= 59) return("30-59 (adulto)")
  "60+ (idoso)"
}

classificar_crime_completo <- function(titulo) {
  txt <- normalizar_texto(titulo)

  # ===== CRIMES LETAIS =====
  if (str_detect(txt, "homicidio|assassin|executado|execucao|morto|morte|matou|matar")) {
    if (str_detect(txt, "feminicidio|mulher morta|esposa morta|namorada morta|companheira morta")) {
      return(list(
        categoria = "Crime Letal Violento",
        tipo_principal = "Feminicídio",
        tipo_detalhado = "Homicídio com contexto doméstico/familiar",
        gravidade = "extrema"
      ))
    }
    if (str_detect(txt, "chacina|massacre|multiplos mortos|varios mortos")) {
      return(list(
        categoria = "Crime Letal Violento",
        tipo_principal = "Chacina",
        tipo_detalhado = "Homicídio múltiplo",
        gravidade = "extrema"
      ))
    }
    return(list(
      categoria = "Crime Letal Violento",
      tipo_principal = "Homicídio",
      tipo_detalhado = "Homicídio consumado",
      gravidade = "extrema"
    ))
  }

  if (str_detect(txt, "tentativa de homicidio|tentou matar|tentou assassinar|tentativa de morte")) {
    return(list(
      categoria = "Crime Letal Violento",
      tipo_principal = "Tentativa de Homicídio",
      tipo_detalhado = "Homicídio tentado",
      gravidade = "muito alta"
    ))
  }

  # ===== VIOLÊNCIA SEXUAL =====
  if (str_detect(txt, "estupro|estuprada|estuprando|violencia sexual")) {
    if (str_detect(txt, "crianca|menor|adolescente|criada|filha")) {
      return(list(
        categoria = "Violência Sexual",
        tipo_principal = "Estupro de Vulnerável",
        tipo_detalhado = "Estupro de criança/adolescente",
        gravidade = "extrema"
      ))
    }
    return(list(
      categoria = "Violência Sexual",
      tipo_principal = "Estupro",
      tipo_detalhado = "Estupro consumado",
      gravidade = "extrema"
    ))
  }

  if (str_detect(txt, "tentativa de estupro|tentou estuprar")) {
    return(list(
      categoria = "Violência Sexual",
      tipo_principal = "Tentativa de Estupro",
      tipo_detalhado = "Estupro tentado",
      gravidade = "muito alta"
    ))
  }

  if (str_detect(txt, "abuso sexual|abusada|pedofilia")) {
    return(list(
      categoria = "Violência Sexual",
      tipo_principal = "Abuso Sexual",
      tipo_detalhado = "Abuso sexual consumado",
      gravidade = "muito alta"
    ))
  }

  # ===== VIOLÊNCIA DOMÉSTICA =====
  if (str_detect(txt, "esposa|marido|companheiro|companheira|namorado|namorada|ex-|ex ")) {
    if (str_detect(txt, "agred|bateu|bater|socc|tapas")) {
      if (str_detect(txt, "morto|morte|matou")) {
        return(list(
          categoria = "Violência Doméstica",
          tipo_principal = "Feminicídio Doméstico",
          tipo_detalhado = "Homicídio por parceiro íntimo",
          gravidade = "extrema"
        ))
      }
      return(list(
        categoria = "Violência Doméstica",
        tipo_principal = "Agressão Doméstica",
        tipo_detalhado = "Violência doméstica com lesão corporal",
        gravidade = "alta"
      ))
    }
  }

  # ===== ROUBO VIOLENTO =====
  if (str_detect(txt, "latrocinio|roubo seguido morte|roubo com morte")) {
    return(list(
      categoria = "Roubo Violento",
      tipo_principal = "Latrocínio",
      tipo_detalhado = "Roubo seguido de morte",
      gravidade = "extrema"
    ))
  }

  if (str_detect(txt, "roubo.*arma|assalto armado|roubo com arma|roubo a tiros")) {
    return(list(
      categoria = "Roubo Violento",
      tipo_principal = "Roubo à Mão Armada",
      tipo_detalhado = "Roubo com uso de arma de fogo",
      gravidade = "muito alta"
    ))
  }

  if (str_detect(txt, "assalto|roubo|saidinha|arrastao")) {
    return(list(
      categoria = "Roubo Violento",
      tipo_principal = "Roubo/Assalto",
      tipo_detalhado = "Roubo/assalto com confronto",
      gravidade = "alta"
    ))
  }

  # ===== LESÃO CORPORAL GRAVE =====
  if (str_detect(txt, "esfaqueado|esfaqueada|facada|facadas|faca")) {
    return(list(
      categoria = "Lesão Corporal Grave",
      tipo_principal = "Esfaqueamento",
      tipo_detalhado = "Lesão com instrumento cortante",
      gravidade = "alta"
    ))
  }

  if (str_detect(txt, "espancado|espancada|espancamento|apedrejado|apedrejada")) {
    return(list(
      categoria = "Lesão Corporal Grave",
      tipo_principal = "Espancamento",
      tipo_detalhado = "Lesão com agressão física grave",
      gravidade = "alta"
    ))
  }

  if (str_detect(txt, "agredido com|atingido com|batido com|objeto")) {
    return(list(
      categoria = "Lesão Corporal Grave",
      tipo_principal = "Agressão com Objeto",
      tipo_detalhado = "Lesão com instrumento contundente",
      gravidade = "alta"
    ))
  }

  # ===== OUTRAS FORMAS DE VIOLÊNCIA =====
  if (str_detect(txt, "sequestro|sequestrada|sequestrado|carcere privado")) {
    return(list(
      categoria = "Privação de Liberdade",
      tipo_principal = "Sequestro/Cárcere Privado",
      tipo_detalhado = "Privação de liberdade com restrição",
      gravidade = "muito alta"
    ))
  }

  if (str_detect(txt, "trafico|faccao|cv|pcc|comando vermelho|primeiro comando")) {
    return(list(
      categoria = "Crime Organizado",
      tipo_principal = "Tráfico de Drogas/Facções",
      tipo_detalhado = "Tráfico associado a facção criminosa",
      gravidade = "alta"
    ))
  }

  if (str_detect(txt, "acerto de contas|rixa|execucao|pistoleiros")) {
    return(list(
      categoria = "Violência Lethal Contextual",
      tipo_principal = "Acerto de Contas",
      tipo_detalhado = "Execução por disputa/vingança",
      gravidade = "extrema"
    ))
  }

  list(
    categoria = "Outros",
    tipo_principal = "Não classificado com precisão",
    tipo_detalhado = "Requer análise manual",
    gravidade = "indefinida"
  )
}

eh_crime_violento_v2 <- function(titulos) {
  purrr::map_lgl(titulos, function(t) {
    res <- classificar_crime_completo(t)
    res$categoria != "Outros"
  })
}

aplicar_classificacao_completa <- function(df_noticias) {
  df_noticias %>%
    mutate(
      classificacao_completa = purrr::map(titulo, classificar_crime_completo),
      categoria      = purrr::map_chr(classificacao_completa, ~.$categoria      %||% "Outros"),
      tipo_principal = purrr::map_chr(classificacao_completa, ~.$tipo_principal %||% "Não classificado"),
      tipo_detalhado = purrr::map_chr(classificacao_completa, ~.$tipo_detalhado %||% "Análise manual necessária"),
      gravidade      = purrr::map_chr(classificacao_completa, ~.$gravidade      %||% "indefinida"),
      classificacao_completa = NULL
    )
}
