# install.packages("remotes")
# remotes::install_github("rfsaldanha/microdatasus")

# Load the library
library(RSQLite)
library(tibble)
library(dplyr)
library(lubridate)
library(tmaptools) # Geocoding
library(httr)
library(jsonlite)
library(microdatasus)

dados_simdofet2018 <- fetch_datasus(year_start = 2018, year_end = 2018, information_system = "SIM-DOFET")
dados_simdofet2019 <- fetch_datasus(year_start = 2019, year_end = 2019, information_system = "SIM-DOFET")
dados_simdofet2020 <- fetch_datasus(year_start = 2020, year_end = 2020, information_system = "SIM-DOFET")
dados_simdofet2021 <- fetch_datasus(year_start = 2021, year_end = 2021, information_system = "SIM-DOFET")
dados_simdofet2022 <- fetch_datasus(year_start = 2022, year_end = 2022, information_system = "SIM-DOFET")
dados_simdofet2023 <- fetch_datasus(year_start = 2023, year_end = 2023, information_system = "SIM-DOFET")

dados_sinasc2018 <- fetch_datasus(year_start = 2018, year_end = 2018, information_system = "SINASC")
dados_sinasc2019 <- fetch_datasus(year_start = 2019, year_end = 2019, information_system = "SINASC")
dados_sinasc2020 <- fetch_datasus(year_start = 2020, year_end = 2020, information_system = "SINASC")
dados_sinasc2021 <- fetch_datasus(year_start = 2021, year_end = 2021, information_system = "SINASC")
dados_sinasc2022 <- fetch_datasus(year_start = 2022, year_end = 2022, information_system = "SINASC")
dados_sinasc2023 <- fetch_datasus(year_start = 2023, year_end = 2023, information_system = "SINASC")


create1_uf_and_filter <- function(data, uf){
  uf_map <- c(
    "11"="RO","12"="AC","13"="AM","14"="RR","15"="PA","16"="AP","17"="TO",
    "21"="MA","22"="PI","23"="CE","24"="RN","25"="PB","26"="PE","27"="AL","28"="SE","29"="BA",
    "31"="MG","32"="ES","33"="RJ","35"="SP",
    "41"="PR","42"="SC","43"="RS",
    "50"="MS","51"="MT","52"="GO","53"="DF"
  )
  
  data <- data %>%
    mutate(
      # uf ocorrência
      uf_ocor = uf_map[
        ifelse(
          is.na(CODMUNOCOR), NA,
          substr(sprintf("%06d", as.integer(CODMUNOCOR)), 1, 2)
        )
      ],
      # uf residência
      uf_resid = uf_map[
        ifelse(
          is.na(CODMUNRES), NA,
          substr(sprintf("%06d", as.integer(CODMUNRES)), 1, 2)
        )
      ]  
    )
  data <- filter(data, uf_ocor == uf)
}

dados_simdofet2018 <- create1_uf_and_filter(dados_simdofet2018, "SP")
dados_simdofet2019 <- create1_uf_and_filter(dados_simdofet2019, "SP")
dados_simdofet2020 <- create1_uf_and_filter(dados_simdofet2020, "SP")
dados_simdofet2021 <- create1_uf_and_filter(dados_simdofet2021, "SP")
dados_simdofet2022 <- create1_uf_and_filter(dados_simdofet2022, "SP")
dados_simdofet2023 <- create1_uf_and_filter(dados_simdofet2023, "SP")

create2_uf_and_filter <- function(data, uf){
  uf_map <- c(
    "11"="RO","12"="AC","13"="AM","14"="RR","15"="PA","16"="AP","17"="TO",
    "21"="MA","22"="PI","23"="CE","24"="RN","25"="PB","26"="PE","27"="AL","28"="SE","29"="BA",
    "31"="MG","32"="ES","33"="RJ","35"="SP",
    "41"="PR","42"="SC","43"="RS",
    "50"="MS","51"="MT","52"="GO","53"="DF"
  )
  
  data <- data %>%
    mutate(
      # uf ocorrência
      uf_ocor = uf_map[
        ifelse(
          is.na(CODMUNNASC), NA,
          substr(sprintf("%06d", as.integer(CODMUNNASC)), 1, 2)
        )
      ],
      # uf residência
      uf_resid = uf_map[
        ifelse(
          is.na(CODMUNRES), NA,
          substr(sprintf("%06d", as.integer(CODMUNRES)), 1, 2)
        )
      ]  
    )
  data <- filter(data, uf_ocor == uf)
}

dados_sinasc2018 <- create2_uf_and_filter(dados_sinasc2018, "SP")
dados_sinasc2019 <- create2_uf_and_filter(dados_sinasc2019, "SP")
dados_sinasc2020 <- create2_uf_and_filter(dados_sinasc2020, "SP")
dados_sinasc2021 <- create2_uf_and_filter(dados_sinasc2021, "SP")
dados_sinasc2022 <- create2_uf_and_filter(dados_sinasc2022, "SP")
dados_sinasc2023 <- create2_uf_and_filter(dados_sinasc2023, "SP")

# ano
dados_simdofet2018$ano <- 2018
dados_simdofet2019$ano <- 2019
dados_simdofet2020$ano <- 2020
dados_simdofet2021$ano <- 2021
dados_simdofet2022$ano <- 2022
dados_simdofet2023$ano <- 2023
dados_sinasc2018$ano <- 2018
dados_sinasc2019$ano <- 2019
dados_sinasc2020$ano <- 2020
dados_sinasc2021$ano <- 2021
dados_sinasc2022$ano <- 2022
dados_sinasc2023$ano <- 2023

# 1 = óbito fetal, 0 = nascido vivo
dados_simdofet2018$evento <- 1
dados_simdofet2019$evento <- 1
dados_simdofet2020$evento <- 1
dados_simdofet2021$evento <- 1
dados_simdofet2022$evento <- 1
dados_simdofet2023$evento <- 1
dados_sinasc2018$evento <- 0
dados_sinasc2019$evento <- 0
dados_sinasc2020$evento <- 0
dados_sinasc2021$evento <- 0
dados_sinasc2022$evento <- 0
dados_sinasc2023$evento <- 0

# Verificar variáveis que entraram
report_new_columns <- function(df_new, df_old,
                               year_new,
                               year_old,
                               ignore_case = TRUE,
                               trim_ws = TRUE) {
  stopifnot(is.data.frame(df_new), is.data.frame(df_old))
  
  # Extrai nomes
  n_new <- colnames(df_new)
  n_old <- colnames(df_old)
  
  # Normalização opcional para comparação
  norm <- function(x) {
    if (trim_ws) x <- trimws(x)
    if (ignore_case) x <- tolower(x)
    x
  }
  n_new_norm <- norm(n_new)
  n_old_norm <- norm(n_old)
  
  # setdiff em nomes normalizados
  new_norm_only <- setdiff(unique(n_new_norm), unique(n_old_norm))
  
  # Mapeia de volta aos nomes originais de df_new (preserva grafia)
  # (pega o primeiro nome original que corresponde a cada normalizado)
  map_back <- function(target_norm, originals, originals_norm) {
    out <- character(0)
    for (z in target_norm) {
      idx <- which(originals_norm == z)
      if (length(idx)) out <- c(out, originals[idx[1]])
    }
    unique(out)
  }
  new_vars <- map_back(new_norm_only, n_new, n_new_norm)
  
  # Mensagens
  if (length(new_vars) == 0) {
    message(sprintf("Nenhuma variável nova em %s em relação a %s.", year_new, year_old))
  } else {
    cat(sprintf("Variáveis novas em %s (vs %s):\n", year_new, year_old))
    cat(paste0(" - ", new_vars), sep = "\n")
    cat("\nTotal: ", length(new_vars), "\n", sep = "")
  }
  
  invisible(new_vars)
}

# Verificar variáveis que saíram
report_dropped_columns <- function(df_new, df_old,
                                   year_new,
                                   year_old,
                                   ignore_case = TRUE,
                                   trim_ws = TRUE) {
  stopifnot(is.data.frame(df_new), is.data.frame(df_old))
  n_new <- colnames(df_new)
  n_old <- colnames(df_old)
  norm <- function(x) { if (trim_ws) x <- trimws(x); if (ignore_case) x <- tolower(x); x }
  n_new_norm <- norm(n_new)
  n_old_norm <- norm(n_old)
  dropped_norm <- setdiff(unique(n_old_norm), unique(n_new_norm))
  map_back <- function(target_norm, originals, originals_norm){
    out <- character(0)
    for (z in target_norm) {
      idx <- which(originals_norm == z)
      if (length(idx)) out <- c(out, originals[idx[1]])
    }
    unique(out)
  }
  dropped <- map_back(dropped_norm, n_old, n_old_norm)
  if (length(dropped) == 0) {
    message(sprintf("Nenhuma variável foi perdida em %s em relação a %s.", year_new, year_old))
  } else {
    cat(sprintf("Variáveis presentes em %s e ausentes em %s:\n", year_old, year_new))
    cat(paste0(" - ", dropped), sep = "\n")
    cat("\nTotal: ", length(dropped), "\n", sep = "")
  }
  invisible(dropped)
}

#---------------------------------- Usando funções para olhar quem entrou e saiu
cat("Entrada de variáveis no dataset SIM-DOFET:")
report_new_columns(df_new = dados_simdofet2019, df_old = dados_simdofet2018,
                   year_new = "2019", year_old = "2018")
report_new_columns(df_new = dados_simdofet2020, df_old = dados_simdofet2019,
                   year_new = "2020", year_old = "2019")
report_new_columns(df_new = dados_simdofet2021, df_old = dados_simdofet2020,
                   year_new = "2021", year_old = "2020")
report_new_columns(df_new = dados_simdofet2022, df_old = dados_simdofet2021,
                   year_new = "2022", year_old = "2021")
report_new_columns(df_new = dados_simdofet2023, df_old = dados_simdofet2022,
                   year_new = "2023", year_old = "2022")

cat("Saída de variáveis no dataset SIM-DOFET:")
report_dropped_columns(df_new = dados_simdofet2019, df_old = dados_simdofet2018,
                       year_new = "2019", year_old = "2018")
report_dropped_columns(df_new = dados_simdofet2020, df_old = dados_simdofet2019,
                       year_new = "2020", year_old = "2019")
report_dropped_columns(df_new = dados_simdofet2021, df_old = dados_simdofet2020,
                       year_new = "2021", year_old = "2020")
report_dropped_columns(df_new = dados_simdofet2022, df_old = dados_simdofet2021,
                       year_new = "2022", year_old = "2021")
report_dropped_columns(df_new = dados_simdofet2023, df_old = dados_simdofet2022,
                       year_new = "2023", year_old = "2022")

cat("Entrada de variáveis no dataset SINASC:")
report_new_columns(df_new = dados_sinasc2019, df_old = dados_sinasc2018,
                   year_new = "2019", year_old = "2018")
report_new_columns(df_new = dados_sinasc2020, df_old = dados_sinasc2019,
                   year_new = "2020", year_old = "2019")
report_new_columns(df_new = dados_sinasc2021, df_old = dados_sinasc2020,
                   year_new = "2021", year_old = "2020")
report_new_columns(df_new = dados_sinasc2022, df_old = dados_sinasc2021,
                   year_new = "2022", year_old = "2021")
report_new_columns(df_new = dados_sinasc2023, df_old = dados_sinasc2022,
                   year_new = "2023", year_old = "2022")

cat("Saída de variáveis no dataset SINASC:")
report_dropped_columns(df_new = dados_sinasc2019, df_old = dados_sinasc2018,
                       year_new = "2019", year_old = "2018")
report_dropped_columns(df_new = dados_sinasc2020, df_old = dados_sinasc2019,
                       year_new = "2020", year_old = "2019")
report_dropped_columns(df_new = dados_sinasc2021, df_old = dados_sinasc2020,
                       year_new = "2021", year_old = "2020")
report_dropped_columns(df_new = dados_sinasc2022, df_old = dados_sinasc2021,
                       year_new = "2022", year_old = "2021")
report_dropped_columns(df_new = dados_sinasc2023, df_old = dados_sinasc2022,
                       year_new = "2023", year_old = "2022")

# Remove a variável única do sim-dofet de 2018 para padronizar
dados_simdofet2018$CRM <- NULL

common_vars <- c("ORIGEM", "CODMUNNATU", "SEXO", "RACACOR", 
                 "CODMUNRES", "CODESTAB", "IDADEMAE", "ESCMAE", 
                 "ESCMAE2010", "SERIESCMAE", "QTDFILVIVO", 
                 "QTDFILMORT", "GRAVIDEZ", "SEMAGESTAC", 
                 "GESTACAO", "PARTO", "PESO", "NUMEROLOTE", 
                 "DTCADASTRO", "VERSAOSIST", "DTRECEBIM", 
                 "DTRECORIGA", "ESCMAEAGR1", "DIFDATA", "CONTADOR")

check_var <- function(df, common_vars){
  # Verifica se todas as colunas em common_vars estão no dataframe
  all_columns_present <- all(common_vars %in% names(df))
  
  if (all_columns_present) {
    print("Todas as colunas estão presentes no dataframe.")
  } else {
    print("Algumas colunas estão faltando no dataframe.")
    missing_columns <- setdiff(common_vars, names(df))
    print(paste("Colunas ausentes:", paste(missing_columns, collapse = ", ")))
  }
}

cat("Verifica se as variáveis previamente catalogadas como comum entre ambas as bases de fato existem:")

check_var(dados_simdofet2018, common_vars)
check_var(dados_simdofet2019, common_vars)
check_var(dados_simdofet2020, common_vars)
check_var(dados_simdofet2021, common_vars)
check_var(dados_simdofet2022, common_vars)
check_var(dados_simdofet2023, common_vars)

check_var(dados_sinasc2018, common_vars)
check_var(dados_sinasc2019, common_vars)
check_var(dados_sinasc2020, common_vars)
check_var(dados_sinasc2021, common_vars)
check_var(dados_sinasc2022, common_vars)
check_var(dados_sinasc2023, common_vars)


# Função para verificar se as colunas não são inteiramente NA ou colunas em branco
check_columns_not_only_na_blank <- function(df, cols) {
  result <- list()
  
  for (col in cols) {
    if (col %in% names(df)) {
      # Verifica se a coluna inteira é NA ou está em branco
      not_only_na_blank <- sum(!is.na(df[[col]]) & df[[col]] != "") > 0
      result[[col]] <- not_only_na_blank
    } else {
      # Se a coluna não existir, é identificada como ausente
      result[[col]] <- NA
    }
  }
  
  return(result)
}

check_columns_results <- function(df, common_vars){
  # Aplica a função ao seu dataframe e common_vars
  not_only_na_blank_check <- check_columns_not_only_na_blank(df, common_vars)
  
  for (col in names(not_only_na_blank_check)) {
    if (is.na(not_only_na_blank_check[[col]])) {
      print(paste("Column", col, "is missing from the dataframe."))
    } else if (not_only_na_blank_check[[col]]) {
      print(paste("Column", col, "is NOT only NA or blank."))
    } else {
      print(paste("Column", col, "is entirely NA or blank."))
    }
  }
}

#---------------- Verifica colunas NA ou em branco
check_columns_results(dados_simdofet2018, common_vars)
check_columns_results(dados_simdofet2019, common_vars)
check_columns_results(dados_simdofet2020, common_vars)
check_columns_results(dados_simdofet2021, common_vars)
check_columns_results(dados_simdofet2022, common_vars)
check_columns_results(dados_simdofet2023, common_vars)
## RACACOR completamente NA ou branco SIM-DOFET 2019, 2020, 2022 e 2023

check_columns_results(dados_sinasc2018, common_vars)
check_columns_results(dados_sinasc2019, common_vars)
check_columns_results(dados_sinasc2020, common_vars)
check_columns_results(dados_sinasc2021, common_vars)
check_columns_results(dados_sinasc2022, common_vars)
check_columns_results(dados_sinasc2023, common_vars)
## DTRECORIGA completamente NA ou branco SINASC 2018, 2019, 2020, 2021, 2022 e 2023

#------------ Colunas existentes am ambas, mas com nomes diferentes
SIM_vars <- c("CODMUNOCOR", "LOCOCOR", "HORAOBITO", "OCUPMAE")

check_var(dados_simdofet2018, SIM_vars)
check_var(dados_simdofet2019, SIM_vars)
check_var(dados_simdofet2020, SIM_vars)
check_var(dados_simdofet2021, SIM_vars)
check_var(dados_simdofet2022, SIM_vars)
check_var(dados_simdofet2023, SIM_vars)

check_columns_results(dados_simdofet2018, SIM_vars)
check_columns_results(dados_simdofet2019, SIM_vars)
check_columns_results(dados_simdofet2020, SIM_vars)
check_columns_results(dados_simdofet2021, SIM_vars)
check_columns_results(dados_simdofet2022, SIM_vars)
check_columns_results(dados_simdofet2023, SIM_vars)

SINASC_vars <- c("CODMUNNASC", "LOCNASC", "HORANASC", "CODOCUPMAE")

check_var(dados_sinasc2018, SINASC_vars)
check_var(dados_sinasc2019, SINASC_vars)
check_var(dados_sinasc2020, SINASC_vars)
check_var(dados_sinasc2021, SINASC_vars)
check_var(dados_sinasc2022, SINASC_vars)
check_var(dados_sinasc2023, SINASC_vars)

check_columns_results(dados_sinasc2018, SINASC_vars)
check_columns_results(dados_sinasc2019, SINASC_vars)
check_columns_results(dados_sinasc2020, SINASC_vars)
check_columns_results(dados_sinasc2021, SINASC_vars)
check_columns_results(dados_sinasc2022, SINASC_vars)
check_columns_results(dados_sinasc2023, SINASC_vars)

# Adiciona colunas padronizadas em ambos datasets seguindo a catalogação prévia
# que identificou que são colunas com o mesmo dado
dados_simdofet2018$codmunocornasc <- dados_simdofet2018$CODMUNOCOR
dados_simdofet2019$codmunocornasc <- dados_simdofet2019$CODMUNOCOR
dados_simdofet2020$codmunocornasc <- dados_simdofet2020$CODMUNOCOR
dados_simdofet2021$codmunocornasc <- dados_simdofet2021$CODMUNOCOR
dados_simdofet2022$codmunocornasc <- dados_simdofet2022$CODMUNOCOR
dados_simdofet2023$codmunocornasc <- dados_simdofet2023$CODMUNOCOR
dados_sinasc2018$codmunocornasc <- dados_sinasc2018$CODMUNNASC
dados_sinasc2019$codmunocornasc <- dados_sinasc2019$CODMUNNASC
dados_sinasc2020$codmunocornasc <- dados_sinasc2020$CODMUNNASC
dados_sinasc2021$codmunocornasc <- dados_sinasc2021$CODMUNNASC
dados_sinasc2022$codmunocornasc <- dados_sinasc2022$CODMUNNASC
dados_sinasc2023$codmunocornasc <- dados_sinasc2023$CODMUNNASC

dados_simdofet2018$lococornasc <- dados_simdofet2018$LOCOCOR
dados_simdofet2019$lococornasc <- dados_simdofet2019$LOCOCOR
dados_simdofet2020$lococornasc <- dados_simdofet2020$LOCOCOR
dados_simdofet2021$lococornasc <- dados_simdofet2021$LOCOCOR
dados_simdofet2022$lococornasc <- dados_simdofet2022$LOCOCOR
dados_simdofet2023$lococornasc <- dados_simdofet2023$LOCOCOR
dados_sinasc2018$lococornasc <- dados_sinasc2018$LOCNASC
dados_sinasc2019$lococornasc <- dados_sinasc2019$LOCNASC
dados_sinasc2020$lococornasc <- dados_sinasc2020$LOCNASC
dados_sinasc2021$lococornasc <- dados_sinasc2021$LOCNASC
dados_sinasc2022$lococornasc <- dados_sinasc2022$LOCNASC
dados_sinasc2023$lococornasc <- dados_sinasc2023$LOCNASC

dados_simdofet2018$horaobitonasc <- dados_simdofet2018$HORAOBITO
dados_simdofet2019$horaobitonasc <- dados_simdofet2019$HORAOBITO
dados_simdofet2020$horaobitonasc <- dados_simdofet2020$HORAOBITO
dados_simdofet2021$horaobitonasc <- dados_simdofet2021$HORAOBITO
dados_simdofet2022$horaobitonasc <- dados_simdofet2022$HORAOBITO
dados_simdofet2023$horaobitonasc <- dados_simdofet2023$HORAOBITO
dados_sinasc2018$horaobitonasc <- dados_sinasc2018$HORANASC
dados_sinasc2019$horaobitonasc <- dados_sinasc2019$HORANASC
dados_sinasc2020$horaobitonasc <- dados_sinasc2020$HORANASC
dados_sinasc2021$horaobitonasc <- dados_sinasc2021$HORANASC
dados_sinasc2022$horaobitonasc <- dados_sinasc2022$HORANASC
dados_sinasc2023$horaobitonasc <- dados_sinasc2023$HORANASC

dados_simdofet2018$ocupmaecodocupmae <- dados_simdofet2018$OCUPMAE
dados_simdofet2019$ocupmaecodocupmae <- dados_simdofet2019$OCUPMAE
dados_simdofet2020$ocupmaecodocupmae <- dados_simdofet2020$OCUPMAE
dados_simdofet2021$ocupmaecodocupmae <- dados_simdofet2021$OCUPMAE
dados_simdofet2022$ocupmaecodocupmae <- dados_simdofet2022$OCUPMAE
dados_simdofet2023$ocupmaecodocupmae <- dados_simdofet2023$OCUPMAE
dados_sinasc2018$ocupmaecodocupmae <- dados_sinasc2018$CODOCUPMAE
dados_sinasc2019$ocupmaecodocupmae <- dados_sinasc2019$CODOCUPMAE
dados_sinasc2020$ocupmaecodocupmae <- dados_sinasc2020$CODOCUPMAE
dados_sinasc2021$ocupmaecodocupmae <- dados_sinasc2021$CODOCUPMAE
dados_sinasc2022$ocupmaecodocupmae <- dados_sinasc2022$CODOCUPMAE
dados_sinasc2023$ocupmaecodocupmae <- dados_sinasc2023$CODOCUPMAE

dados_simdofet2018$codmunnatucodufnatu <- dados_simdofet2018$CODMUNNATU
dados_simdofet2019$codmunnatucodufnatu <- dados_simdofet2019$CODMUNNATU
dados_simdofet2020$codmunnatucodufnatu <- dados_simdofet2020$CODMUNNATU
dados_simdofet2021$codmunnatucodufnatu <- dados_simdofet2021$CODMUNNATU
dados_simdofet2022$codmunnatucodufnatu <- dados_simdofet2022$CODMUNNATU
dados_simdofet2023$codmunnatucodufnatu <- dados_simdofet2023$CODMUNNATU
dados_sinasc2018$codmunnatucodufnatu <- dados_sinasc2018$CODUFNATU
dados_sinasc2019$codmunnatucodufnatu<- dados_sinasc2019$CODUFNATU
dados_sinasc2020$codmunnatucodufnatu <- dados_sinasc2020$CODUFNATU
dados_sinasc2021$codmunnatucodufnatu <- dados_sinasc2021$CODUFNATU
dados_sinasc2022$codmunnatucodufnatu <- dados_sinasc2022$CODUFNATU
dados_sinasc2023$codmunnatucodufnatu <- dados_sinasc2023$CODUFNATU

# Indicador de pandemia (0 = sem pandemia, 1 = em pandemia)
dados_simdofet2018$pandemia <- 0
dados_simdofet2019$pandemia <- 0
dados_simdofet2020$pandemia <- 1
dados_simdofet2021$pandemia <- 1
dados_simdofet2022$pandemia <- 0
dados_simdofet2023$pandemia <- 0
dados_sinasc2018$pandemia <- 0
dados_sinasc2019$pandemia <- 0
dados_sinasc2020$pandemia <- 1
dados_sinasc2021$pandemia <- 1
dados_sinasc2022$pandemia <- 0
dados_sinasc2023$pandemia <- 0

# Todas as colunas em comum até o momento no SIM-DOFET e SINASC
all_vars <- c(
  "codmunnatucodufnatu", 
  "SEXO", "RACACOR", 
  "CODMUNRES", "CODESTAB", 
  "IDADEMAE", "ESCMAE", 
  "ESCMAE2010", "SERIESCMAE", 
  "QTDFILVIVO", "QTDFILMORT", 
  "GRAVIDEZ", "SEMAGESTAC", 
  "GESTACAO", "PARTO", 
  "PESO", "NUMEROLOTE", 
  "DTCADASTRO", "VERSAOSIST", 
  "DTRECEBIM", "ESCMAEAGR1", "DIFDATA", 
  "CONTADOR", "codmunocornasc", 
  "lococornasc", "horaobitonasc", 
  "ocupmaecodocupmae", 
  "uf_ocor", "uf_resid", 
  "ano", "evento", "pandemia")

# Seleciona apenas as colunas obtidas até agora
simdofet2018 <- dados_simdofet2018 %>% select(all_of(all_vars))
simdofet2019 <- dados_simdofet2019 %>% select(all_of(all_vars))
simdofet2020 <- dados_simdofet2020 %>% select(all_of(all_vars))
simdofet2021 <- dados_simdofet2021 %>% select(all_of(all_vars))
simdofet2022 <- dados_simdofet2022 %>% select(all_of(all_vars))
simdofet2023 <- dados_simdofet2023 %>% select(all_of(all_vars))

sinasc2018 <- dados_sinasc2018 %>% select(all_of(all_vars))
sinasc2019 <- dados_sinasc2019 %>% select(all_of(all_vars))
sinasc2020 <- dados_sinasc2020 %>% select(all_of(all_vars))
sinasc2021 <- dados_sinasc2021 %>% select(all_of(all_vars))
sinasc2022 <- dados_sinasc2022 %>% select(all_of(all_vars))
sinasc2023 <- dados_sinasc2023 %>% select(all_of(all_vars))

# Combina todos os dataframes SIM-DOFET e SINASC em um único dataframe
combined_df <- bind_rows(
  simdofet2018, 
  simdofet2019, 
  simdofet2020, 
  simdofet2021,
  simdofet2022,
  simdofet2023,
  sinasc2018, 
  sinasc2019, 
  sinasc2020, 
  sinasc2021,
  sinasc2022,
  sinasc2023
)

# Converte nomes de colunas para letras minúsculas
names(combined_df) <- tolower(names(combined_df))

names(combined_df)
table(combined_df$evento)

# Função para calcular a porcentagem de valores ausentes (NA ou em branco) em cada coluna
percentage_missing <- function(df) {
  sapply(df, function(x) {
    sum(is.na(x) | x == "") / length(x) * 100
  })
}

# Aplica a função ao seu dataframe combined_data
missing_percentages <- percentage_missing(combined_df)

# Cria um tibble com as porcentagens
missing_percentage_table <- tibble(
  Column = names(missing_percentages),
  Missing_Percentage = missing_percentages
)

# Exibir a tabela
print(missing_percentage_table, n = 34)


combined_df <- combined_df %>% select(-seriescmae) # 30% de NA, então está sendo removido

# Verifica se há algum valor NA no dataframe
has_na <- any(is.na(combined_df))

# Exibir o resultado
if (has_na) {
  print("The dataframe contains NA values.")
} else {
  print("The dataframe does not contain any NA values.")
}

combined_df <- as_tibble(combined_df)


saveRDS(combined_df, file = "dataset_combined_df.rds")
#------------------------------------------------------------------------------

