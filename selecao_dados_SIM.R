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
library(purrr)
library(survival)
library(survminer)
library(patchwork)
library(RColorBrewer)
library(readxl)
library(stringr)
library(tidyr)
library(scales)

# #-------------------- Bloco de preparação dos dados ----------------------------
# # dados_simdofet2023 <- fetch_datasus(year_start = 2023, year_end = 2023, information_system = "SIM-DOFET")
# #
# # saveRDS(dados_simdofet2023, file = "dados_simdofet2023.rds")
# dados_simdofet2023 <- readRDS("dados_simdofet2023.rds")
# 
# create1_uf_and_filter <- function(data, uf){
#   uf_map <- c(
#     "11"="RO","12"="AC","13"="AM","14"="RR","15"="PA","16"="AP","17"="TO",
#     "21"="MA","22"="PI","23"="CE","24"="RN","25"="PB","26"="PE","27"="AL","28"="SE","29"="BA",
#     "31"="MG","32"="ES","33"="RJ","35"="SP",
#     "41"="PR","42"="SC","43"="RS",
#     "50"="MS","51"="MT","52"="GO","53"="DF"
#   )
# 
#   data <- data %>%
#     mutate(
#       # uf ocorrência
#       uf_ocor = uf_map[
#         ifelse(
#           is.na(CODMUNOCOR), NA,
#           substr(sprintf("%06d", as.integer(CODMUNOCOR)), 1, 2)
#         )
#       ],
#       # uf residência
#       uf_resid = uf_map[
#         ifelse(
#           is.na(CODMUNRES), NA,
#           substr(sprintf("%06d", as.integer(CODMUNRES)), 1, 2)
#         )
#       ]
#     )
#   data <- filter(data, uf_resid == uf)
# }
# 
# dados_simdofet2023 <- create1_uf_and_filter(dados_simdofet2023, "SP")
# 
# 
# # 1 = óbito fetal, 0 = nascido vivo
# dados_simdofet2023$EVENTO <- 1
# 
# common_vars <- c("SEXO", "CODMUNRES", "IDADEMAE",
#                  "ESCMAE2010", "QTDFILVIVO",
#                  "QTDFILMORT", "GRAVIDEZ",
#                  "SEMAGESTAC", "PARTO", "PESO", "OBITOPARTO", "EVENTO")
# 
# check_var <- function(df, common_vars){
#   # Verifica se todas as colunas em common_vars estão no dataframe
#   all_columns_present <- all(common_vars %in% names(df))
# 
#   if (all_columns_present) {
#     print("Todas as colunas estão presentes no dataframe.")
#   } else {
#     print("Algumas colunas estão faltando no dataframe.")
#     missing_columns <- setdiff(common_vars, names(df))
#     print(paste("Colunas ausentes:", paste(missing_columns, collapse = ", ")))
#   }
# }
# 
# cat("Verifica se as variáveis previamente catalogadas como comum entre ambas as bases de fato existem:")
# 
# check_var(dados_simdofet2023, common_vars)
# 
# 
# # Função para verificar se as colunas não são inteiramente NA ou colunas em branco
# check_columns_not_only_na_blank <- function(df, cols) {
#   result <- list()
# 
#   for (col in cols) {
#     if (col %in% names(df)) {
#       # Verifica se a coluna inteira é NA ou está em branco
#       not_only_na_blank <- sum(!is.na(df[[col]]) & df[[col]] != "") > 0
#       result[[col]] <- not_only_na_blank
#     } else {
#       # Se a coluna não existir, é identificada como ausente
#       result[[col]] <- NA
#     }
#   }
# 
#   return(result)
# }
# 
# check_columns_results <- function(df, common_vars){
#   # Aplica a função ao seu dataframe e common_vars
#   not_only_na_blank_check <- check_columns_not_only_na_blank(df, common_vars)
# 
#   for (col in names(not_only_na_blank_check)) {
#     if (is.na(not_only_na_blank_check[[col]])) {
#       print(paste("Column", col, "is missing from the dataframe."))
#     } else if (not_only_na_blank_check[[col]]) {
#       print(paste("Column", col, "is NOT only NA or blank."))
#     } else {
#       print(paste("Column", col, "is entirely NA or blank."))
#     }
#   }
# }
# 
# #---------------- Verifica colunas NA ou em branco
# check_columns_results(dados_simdofet2023, common_vars)
# 
# #------------ Colunas existentes am ambas, mas com nomes diferentes
# SIM_vars <- c("LOCOCOR")
# 
# check_var(dados_simdofet2023, SIM_vars)
# 
# check_columns_results(dados_simdofet2023, SIM_vars)
# 
# # Adiciona colunas padronizadas em ambos datasets seguindo a catalogação prévia
# # que identificou que são colunas com o mesmo dado
# dados_simdofet2023$lococornasc <- dados_simdofet2023$LOCOCOR
# 
# # Todas as colunas em comum até o momento no SIM-DOFET e SINASC
# all_vars <- c(
#   "SEXO",
#   "CODMUNRES",
#   "IDADEMAE",
#   "ESCMAE2010",
#   "QTDFILVIVO",
#   "QTDFILMORT",
#   "GRAVIDEZ",
#   "SEMAGESTAC",
#   "PARTO",
#   "PESO",
#   "lococornasc",
#   "uf_resid",
#   "EVENTO",
#   "OBITOPARTO")
# 
# # Seleciona apenas as colunas obtidas até agora
# simdofet2023 <- dados_simdofet2023 %>% select(all_of(all_vars))
# 
# dados_def_ob <- simdofet2023
# #vamos considerar SEMAGESTAC > = 20 OU peso >500g para óbitos fetais
# table(dados_def_ob$SEMAGESTAC, dados_def_ob$PESO, useNA = "always")
# 
# dados_def_ob$indic_gestac <- ifelse(is.na(dados_def_ob$SEMAGESTAC), 1, 0)
# dados_def_ob$indic_peso <- ifelse(is.na(dados_def_ob$PESO), 1, 0)
# table(dados_def_ob$indic_gestac, dados_def_ob$indic_peso)
# 
# #   0      1
# # 0 4384  176
# # 1   78  169
# 
# #vamos perder 169 casos que é NA para peso e semagestac
# 
# dados_def_ob$obito_fetal <- ifelse(!is.na(dados_def_ob$SEMAGESTAC) & dados_def_ob$SEMAGESTAC >= 20, 1,
#                                    ifelse(!is.na(dados_def_ob$PESO) & dados_def_ob$PESO >= 500, 1,
#                                           ifelse(is.na(dados_def_ob$SEMAGESTAC) & is.na(dados_def_ob$PESO), NA, 0)))
# table(dados_def_ob$obito_fetal, useNA = "always")
# 
# # 0    1   <NA>
# # 190 4448  169
# 
# # 190 casos que são do SIM-DOFET, mas não se encaixam na definição de óbito fetal e nem é NA para peso e semagestac
# 
# #ficamos com 4448 casos de óbitos fetais (SEMAGESTAC > = 20 OU peso > 500g)
# 
# dados_def_ob_final <- dados_def_ob %>%
#   filter(obito_fetal == 1) %>%
#   select( "SEXO",
#           "CODMUNRES",
#           "IDADEMAE",
#           "ESCMAE2010",
#           "QTDFILVIVO",
#           "QTDFILMORT",
#           "GRAVIDEZ",
#           "SEMAGESTAC",
#           "PARTO",
#           "PESO",
#           "lococornasc",
#           "uf_resid",
#           "EVENTO",
#           "OBITOPARTO")
# 
# sim_df <- dados_def_ob_final
# 
# # Converte nomes de colunas para letras minúsculas
# names(sim_df) <- tolower(names(sim_df))
# 
# names(sim_df)
# table(sim_df$evento)
# 
# # Nasc vivos  Óbito fetal
# # 503910        4448
# 
# # total
# # [1] 508358
# 
# sim_df$sexo[sim_df$sexo == 0] <- NA
# sim_df$lococornasc[sim_df$lococornasc == 9] <- NA
# sim_df$escmae2010[sim_df$escmae2010 == 9] <- NA
# sim_df$qtdfilvivo[sim_df$qtdfilvivo == 99] <- NA
# sim_df$qtdfilmort[sim_df$qtdfilmort == 99] <- NA
# sim_df$gravidez[sim_df$gravidez == 9] <- NA
# sim_df$parto[sim_df$parto == 9] <- NA
# sim_df$obitoparto[sim_df$obitoparto == 9] <- NA
# sim_df$semagestac[sim_df$semagestac == 99] <- NA
# sim_df$idademae[sim_df$idademae == 99] <- NA
# 
# 
# # Considera apenas casos de Idade mãe entre 10 e 49 anos
# # sim_df2 <- filter(sim_df, sim_df$idademae >= 10 & sim_df$idademae <= 49)
# sim_df2 <- sim_df
# # > nrow(sim_df2)
# # [1] 508038
# 
# # Função para calcular a porcentagem de valores ausentes (NA ou em branco) em cada coluna
# percentage_missing <- function(df) {
#   sapply(df, function(x) {
#     sum(is.na(x) | x == "") / length(x) * 100
#   })
# }
# 
# # Aplica a função ao seu dataframe combined_data
# missing_percentages <- percentage_missing(sim_df2)
# 
# # Cria um tibble com as porcentagens
# missing_percentage_table <- tibble(
#   Column = names(missing_percentages),
#   Missing_Percentage = missing_percentages
# )
# 
# # Exibir a tabela
# print(missing_percentage_table, n = 34)
# 
# # Verifica se há algum valor NA no dataframe
# has_na <- any(is.na(sim_df2))
# 
# # Exibir o resultado
# if (has_na) {
#   print("The dataframe contains NA values.")
# } else {
#   print("The dataframe does not contain any NA values.")
# }
# 
# sim_df2 <- as_tibble(sim_df2)
# 
# # Selecionar apenas os casos completos nessas variáveis
# sim_df2_clean <- sim_df2[complete.cases(sim_df2[, ]), ]
# 
# # # teste <- filter(sim_df2_clean, as.numeric(sim_df2_clean$semagestac) >= 28)
# # # dim(teste)
# # 3268 total de casos válidos
# # 1894 (57.96%) casos de natimortos (semagestac >= 28, definição OMS)
# # 1374 (42.04%) demais casos de óbitos fetais
# 
# # # teste <- filter(sim_df2_clean, as.numeric(sim_df2_clean$semagestac) >= 20)
# # # dim(teste)
# # 3268 total de casos válidos
# # 3251 (99.48%) casos de natimortos (semagestac >= 20, definição EUA)
# # 17 (0.52%) demais casos de óbitos fetais
# 
# 
# 
# # Observação:
# # 1 – vaginal; 2 – cesáreo
# # table(sim_df2_clean$parto)
# # 1    2
# # 2312 956
# #
# # 1 = 70.75%
# # 2 = 29.25%
# 
# 
# dim(sim_df2_clean)
# 
# # -------------------- Adiciona colunas sobre as RRAS --------------------------
# # 1) Garantir o tipo/forma do código do município
# #    (no seu fluxo, 'codmunres' já está em minúsculo)
# stopifnot("codmunres" %in% names(sim_df2_clean))
# 
# sim_df2_clean <- sim_df2_clean %>%
#   mutate(
#     codmunres = as.character(codmunres),
#     codmunres = str_trim(codmunres)
#   )
# 
# # 2) Ler e preparar a tabela auxiliar município -> RRAS
# #    Ajuste o nome do arquivo/campos se necessário
# df_aux_rras <- read_excel("tabela_auxiliar_municipios.xlsx") %>%
#   filter(uf %in% c("São Paulo", "SP")) %>%
#   select(codmunres, cod_macro_r_saude, macro_r_saude) %>%
#   rename(
#     cod_mun_ibge = codmunres,
#     rras_id_raw  = cod_macro_r_saude,
#     rras_nome_raw = macro_r_saude
#   ) %>%
#   mutate(
#     cod_mun_ibge = as.character(cod_mun_ibge),
#     cod_mun_ibge = str_trim(cod_mun_ibge),
# 
#     # Extrair só o número do identificador
#     rras_id = as.integer(str_replace_all(as.character(rras_id_raw), "[^0-9]", "")),
# 
#     # Padronizar nome textual da RRAS
#     rras_nome = str_to_upper(as.character(rras_nome_raw)),
#     rras_nome = str_replace(rras_nome, "^(RRAS)\\s*0*([0-9]+)$", "\\1 \\2")
#   ) %>%
#   select(cod_mun_ibge, rras_id, rras_nome) %>%
#   distinct()
# 
# # 3) Join pelo município de residência
# sim_df2_clean <- sim_df2_clean %>%
#   left_join(df_aux_rras, by = c("codmunres" = "cod_mun_ibge"))
# 
# # 4) Checagem rápida de perdas de identificação
# n_total <- nrow(sim_df2_clean)
# n_sem_rras <- sum(is.na(sim_df2_clean$rras_id))
# 
# message("RRAS - total de linhas no dataset final: ", n_total)
# message("RRAS - linhas sem identificação de RRAS: ", n_sem_rras,
#         " (", round(100 * n_sem_rras / n_total, 2), "%)")
# 
# # 5) Criar codificações úteis para modelos com frailty e/ou INLA
# rras_levels <- sort(unique(sim_df2_clean$rras_id[!is.na(sim_df2_clean$rras_id)]))
# 
# sim_df2_clean <- sim_df2_clean %>%
#   mutate(
#     rras_fac = factor(rras_id, levels = rras_levels),
#     rras_int = as.integer(rras_fac)
#   )
# 
# sim_df2_clean <- sim_df2_clean %>% filter(!is.na(rras_id))
# 
# # saveRDS(sim_df2_clean, file = "dataset_sim_df.rds")
# #-------------------------------------------------------------------------------

sim_df2_clean <- readRDS("dataset_sim_df.rds")
################################################################################
##################### DESCRITIVA SÓ DO SIM-DOFET ISOLADO #######################
################################################################################
dados_sp_cln <- sim_df2_clean[, c(
  "sexo", 
  "idademae", 
  "escmae2010", 
  "qtdfilvivo", 
  "qtdfilmort", 
  "gravidez", 
  "semagestac", 
  "parto", 
  "peso", 
  "lococornasc", 
  "obitoparto"
  )]

# Recodificações principais e criação de objetos de tempo/evento
# Recodificação essencial para análises seguintes

dados_sp_cln <- dados_sp_cln %>%
  mutate(
    semagestac = suppressWarnings(as.numeric(semagestac)),
    idademae = suppressWarnings(as.numeric(idademae)),
    peso = suppressWarnings(as.numeric(peso)),
    qtdfilvivo = suppressWarnings(as.numeric(qtdfilvivo)),
    qtdfilmort = suppressWarnings(as.numeric(qtdfilmort))
  )

dados_eda_clean <- dados_sp_cln %>%
  mutate(
    sexo = case_when(sexo %in% c("1","2") ~ sexo, TRUE ~ NA_character_) %>%
      factor(levels = c("1","2"), labels = c("Masculino","Feminino")),
    gravidez = case_when(
      gravidez == "1" ~ "Única",
      gravidez == "2" ~ "Múltipla",
      gravidez == "3" ~ "Múltipla",
      TRUE ~ NA_character_) %>% 
      factor(levels = c("Única", "Múltipla")),
    parto = factor(parto, levels = c("1","2"), labels = c("Vaginal","Cesáreo")),
    lococornasc = case_when(
      lococornasc == "1" ~ "Hospital",
      lococornasc == "2" ~ "Outros",
      lococornasc == "3" ~ "Outros",
      lococornasc == "4" ~ "Outros",
      lococornasc == "5" ~ "Outros",
      TRUE ~ NA_character_) %>% 
      factor(levels = c("Hospital", "Outros")),
    escmae2010 = case_when(
      as.character(escmae2010) == "0" ~ "Baixa escolaridade",
      as.character(escmae2010) == "1" ~ "Baixa escolaridade",
      as.character(escmae2010) == "2" ~ "Baixa escolaridade",
      as.character(escmae2010) == "3" ~ "Média escolaridade",
      as.character(escmae2010) == "4" ~ "Alta escolaridade",
      as.character(escmae2010) == "5" ~ "Alta escolaridade",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Baixa escolaridade",
                            "Média escolaridade",
                            "Alta escolaridade"),
                 ordered = TRUE),
    idademae_categorico = case_when(
      idademae < 20 ~ "<20",
      idademae >= 20 &  idademae < 35 ~ "20–34",
      idademae >= 35 ~ "35+",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("<20","20–34","35+")),
    peso_categorico = case_when(
      peso < 2500 ~ "<2500g",
      peso >= 2500 &  peso < 4000 ~ "2500g-3999g",
      peso >= 4000 ~ "4000g+",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("<2500g","2500g-3999g","4000g+")),
    qtdfilvivo_categorico = case_when(
      qtdfilvivo == 0 ~ "Não",
      qtdfilvivo == 1 ~ "Sim",
      qtdfilvivo > 1 ~ "Sim",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Não", "Sim")),
    qtdfilmort_categorico = case_when(
      qtdfilmort == 0 ~ "Não",
      qtdfilmort == 1 ~ "Sim",
      qtdfilmort > 1 ~ "Sim",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Não", "Sim")),
    obitoparto = case_when(
      obitoparto == 1 ~ "Antes",
      obitoparto == 2 ~ "Durante",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Antes", "Durante")),
    # Objetos de tempo e eventos para as duas perspectivas
    tempo = semagestac
  ) 

#-------------------------------------------------------------------------------
#--------------------- Variáveis numéricas de interesse ------------------------
#-------------------------------------------------------------------------------
num_vars <- c("idademae", "qtdfilvivo", "qtdfilmort", "semagestac", "peso")
num_vars <- intersect(num_vars, names(dados_eda_clean))  

num_summary <- map_dfr(num_vars, function(v){
  x <- dados_eda_clean[[v]]
  q1 <- quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)  # Q1
  q3 <- quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)  # Q3
  tibble(
    variavel = v,
    n        = sum(!is.na(x)),
    media    = mean(x, na.rm = TRUE),
    dp       = sd(x, na.rm = TRUE),        
    mediana  = median(x, na.rm = TRUE),
    q1       = as.numeric(q1),
    q3       = as.numeric(q3)
  )
}) %>% arrange(variavel)

knitr::kable(
  num_summary,
  digits  = c(0, 0, 2, 2, 2, 2, 2),
  caption = "Medidas: n, média, DP, mediana, Q1, Q3"
)

#-------------------------------------------------------------------------------
#--------------------- Variáveis categóricas de interesse ----------------------
#-------------------------------------------------------------------------------
cat_vars <- c("sexo",
              "escmae2010",
              "gravidez",
              "parto",
              "lococornasc",
              "obitoparto", 
              "idademae_categorico",
              "peso_categorico",
              "qtdfilvivo_categorico",
              "qtdfilmort_categorico")
cat_vars <- intersect(cat_vars, names(dados_eda_clean))

cat_summary_all <- map_dfr(cat_vars, function(v){
  tab <- janitor::tabyl(dados_eda_clean[[v]], show_na = TRUE)  # n e percent prontos
  names(tab)[1] <- "nivel"
  tab %>%
    transmute(
      variavel = v,
      nivel    = as.character(nivel),
      n        = as.integer(n),                         # mantém contagem (não em proporção)
      percent  = scales::percent(percent, accuracy = 0.1)  # formata a proporção 0–1 em “xx.x%”
    )
}) %>% arrange(variavel)

knitr::kable(cat_summary_all,
             caption = "Categóricas — n e % do total"
)

#-------------------------------------------------------------------------------
#--------------------------- Kaplan-meier e Log-Rank ---------------------------
#-------------------------------------------------------------------------------
###################################
#sexo
###################################
# Como todos os casos são óbitos (não há censura), criamos um status = 1
dados_eda_clean <- dados_eda_clean %>%
  mutate(status_evento = 1L)

# Ajusta o modelo de sobrevivência estratificado por sexo
km_sexo <- survfit(Surv(tempo, status_evento) ~ sexo, data = dados_eda_clean)

# Visualização da curva empírica (Kaplan–Meier) por sexo
grafico_km_sexo <- ggsurvplot(
  km_sexo,
  data = dados_eda_clean,
  conf.int = FALSE,          # sem intervalos de confiança, pois não há censura
  xlab = "Semanas de gestação",
  ylab = "S(t) empírico (óbitos fetais)",
  title = "Curva empírica do tempo até o óbito fetal por sexo",
  legend.title = "",
  legend.labs = levels(dados_eda_clean$sexo),
  ggtheme = theme_minimal()
)

print(grafico_km_sexo)

# png("km_obitos_sexo.png", units="in", width=6, height=4, res=300)
# print(grafico_km_sexo)
# dev.off()

# Teste de log‑rank (equivalente ao teste de Mann–Whitney sem censura, Kruskal‑Wallis caso mais de 2 categorias)
logrank_sexo <- survdiff(Surv(tempo, status_evento) ~ sexo, data = dados_eda_clean)
print(logrank_sexo)

# Para extrair o p‑valor do teste de log‑rank:
p_valor <- 1 - pchisq(logrank_sexo$chisq, df = length(logrank_sexo$n) - 1)
print(paste("p‑valor do teste de log-rank:", signif(p_valor, 3)))

###################################
#Escolaridade da mãe (escmae2010)
##################################
km_esc <- survfit(Surv(tempo, status_evento) ~ escmae2010, data = dados_eda_clean)
grafico_km_esc <- ggsurvplot(
  km_esc, data = dados_eda_clean,
  conf.int = FALSE,
  xlab = "Semanas de gestação",
  ylab = "S(t) empírico (óbitos fetais)",
  title = "Curva empírica por escolaridade materna",
  legend.title = "",
  legend.labs = levels(dados_eda_clean$escmae2010),
  ggtheme = theme_minimal()
)
print(grafico_km_esc)

# png("km_obitos_escolaridade.png", units="in", width=6, height=4, res=300)
# print(grafico_km_esc)
# dev.off()

# Teste de log‑rank
logrank_esc <- survdiff(Surv(tempo, status_evento) ~ escmae2010, data = dados_eda_clean)
p_esc <- 1 - pchisq(logrank_esc$chisq, df = length(logrank_esc$n) - 1)
print(logrank_esc)

# Extrair o p‑valor
print(paste("p‑valor:", signif(p_esc, 3)))

###################################
#Tipo de gravidez (gravidez)
##################################
km_grav <- survfit(Surv(tempo, status_evento) ~ gravidez, data = dados_eda_clean)
grafico_km_grav <- ggsurvplot(
  km_grav, data = dados_eda_clean,
  conf.int = FALSE,
  xlab = "Semanas de gestação",
  ylab = "S(t) empírico (óbitos fetais)",
  title = "Curva empírica por tipo de gravidez",
  legend.title = "",
  legend.labs = levels(dados_eda_clean$gravidez),
  ggtheme = theme_minimal()
)
print(grafico_km_grav)

# png("km_obitos_gravidez.png", units="in", width=6, height=4, res=300)
# print(grafico_km_grav)
# dev.off()

# Teste de log‑rank
logrank_grav <- survdiff(Surv(tempo, status_evento) ~ gravidez, data = dados_eda_clean)
p_grav <- 1 - pchisq(logrank_grav$chisq, df = length(logrank_grav$n) - 1)
print(logrank_grav)

# Extrair o p‑valor
print(paste("p‑valor:", signif(p_grav, 3)))

###################################
#Tipo de parto (parto)
##################################
km_parto <- survfit(Surv(tempo, status_evento) ~ parto, data = dados_eda_clean)
grafico_km_parto <- ggsurvplot(
  km_parto, data = dados_eda_clean,
  conf.int = FALSE,
  xlab = "Semanas de gestação",
  ylab = "S(t) empírico (óbitos fetais)",
  title = "Curva empírica por tipo de parto",
  legend.title = "",
  legend.labs = levels(dados_eda_clean$parto),
  ggtheme = theme_minimal()
)
print(grafico_km_parto)

# png("km_obitos_parto.png", units="in", width=6, height=4, res=300)
# print(grafico_km_parto)
# dev.off()

# Teste de log‑rank
logrank_parto <- survdiff(Surv(tempo, status_evento) ~ parto, data = dados_eda_clean)
p_parto <- 1 - pchisq(logrank_parto$chisq, df = length(logrank_parto$n) - 1)
print(logrank_parto)

# Extrair o p‑valor
print(paste("p‑valor:", signif(p_parto, 3)))

###################################
#Local de ocorrência (lococornasc)
##################################
km_loc <- survfit(Surv(tempo, status_evento) ~ lococornasc, data = dados_eda_clean)
grafico_km_loc <- ggsurvplot(
  km_loc, data = dados_eda_clean,
  conf.int = FALSE,
  xlab = "Semanas de gestação",
  ylab = "S(t) empírico (óbitos fetais)",
  title = "Curva empírica por local de ocorrência",
  legend.title = "",
  legend.labs = levels(dados_eda_clean$lococornasc),
  ggtheme = theme_minimal()
)
print(grafico_km_loc)

# png("km_obitos_lococor.png", units="in", width=6, height=4, res=300)
# print(grafico_km_loc)
# dev.off()

# Teste de log‑rank
logrank_loc <- survdiff(Surv(tempo, status_evento) ~ lococornasc, data = dados_eda_clean)
p_loc <- 1 - pchisq(logrank_loc$chisq, df = length(logrank_loc$n) - 1)
print(logrank_loc)

# Extrair o p‑valor
print(paste("p‑valor:", signif(p_loc, 3)))

###################################
#Momento do óbito (obitoparto)
##################################
km_obito <- survfit(Surv(tempo, status_evento) ~ obitoparto, data = dados_eda_clean)
grafico_km_obito <- ggsurvplot(
  km_obito, data = dados_eda_clean,
  conf.int = FALSE,
  xlab = "Semanas de gestação",
  ylab = "S(t)",
  title = "KM (sem censura) - óbito fetal (por momento do óbito em relação ao parto)",
  legend.title = "",
  legend.labs = levels(dados_eda_clean$obitoparto),
  xlim       = c(19, 45), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme    = theme_minimal(base_size = 12)
)
print(grafico_km_obito)

png("km_obitos_obitoparto.png", units="in", width=7.5, height=4, res=112)
print(grafico_km_obito)
dev.off()

# Teste de log‑rank
logrank_obito <- survdiff(Surv(tempo, status_evento) ~ obitoparto, data = dados_eda_clean)
p_obito <- 1 - pchisq(logrank_obito$chisq, df = length(logrank_obito$n) - 1)
print(logrank_obito)

# Extrair o p‑valor
print(paste("p‑valor:", signif(p_obito, 3)))

###########################################
#Faixa etária da mãe (idademae_categorico)
##########################################
km_idade <- survfit(Surv(tempo, status_evento) ~ idademae_categorico, data = dados_eda_clean)
grafico_km_idade <- ggsurvplot(
  km_idade, data = dados_eda_clean,
  conf.int = FALSE,
  xlab = "Semanas de gestação",
  ylab = "S(t) empírico (óbitos fetais)",
  title = "Curva empírica por faixa etária da mãe",
  legend.title = "",
  legend.labs = levels(dados_eda_clean$idademae_categorico),
  ggtheme = theme_minimal()
)
print(grafico_km_idade)

# png("km_obitos_idademae.png", units="in", width=6, height=4, res=300)
# print(grafico_km_idade)
# dev.off()

# Teste de log‑rank
logrank_idade <- survdiff(Surv(tempo, status_evento) ~ idademae_categorico, data = dados_eda_clean)
p_idade <- 1 - pchisq(logrank_idade$chisq, df = length(logrank_idade$n) - 1)
print(logrank_idade)

# Extrair o p‑valor
print(paste("p‑valor:", signif(p_idade, 3)))

###########################################
#Faixa de peso (peso_categorico)
##########################################
km_peso <- survfit(Surv(tempo, status_evento) ~ peso_categorico, data = dados_eda_clean)
grafico_km_peso <- ggsurvplot(
  km_peso, data = dados_eda_clean,
  conf.int = FALSE,
  xlab = "Semanas de gestação",
  ylab = "S(t) empírico (óbitos fetais)",
  title = "Curva empírica por faixa de peso ao nascer",
  legend.title = "",
  legend.labs = levels(dados_eda_clean$peso_categorico),
  ggtheme = theme_minimal()
)
print(grafico_km_peso)

# png("km_obitos_peso.png", units="in", width=6, height=4, res=300)
# print(grafico_km_peso)
# dev.off()

# Teste de log‑rank
logrank_peso <- survdiff(Surv(tempo, status_evento) ~ peso_categorico, data = dados_eda_clean)
p_peso <- 1 - pchisq(logrank_peso$chisq, df = length(logrank_peso$n) - 1)
print(logrank_peso)

# Extrair o p‑valor
print(paste("p‑valor:", signif(p_peso, 3)))

##################################################
#Presença de filhos vivos (qtdfilvivo_categorico)
#################################################
km_filhos_vivos <- survfit(Surv(tempo, status_evento) ~ qtdfilvivo_categorico, data = dados_eda_clean)
grafico_km_filhos_vivos <- ggsurvplot(
  km_filhos_vivos, data = dados_eda_clean,
  conf.int = FALSE,
  xlab = "Semanas de gestação",
  ylab = "S(t) empírico (óbitos fetais)",
  title = "Curva empírica por presença de filhos vivos",
  legend.title = "",
  legend.labs = levels(dados_eda_clean$qtdfilvivo_categorico),
  ggtheme = theme_minimal()
)
print(grafico_km_filhos_vivos)

# png("km_obitos_qtfilhosvivos.png", units="in", width=6, height=4, res=300)
# print(grafico_km_filhos_vivos)
# dev.off()

# Teste de log‑rank
logrank_filhos_vivos <- survdiff(Surv(tempo, status_evento) ~ qtdfilvivo_categorico, data = dados_eda_clean)
p_filhos_vivos <- 1 - pchisq(logrank_filhos_vivos$chisq, df = length(logrank_filhos_vivos$n) - 1)
print(logrank_filhos_vivos)

# Extrair o p‑valor
print(paste("p‑valor:", signif(p_filhos_vivos, 3)))

##################################################
#Presença de filhos mortos (qtdfilmort_categorico)
#################################################
km_filhos_mortos <- survfit(Surv(tempo, status_evento) ~ qtdfilmort_categorico, data = dados_eda_clean)
grafico_km_filhos_mortos <- ggsurvplot(
  km_filhos_mortos, data = dados_eda_clean,
  conf.int = FALSE,
  xlab = "Semanas de gestação",
  ylab = "S(t) empírico (óbitos fetais)",
  title = "Curva empírica por presença de filhos mortos",
  legend.title = "",
  legend.labs = levels(dados_eda_clean$qtdfilmort_categorico),
  ggtheme = theme_minimal()
)
print(grafico_km_filhos_mortos)

# png("km_obitos_qtfilhosmortos.png", units="in", width=6, height=4, res=300)
# print(grafico_km_filhos_mortos)
# dev.off()

# Teste de log‑rank
logrank_filhos_mortos <- survdiff(Surv(tempo, status_evento) ~ qtdfilmort_categorico, data = dados_eda_clean)
p_filhos_mortos <- 1 - pchisq(logrank_filhos_mortos$chisq, df = length(logrank_filhos_mortos$n) - 1)
print(logrank_filhos_mortos)

# Extrair o p‑valor
print(paste("p‑valor:", signif(p_filhos_mortos, 3)))