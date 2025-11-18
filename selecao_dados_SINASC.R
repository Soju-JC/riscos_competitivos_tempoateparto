library(microdatasus)
library(dplyr)
library(purrr)
library(survival)
library(survminer)
library(patchwork)
library(RColorBrewer)

# dados_sinasc2023 <- fetch_datasus(year_start = 2023, year_end = 2023, information_system = "SINASC")
# 
# saveRDS(dados_sinasc2023, file = "dados_sinasc2023.rds")
dados_sinasc <- readRDS("dados_sinasc2023.rds")

#selecionar só os casos de SP
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
  data <- filter(data, uf_resid == uf)
}


dados_sinasc_sp <- create2_uf_and_filter(dados_sinasc , "SP")

# Função para gerar tabelas de frequência com NA
freq_table <- function(data, vars) {
  result <- list()
  
  for (var in vars) {
    if (var %in% names(data)) {
      tab <- table(data[[var]], useNA = "ifany")
      freq <- as.data.frame(tab)
      names(freq) <- c(var, "Freq")
      result[[var]] <- freq
    } else {
      warning(paste("Variável", var, "não encontrada no data frame."))
    }
  }
  
  return(result)
}

freq_table(dados_sinasc_sp, c("TPROBSON"))

table(dados_sinasc_sp$SEMAGESTAC, dados_sinasc_sp$PESO, useNA = "always")

##variaveis selecionadas para a base de dados
dados_final <- dados_sinasc_sp %>% 
  select(
        CODMUNRES,
        LOCNASC,
        IDADEMAE,
        ESCMAE2010,
        RACACORMAE,
        PARIDADE,
        QTDFILVIVO,
        QTDFILMORT,
        QTDGESTANT,
        QTDPARTNOR,
        QTDPARTCES,
        CONSULTAS,
        MESPRENAT,
        GRAVIDEZ,
        PARTO,
        STTRABPART,
        STCESPARTO,
        SEXO,
        APGAR5,
        PESO,
        IDANOMAL,
        SEMAGESTAC
        )

# 1 = óbito fetal, 0 = nascido vivo
dados_final$EVENTO <- 0

##arrumar algumas variáveis
dados_final$IDADEMAE <- as.numeric(dados_final$IDADEMAE)
dados_final$IDADEMAE <- ifelse(dados_final$IDADEMAE == 99, NA, dados_final$IDADEMAE)

dados_final$QTDGESTANT <- as.numeric(dados_final$QTDGESTANT)
dados_final$QTDGESTANT <- ifelse(dados_final$QTDGESTANT == 99, NA, dados_final$QTDGESTANT)

dados_final$QTDPARTNOR <- as.numeric(dados_final$QTDPARTNOR)
dados_final$QTDPARTNOR <- ifelse(dados_final$QTDPARTNOR == 99, NA, dados_final$QTDPARTNOR)

dados_final$QTDPARTCES <- as.numeric(dados_final$QTDPARTCES)
dados_final$QTDPARTCES <- ifelse(dados_final$QTDPARTCES == 99, NA, dados_final$QTDPARTCES)

dados_final$MESPRENAT <- as.numeric(dados_final$MESPRENAT)
dados_final$MESPRENAT <- ifelse(dados_final$MESPRENAT == 99, NA, dados_final$MESPRENAT)

dados_final$APGAR5 <- as.numeric(dados_final$APGAR5)
dados_final$APGAR5 <- ifelse(dados_final$APGAR5 == 99, NA, dados_final$APGAR5)

dados_final$PESO <- as.numeric(dados_final$PESO)

dados_final$SEMAGESTAC <- as.numeric(dados_final$SEMAGESTAC)

# Recodificação de valores específicos para NA

dados_final$LOCNASC[dados_final$LOCNASC == 9] <- NA
dados_final$ESCMAE2010[dados_final$ESCMAE2010 == 9] <- NA
dados_final$CONSULTAS[dados_final$CONSULTAS == 9] <- NA
dados_final$GRAVIDEZ[dados_final$GRAVIDEZ == 9] <- NA
dados_final$PARTO[dados_final$PARTO == 9] <- NA
dados_final$STTRABPART[dados_final$STTRABPART == 9] <- NA
dados_final$STCESPARTO[dados_final$STCESPARTO == 9] <- NA
dados_final$SEXO[dados_final$SEXO == 0] <- NA
dados_final$IDANOMAL[dados_final$IDANOMAL == 9] <- NA
dados_final$QTDFILVIVO[dados_final$QTDFILVIVO == 99] <- NA
dados_final$QTDFILMORT[dados_final$QTDFILMORT == 99] <- NA

# freq_table(dados_final, c(#"CODESTAB",
#   #"CODMUNNASC",
#   #"CODMUNRES",
#   "LOCNASC",
#   "IDADEMAE",
#   #"ESTCIVMAE",
#   "ESCMAE2010",
#   "RACACORMAE",
#   #"CODOCUPMAE",
#   "PARIDADE",
#   "QTDGESTANT",
#   "QTDPARTNOR",
#   "QTDPARTCES",
#   "CONSULTAS",
#   "MESPRENAT",
#   "GRAVIDEZ",
#   "PARTO",
#   "STTRABPART",
#   "STCESPARTO",
#   "SEXO",
#   "APGAR5",
#   #"PESO",
#   "IDANOMAL"
#   #"SEMAGESTAC"
# ))

summary(dados_final$SEMAGESTAC)
summary(dados_final$PESO)


colnames(dados_final)[colnames(dados_final) == "LOCNASC"] <- "LOCOCORNASC"
#ver dados completos

# Definir vetor de variáveis de interesse
vars <- c(
  "CODMUNRES",
  "LOCOCORNASC",
  "IDADEMAE",
  "ESCMAE2010",
  "RACACORMAE",
  "PARIDADE",
  "QTDFILVIVO",
  "QTDFILMORT",
  "QTDGESTANT",
  "QTDPARTNOR",
  "QTDPARTCES",
  "CONSULTAS",
  "MESPRENAT",
  "GRAVIDEZ",
  "PARTO",
  "STTRABPART",
  "STCESPARTO",
  "SEXO",
  "APGAR5",
  "PESO",
  "IDANOMAL",
  "SEMAGESTAC",
  "EVENTO"
)

# Considera apenas casos de Idade mãe entre 10 e 49 anos
dados_final <- filter(dados_final, dados_final$IDADEMAE >= 10 & dados_final$IDADEMAE <= 49)

# Converte nomes de colunas para letras minúsculas
names(dados_final) <- tolower(names(dados_final))

names(dados_final)

# Função para calcular a porcentagem de valores ausentes (NA ou em branco) em cada coluna
percentage_missing <- function(df) {
  sapply(df, function(x) {
    sum(is.na(x) | x == "" | x == " ") / length(x) * 100
  })
}

# Aplica a função ao seu dataframe combined_data
missing_percentages <- percentage_missing(dados_final)

# Cria um tibble com as porcentagens
missing_percentage_table <- tibble(
  Column = names(missing_percentages),
  Missing_Percentage = missing_percentages
)

# Exibir a tabela
print(missing_percentage_table, n = 34)

# Selecionar apenas os casos completos nessas variáveis
vars <- tolower(vars)
dados_completos <- dados_final[complete.cases(dados_final[, vars]), ]

dim(dados_final)

dim(dados_completos)

# saveRDS(dados_completos, file = "dataset_sinasc_df.rds")

################################################################################
####################### DESCRITIVA SÓ DO SINASC ISOLADO ########################
################################################################################
dados_sp_cln <- dados_completos[, c(
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
  "consultas",
  "apgar5",
  "idanomal",
  "racacormae",
  "qtdgestant",
  "qtdpartnor",
  "qtdpartces",
  "mesprenat",
  "sttrabpart",
  "stcesparto",
  "paridade"
)]

# Recodificações principais e criação de objetos de tempo/evento
# Recodificação essencial para análises seguintes

dados_sp_cln <- dados_sp_cln %>%
  mutate(
    semagestac = suppressWarnings(as.numeric(semagestac)),
    idademae = suppressWarnings(as.numeric(idademae)),
    peso = suppressWarnings(as.numeric(peso)),
    qtdfilvivo = suppressWarnings(as.numeric(qtdfilvivo)),
    qtdfilmort = suppressWarnings(as.numeric(qtdfilmort)),
    qtdgestant = suppressWarnings(as.numeric(qtdgestant)),
    qtdpartnor = suppressWarnings(as.numeric(qtdpartnor)),
    qtdpartces = suppressWarnings(as.numeric(qtdpartces)),
    apgar5 = suppressWarnings(as.numeric(apgar5)),
    mesprenat = suppressWarnings(as.numeric(mesprenat))
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
      TRUE ~ NA_character_) %>% 
      factor(levels = c("Hospital", "Outros")),
    consultas = case_when(
      consultas == "1" ~ "Nenhuma",
      consultas == "2" ~ "De 1 a 3",
      consultas == "3" ~ "De 4 a 6",
      consultas == "4" ~ "7 e mais",
      TRUE ~ NA_character_) %>% 
      factor(levels = c("Nenhuma","De 1 a 3","De 4 a 6", "7 e mais")),
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
    idanomal = case_when(
      idanomal == 1 ~ "Sim",
      idanomal == 2 ~ "Não",
      TRUE ~ NA_character_) %>% 
      factor(levels = c("Sim","Não")),
    racacormae = case_when(
      racacormae == 1 ~ "Branca",
      racacormae == 2 ~ "Preta",
      racacormae == 3 ~ "Amarela",
      racacormae == 4 ~ "Parda",
      racacormae == 5 ~ "Indígena",
      TRUE ~ NA_character_) %>% 
      factor(levels = c("Branca","Preta", "Amarela", "Parda","Indígena")),
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
    sttrabpart = case_when(
      sttrabpart == 1 ~ "Sim",
      sttrabpart == 2 ~ "Não",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Sim", "Não")),
    stcesparto = case_when(
      stcesparto == 1 ~ "Sim",
      stcesparto == 2 ~ "Não",
      stcesparto == 3 ~ "Não se aplica",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Sim", "Não", "Não se aplica")),
    paridade = case_when(
      paridade == 0 ~ "Nulípara",
      paridade == 1 ~ "Multípara",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Nulípara", "Multípara")),
    apgar5_categorico = case_when(
      apgar5 > 7 ~ "7+",
      apgar5 <= 7 ~ "7 ou menos",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("7+", "7 ou menos")),
    qtdgestant_categorico = case_when(
      qtdgestant == 0 ~ "Não",
      qtdgestant > 0 ~ "Sim",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Não", "Sim")),
    qtdpartnor_categorico = case_when(
      qtdpartnor == 0 ~ "Não",
      qtdpartnor > 0 ~ "Sim",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Não", "Sim")),
    qtdpartces_categorico = case_when(
      qtdpartces == 0 ~ "Não",
      qtdpartces > 0 ~ "Sim",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Não", "Sim")),
    mesprenat_categorico = case_when(
      mesprenat == 1 ~ "Primeiro trimestre",
      mesprenat == 2 ~ "Primeiro trimestre",
      mesprenat == 3 ~ "Primeiro trimestre",
      mesprenat == 4 ~ "Segundo trimestre",
      mesprenat == 5 ~ "Segundo trimestre",
      mesprenat == 6 ~ "Segundo trimestre",
      mesprenat == 7 ~ "Terceiro trimestre",
      mesprenat == 8 ~ "Terceiro trimestre",
      mesprenat == 9 ~ "Terceiro trimestre",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Primeiro trimestre", "Segundo trimestre", "Terceiro trimestre")),
    # Objetos de tempo e eventos para as duas perspectivas
    tempo = semagestac
  ) 

#-------------------------------------------------------------------------------
#--------------------- Variáveis numéricas de interesse ------------------------
#-------------------------------------------------------------------------------
num_vars <- c(
  "idademae", "qtdfilvivo", "qtdfilmort", "qtdgestant", "qtdpartnor", 
  "qtdpartces", "apgar5", "mesprenat","semagestac", "peso"
  )
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
              "consultas",
              "idanomal",
              "racacormae",
              "sttrabpart",
              "stcesparto",
              "paridade",
              "idademae_categorico",
              "peso_categorico",
              "qtdfilvivo_categorico",
              "qtdfilmort_categorico",
              "apgar5_categorico",
              "qtdgestant_categorico",
              "qtdpartnor_categorico",
              "qtdpartces_categorico",
              "mesprenat_categorico")
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

# Preparação: inclusão do status_evento (todos os eventos observados)
dados_eda_clean <- dados_eda_clean %>%
  mutate(status_evento = 1L)

#------------------------------------------------------------------------------
# 1. Sexo do recém-nascido             ****************************************
#------------------------------------------------------------------------------
# Ajuste do modelo Kaplan–Meier por sexo
km_sexo <- survfit(Surv(tempo, status_evento) ~ sexo, data = dados_eda_clean)

# Plot da curva empírica para sexo
grafico_km_sexo <- ggsurvplot(
  km_sexo,
  data       = dados_eda_clean,
  conf.int   = FALSE,         # sem intervalo de confiança, pois não há censura
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica do tempo até o parto por sexo",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$sexo),
  ggtheme    = theme_minimal()
)
print(grafico_km_sexo)

# png("km_nascidosvivos_sexo.png", units="in", width=6, height=4, res=300)
# print(grafico_km_sexo)
# dev.off()

# Teste de Log‑Rank para comparar as curvas entre sexos
logrank_sexo <- survdiff(Surv(tempo, status_evento) ~ sexo, data = dados_eda_clean)
print(logrank_sexo)

p_sexo <- 1 - pchisq(logrank_sexo$chisq, df = length(logrank_sexo$n) - 1)
print(paste("p-valor:", signif(p_sexo, 3)))

# Tenta detectar se de fato existe diferença
#------  efeito mediana
medianas_sexo <- tapply(dados_eda_clean$tempo, dados_eda_clean$sexo, median, na.rm = TRUE)
medianas_sexo

#------  Estimador wilcox de diferença de localização (Hodges–Lehmann)
wilcox_sexo <- wilcox.test(tempo ~ sexo,
                           data = dados_eda_clean,
                           conf.int = TRUE,
                           conf.level = 0.95,
                           exact = FALSE)  # exact=FALSE pela amostra grande

wilcox_sexo
#ex de interpretação
# O estimador de Hodges–Lehmann indicou que o tempo de gestação em nascidos do sexo 
# masculino é, em mediana, 0,1 semana menor do que em nascidos do 
# sexo feminino (IC95%: −0,12 a −0,08 semana). Embora a diferença seja 
# estatisticamente significativa, a magnitude é muito pequena.

# Interpretação: valores de d em torno de 0,2 = efeito pequeno, 0,5 = médio, 0,8 = grande.

#------ Via modelo de cox
# Garantir que "Feminino" seja a categoria de referência
dados_eda_clean$sexo <- relevel(dados_eda_clean$sexo, ref = "Feminino")

# Ajustar o modelo de Cox
cox_sexo <- coxph(Surv(tempo, status_evento) ~ sexo, data = dados_eda_clean)
summary(cox_sexo)

#Se HR > 1: o grupo no numerador (Masculino) tem partos mais precoces, em média (gestação mais curta).
#Se HR < 1: partos mais tardios que o grupo de referência.
#HR + IC95% (magnitude relativa)
HR_sexo <- exp(coef(cox_sexo))
IC_sexo <- exp(confint(cox_sexo))

HR_sexo
IC_sexo

#------------------------------------------------------------------------------
# 2. Escolaridade da mãe (escmae2010)  ****************************************
#------------------------------------------------------------------------------
km_esc <- survfit(Surv(tempo, status_evento) ~ escmae2010, data = dados_eda_clean)

grafico_km_esc <- ggsurvplot(
  km_esc,
  data       = dados_eda_clean,
  conf.int   = FALSE,
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica por escolaridade materna",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$escmae2010),
  ggtheme    = theme_minimal()
)
print(grafico_km_esc)

# png("km_nascidosvivos_escolaridade.png", units="in", width=6, height=4, res=300)
# print(grafico_km_esc)
# dev.off()

logrank_esc <- survdiff(Surv(tempo, status_evento) ~ escmae2010, data = dados_eda_clean)
print(logrank_esc)

p_esc <- 1 - pchisq(logrank_esc$chisq, df = length(logrank_esc$n) - 1)
print(paste("p-valor:", signif(p_esc, 3)))

#------------------------------------------------------------------------------
# 3. Tipo de gravidez (gravidez: Única vs Múltipla)
#------------------------------------------------------------------------------
km_grav <- survfit(Surv(tempo, status_evento) ~ gravidez, data = dados_eda_clean)

grafico_km_grav <- ggsurvplot(
  km_grav,
  data       = dados_eda_clean,
  conf.int   = FALSE,
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica por tipo de gravidez",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$gravidez),
  ggtheme    = theme_minimal()
)
print(grafico_km_grav)

# png("km_nascidosvivos_gravidez.png", units="in", width=6, height=4, res=300)
# print(grafico_km_grav)
# dev.off()

logrank_grav <- survdiff(Surv(tempo, status_evento) ~ gravidez, data = dados_eda_clean)
print(logrank_grav)

p_grav <- 1 - pchisq(logrank_grav$chisq, df = length(logrank_grav$n) - 1)
print(paste("p-valor:", signif(p_grav, 3)))

#------------------------------------------------------------------------------
# 4. Tipo de parto (Vaginal vs Cesáreo)****************************************
#------------------------------------------------------------------------------
km_parto <- survfit(Surv(tempo, status_evento) ~ parto, data = dados_eda_clean)

grafico_km_parto <- ggsurvplot(
  km_parto,
  data       = dados_eda_clean,
  conf.int   = FALSE,
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica por tipo de parto",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$parto),
  ggtheme    = theme_minimal()
)
print(grafico_km_parto)

# png("km_nascidosvivos_parto.png", units="in", width=6, height=4, res=300)
# print(grafico_km_parto)
# dev.off()

logrank_parto <- survdiff(Surv(tempo, status_evento) ~ parto, data = dados_eda_clean)
print(logrank_parto)

p_parto <- 1 - pchisq(logrank_parto$chisq, df = length(logrank_parto$n) - 1)
print(paste("p-valor:", signif(p_parto, 3)))

#------------------------------------------------------------------------------
# 5. Local de ocorrência do parto (Hospital vs Outros)*************************
#------------------------------------------------------------------------------
km_loc <- survfit(Surv(tempo, status_evento) ~ lococornasc, data = dados_eda_clean)

grafico_km_loc <- ggsurvplot(
  km_loc,
  data       = dados_eda_clean,
  conf.int   = FALSE,
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica por local de ocorrência",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$lococornasc),
  ggtheme    = theme_minimal()
)
print(grafico_km_loc)

png("km_nascidosvivos_loconasc.png", units="in", width=6, height=4, res=300)
print(grafico_km_loc)
dev.off()

logrank_loc <- survdiff(Surv(tempo, status_evento) ~ lococornasc, data = dados_eda_clean)
print(logrank_loc)

p_loc <- 1 - pchisq(logrank_loc$chisq, df = length(logrank_loc$n) - 1)
print(paste("p-valor:", signif(p_loc, 3)))

#------------------------------------------------------------------------------
# 6. Número de consultas pré-natais
#------------------------------------------------------------------------------
km_consultas <- survfit(Surv(tempo, status_evento) ~ consultas, data = dados_eda_clean)

grafico_km_consultas <- ggsurvplot(
  km_consultas,
  data       = dados_eda_clean,
  conf.int   = FALSE,
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica por número de consultas pré-natais",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$consultas),
  ggtheme    = theme_minimal()
)
print(grafico_km_consultas)

# png("km_nascidosvivos_consultas.png", units="in", width=6, height=4, res=300)
# print(grafico_km_consultas)
# dev.off()

logrank_consultas <- survdiff(Surv(tempo, status_evento) ~ consultas, data = dados_eda_clean)
print(logrank_consultas)

p_consultas <- 1 - pchisq(logrank_consultas$chisq, df = length(logrank_consultas$n) - 1)
print(paste("p-valor:", signif(p_consultas, 3)))

#------------------------------------------------------------------------------
# 7. Presença de anomalias congênitas (Sim/Não)
#------------------------------------------------------------------------------
km_anomalia <- survfit(Surv(tempo, status_evento) ~ idanomal, data = dados_eda_clean)

grafico_km_anomalia <- ggsurvplot(
  km_anomalia,
  data       = dados_eda_clean,
  conf.int   = FALSE,
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica por presença de anomalias congênitas",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$idanomal),
  ggtheme    = theme_minimal()
)
print(grafico_km_anomalia)

# png("km_nascidosvivos_anomalia.png", units="in", width=6, height=4, res=300)
# print(grafico_km_anomalia)
# dev.off()

logrank_anomalia <- survdiff(Surv(tempo, status_evento) ~ idanomal, data = dados_eda_clean)
print(logrank_anomalia)

p_anomalia <- 1 - pchisq(logrank_anomalia$chisq, df = length(logrank_anomalia$n) - 1)
print(paste("p-valor:", signif(p_anomalia, 3)))

#------------------------------------------------------------------------------
# 8. Raça/Cor da mãe                   ****************************************
#------------------------------------------------------------------------------
km_raca <- survfit(Surv(tempo, status_evento) ~ racacormae, data = dados_eda_clean)

grafico_km_raca <- ggsurvplot(
  km_raca,
  data       = dados_eda_clean,
  conf.int   = FALSE,
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica por raça/cor da mãe",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$racacormae),
  ggtheme    = theme_minimal()
)
print(grafico_km_raca)

# png("km_nascidosvivos_racamae.png", units="in", width=6, height=4, res=300)
# print(grafico_km_raca)
# dev.off()

logrank_raca <- survdiff(Surv(tempo, status_evento) ~ racacormae, data = dados_eda_clean)
print(logrank_raca)

p_raca <- 1 - pchisq(logrank_raca$chisq, df = length(logrank_raca$n) - 1)
print(paste("p-valor:", signif(p_raca, 3)))

#------------------------------------------------------------------------------
# 9. Indução do trabalho de parto (sttrabpart)*********************************
#------------------------------------------------------------------------------
km_trabalho <- survfit(Surv(tempo, status_evento) ~ sttrabpart, data = dados_eda_clean)

grafico_km_trabalho <- ggsurvplot(
  km_trabalho,
  data       = dados_eda_clean,
  conf.int   = FALSE,
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica por indução do trabalho de parto",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$sttrabpart),
  ggtheme    = theme_minimal()
)
print(grafico_km_trabalho)

# png("km_nascidosvivos_indtrabalhoparto.png", units="in", width=6, height=4, res=300)
# print(grafico_km_trabalho)
# dev.off()

logrank_trabalho <- survdiff(Surv(tempo, status_evento) ~ sttrabpart, data = dados_eda_clean)
print(logrank_trabalho)

p_trabalho <- 1 - pchisq(logrank_trabalho$chisq, df = length(logrank_trabalho$n) - 1)
print(paste("p-valor:", signif(p_trabalho, 3)))

#------------------------------------------------------------------------------
# 10. Cesárea programada (stcesparto) * ***************************************
#------------------------------------------------------------------------------
km_cesarea <- survfit(Surv(tempo, status_evento) ~ stcesparto, data = dados_eda_clean)

grafico_km_cesarea <- ggsurvplot(
  km_cesarea,
  data       = dados_eda_clean,
  conf.int   = FALSE,
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica por realização de cesárea programada",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$stcesparto),
  ggtheme    = theme_minimal()
)
print(grafico_km_cesarea)

# png("km_nascidosvivos_stcesparto.png", units="in", width=6, height=4, res=300)
# print(grafico_km_cesarea)
# dev.off()

logrank_cesarea <- survdiff(Surv(tempo, status_evento) ~ stcesparto, data = dados_eda_clean)
print(logrank_cesarea)

p_cesarea <- 1 - pchisq(logrank_cesarea$chisq, df = length(logrank_cesarea$n) - 1)
print(paste("p-valor:", signif(p_cesarea, 3)))

#------------------------------------------------------------------------------
# 11. Paridade (Nulípara vs Multípara) ****************************************
#------------------------------------------------------------------------------
km_paridade <- survfit(Surv(tempo, status_evento) ~ paridade, data = dados_eda_clean)

grafico_km_paridade <- ggsurvplot(
  km_paridade,
  data       = dados_eda_clean,
  conf.int   = FALSE,
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica por paridade",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$paridade),
  ggtheme    = theme_minimal()
)
print(grafico_km_paridade)

# png("km_nascidosvivos_paridade.png", units="in", width=6, height=4, res=300)
# print(grafico_km_paridade)
# dev.off()

logrank_paridade <- survdiff(Surv(tempo, status_evento) ~ paridade, data = dados_eda_clean)
print(logrank_paridade)

p_paridade <- 1 - pchisq(logrank_paridade$chisq, df = length(logrank_paridade$n) - 1)
print(paste("p-valor:", signif(p_paridade, 3)))

#------------------------------------------------------------------------------
# 12. Faixa etária da mãe (idademae_categorico)********************************
#------------------------------------------------------------------------------
km_idade <- survfit(Surv(tempo, status_evento) ~ idademae_categorico, data = dados_eda_clean)

grafico_km_idade <- ggsurvplot(
  km_idade,
  data       = dados_eda_clean,
  conf.int   = FALSE,
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica por faixa etária da mãe",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$idademae_categorico),
  ggtheme    = theme_minimal()
)
print(grafico_km_idade)

# png("km_nascidosvivos_idademae.png", units="in", width=6, height=4, res=300)
# print(grafico_km_idade)
# dev.off()

logrank_idade <- survdiff(Surv(tempo, status_evento) ~ idademae_categorico, data = dados_eda_clean)
print(logrank_idade)

p_idade <- 1 - pchisq(logrank_idade$chisq, df = length(logrank_idade$n) - 1)
print(paste("p-valor:", signif(p_idade, 3)))

#------------------------------------------------------------------------------
# 13. Faixa de peso ao nascer (peso_categorico)
#------------------------------------------------------------------------------
km_peso <- survfit(Surv(tempo, status_evento) ~ peso_categorico, data = dados_eda_clean)

grafico_km_peso <- ggsurvplot(
  km_peso,
  data       = dados_eda_clean,
  conf.int   = FALSE,
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica por faixa de peso ao nascer",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$peso_categorico),
  ggtheme    = theme_minimal()
)
print(grafico_km_peso)

# png("km_nascidosvivos_peso.png", units="in", width=6, height=4, res=300)
# print(grafico_km_peso)
# dev.off()

logrank_peso <- survdiff(Surv(tempo, status_evento) ~ peso_categorico, data = dados_eda_clean)
print(logrank_peso)

p_peso <- 1 - pchisq(logrank_peso$chisq, df = length(logrank_peso$n) - 1)
print(paste("p-valor:", signif(p_peso, 3)))

#------------------------------------------------------------------------------
# 14. Presença de filhos vivos previamente (qtdfilvivo_categorico)*************
#------------------------------------------------------------------------------
km_filhos_vivos <- survfit(Surv(tempo, status_evento) ~ qtdfilvivo_categorico, data = dados_eda_clean)

grafico_km_filhos_vivos <- ggsurvplot(
  km_filhos_vivos,
  data       = dados_eda_clean,
  conf.int   = FALSE,
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica por presença de filhos vivos prévios",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$qtdfilvivo_categorico),
  ggtheme    = theme_minimal()
)
print(grafico_km_filhos_vivos)

# png("km_nascidosvivos_qtdfilvivo.png", units="in", width=6, height=4, res=300)
# print(grafico_km_filhos_vivos)
# dev.off()

logrank_filhos_vivos <- survdiff(Surv(tempo, status_evento) ~ qtdfilvivo_categorico, data = dados_eda_clean)
print(logrank_filhos_vivos)

p_filhos_vivos <- 1 - pchisq(logrank_filhos_vivos$chisq, df = length(logrank_filhos_vivos$n) - 1)
print(paste("p-valor:", signif(p_filhos_vivos, 3)))

#------------------------------------------------------------------------------
# 15. Presença de filhos mortos previamente (qtdfilmort_categorico)************
#------------------------------------------------------------------------------
km_filhos_mortos <- survfit(Surv(tempo, status_evento) ~ qtdfilmort_categorico, data = dados_eda_clean)

grafico_km_filhos_mortos <- ggsurvplot(
  km_filhos_mortos,
  data       = dados_eda_clean,
  conf.int   = FALSE,
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica por presença de filhos mortos prévios",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$qtdfilmort_categorico),
  ggtheme    = theme_minimal()
)
print(grafico_km_filhos_mortos)

# png("km_nascidosvivos_qtfilhosmortos.png", units="in", width=6, height=4, res=300)
# print(grafico_km_filhos_mortos)
# dev.off()

logrank_filhos_mortos <- survdiff(Surv(tempo, status_evento) ~ qtdfilmort_categorico, data = dados_eda_clean)
print(logrank_filhos_mortos)

p_filhos_mortos <- 1 - pchisq(logrank_filhos_mortos$chisq, df = length(logrank_filhos_mortos$n) - 1)
print(paste("p-valor:", signif(p_filhos_mortos, 3)))

#------------------------------------------------------------------------------
# 16. Escore de Apgar aos 5 minutos (apgar5_categorico)
#------------------------------------------------------------------------------
km_apgar <- survfit(Surv(tempo, status_evento) ~ apgar5_categorico, data = dados_eda_clean)

grafico_km_apgar <- ggsurvplot(
  km_apgar,
  data       = dados_eda_clean,
  conf.int   = FALSE,
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica por escore Apgar aos 5 minutos",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$apgar5_categorico),
  ggtheme    = theme_minimal()
)
print(grafico_km_apgar)

# png("km_nascidosvivos_apgar.png", units="in", width=6, height=4, res=300)
# print(grafico_km_apgar)
# dev.off()

logrank_apgar <- survdiff(Surv(tempo, status_evento) ~ apgar5_categorico, data = dados_eda_clean)
print(logrank_apgar)

p_apgar <- 1 - pchisq(logrank_apgar$chisq, df = length(logrank_apgar$n) - 1)
print(paste("p-valor:", signif(p_apgar, 3)))

#------------------------------------------------------------------------------
# 17. Número de gestações anteriores (qtdgestant_categorico)*******************
#------------------------------------------------------------------------------
km_gestant <- survfit(Surv(tempo, status_evento) ~ qtdgestant_categorico, data = dados_eda_clean)

grafico_km_gestant <- ggsurvplot(
  km_gestant,
  data       = dados_eda_clean,
  conf.int   = FALSE,
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica por número de gestações anteriores",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$qtdgestant_categorico),
  ggtheme    = theme_minimal()
)
print(grafico_km_gestant)

# png("km_nascidosvivos_qtdgestant.png", units="in", width=6, height=4, res=300)
# print(grafico_km_gestant)
# dev.off()

logrank_gestant <- survdiff(Surv(tempo, status_evento) ~ qtdgestant_categorico, data = dados_eda_clean)
print(logrank_gestant)

p_gestant <- 1 - pchisq(logrank_gestant$chisq, df = length(logrank_gestant$n) - 1)
print(paste("p-valor:", signif(p_gestant, 3)))

#------------------------------------------------------------------------------
# 18. Número de partos vaginais anteriores (qtdpartnor_categorico)*************
#------------------------------------------------------------------------------
km_partnor <- survfit(Surv(tempo, status_evento) ~ qtdpartnor_categorico, data = dados_eda_clean)

grafico_km_partnor <- ggsurvplot(
  km_partnor,
  data       = dados_eda_clean,
  conf.int   = FALSE,
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica por número de partos vaginais anteriores",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$qtdpartnor_categorico),
  ggtheme    = theme_minimal()
)
print(grafico_km_partnor)

# png("km_nascidosvivos_qtdpartnor.png", units="in", width=6, height=4, res=300)
# print(grafico_km_partnor)
# dev.off()

logrank_partnor <- survdiff(Surv(tempo, status_evento) ~ qtdpartnor_categorico, data = dados_eda_clean)
print(logrank_partnor)

p_partnor <- 1 - pchisq(logrank_partnor$chisq, df = length(logrank_partnor$n) - 1)
print(paste("p-valor:", signif(p_partnor, 3)))

#------------------------------------------------------------------------------
# 19. Número de partos cesáreos anteriores (qtdpartces_categorico)*************
#------------------------------------------------------------------------------
km_partces <- survfit(Surv(tempo, status_evento) ~ qtdpartces_categorico, data = dados_eda_clean)

grafico_km_partces <- ggsurvplot(
  km_partces,
  data       = dados_eda_clean,
  conf.int   = FALSE,
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica por número de partos cesáreos anteriores",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$qtdpartces_categorico),
  ggtheme    = theme_minimal()
)
print(grafico_km_partces)

# png("km_nascidosvivos_qtdpartces.png", units="in", width=6, height=4, res=300)
# print(grafico_km_partces)
# dev.off()

logrank_partces <- survdiff(Surv(tempo, status_evento) ~ qtdpartces_categorico, data = dados_eda_clean)
print(logrank_partces)

p_partces <- 1 - pchisq(logrank_partces$chisq, df = length(logrank_partces$n) - 1)
print(paste("p-valor:", signif(p_partces, 3)))

#------------------------------------------------------------------------------
# 20. Trimestre de início do pré-natal (mesprenat_categorico)******************
#------------------------------------------------------------------------------
km_mesprenat <- survfit(Surv(tempo, status_evento) ~ mesprenat_categorico, data = dados_eda_clean)

grafico_km_mesprenat <- ggsurvplot(
  km_mesprenat,
  data       = dados_eda_clean,
  conf.int   = FALSE,
  xlab       = "Semanas de gestação",
  ylab       = "S(t) empírico (nascidos vivos)",
  title      = "Curva empírica por trimestre de início do pré-natal",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$mesprenat_categorico),
  ggtheme    = theme_minimal()
)
print(grafico_km_mesprenat)

# png("km_nascidosvivos_mesprenat.png", units="in", width=6, height=4, res=300)
# print(grafico_km_mesprenat)
# dev.off()

logrank_mesprenat <- survdiff(Surv(tempo, status_evento) ~ mesprenat_categorico, data = dados_eda_clean)
print(logrank_mesprenat)

p_mesprenat <- 1 - pchisq(logrank_mesprenat$chisq, df = length(logrank_mesprenat$n) - 1)
print(paste("p-valor:", signif(p_mesprenat, 3)))