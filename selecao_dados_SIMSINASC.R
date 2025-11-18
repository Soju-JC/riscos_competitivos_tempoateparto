library(microdatasus)
library(dplyr)
library(purrr)
library(survival)
library(survminer)
library(patchwork)
library(RColorBrewer)

dados_simsp <- readRDS("dataset_sim_df.rds") # selecao_dados_SIM

dados_sinascsp <- readRDS("dataset_sinasc_df.rds") # selecao_dados_SINASC

# Identificar nomes de variáveis em comum
vars_comuns <- intersect(names(dados_sinascsp), names(dados_simsp))

dados_sinascsp_comum <- dados_sinascsp[ , vars_comuns]
dados_simsp_comum    <- dados_simsp[ , vars_comuns]

# Joint
dados_sp_cln <- rbind(dados_sinascsp_comum, dados_simsp_comum)

#------------------------------- Variáveis
dplyr::glimpse(dados_sp_cln, width = 80)

# Variáveis numéricas
dados_sp_cln <- dados_sp_cln %>%
  mutate(
    semagestac = suppressWarnings(as.numeric(semagestac)),
    idademae = suppressWarnings(as.numeric(idademae)),
    peso = suppressWarnings(as.numeric(peso)),
    qtdfilvivo = suppressWarnings(as.numeric(qtdfilvivo)),
    qtdfilmort = suppressWarnings(as.numeric(qtdfilmort)),
    evento = suppressWarnings(as.integer(evento))
  )

################################################################################
######################## DESCRITIVA DOS DATASETS UNIDOS ########################
################################################################################
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
    # Objetos de tempo e eventos para as duas perspectivas
    tempo = semagestac,
    status_fd = ifelse(evento == 1L, 1L, 0L),   # 1 = óbito fetal é causa
    status_lv = ifelse(evento == 0L, 1L, 0L),   # 1 = nascimento vivo é causa
    fstatus   = case_when(evento == 1L ~ 1L,    # 1 = óbito fetal
                          evento == 0L ~ 2L,    # 2 = nascido vivo (competidor)
                          TRUE ~ 0L)            # 0 = censura
  )

#--------------------- Tamanho, taxa bruta de eventos e histogramas
n <- nrow(dados_eda_clean)
taxa_obito <- mean(dados_eda_clean$status_fd)
tab_evento <- table(Evento = ifelse(dados_eda_clean$status_fd==1,"Óbito fetal","Nascido vivo"))

list(n_registros = n, taxa_óbito_fetal = round(taxa_obito, 4))
tab_evento

# Distribuição da idade gestacional
p1 <- ggplot(dados_eda_clean, aes(x = tempo)) +
  geom_histogram() + labs(x="Semanas de gestação", y="Frequência",
                          title="Distribuição de idade gestacional (tempo)") +
  theme_minimal(base_size = 12)
p1

# Distribuição da idade materna
p2 <- ggplot(dados_eda_clean, aes(x = idademae)) +
  geom_histogram(bins = 39) + labs(x="Idade materna (anos)", y="Frequência") +
  theme_minimal(base_size = 12)
p2

# Distribuição do peso
p3 <- ggplot(dados_eda_clean, aes(x = peso)) +
  geom_histogram(bins = 60) + labs(x="Peso ao nascer (g)", y="Frequência") +
  theme_minimal(base_size = 12)
p3

# Distribuição da quantidade de filhos vivos
p_qtdfilvivo <- ggplot(dados_eda_clean, aes(x = qtdfilvivo)) +
  geom_bar() + 
  scale_x_continuous(breaks = seq(0, max(dados_eda_clean$qtdfilvivo, na.rm = TRUE), by = 1)) +
  labs(
    x = "Filhos vivos (contagem)",
    y = "Frequência (n)",
    title = "Distribuição de filhos vivos prévios (qtdfilvivo)"
  ) +
  theme_minimal(base_size = 12)

p_qtdfilvivo

# Distribuição da quantidade de filhos mortos
p_qtdfilmort <- ggplot(dados_eda_clean, aes(x = qtdfilmort)) +
  geom_bar() + 
  scale_x_continuous(breaks = seq(0, max(dados_eda_clean$qtdfilmort, na.rm = TRUE), by = 1)) +
  labs(
    x = "Óbitos fetais (contagem)",
    y = "Frequência (n)",
    title = "Distribuição de óbitos fetais prévios (qtdfilmort)"
  ) +
  theme_minimal(base_size = 12)

p_qtdfilmort

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
      percent  = scales::percent(percent, accuracy = 0.01)  # formata a proporção 0–1 em “xx.x%”
    )
}) %>% arrange(variavel)

knitr::kable(cat_summary_all,
             caption = "Categóricas — n e % do total"
)

#-------------------------------------------------------------------------------
#--------------------------- Kaplan-meier e Log-Rank ---------------------------
#-------------------------------------------------------------------------------

#-------------------------------- KM global

# Óbito fetal como evento (censura = nascido vivo) 
km_fd <- survfit(Surv(tempo, status_fd) ~ 1, data = dados_eda_clean)
grafico_km_fd <- ggsurvplot(
  km_fd,
  data       = dados_eda_clean,
  conf.int   = TRUE,         # Há censura
  xlab       = "Semanas de gestação",
  ylab       = "S(t) (óbito fetal como evento)",
  title      = "KM – Óbito fetal (nascido vivo censurado)",
  legend     = "none",       # remove a legenda
  ylim       = c(0.985, 1.00),   # Zoom
  ggtheme    = theme_minimal(base_size = 12)
)
print(grafico_km_fd)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("km_global_fd.png", units="in", width = 10, height = 5, res=300)
print(grafico_km_fd)
dev.off()

# Nascido vivo como evento (óbito fetal censurado) 
km_lv <- survfit(Surv(tempo, status_lv) ~ 1, data = dados_eda_clean)
grafico_km_lv <- ggsurvplot(
  km_lv,
  data       = dados_eda_clean,
  conf.int   = TRUE,         # Há censura
  xlab       = "Semanas de gestação",
  ylab       = "S(t) (nascido vivo como evento)",
  title      = "KM – Nascido vivo (óbito fetal censurado)",
  legend     = "none",       # remove a legenda
  ylim       = c(0, 1.00),   # Zoom
  ggtheme    = theme_minimal(base_size = 12),
  palette      = "blue",
  conf.int.fill  = "lightblue"
)
print(grafico_km_lv)

# Painel
p1 <- grafico_km_fd$plot +
  theme(plot.margin = unit(c(5.5, 10, 5.5, 5.5), "pt"))

p2 <- grafico_km_lv$plot +
  theme(plot.margin = unit(c(5.5, 5.5, 5.5, 10), "pt"))

painel_km_global <- ggarrange(
  p1, p2,
  ncol          = 2,
  nrow          = 1,
  legend        = "none"
)

print(painel_km_global)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("km_global.png", units="in", width=10, height=5, res=300)
print(painel_km_global)
dev.off()

#----------- KM, CIF, Testes de Log-rank e de Gray para cada caso

#------------------------------------------------------------------------------
# 1. Sexo do recém-nascido             
#------------------------------------------------------------------------------

#------------------------------- Kaplan–Meier

# Óbito fetal como evento (censura = nascido vivo) 
km_fd_sexo <- survfit(Surv(tempo, status_fd) ~ sexo, data = dados_eda_clean)
grafico_kmglobal_sexo_fd <- ggsurvplot(
  km_fd_sexo,
  data       = dados_eda_clean,
  conf.int   = TRUE,        
  xlab       = "Semanas de gestação",
  ylab       = "S(t) — óbito fetal",
  title      = "KM — Óbito fetal (censura: nascido vivo)\npor sexo",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$sexo),
  ylim = c(0.987, 1.00), 
  ggtheme = theme_minimal(base_size = 12)
)
print(grafico_kmglobal_sexo_fd)

# Nascido vivo como evento (óbito fetal censurado) 
km_lv_sexo <- survfit(Surv(tempo, status_lv) ~ sexo, data = dados_eda_clean)
grafico_kmglobal_sexo_lv <- ggsurvplot(
  km_lv_sexo,
  data       = dados_eda_clean,
  conf.int   = TRUE,        
  xlab       = "Semanas de gestação",
  ylab       = "S(t) — nascido vivo",
  title      = "KM — Nascido vivo (censura: óbito fetal)\npor sexo",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$sexo),
  ylim = c(0, 1), # Não chega exatamente à zero (0.0025)
  ggtheme = theme_minimal(base_size = 12)
)
print(grafico_kmglobal_sexo_lv)

# Painel
p1 <- grafico_kmglobal_sexo_fd$plot +
  theme(plot.margin = unit(c(5.5, 10, 5.5, 5.5), "pt"))

p2 <- grafico_kmglobal_sexo_lv$plot +
  theme(plot.margin = unit(c(5.5, 5.5, 5.5, 10), "pt"))

painel_kmglobal_sexo <- ggarrange(
  p1, p2,
  ncol          = 2,
  nrow          = 1,
  common.legend = TRUE,
  legend        = "bottom"
)

print(painel_kmglobal_sexo)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("kmglobal_sexo.png", units="in", width=10, height=5, res=300)
print(painel_kmglobal_sexo)
dev.off()

#------------------------------- Log-Rank
# LOG-RANK para óbito fetal como evento de interesse (censura nascidos vivos)
logrank_fd_sexo <- survdiff(Surv(tempo, status_fd) ~ sexo, data = dados_eda_clean)
print(logrank_fd_sexo)

p_sexo <- 1 - pchisq(logrank_fd_sexo$chisq, df = length(logrank_fd_sexo$n) - 1)
print(paste("p-valor:", signif(p_sexo, 3)))

# LOG-RANK  para nascidos vivos como evento de interesse (censura óbito fetal)
logrank_lv_sexo <- survdiff(Surv(tempo, status_lv) ~ sexo, data = dados_eda_clean)
print(logrank_lv_sexo)

p_sexo <- 1 - pchisq(logrank_lv_sexo$chisq, df = length(logrank_lv_sexo$n) - 1)
print(paste("p-valor:", signif(p_sexo, 3)))

#------------------------------- Teste de gray
# Transformar fstatus em fator com rótulos desejados
dados_eda_clean$fstatus_cr <- factor(
  dados_eda_clean$fstatus,
  levels = c(0, 1, 2),
  labels = c("Censura", "Óbito fetal", "Nascimento vivo")
)

# Teste de Gray da variável sexo
ci_sexo <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus_cr,
  group   = dados_eda_clean$sexo,
  cencode = "Censura"   # nível que representa censura
)
print(ci_sexo)

# Apenas o teste (sem informações extras)
ci_sexo$Tests

#------------------------------- CIF

# CIF da variável sexo (1 = óbito fetal, 2 = nascido vivo)
p_cif_sexo <- ggcompetingrisks(
  fit            = ci_sexo,
  multiple_panels = TRUE,   # Plota ou não cada grupo em painéis separados
  ggtheme         = theme_minimal(base_size = 12),
  palette         = c("#1f77b4", "#d62728"),
  legend.title    = "Evento",
  legend.labs     = c("Óbito fetal", "Nascimento vivo"),
  xlab            = "Semanas de gestação",
  ylab            = "Incidência acumulada",
  title           = "Gráfico de Incidência Acumulada (CIF)"
)
print(p_cif_sexo)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("cif_sexo.png", units="in", width = 10, height = 5, res=300)
print(p_cif_sexo)
dev.off()

# CIF da variável sexo (olhando mais de perto o evento de óbitos)

# Selecionar apenas as curvas do evento 1 (óbito fetal)
ci_sexo_evento1 <- ci_sexo[c("Masculino Óbito fetal", "Feminino Óbito fetal")]

# Extrair os nomes sem o código do evento
group_names_sexo <- sub(" 1$", "", names(ci_sexo_evento1)[names(ci_sexo_evento1) != "Tests"])
# Isso gera c("Masculino", "Feminino")

# Construir o gráfico
p_cif_fd_sexo <- ggcompetingrisks(
  fit             = ci_sexo_evento1,
  multiple_panels = FALSE,
  ggtheme         = theme_minimal(base_size = 12)
) +
  # Mapea cada sexo a um tipo de linha de forma explícita
  scale_linetype_manual(
    name   = "Sexo",
    values = c("Feminino" = "solid",
               "Masculino" = "dashed"),
    labels = c("Feminino", "Masculino"),
    breaks = c("Feminino", "Masculino")
  ) +
  # Esconde a legenda de cor e pinta as linhas da legenda de vermelho
  guides(
    color = "none" ,
    linetype = guide_legend(
      override.aes = list(colour = rep("#d62728", length(group_names_sexo)))
    )
  ) +
  labs(
    title = "CIF apenas dos óbitos (sexo)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  )

print(p_cif_fd_sexo)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("cif_fd_sexo.png", units="in", width = 10, height = 5, res=300)
print(p_cif_fd_sexo)
dev.off()

#------------------------------------------------------------------------------
# 2. Faixa etária da mãe (idademae_categorico)
#------------------------------------------------------------------------------

#------------------------------- Kaplan–Meier

# Óbito fetal como evento (censura = nascido vivo)
km_fd_idade <- survfit(
  Surv(tempo, status_fd) ~ idademae_categorico,
  data = dados_eda_clean
)

grafico_kmglobal_idade_fd <- ggsurvplot(
  km_fd_idade,
  data        = dados_eda_clean,
  conf.int    = TRUE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t) — óbito fetal",
  title       = "KM — Óbito fetal (censura: nascido vivo)\npor faixa etária materna",
  legend.title = "Faixa etária materna",
  legend.labs  = levels(dados_eda_clean$idademae_categorico),
  ylim        = c(0.985, 1.00),   # mesmo padrão usado no sexo
  ggtheme     = theme_minimal(base_size = 12)
)
print(grafico_kmglobal_idade_fd)

# Nascido vivo como evento (óbito fetal censurado)
km_lv_idade <- survfit(
  Surv(tempo, status_lv) ~ idademae_categorico,
  data = dados_eda_clean
)

grafico_kmglobal_idade_lv <- ggsurvplot(
  km_lv_idade,
  data        = dados_eda_clean,
  conf.int    = TRUE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t) — nascido vivo",
  title       = "KM — Nascido vivo (censura: óbito fetal)\npor faixa etária materna",
  legend.title = "Faixa etária materna",
  legend.labs  = levels(dados_eda_clean$idademae_categorico),
  ylim        = c(0, 1),          # segue o padrão do sexo
  ggtheme     = theme_minimal(base_size = 12)
)
print(grafico_kmglobal_idade_lv)

# Painel 
p1_idade <- grafico_kmglobal_idade_fd$plot +
  theme(plot.margin = unit(c(5.5, 10, 5.5, 5.5), "pt"))

p2_idade <- grafico_kmglobal_idade_lv$plot +
  theme(plot.margin = unit(c(5.5, 5.5, 5.5, 10), "pt"))

painel_kmglobal_idade <- ggarrange(
  p1_idade, p2_idade,
  ncol          = 2,
  nrow          = 1,
  common.legend = TRUE,
  legend        = "bottom"
)
print(painel_kmglobal_idade)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("kmglobal_idade_mae.png", units = "in", width = 10, height = 5, res = 300)
print(painel_kmglobal_idade)
dev.off()

#------------------------------- Log-Rank

# LOG-RANK — óbito fetal como evento de interesse (censura: nascido vivo)
logrank_fd_idade <- survdiff(
  Surv(tempo, status_fd) ~ idademae_categorico,
  data = dados_eda_clean
)
print(logrank_fd_idade)

p_idade_fd <- 1 - pchisq(
  logrank_fd_idade$chisq,
  df = length(logrank_fd_idade$n) - 1
)
print(paste("p-valor:",
            signif(p_idade_fd, 3)))

# LOG-RANK — nascido vivo como evento de interesse (censura: óbito fetal)
logrank_lv_idade <- survdiff(
  Surv(tempo, status_lv) ~ idademae_categorico,
  data = dados_eda_clean
)
print(logrank_lv_idade)

p_idade_lv <- 1 - pchisq(
  logrank_lv_idade$chisq,
  df = length(logrank_lv_idade$n) - 1
)
print(paste("p-valor:",
            signif(p_idade_lv, 3)))

#------------------------------- Teste de Gray

# Teste de Gray para a variável idademae_categorico
ci_idade <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus_cr,
  group   = dados_eda_clean$idademae_categorico,
  cencode = "Censura"
)
print(ci_idade)

# Apenas o teste (sem informações extras)
ci_idade$Tests

#------------------------------- CIF
p_cif_idade <- ggcompetingrisks(
  fit             = ci_idade,
  multiple_panels = TRUE,   # painel por categoria de idade materna
  ggtheme         = theme_minimal(base_size = 12),
  palette         = c("#1f77b4", "#d62728"),   # 1 = óbito fetal, 2 = nascido vivo
  legend.title    = "Evento",
  legend.labs     = c("Óbito fetal", "Nascimento vivo"),
  xlab            = "Semanas de gestação",
  ylab            = "Incidência acumulada",
  title           = "Gráfico de Incidência Acumulada (CIF) por faixa etária materna"
)
print(p_cif_idade)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("cif_idademae.png", units = "in", width = 10, height = 5, res = 300)
print(p_cif_idade)
dev.off()

# CIF focada apenas no evento óbito fetal (evento 1) por faixa etária

# Selecionar apenas as curvas com o evento "Óbito fetal"
ci_idade_evento1 <- ci_idade[grep("Óbito fetal$", names(ci_idade))]

# Verificar nomes
print(names(ci_idade_evento1))

# Converter o objeto 'cuminc' (apenas evento 1) em um data.frame longo
df_cif_fd_idade <- purrr::map_dfr(
  names(ci_idade_evento1),
  function(nm) {
    comp <- ci_idade_evento1[[nm]]
    
    # Extrair a faixa etária removendo o sufixo " Óbito fetal"
    faixa <- sub(" Óbito fetal$", "", nm)
    
    # Construir data.frame com tempo, CIF e faixa etária
    data.frame(
      tempo = comp$time,
      cif   = comp$est,
      faixa = faixa,
      stringsAsFactors = FALSE
    )
  }
) %>%
  # Garantir ordem das faixas etárias de interesse
  dplyr::mutate(
    faixa = factor(faixa, levels = c("<20", "20–34", "35+"))
  )

# Conferir se as faixas ficaram corretas
print(table(df_cif_fd_idade$faixa, useNA = "ifany"))

# Construção do gráfico CIF apenas dos óbitos fetais
p_cif_fd_idade <- ggplot(df_cif_fd_idade,
                         aes(x = tempo,
                             y = cif,
                             linetype = faixa)) +
  geom_step(
    color    = "#d62728",  # todas as curvas em vermelho (destacando óbito fetal)
    linewidth = 0.7
  ) +
  scale_linetype_manual(
    name   = "Faixa etária materna",
    values = c(
      "<20"   = "solid",
      "20–34" = "dashed",
      "35+"   = "dotted"
    )
  ) +
  labs(
    title = "CIF apenas para óbitos fetais por faixa etária materna",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )

print(p_cif_fd_idade)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("cif_fd_idademae.png", units = "in", width = 10, height = 5, res = 300)
print(p_cif_fd_idade)
dev.off()

#------------------------------------------------------------------------------
# 3. Escolaridade da mãe (escmae2010)  
#------------------------------------------------------------------------------

#------------------------------- Kaplan–Meier

# Óbito fetal como evento (censura = nascido vivo)
km_fd_esc <- survfit(
  Surv(tempo, status_fd) ~ escmae2010,
  data = dados_eda_clean
)

grafico_kmglobal_esc_fd <- ggsurvplot(
  km_fd_esc,
  data        = dados_eda_clean,
  conf.int    = TRUE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t) — óbito fetal",
  title       = "KM — Óbito fetal (censura: nascido vivo)\npor escolaridade materna",
  legend.title = "Escolaridade materna",
  legend.labs  = levels(dados_eda_clean$escmae2010),
  ylim        = c(0.975, 1.00),   # mesmo padrão das demais KM de óbito fetal
  ggtheme     = theme_minimal(base_size = 12)
)
print(grafico_kmglobal_esc_fd)

# Nascido vivo como evento (óbito fetal censurado)
km_lv_esc <- survfit(
  Surv(tempo, status_lv) ~ escmae2010,
  data = dados_eda_clean
)

grafico_kmglobal_esc_lv <- ggsurvplot(
  km_lv_esc,
  data        = dados_eda_clean,
  conf.int    = TRUE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t) — nascido vivo",
  title       = "KM — Nascido vivo (censura: óbito fetal)\npor escolaridade materna",
  legend.title = "Escolaridade materna",
  legend.labs  = levels(dados_eda_clean$escmae2010),
  ylim        = c(0, 1),
  ggtheme     = theme_minimal(base_size = 12)
)
print(grafico_kmglobal_esc_lv)

# Painel com os dois KM (óbito fetal e nascido vivo)

p1_esc <- grafico_kmglobal_esc_fd$plot +
  theme(plot.margin = unit(c(5.5, 10, 5.5, 5.5), "pt"))

p2_esc <- grafico_kmglobal_esc_lv$plot +
  theme(plot.margin = unit(c(5.5, 5.5, 5.5, 10), "pt"))

painel_kmglobal_esc <- ggarrange(
  p1_esc, p2_esc,
  ncol          = 2,
  nrow          = 1,
  common.legend = TRUE,
  legend        = "bottom"
)

print(painel_kmglobal_esc)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("kmglobal_escmae2010.png", units = "in", width = 10, height = 5, res = 300)
print(painel_kmglobal_esc)
dev.off()

#------------------------------- Log-Rank

# LOG-RANK — óbito fetal como evento de interesse (censura: nascido vivo)
logrank_fd_esc <- survdiff(
  Surv(tempo, status_fd) ~ escmae2010,
  data = dados_eda_clean
)
print(logrank_fd_esc)

p_esc_fd <- 1 - pchisq(
  logrank_fd_esc$chisq,
  df = length(logrank_fd_esc$n) - 1
)
print(paste(
  "p-valor:",
  signif(p_esc_fd, 3)
))

# LOG-RANK — nascido vivo como evento de interesse (censura: óbito fetal)
logrank_lv_esc <- survdiff(
  Surv(tempo, status_lv) ~ escmae2010,
  data = dados_eda_clean
)
print(logrank_lv_esc)

p_esc_lv <- 1 - pchisq(
  logrank_lv_esc$chisq,
  df = length(logrank_lv_esc$n) - 1
)
print(paste(
  "p-valor:",
  signif(p_esc_lv, 3)
))

#------------------------------- Teste de Gray

# Teste de Gray para escmae2010
ci_esc <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus_cr,
  group   = dados_eda_clean$escmae2010,
  cencode = "Censura"
)
print(ci_esc)

# Apenas o teste (sem informações extras)
ci_esc$Tests

#------------------------------- CIF

# Devido a como foi nomeado as categorias, nesse caso será necessário uma 
#manipulação mais complexa

# Renomeia para "Grupo|Evento"
nm <- names(ci_esc)
idx <- nm != "Tests"
nm[idx] <- gsub(" Óbito fetal$",      "|Óbito fetal",      nm[idx])
nm[idx] <- gsub(" Nascimento vivo$",  "|Nascimento vivo",  nm[idx])
names(ci_esc) <- nm

ci <- ci_esc
print(names(ci_esc))

curve_names <- setdiff(names(ci), "Tests")

# Contagem de quantos pontos de tempo existem na curva de incidência acumulada 
#para cada combinação de (escolaridade, evento).
cif_df_all <- purrr::map_dfr(curve_names, function(nm) {
  comp <- ci[[nm]]
  
  # nm no formato "Grupo|Evento"
  partes <- strsplit(nm, "\\|")[[1]]
  grupo  <- partes[1]
  evento <- partes[2]
  
  tibble::tibble(
    tempo  = comp$time,
    est    = comp$est,
    grupo  = grupo,
    evento = evento
  )
}) %>%
  dplyr::mutate(
    # Ordem desejada DOS GRUPOS (escolaridade)
    grupo = factor(
      grupo,
      levels = c("Baixa escolaridade",
                 "Média escolaridade",
                 "Alta escolaridade")
    ),
    # Ordem desejada DOS EVENTOS
    evento = factor(
      evento,
      levels = c("Óbito fetal", "Nascimento vivo")
    )
  ) %>%
  dplyr::arrange(grupo, evento, tempo)

# Visualizando um exemplo dentro do objeto para verificar
cif_df_all %>%
  dplyr::filter(grupo == "Baixa escolaridade",
                evento == "Óbito fetal") %>%
  dplyr::select(tempo, est) %>%
  head()

print(table(cif_df_all$grupo, cif_df_all$evento))

p_cif_esc <- ggplot(
  cif_df_all,
  aes(x = tempo,
      y = est,
      colour   = evento,
      linetype = evento)
) +
  geom_step(linewidth = 0.7) +
  facet_wrap(~ grupo, nrow = 1) +  # ordem dos painéis = níveis de 'grupo'
  scale_colour_manual(
    name   = "Evento",
    values = c(
      "Óbito fetal"     = "#d62728",
      "Nascimento vivo" = "#1f77b4"
    )
  ) +
  scale_linetype_manual(
    name   = "Evento",
    values = c(
      "Óbito fetal"     = "solid",
      "Nascimento vivo" = "solid"
    )
  ) +
  labs(
    title = "CIF por escolaridade da mãe",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )

print(p_cif_esc)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("cif_escmae2010.png", units = "in", width = 10, height = 5, res = 300)
print(p_cif_esc)
dev.off()

# CIF apenas para o evento óbito fetal (1) por escolaridade materna

cif_df_fd <- cif_df_all %>%
  dplyr::filter(evento == "Óbito fetal") %>%
  dplyr::arrange(grupo, tempo)

p_cif_fd_esc <- ggplot(
  cif_df_fd,
  aes(x = tempo,
      y = est,
      linetype = grupo)
) +
  geom_step(
    color     = "#d62728",
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Escolaridade materna",
    values = c(
      "Baixa escolaridade" = "solid",
      "Média escolaridade" = "dashed",
      "Alta escolaridade"  = "dotted"
    ),
    breaks = c("Baixa escolaridade",
               "Média escolaridade",
               "Alta escolaridade") 
  ) +
  labs(
    title = "CIF apenas dos óbitos fetais (escolaridade)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top"
  )

print(p_cif_fd_esc)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("cif_fd_escmae2010.png", units = "in", width = 10, height = 5, res = 300)
print(p_cif_fd_esc)
dev.off()

#------------------------------------------------------------------------------
# 4. Presença de filhos vivos previamente (qtdfilvivo_categorico)
#------------------------------------------------------------------------------

#------------------------------- Kaplan–Meier

# Óbito fetal como evento (censura = nascido vivo)
km_fd_filvivo <- survfit(
  Surv(tempo, status_fd) ~ qtdfilvivo_categorico,
  data = dados_eda_clean
)

grafico_kmglobal_filvivo_fd <- ggsurvplot(
  km_fd_filvivo,
  data        = dados_eda_clean,
  conf.int    = TRUE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t) — óbito fetal",
  title       = "KM — Óbito fetal (censura: nascido vivo)\npor presença de filhos vivos previamente",
  legend.title = "Filhos vivos previamente",
  legend.labs  = levels(dados_eda_clean$qtdfilvivo_categorico),
  ylim        = c(0.987, 1.00),   # mesma escala adotada nas demais KM de óbito fetal
  ggtheme     = theme_minimal(base_size = 12)
)
print(grafico_kmglobal_filvivo_fd)

# Nascido vivo como evento (óbito fetal censurado)
km_lv_filvivo <- survfit(
  Surv(tempo, status_lv) ~ qtdfilvivo_categorico,
  data = dados_eda_clean
)

grafico_kmglobal_filvivo_lv <- ggsurvplot(
  km_lv_filvivo,
  data        = dados_eda_clean,
  conf.int    = TRUE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t) — nascido vivo",
  title       = "KM — Nascido vivo (censura: óbito fetal)\npor presença de filhos vivos previamente",
  legend.title = "Filhos vivos previamente",
  legend.labs  = levels(dados_eda_clean$qtdfilvivo_categorico),
  ylim        = c(0, 1),
  ggtheme     = theme_minimal(base_size = 12)
)
print(grafico_kmglobal_filvivo_lv)


# Painel com os dois KM (óbito fetal e nascido vivo)

# Ajuste de margens para aproximar os gráficos no painel
p1_filvivo <- grafico_kmglobal_filvivo_fd$plot +
  theme(plot.margin = unit(c(5.5, 10, 5.5, 5.5), "pt"))

p2_filvivo <- grafico_kmglobal_filvivo_lv$plot +
  theme(plot.margin = unit(c(5.5, 5.5, 5.5, 10), "pt"))

painel_kmglobal_filvivo <- ggarrange(
  p1_filvivo, p2_filvivo,
  ncol          = 2,
  nrow          = 1,
  common.legend = TRUE,
  legend        = "bottom"
)

print(painel_kmglobal_filvivo)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("kmglobal_filvivo.png", units = "in", width = 10, height = 5, res = 300)
print(painel_kmglobal_filvivo)
dev.off()

#------------------------------- Log-Rank

# LOG-RANK — óbito fetal como evento de interesse (censura: nascido vivo)
logrank_fd_filvivo <- survdiff(
  Surv(tempo, status_fd) ~ qtdfilvivo_categorico,
  data = dados_eda_clean
)
print(logrank_fd_filvivo)

p_filvivo_fd <- 1 - pchisq(
  logrank_fd_filvivo$chisq,
  df = length(logrank_fd_filvivo$n) - 1
)
print(paste(
  "p-valor:",
  signif(p_filvivo_fd, 3)
))

# LOG-RANK — nascido vivo como evento de interesse (censura: óbito fetal)
logrank_lv_filvivo <- survdiff(
  Surv(tempo, status_lv) ~ qtdfilvivo_categorico,
  data = dados_eda_clean
)
print(logrank_lv_filvivo)

p_filvivo_lv <- 1 - pchisq(
  logrank_lv_filvivo$chisq,
  df = length(logrank_lv_filvivo$n) - 1
)
print(paste(
  "p-valor:",
  signif(p_filvivo_lv, 3)
))

#------------------------------- Teste de Gray

# Teste de Gray para qtdfilvivo_categorico
ci_filvivo <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus_cr,
  group   = dados_eda_clean$qtdfilvivo_categorico,
  cencode = "Censura"
)
print(ci_filvivo)

# Apenas o teste (sem informações extras)
ci_filvivo$Tests

#------------------------------- CIF

p_cif_filvivo <- ggcompetingrisks(
  fit             = ci_filvivo,
  multiple_panels = TRUE,   # um painel para cada categoria ("Não", "Sim")
  ggtheme         = theme_minimal(base_size = 12),
  palette         = c("#1f77b4", "#d62728"),   # 1 = óbito fetal, 2 = nascido vivo
  legend.title    = "Evento",
  legend.labs     = c("Óbito fetal", "Nascimento vivo"),
  xlab            = "Semanas de gestação",
  ylab            = "Incidência acumulada",
  title           = "Gráfico de Incidência Acumulada (CIF) por presença de filhos vivos previamente"
)
print(p_cif_filvivo)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("cif_filvivo.png", units = "in", width = 10, height = 5, res = 300)
print(p_cif_filvivo)
dev.off()

# CIF apenas para o evento óbito fetal (1) por presença de filhos vivos previamente

# Selecionar apenas as curvas cujo nome termina em "Óbito fetal"
ci_filvivo_evento1 <- ci_filvivo[grep("Óbito fetal$", names(ci_filvivo))]

# Conferir nomes
print(names(ci_filvivo_evento1))

# Converter para data.frame longo: tempo, CIF, grupo ("Não"/"Sim")
df_cif_fd_filvivo <- purrr::map_dfr(
  names(ci_filvivo_evento1),
  function(nm) {
    comp <- ci_filvivo_evento1[[nm]]
    
    # Extrai o rótulo do grupo removendo o sufixo " Óbito fetal"
    grupo <- sub(" Óbito fetal$", "", nm)
    
    data.frame(
      tempo = comp$time,
      cif   = comp$est,
      grupo = grupo,
      stringsAsFactors = FALSE
    )
  }
) %>%
  dplyr::mutate(
    grupo = factor(grupo, levels = c("Não", "Sim"))
  )

# Conferir se níveis ficaram corretos
print(table(df_cif_fd_filvivo$grupo, useNA = "ifany"))

# Construir gráfico CIF apenas dos óbitos fetais
p_cif_fd_filvivo <- ggplot(df_cif_fd_filvivo,
                           aes(x = tempo,
                               y = cif,
                               linetype = grupo)) +
  geom_step(
    color     = "#d62728",  # todas as curvas em vermelho (destacando óbito fetal)
    linewidth = 0.7
  ) +
  scale_linetype_manual(
    name   = "Filhos vivos previamente",
    values = c(
      "Não" = "solid",
      "Sim" = "dashed"
    )
  ) +
  labs(
    title = "CIF apenas para óbitos fetais por presença de filhos vivos previamente",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )

print(p_cif_fd_filvivo)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("cif_fd_filvivo.png", units = "in", width = 10, height = 5, res = 300)
print(p_cif_fd_filvivo)
dev.off()

#------------------------------------------------------------------------------
# 5. Presença de filhos mortos previamente (qtdfilmort_categorico)
#------------------------------------------------------------------------------

#------------------------------- Kaplan–Meier

# Óbito fetal como evento (censura = nascido vivo)
km_fd_filmort <- survfit(
  Surv(tempo, status_fd) ~ qtdfilmort_categorico,
  data = dados_eda_clean
)

grafico_kmglobal_filmort_fd <- ggsurvplot(
  km_fd_filmort,
  data        = dados_eda_clean,
  conf.int    = TRUE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t) — óbito fetal",
  title       = "KM — Óbito fetal (censura: nascido vivo)\npor presença de filhos mortos previamente",
  legend.title = "Filhos mortos previamente",
  legend.labs  = levels(dados_eda_clean$qtdfilmort_categorico),
  ylim        = c(0.975, 1.00),   # mesma escala das demais KM de óbito fetal
  ggtheme     = theme_minimal(base_size = 12)
)
print(grafico_kmglobal_filmort_fd)

# Nascido vivo como evento (óbito fetal censurado)
km_lv_filmort <- survfit(
  Surv(tempo, status_lv) ~ qtdfilmort_categorico,
  data = dados_eda_clean
)

grafico_kmglobal_filmort_lv <- ggsurvplot(
  km_lv_filmort,
  data        = dados_eda_clean,
  conf.int    = TRUE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t) — nascido vivo",
  title       = "KM — Nascido vivo (censura: óbito fetal)\npor presença de filhos mortos previamente",
  legend.title = "Filhos mortos previamente",
  legend.labs  = levels(dados_eda_clean$qtdfilmort_categorico),
  ylim        = c(0, 1),
  ggtheme     = theme_minimal(base_size = 12)
)
print(grafico_kmglobal_filmort_lv)

# Painel com os dois KM (óbito fetal e nascido vivo)

# Ajuste fino de margens para emparelhar os dois gráficos no painel
p1_filmort <- grafico_kmglobal_filmort_fd$plot +
  theme(plot.margin = unit(c(5.5, 10, 5.5, 5.5), "pt"))

p2_filmort <- grafico_kmglobal_filmort_lv$plot +
  theme(plot.margin = unit(c(5.5, 5.5, 5.5, 10), "pt"))

painel_kmglobal_filmort <- ggarrange(
  p1_filmort, p2_filmort,
  ncol          = 2,
  nrow          = 1,
  common.legend = TRUE,
  legend        = "bottom"
)

print(painel_kmglobal_filmort)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("kmglobal_filmort.png", units = "in", width = 10, height = 5, res = 300)
print(painel_kmglobal_filmort)
dev.off()

#------------------------------- Log-Rank

# LOG-RANK — óbito fetal como evento de interesse (censura: nascido vivo)
logrank_fd_filmort <- survdiff(
  Surv(tempo, status_fd) ~ qtdfilmort_categorico,
  data = dados_eda_clean
)
print(logrank_fd_filmort)

p_filmort_fd <- 1 - pchisq(
  logrank_fd_filmort$chisq,
  df = length(logrank_fd_filmort$n) - 1
)
print(paste(
  "p-valor:",
  signif(p_filmort_fd, 3)
))

# LOG-RANK — nascido vivo como evento de interesse (censura: óbito fetal)
logrank_lv_filmort <- survdiff(
  Surv(tempo, status_lv) ~ qtdfilmort_categorico,
  data = dados_eda_clean
)
print(logrank_lv_filmort)

p_filmort_lv <- 1 - pchisq(
  logrank_lv_filmort$chisq,
  df = length(logrank_lv_filmort$n) - 1
)
print(paste(
  "p-valor:",
  signif(p_filmort_lv, 3)
))

#------------------------------- Teste de Gray
# Teste de Gray para qtdfilmort_categorico
ci_filmort <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus_cr,
  group   = dados_eda_clean$qtdfilmort_categorico,
  cencode = "Censura"
)
print(ci_filmort)

# Apenas o teste (sem informações extras)
ci_filmort$Tests

#------------------------------- CIF

p_cif_filmort <- ggcompetingrisks(
  fit             = ci_filmort,
  multiple_panels = TRUE,   # painel separado para "Não" e "Sim"
  ggtheme         = theme_minimal(base_size = 12),
  palette         = c("#1f77b4", "#d62728"),   # 1 = óbito fetal, 2 = nascido vivo
  legend.title    = "Evento",
  legend.labs     = c("Óbito fetal", "Nascimento vivo"),
  xlab            = "Semanas de gestação",
  ylab            = "Incidência acumulada",
  title           = "Gráfico de Incidência Acumulada (CIF) por presença de filhos mortos previamente"
)
print(p_cif_filmort)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("cif_filmort.png", units = "in", width = 10, height = 5, res = 300)
print(p_cif_filmort)
dev.off()

# CIF apenas para o evento óbito fetal (1) por presença de filhos mortos previamente

# Selecionar apenas as curvas cujo nome termina em "Óbito fetal"
ci_filmort_evento1 <- ci_filmort[grep("Óbito fetal$", names(ci_filmort))]

# Conferir nomes
print(names(ci_filmort_evento1))

# Converter em data.frame longo com tempo, CIF e grupo ("Não"/"Sim")
df_cif_fd_filmort <- purrr::map_dfr(
  names(ci_filmort_evento1),
  function(nm) {
    comp <- ci_filmort_evento1[[nm]]
    
    # Extrai o rótulo do grupo removendo o sufixo " Óbito fetal"
    grupo <- sub(" Óbito fetal$", "", nm)
    
    data.frame(
      tempo = comp$time,
      cif   = comp$est,
      grupo = grupo,
      stringsAsFactors = FALSE
    )
  }
) %>%
  dplyr::mutate(
    grupo = factor(grupo, levels = c("Não", "Sim"))
  )

# Conferência rápida
print(table(df_cif_fd_filmort$grupo, useNA = "ifany"))

# Construção do gráfico CIF apenas para óbitos fetais
p_cif_fd_filmort <- ggplot(df_cif_fd_filmort,
                           aes(x = tempo,
                               y = cif,
                               linetype = grupo)) +
  geom_step(
    color     = "#d62728",  # todas as curvas em vermelho para destacar óbito fetal
    linewidth = 0.7
  ) +
  scale_linetype_manual(
    name   = "Filhos mortos previamente",
    values = c(
      "Não" = "solid",
      "Sim" = "dashed"
    )
  ) +
  labs(
    title = "CIF apenas para óbitos fetais por presença de filhos mortos previamente",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )

print(p_cif_fd_filmort)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("cif_fd_filmort.png", units = "in", width = 10, height = 5, res = 300)
print(p_cif_fd_filmort)
dev.off()

#------------------------------------------------------------------------------
# 6. Tipo de gravidez (gravidez: Única vs Múltipla)
#------------------------------------------------------------------------------

#------------------------------- Kaplan–Meier

# Óbito fetal como evento (censura = nascido vivo)
km_fd_grav <- survfit(
  Surv(tempo, status_fd) ~ gravidez,
  data = dados_eda_clean
)

grafico_kmglobal_grav_fd <- ggsurvplot(
  km_fd_grav,
  data        = dados_eda_clean,
  conf.int    = TRUE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t) — óbito fetal",
  title       = "KM — Óbito fetal (censura: nascido vivo)\npor tipo de gravidez",
  legend.title = "Tipo de gravidez",
  legend.labs  = levels(dados_eda_clean$gravidez),
  ylim        = c(0.950, 1.00),
  ggtheme     = theme_minimal(base_size = 12)
)
print(grafico_kmglobal_grav_fd)

# Nascido vivo como evento (óbito fetal censurado)
km_lv_grav <- survfit(
  Surv(tempo, status_lv) ~ gravidez,
  data = dados_eda_clean
)

grafico_kmglobal_grav_lv <- ggsurvplot(
  km_lv_grav,
  data        = dados_eda_clean,
  conf.int    = TRUE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t) — nascido vivo",
  title       = "KM — Nascido vivo (censura: óbito fetal)\npor tipo de gravidez",
  legend.title = "Tipo de gravidez",
  legend.labs  = levels(dados_eda_clean$gravidez),
  ylim        = c(0, 1),
  ggtheme     = theme_minimal(base_size = 12)
)
print(grafico_kmglobal_grav_lv)

# Painel com os dois KM (óbito fetal e nascido vivo)

p1_grav <- grafico_kmglobal_grav_fd$plot +
  theme(plot.margin = unit(c(5.5, 10, 5.5, 5.5), "pt"))

p2_grav <- grafico_kmglobal_grav_lv$plot +
  theme(plot.margin = unit(c(5.5, 5.5, 5.5, 10), "pt"))

painel_kmglobal_grav <- ggarrange(
  p1_grav, p2_grav,
  ncol          = 2,
  nrow          = 1,
  common.legend = TRUE,
  legend        = "bottom"
)

print(painel_kmglobal_grav)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("kmglobal_gravidez.png", units = "in", width = 10, height = 5, res = 300)
print(painel_kmglobal_grav)
dev.off()

#------------------------------- Log-Rank

# LOG-RANK — óbito fetal como evento de interesse (censura: nascido vivo)
logrank_fd_grav <- survdiff(
  Surv(tempo, status_fd) ~ gravidez,
  data = dados_eda_clean
)
print(logrank_fd_grav)

p_grav_fd <- 1 - pchisq(
  logrank_fd_grav$chisq,
  df = length(logrank_fd_grav$n) - 1
)
print(paste(
  "p-valor:",
  signif(p_grav_fd, 3)
))

# LOG-RANK — nascido vivo como evento de interesse (censura: óbito fetal)
logrank_lv_grav <- survdiff(
  Surv(tempo, status_lv) ~ gravidez,
  data = dados_eda_clean
)
print(logrank_lv_grav)

p_grav_lv <- 1 - pchisq(
  logrank_lv_grav$chisq,
  df = length(logrank_lv_grav$n) - 1
)
print(paste(
  "p-valor:",
  signif(p_grav_lv, 3)
))

#------------------------------- Teste de Gray

# Teste de Gray e CIF para tipo de gravidez
ci_grav <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus_cr,
  group   = dados_eda_clean$gravidez,
  cencode = "Censura"
)
print(ci_grav)

# Apenas o teste (sem informações extras)
ci_filvivo$Tests

#------------------------------- CIF

p_cif_grav <- ggcompetingrisks(
  fit             = ci_grav,
  multiple_panels = TRUE,   # um painel para Única e outro para Múltipla
  ggtheme         = theme_minimal(base_size = 12),
  palette         = c("#1f77b4", "#d62728"),   # 1 = Óbito fetal, 2 = Nascido vivo
  legend.title    = "Evento",
  legend.labs     = c("Óbito fetal", "Nascimento vivo"),
  xlab            = "Semanas de gestação",
  ylab            = "Incidência acumulada",
  title           = "CIF por tipo de gravidez"
)
print(p_cif_grav)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("cif_gravidez.png", units = "in", width = 10, height = 5, res = 300)
print(p_cif_grav)
dev.off()

# CIF apenas do evento óbito fetal por tipo de gravidez

# Selecionar apenas as curvas cujo nome termina em "Óbito fetal"
ci_grav_evento1 <- ci_grav[grep("Óbito fetal$", names(ci_grav))]

# Conferir nomes
print(names(ci_grav_evento1))

# Converter em data.frame longo
df_cif_fd_grav <- purrr::map_dfr(
  names(ci_grav_evento1),
  function(nm) {
    comp <- ci_grav_evento1[[nm]]
    
    # Remove o sufixo " Óbito fetal" para obter o nome do grupo
    grupo <- sub(" Óbito fetal$", "", nm)
    
    data.frame(
      tempo = comp$time,
      cif   = comp$est,
      grupo = grupo,
      stringsAsFactors = FALSE
    )
  }
) %>%
  dplyr::mutate(
    grupo = factor(grupo, levels = c("Única", "Múltipla"))
  ) %>%
  dplyr::arrange(grupo, tempo)

print(table(df_cif_fd_grav$grupo, useNA = "ifany"))

# Gráfico: CIF apenas para óbitos fetais por tipo de gravidez
p_cif_fd_grav <- ggplot(
  df_cif_fd_grav,
  aes(x = tempo,
      y = cif,
      linetype = grupo)
) +
  geom_step(
    color     = "#d62728",  # todas as curvas em vermelho
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Tipo de gravidez",
    values = c(
      "Única"    = "solid",
      "Múltipla" = "dashed"
    )
  ) +
  labs(
    title = "CIF apenas dos óbitos fetais por tipo de gravidez",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top"
  )

print(p_cif_fd_grav)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("cif_fd_gravidez.png", units = "in", width = 10, height = 5, res = 300)
print(p_cif_fd_grav)
dev.off()

#------------------------------------------------------------------------------
# 7. Tipo de parto (Vaginal vs Cesáreo)
#------------------------------------------------------------------------------

#------------------------------- Kaplan–Meier

# Óbito fetal como evento (censura = nascido vivo)
km_fd_parto <- survfit(
  Surv(tempo, status_fd) ~ parto,
  data = dados_eda_clean
)

grafico_kmglobal_parto_fd <- ggsurvplot(
  km_fd_parto,
  data        = dados_eda_clean,
  conf.int    = TRUE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t) — óbito fetal",
  title       = "KM — Óbito fetal (censura: nascido vivo)\npor tipo de parto",
  legend.title = "Tipo de parto",
  legend.labs  = levels(dados_eda_clean$parto),
  ylim        = c(0.979, 1.00),
  ggtheme     = theme_minimal(base_size = 12)
)
print(grafico_kmglobal_parto_fd)

# Nascido vivo como evento (óbito fetal censurado)
km_lv_parto <- survfit(
  Surv(tempo, status_lv) ~ parto,
  data = dados_eda_clean
)

grafico_kmglobal_parto_lv <- ggsurvplot(
  km_lv_parto,
  data        = dados_eda_clean,
  conf.int    = TRUE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t) — nascido vivo",
  title       = "KM — Nascido vivo (censura: óbito fetal)\npor tipo de parto",
  legend.title = "Tipo de parto",
  legend.labs  = levels(dados_eda_clean$parto),
  ylim        = c(0, 1),
  ggtheme     = theme_minimal(base_size = 12)
)
print(grafico_kmglobal_parto_lv)

# Painel com os dois KM (óbito fetal e nascido vivo)

p1_parto <- grafico_kmglobal_parto_fd$plot +
  theme(plot.margin = unit(c(5.5, 10, 5.5, 5.5), "pt"))

p2_parto <- grafico_kmglobal_parto_lv$plot +
  theme(plot.margin = unit(c(5.5, 5.5, 5.5, 10), "pt"))

painel_kmglobal_parto <- ggarrange(
  p1_parto, p2_parto,
  ncol          = 2,
  nrow          = 1,
  common.legend = TRUE,
  legend        = "bottom"
)

print(painel_kmglobal_parto)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("kmglobal_parto.png", units = "in", width = 10, height = 5, res = 300)
print(painel_kmglobal_parto)
dev.off()

#------------------------------- Log-Rank

# LOG-RANK — óbito fetal como evento de interesse (censura: nascido vivo)
logrank_fd_parto <- survdiff(
  Surv(tempo, status_fd) ~ parto,
  data = dados_eda_clean
)
print(logrank_fd_parto)

p_parto_fd <- 1 - pchisq(
  logrank_fd_parto$chisq,
  df = length(logrank_fd_parto$n) - 1
)
print(paste(
  "p-valor:",
  signif(p_parto_fd, 3)
))

# LOG-RANK — nascido vivo como evento de interesse (censura: óbito fetal)
logrank_lv_parto <- survdiff(
  Surv(tempo, status_lv) ~ parto,
  data = dados_eda_clean
)
print(logrank_lv_parto)

p_parto_lv <- 1 - pchisq(
  logrank_lv_parto$chisq,
  df = length(logrank_lv_parto$n) - 1
)
print(paste(
  "p-valor:",
  signif(p_parto_lv, 3)
))

#------------------------------- Teste de Gray

# Teste de Gray e CIF para tipo de parto
ci_parto <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus_cr,
  group   = dados_eda_clean$parto,
  cencode = "Censura"
)
print(ci_parto)

# Apenas o teste (sem informações extras)
ci_filmort$Tests

#------------------------------- CIF

p_cif_parto <- ggcompetingrisks(
  fit             = ci_parto,
  multiple_panels = TRUE,   # um painel para Vaginal e outro para Cesáreo
  ggtheme         = theme_minimal(base_size = 12),
  palette         = c("#1f77b4", "#d62728"),   # 1 = Óbito fetal, 2 = Nascido vivo
  legend.title    = "Evento",
  legend.labs     = c("Óbito fetal", "Nascimento vivo"),
  xlab            = "Semanas de gestação",
  ylab            = "Incidência acumulada",
  title           = "CIF por tipo de parto"
)
print(p_cif_parto)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("cif_parto.png", units = "in", width = 10, height = 5, res = 300)
print(p_cif_parto)
dev.off()

# CIF apenas do evento óbito fetal por tipo de parto

# Selecionar apenas as curvas cujo nome termina em "Óbito fetal"
ci_parto_evento1 <- ci_parto[grep("Óbito fetal$", names(ci_parto))]

# Conferir nomes
print(names(ci_parto_evento1))

# Converter em data.frame longo
df_cif_fd_parto <- purrr::map_dfr(
  names(ci_parto_evento1),
  function(nm) {
    comp <- ci_parto_evento1[[nm]]
    
    # Remove o sufixo " Óbito fetal" para obter o nome do grupo
    grupo <- sub(" Óbito fetal$", "", nm)
    
    data.frame(
      tempo = comp$time,
      cif   = comp$est,
      grupo = grupo,
      stringsAsFactors = FALSE
    )
  }
) %>%
  dplyr::mutate(
    grupo = factor(grupo, levels = c("Vaginal", "Cesáreo"))
  ) %>%
  dplyr::arrange(grupo, tempo)

print(table(df_cif_fd_parto$grupo, useNA = "ifany"))

# Gráfico: CIF apenas para óbitos fetais por tipo de parto
p_cif_fd_parto <- ggplot(
  df_cif_fd_parto,
  aes(x = tempo,
      y = cif,
      linetype = grupo)
) +
  geom_step(
    color     = "#d62728",  # todas as curvas em vermelho
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Tipo de parto",
    values = c(
      "Vaginal" = "solid",
      "Cesáreo" = "dashed"
    )
  ) +
  labs(
    title = "CIF apenas dos óbitos fetais por tipo de parto",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top"
  )

print(p_cif_fd_parto)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("cif_fd_parto.png", units = "in", width = 10, height = 5, res = 300)
print(p_cif_fd_parto)
dev.off()

#------------------------------------------------------------------------------
# 8. Faixa de peso ao nascer (peso_categorico)
#------------------------------------------------------------------------------

#------------------------------- Kaplan–Meier

# Óbito fetal como evento (censura = nascido vivo)
km_fd_peso <- survfit(
  Surv(tempo, status_fd) ~ peso_categorico,
  data = dados_eda_clean
)

grafico_kmglobal_peso_fd <- ggsurvplot(
  km_fd_peso,
  data        = dados_eda_clean,
  conf.int    = TRUE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t) — óbito fetal",
  title       = "KM — Óbito fetal (censura: nascido vivo)\npor faixa de peso ao nascer",
  legend.title = "Peso ao nascer",
  legend.labs  = levels(dados_eda_clean$peso_categorico),
  ylim        = c(0.900, 1.00),
  ggtheme     = theme_minimal(base_size = 12)
)
print(grafico_kmglobal_peso_fd)

# Nascido vivo como evento (óbito fetal censurado)
km_lv_peso <- survfit(
  Surv(tempo, status_lv) ~ peso_categorico,
  data = dados_eda_clean
)

grafico_kmglobal_peso_lv <- ggsurvplot(
  km_lv_peso,
  data        = dados_eda_clean,
  conf.int    = TRUE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t) — nascido vivo",
  title       = "KM — Nascido vivo (censura: óbito fetal)\npor faixa de peso ao nascer",
  legend.title = "Peso ao nascer",
  legend.labs  = levels(dados_eda_clean$peso_categorico),
  ylim        = c(0, 1),
  ggtheme     = theme_minimal(base_size = 12)
)
print(grafico_kmglobal_peso_lv)

# Painel com os dois KM (óbito fetal e nascido vivo)

p1_peso <- grafico_kmglobal_peso_fd$plot +
  theme(plot.margin = unit(c(5.5, 10, 5.5, 5.5), "pt"))

p2_peso <- grafico_kmglobal_peso_lv$plot +
  theme(plot.margin = unit(c(5.5, 5.5, 5.5, 10), "pt"))

painel_kmglobal_peso <- ggarrange(
  p1_peso, p2_peso,
  ncol          = 2,
  nrow          = 1,
  common.legend = TRUE,
  legend        = "bottom"
)

print(painel_kmglobal_peso)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("kmglobal_peso_categorico.png", units = "in", width = 10, height = 5, res = 300)
print(painel_kmglobal_peso)
dev.off()

#------------------------------- Log-Rank

# LOG-RANK — óbito fetal como evento de interesse (censura: nascido vivo)
logrank_fd_peso <- survdiff(
  Surv(tempo, status_fd) ~ peso_categorico,
  data = dados_eda_clean
)
print(logrank_fd_peso)

p_peso_fd <- 1 - pchisq(
  logrank_fd_peso$chisq,
  df = length(logrank_fd_peso$n) - 1
)
print(paste(
  "p-valor:",
  signif(p_peso_fd, 3)
))

# LOG-RANK — nascido vivo como evento de interesse (censura: óbito fetal)
logrank_lv_peso <- survdiff(
  Surv(tempo, status_lv) ~ peso_categorico,
  data = dados_eda_clean
)
print(logrank_lv_peso)

p_peso_lv <- 1 - pchisq(
  logrank_lv_peso$chisq,
  df = length(logrank_lv_peso$n) - 1
)
print(paste(
  "p-valor:",
  signif(p_peso_lv, 3)
))

#------------------------------- Teste de Gray

# Teste de Gray e CIF para peso_categorico
ci_peso <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus_cr,
  group   = dados_eda_clean$peso_categorico,
  cencode = "Censura"
)
print(ci_peso)

# Apenas o teste (sem informações extras)
ci_filvivo$Tests

#------------------------------- CIF

p_cif_peso <- ggcompetingrisks(
  fit             = ci_peso,
  multiple_panels = TRUE,   # um painel para cada faixa de peso
  ggtheme         = theme_minimal(base_size = 12),
  palette         = c("#1f77b4", "#d62728"),   # 1 = Óbito fetal, 2 = Nascido vivo
  legend.title    = "Evento",
  legend.labs     = c("Óbito fetal", "Nascimento vivo"),
  xlab            = "Semanas de gestação",
  ylab            = "Incidência acumulada",
  title           = "CIF por faixa de peso ao nascer"
)
print(p_cif_peso)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("cif_peso_categorico.png", units = "in", width = 10, height = 5, res = 300)
print(p_cif_peso)
dev.off()

# CIF apenas do evento óbito fetal por faixa de peso ao nascer (sem IC)

# Selecionar apenas as curvas cujo nome termina em "Óbito fetal"
ci_peso_evento1 <- ci_peso[grep("Óbito fetal$", names(ci_peso))]

# Conferir nomes
print(names(ci_peso_evento1))

# Converter em data.frame longo
df_cif_fd_peso <- purrr::map_dfr(
  names(ci_peso_evento1),
  function(nm) {
    comp <- ci_peso_evento1[[nm]]
    
    # Remove o sufixo " Óbito fetal" para obter o nome da faixa
    grupo <- sub(" Óbito fetal$", "", nm)
    
    data.frame(
      tempo = comp$time,
      cif   = comp$est,
      grupo = grupo,
      stringsAsFactors = FALSE
    )
  }
) %>%
  dplyr::mutate(
    grupo = factor(
      grupo,
      levels = c("<2500g", "2500g-3999g", "4000g+")
    )
  ) %>%
  dplyr::arrange(grupo, tempo)

print(table(df_cif_fd_peso$grupo, useNA = "ifany"))

# Gráfico: CIF apenas para óbitos fetais por faixa de peso ao nascer
p_cif_fd_peso <- ggplot(
  df_cif_fd_peso,
  aes(x = tempo,
      y = cif,
      linetype = grupo)
) +
  geom_step(
    color     = "#d62728",  # todas as curvas em vermelho
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Peso ao nascer",
    values = c(
      "<2500g"       = "solid",
      "2500g-3999g"  = "dashed",
      "4000g+"       = "dotted"
    )
  ) +
  labs(
    title = "CIF apenas dos óbitos fetais\npor faixa de peso ao nascer",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top"
  )

print(p_cif_fd_peso)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("cif_fd_peso_categorico.png", units = "in", width = 10, height = 5, res = 300)
print(p_cif_fd_peso)
dev.off()

#------------------------------------------------------------------------------
# 9. Local de ocorrência do parto (Hospital vs Outros)
#------------------------------------------------------------------------------

#------------------------------- Kaplan–Meier

# Óbito fetal como evento (censura = nascido vivo)
km_fd_loc <- survfit(
  Surv(tempo, status_fd) ~ lococornasc,
  data = dados_eda_clean
)

grafico_kmglobal_loc_fd <- ggsurvplot(
  km_fd_loc,
  data        = dados_eda_clean,
  conf.int    = TRUE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t) — óbito fetal",
  title       = "KM — Óbito fetal (censura: nascido vivo)\npor local de ocorrência do parto",
  legend.title = "Local de ocorrência",
  legend.labs  = levels(dados_eda_clean$lococornasc),
  ylim        = c(0.950, 1.00),
  ggtheme     = theme_minimal(base_size = 12)
)
print(grafico_kmglobal_loc_fd)

# Nascido vivo como evento (óbito fetal censurado)
km_lv_loc <- survfit(
  Surv(tempo, status_lv) ~ lococornasc,
  data = dados_eda_clean
)

grafico_kmglobal_loc_lv <- ggsurvplot(
  km_lv_loc,
  data        = dados_eda_clean,
  conf.int    = TRUE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t) — nascido vivo",
  title       = "KM — Nascido vivo (censura: óbito fetal)\npor local de ocorrência do parto",
  legend.title = "Local de ocorrência",
  legend.labs  = levels(dados_eda_clean$lococornasc),
  ylim        = c(0, 1),
  ggtheme     = theme_minimal(base_size = 12)
)
print(grafico_kmglobal_loc_lv)

# Painel com os dois KM (óbito fetal e nascido vivo)

p1_loc <- grafico_kmglobal_loc_fd$plot +
  theme(plot.margin = unit(c(5.5, 10, 5.5, 5.5), "pt"))

p2_loc <- grafico_kmglobal_loc_lv$plot +
  theme(plot.margin = unit(c(5.5, 5.5, 5.5, 10), "pt"))

painel_kmglobal_loc <- ggarrange(
  p1_loc, p2_loc,
  ncol          = 2,
  nrow          = 1,
  common.legend = TRUE,
  legend        = "bottom"
)

print(painel_kmglobal_loc)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("kmglobal_lococornasc.png", units = "in", width = 10, height = 5, res = 300)
print(painel_kmglobal_loc)
dev.off()

#------------------------------- Log-Rank

# LOG-RANK — óbito fetal como evento de interesse (censura: nascido vivo)
logrank_fd_loc <- survdiff(
  Surv(tempo, status_fd) ~ lococornasc,
  data = dados_eda_clean
)
print(logrank_fd_loc)

p_loc_fd <- 1 - pchisq(
  logrank_fd_loc$chisq,
  df = length(logrank_fd_loc$n) - 1
)
print(paste(
  "p-valor:",
  signif(p_loc_fd, 3)
))

# LOG-RANK — nascido vivo como evento de interesse (censura: óbito fetal)
logrank_lv_loc <- survdiff(
  Surv(tempo, status_lv) ~ lococornasc,
  data = dados_eda_clean
)
print(logrank_lv_loc)

p_loc_lv <- 1 - pchisq(
  logrank_lv_loc$chisq,
  df = length(logrank_lv_loc$n) - 1
)
print(paste(
  "p-valor:",
  signif(p_loc_lv, 3)
))

#------------------------------- Teste de Gray

# Teste de Gray e CIF para local de ocorrência
ci_loc <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus_cr,
  group   = dados_eda_clean$lococornasc,
  cencode = "Censura"
)
print(ci_loc)

# Apenas o teste (sem informações extras)
ci_filmort$Tests

#------------------------------- CIF

p_cif_loc <- ggcompetingrisks(
  fit             = ci_loc,
  multiple_panels = TRUE,   # um painel para Hospital e outro para Outros
  ggtheme         = theme_minimal(base_size = 12),
  palette         = c("#1f77b4", "#d62728"),   # 1 = Óbito fetal, 2 = Nascido vivo
  legend.title    = "Evento",
  legend.labs     = c("Óbito fetal", "Nascimento vivo"),
  xlab            = "Semanas de gestação",
  ylab            = "Incidência acumulada",
  title           = "CIF por local de ocorrência do parto"
)
print(p_cif_loc)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("cif_lococornasc.png", units = "in", width = 10, height = 5, res = 300)
print(p_cif_loc)
dev.off()

# CIF apenas do evento óbito fetal por local de ocorrência (sem IC)

# Selecionar apenas as curvas cujo nome termina em "Óbito fetal"
ci_loc_evento1 <- ci_loc[grep("Óbito fetal$", names(ci_loc))]

# Conferir nomes (opcional)
print(names(ci_loc_evento1))

# Converter em data.frame longo
df_cif_fd_loc <- purrr::map_dfr(
  names(ci_loc_evento1),
  function(nm) {
    comp <- ci_loc_evento1[[nm]]
    
    # Remove o sufixo " Óbito fetal" para obter o nome do grupo
    grupo <- sub(" Óbito fetal$", "", nm)
    
    data.frame(
      tempo = comp$time,
      cif   = comp$est,
      grupo = grupo,
      stringsAsFactors = FALSE
    )
  }
) %>%
  dplyr::mutate(
    grupo = factor(grupo, levels = c("Hospital", "Outros"))
  ) %>%
  dplyr::arrange(grupo, tempo)

print(table(df_cif_fd_loc$grupo, useNA = "ifany"))

# Gráfico: CIF apenas para óbitos fetais por local de ocorrência
p_cif_fd_loc <- ggplot(
  df_cif_fd_loc,
  aes(x = tempo,
      y = cif,
      linetype = grupo)
) +
  geom_step(
    color     = "#d62728",  # todas as curvas em vermelho
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Local de ocorrência",
    values = c(
      "Hospital" = "solid",
      "Outros"   = "dashed"
    )
  ) +
  labs(
    title = "CIF apenas dos óbitos fetais por local de ocorrência do parto",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top"
  )

print(p_cif_fd_loc)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("cif_fd_lococornasc.png", units = "in", width = 10, height = 5, res = 300)
print(p_cif_fd_loc)
dev.off()