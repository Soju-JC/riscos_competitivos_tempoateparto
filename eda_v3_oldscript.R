# 2. Pacotes
# Instala e carrega pacotes 
pkg <- function(x){
  if (!requireNamespace(x, quietly = TRUE)) install.packages(x, dep = TRUE)
  suppressPackageStartupMessages(library(x, character.only = TRUE))
}

lapply(c("tidyverse","data.table","janitor","stringr",
         "forcats","readxl", "survival","survminer",
         "cmprsk","muhaz","Matrix","viridis",
         "naniar","visdat","skimr", "purrr"), pkg)

# Carregamento 
cat("=== ETAPA 1: CARREGAMENTO DOS DADOS ===\n")

# dataset_combined_df gerado em load_data2.Rmd
dados_sp <- readRDS("dataset_combined_df.rds")

dados_simsp <- readRDS("dataset_sim_df.rds")
dados_simsp <- dados_simsp[, "obitoparto"]

dados_sinascsp <- readRDS("dataset_sinasc_df.rds")
dados_sinascsp <- dados_sinascsp[, c("consultas", 
                                     "apgar5", 
                                     "idanomal", 
                                     "racacormae", 
                                     "qtdgestant", 
                                     "qtdpartnor", 
                                     "qtdpartces", 
                                     "mesprenat", 
                                     "sttrabpart", 
                                     "stcesparto", 
                                     "paridade")]

dim(dados_sp)
dplyr::glimpse(dados_sp, width = 80)

# 4. Diagnóstico de faltantes
# Variáveis numéricas
dados_sp_cln <- dados_sp %>%
  mutate(
    semagestac = suppressWarnings(as.numeric(semagestac)),
    idademae = suppressWarnings(as.numeric(idademae)),
    peso = suppressWarnings(as.numeric(peso)),
    qtdfilvivo = suppressWarnings(as.numeric(qtdfilvivo)),
    qtdfilmort = suppressWarnings(as.numeric(qtdfilmort)),
    evento = suppressWarnings(as.integer(evento))
  )

# Variáveis de interesse simultaneo em SIM-DOFET e SINASC
vars_all <- c("sexo",
              "codmunres",
              "idademae",
              "escmae2010",
              "qtdfilvivo",
              "qtdfilmort",
              "gravidez",
              "semagestac",
              "parto",
              "peso",
              "lococornasc",
              "evento",
              "uf_resid")

present <- intersect(vars_all, names(dados_sp))

# Função geral de contagem de NA pós limpeza
miss_summ <- function(df){
  tibble(
    variavel = names(df),
    n_na     = sapply(df, function(x) sum(is.na(x))),
    prop_na  = round(n_na / nrow(df), 4)*100,
    n_unique = sapply(df, function(x) length(unique(x)))
  ) %>% arrange(desc(prop_na))
}

miss_tbl <- miss_summ(dados_sp_cln)

knitr::kable(head(miss_tbl, 20),
             caption = "Variáveis por proporção de faltantes")

naniar::gg_miss_var(dados_sp_cln %>% select(any_of(present))) +
  ggtitle("n de faltantes por variável") +
  theme_minimal(base_size = 12)

# Preparação para facetas (rótulos) 
dados_sp_cln_facets <- dados_sp_cln %>%
  mutate(
    evento_fac   = factor(evento, levels = c(0,1),
                          labels = c("Nascido vivo","Óbito fetal"))
  )

# % faltantes por variável, facetado por evento
p <- naniar::gg_miss_var(
  dados_sp_cln_facets,
  facet   = evento_fac,   # Facetas por categoria do evento
  show_pct = TRUE         # Eixo em 0–100 (percentual)
) +
  labs(
    title = "% de faltantes por variável (facetas = Evento)",
    subtitle = "Comparação direta Nascido vivo vs. Óbito fetal",
    x = NULL, y = "% faltantes"
  ) +
  # Fixa o eixo Y de 0 a 100% em todas as facetas 
  scale_y_continuous(
    limits = c(0, 100),                
    breaks = seq(0, 100, by = 10),     
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.02))  
  ) +
  theme_minimal(base_size = 12)

p

# 5. Recodificações principais e criação de objetos de tempo/evento
# Recodificação essencial para análises seguintes
dados_eda <- dados_sp_cln %>%
  mutate(
    sexo = case_when(sexo %in% c("1","2") ~ sexo, TRUE ~ NA_character_) %>%
      factor(levels = c("1","2"), labels = c("Masculino","Feminino")),
    gravidez = case_when(
      gravidez == "1" ~ "Única",
      gravidez == "2" ~ "Dupla",
      gravidez == "3" ~ "Tripla+",
      TRUE ~ NA_character_) %>% 
      factor(levels = c("Única","Dupla","Tripla+")),
    parto = factor(parto, levels = c("1","2"), labels = c("Vaginal","Cesáreo")),
    lococornasc = case_when(
      lococornasc == "1" ~ "Hospital",
      lococornasc == "2" ~ "Outros est. saúde",
      lococornasc == "3" ~ "Domicílio",
      lococornasc == "4" ~ "Via pública",
      lococornasc == "5" ~ "Outros",
      TRUE ~ NA_character_) %>% 
      factor(levels = c("Hospital","Outros est. saúde","Domicílio",
                        "Via pública","Outros")),
    escmae2010 = case_when(
      as.character(escmae2010) == "0" ~ "Sem escolaridade",
      as.character(escmae2010) == "1" ~ "Primeiro Fundamental",
      as.character(escmae2010) == "2" ~ "Segundo Fundamental",
      as.character(escmae2010) == "3" ~ "Médio",
      as.character(escmae2010) == "4" ~ "Superior incompleto",
      as.character(escmae2010) == "5" ~ "Superior completo",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Sem escolaridade",
                            "Primeiro Fundamental",
                            "Segundo Fundamental",
                            "Médio",
                            "Superior incompleto",
                            "Superior completo"),
                 ordered = TRUE),
    idademae_faixa = case_when(
      idademae < 20 ~ "<20",
      idademae >= 20 &  idademae < 35 ~ "20–34",
      idademae >= 35 ~ "35+",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("<20","20–34","35+")),
    peso_faixa = case_when(
      peso < 2500 ~ "<2500g",
      peso >= 2500 &  peso < 4000 ~ "2500g-3999g",
      peso >= 4000 ~ "4000g+",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("<2500g","2500g-3999g","4000g+")),
    qtdfilvivo_faixa = case_when(
      qtdfilvivo == 0 ~ "Nenhum",
      qtdfilvivo == 1 ~ "Um",
      qtdfilvivo > 1 ~ "Múltiplos",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Nenhum","Um","Múltiplos")),
    qtdfilmort_faixa = case_when(
      qtdfilmort == 0 ~ "Nenhum",
      qtdfilmort == 1 ~ "Um",
      qtdfilmort > 1 ~ "Múltiplos",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Nenhum","Um","Múltiplos")),
    qtdfilvivo_bin = case_when(
      qtdfilvivo == 0 ~ "Não",
      qtdfilvivo != 0 & is.na(qtdfilvivo) == FALSE ~ "Sim",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Não","Sim")),
    qtdfilmort_bin = case_when(
      qtdfilmort == 0 ~ "Não",
      qtdfilmort != 0 & is.na(qtdfilvivo) == FALSE ~ "Sim",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Não","Sim")),
    # Objetos de tempo e eventos para as duas perspectivas
    tempo = semagestac,
    status_fd = ifelse(evento == 1L, 1L, 0L),   # 1 = óbito fetal é causa
    status_lv = ifelse(evento == 0L, 1L, 0L),   # 1 = nascimento vivo é causa
    fstatus   = case_when(evento == 1L ~ 1L,    # 1 = óbito fetal
                          evento == 0L ~ 2L,    # 2 = nascido vivo (competidor)
                          TRUE ~ 0L)            # 0 = censura
  ) 

# Selecionar apenas os casos completos nessas variáveis
dados_eda_clean <- dados_eda[complete.cases(dados_eda[, ]), ]

# Óbitos 
# > dim(dados_teste1)
# [1] 3348   21
# 
# Nascidos vivos
# > dim(dados_teste2)
# [1] 500722     21

# 6 Descrições básicas

## 6.1 Descrição básica
# Tamanho e taxa bruta de eventos
n <- nrow(dados_eda_clean)
taxa_obito <- mean(dados_eda_clean$status_fd)
tab_evento <- table(Evento = ifelse(dados_eda_clean$status_fd==1,"Óbito fetal","Nascido vivo"))

list(n_registros = n, taxa_óbito_fetal = round(taxa_obito, 4))
tab_evento

### Distribuição da idade gestacional
# Distribuição da idade gestacional
p1 <- ggplot(dados_eda_clean, aes(x = tempo)) +
  geom_histogram() + labs(x="Semanas de gestação", y="Frequência",
                          title="Distribuição de idade gestacional (tempo)") +
  theme_minimal(base_size = 12)
p1

### Distribuição da idade materna
# Distribuição da idade materna
p2 <- ggplot(dados_eda_clean, aes(x = idademae)) +
  geom_histogram(bins = 39) + labs(x="Idade materna (anos)", y="Frequência") +
  theme_minimal(base_size = 12)
p2

### Distribuição do peso
# Distribuição do peso
p3 <- ggplot(dados_eda_clean, aes(x = peso)) +
  geom_histogram(bins = 60) + labs(x="Peso ao nascer (g)", y="Frequência") +
  theme_minimal(base_size = 12)
p3

### Distribuição da quantidade de filhos vivos
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

### Distribuição da quantidade de filhos mortos
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

# Base de dados atual
dplyr::glimpse(dados_eda_clean, width = 80)

### Variáveis numéricas de interesse 

# Variáveis quantitativas (numéricas)
# calcula summary
# Variáveis numéricas de interesse 
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

### Variáveis categóricas de interesse 
# Identificar variáveis categóricas (factor ou character)
cat_vars <- c("sexo",
              "escmae2010",
              "gravidez",
              "parto",
              "lococornasc",
              "idademae_faixa",
              "peso_faixa",
              "qtdfilvivo_faixa",
              "qtdfilmort_faixa", 
              "qtdfilmort_bin", 
              "qtdfilvivo_bin")
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

## 6.2 Kaplan–Meier (KM) global sob as duas perspectivas
# Óbito fetal como evento (censura = nascido vivo) 
km_fd <- survfit(Surv(tempo, status_fd) ~ 1, data = dados_eda_clean)
survminer::ggsurvplot(
  km_fd, data = dados_eda_clean, conf.int = TRUE,
  xlab = "Semanas", ylab = "S(t) (óbito fetal como evento)",
  title = "KM – Óbito fetal (nascido vivo censurado)",
  ggtheme = theme_minimal(base_size = 12),
  ylim = c(0.985, 1.00)   # Zoom
)

# Nascido vivo como evento (óbito fetal censurado) 
km_lv <- survfit(Surv(tempo, status_lv) ~ 1, data = dados_eda_clean)
survminer::ggsurvplot(km_lv, data = dados_eda_clean, conf.int = TRUE,
                      xlab = "Semanas", ylab = "S(t) (nascido vivo como evento)",
                      title = "KM – Nascido vivo (óbito fetal censurado)",
                      ggtheme = theme_minimal(base_size = 12))

# 7. KM, CIF, Testes de Log-rank e de Gray para cada caso 

## Sexo

### KM

# Óbito fetal como evento (censura = nascido vivo) 
km_fd_sexo <- survfit(Surv(tempo, status_fd) ~ sexo, data = dados_eda_clean)
survminer::ggsurvplot(
  km_fd_sexo, data = dados_eda_clean, conf.int = TRUE,
  xlab = "Semanas", ylab = "S(t) — óbito fetal",
  title = "KM — Óbito fetal (censura: nascido vivo) por sexo",
  ggtheme = theme_minimal(base_size = 12),
  ylim = c(0.987, 1.00)  
)

# Nascido vivo como evento (óbito fetal censurado) 
km_lv_sexo <- survfit(Surv(tempo, status_lv) ~ sexo, data = dados_eda_clean)
survminer::ggsurvplot(
  km_lv_sexo, data = dados_eda_clean, conf.int = TRUE,
  xlab = "Semanas", ylab = "S(t) — nascido vivo",
  title = "KM — Nascido vivo (censura: óbito fetal) por sexo",
  ggtheme = theme_minimal(base_size = 12)
)

### Log-Rank
# Log-rank para óbito fetal como evento de interesse (censura nascidos vivos)
logrank_fd_sexo <- survdiff(Surv(tempo, status_fd) ~ sexo, data = dados_eda_clean)
print(logrank_fd_sexo)

# Log-rank para nascidos vivos como evento de interesse (censura óbito fetal)
logrank_fd_sexo <- survdiff(Surv(tempo, status_lv) ~ sexo, data = dados_eda_clean)
print(logrank_fd_sexo)

### Teste de Gray e Incidência acumulada (CIF) 
# Teste de Gray da variável sexo
ci_sexo <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus,
  group   = dados_eda_clean$sexo,
  cencode = 0
)

print(ci_sexo)

# CIF da variável sexo (1 = óbito fetal, 2 = nascido vivo)
p_cif_sexo <- ggcompetingrisks(
  fit            = ci_sexo,
  multiple_panels = TRUE,   # Plota ou não cada grupo em painéis separados
  ggtheme         = theme_minimal(base_size = 12),
  palette         = c("#d62728", "#1f77b4"),
  legend.title    = "Evento",
  legend.labs     = c("Óbito fetal", "Nascimento vivo"),
  xlab            = "Semanas de gestação",
  ylab            = "Incidência acumulada",
  title           = "Gráfico de Incidência Acumulada (CIF)"
)

print(p_cif_sexo)

# CIF da variável sexo (olhando mais de perto o evento de óbitos)

# Selecionar apenas as curvas do evento 1 (óbito fetal)
ci_sexo_evento1 <- ci_sexo[c("Masculino 1", "Feminino 1")]

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
    color = "none",
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

## Faixa etária da mãe

### KM

# Definir rótulos desejados (na ordem dos níveis)
rotulos_idade <- c("<20", "20–34", "≥35")

# Óbito fetal como evento
km_fd_idademae_faixa <- survfit(Surv(tempo, status_fd) ~ idademae_faixa, data = dados_eda_clean)
survminer::ggsurvplot(
  km_fd_idademae_faixa, 
  data         = dados_eda_clean, 
  conf.int    = TRUE,
  xlab         = "Semanas",
  ylab         = "S(t) — óbito fetal",
  title        = "KM — Óbito fetal (censura: nascido vivo) por faixa etária",
  legend.title = "Faixa etária",
  legend.labs  = rotulos_idade,
  ggtheme      = theme_minimal(base_size = 12),
  ylim         = c(0.987, 1.00)
)

# Nascido vivo como evento
km_lv_idademae_faixa <- survfit(Surv(tempo, status_lv) ~ idademae_faixa, data = dados_eda_clean)
survminer::ggsurvplot(
  km_lv_idademae_faixa, data = dados_eda_clean, conf.int = TRUE,
  xlab         = "Semanas",
  ylab         = "S(t) — nascido vivo",
  title        = "KM — Nascido vivo (censura: óbito fetal) por faixa etária",
  legend.title = "Faixa etária",
  legend.labs  = rotulos_idade,
  ggtheme      = theme_minimal(base_size = 12)
)

### Log-Rank

# Log-rank para óbito fetal como evento de interesse (censura nascidos vivos)
logrank_fd_idademae_faixa <- survdiff(Surv(tempo, status_fd) ~ idademae_faixa, data = dados_eda_clean)
print(logrank_fd_idademae_faixa)

# Log-rank para nascidos vivos como evento de interesse (censura óbito fetal)
logrank_fd_idademae_faixa <- survdiff(Surv(tempo, status_lv) ~ idademae_faixa, data = dados_eda_clean)
print(logrank_fd_idademae_faixa)

### Teste de Gray e Incidência acumulada (CIF) 

# Teste de Gray da variável idademae_faixa
ci_idademae_faixa <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus,
  group   = dados_eda_clean$idademae_faixa,
  cencode = 0
)

print(ci_idademae_faixa)

# CIF da variável sexo (1 = óbito fetal, 2 = nascido vivo)
p_cif_idademae_faixa <- ggcompetingrisks(
  fit            = ci_idademae_faixa,
  multiple_panels = TRUE, # Plota ou não cada grupo em painéis separados
  ggtheme         = theme_minimal(base_size = 12),
  legend.title    = "Evento",
  legend.labs     = c("Óbito fetal", "Nascimento vivo"),
  xlab            = "Semanas de gestação",
  ylab            = "Incidência acumulada",
  title           = "Gráfico de Incidência Acumulada (CIF)"
)

print(p_cif_idademae_faixa)

# CIF da variável idademae_faixa (olhando mais de perto o evento de óbitos)

ci_idade_e1 <- ci_idademae_faixa[c("<20 1", "20–34 1", "35+ 1")]
group_names <- sub(" 1$", "", names(ci_idade_e1)[names(ci_idade_e1) != "Tests"])

p_cif_fd <- ggcompetingrisks(
  fit             = ci_idade_e1,
  multiple_panels = FALSE,
  ggtheme         = theme_minimal(base_size = 12)
) +
  # Especifica mapeamento nomeado para cada nível e usa breaks na ordem correta
  scale_linetype_manual(
    name   = "Faixa etária",
    values = c("<20"   = "solid",
               "20–34" = "dotted",
               "35+"   = "dashed"),
    labels = c("<20", "20–34", "35+"),
    breaks = c("<20", "20–34", "35+")
  ) +
  guides(color = "none",
         linetype = guide_legend(
           override.aes = list(colour = rep("#d62728", length(group_names)))
         )) +
  labs(
    title = "CIF apenas dos óbitos (faixa etária)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  )

print(p_cif_fd)

## Escolaridade da mãe

### KM

# Óbito fetal como evento (censura = nascido vivo)
km_fd_esc10 <- survfit(Surv(tempo, status_fd) ~ escmae2010, data = dados_eda_clean)

# Rótulos na ordem exata das curvas
labs_fd <- sub("^.*=", "", names(km_fd_esc10$strata))

survminer::ggsurvplot(
  km_fd_esc10, data = dados_eda_clean, conf.int = TRUE,
  conf.int.style = "step", linetype = 1, size = 0.7,
  xlab = "Semanas", ylab = "S(t) — óbito fetal",
  title = "KM — Óbito fetal (censura: nascido vivo) por escolaridade",
  ggtheme = theme_minimal(base_size = 12),
  ylim = c(0.95, 1.00),
  legend.title = NULL,     # sem título
  legend.labs  = labs_fd   # apenas o nome da categoria
)

# Nascido vivo como evento (óbito fetal censurado)
km_lv_esc10 <- survfit(Surv(tempo, status_lv) ~ escmae2010, data = dados_eda_clean)

labs_lv <- sub("^.*=", "", names(km_lv_esc10$strata))

survminer::ggsurvplot(
  km_lv_esc10, data = dados_eda_clean, conf.int = TRUE,
  conf.int.style = "step", linetype = 1, size = 0.7,
  xlab = "Semanas", ylab = "S(t) — nascido vivo",
  title = "KM — Nascido vivo (censura: óbito fetal) por escolaridade",
  ggtheme = theme_minimal(base_size = 12),
  legend.title = NULL,
  legend.labs  = labs_lv
)

### Log-Rank

# Log-rank para óbito fetal como evento de interesse (censura nascidos vivos)
logrank_fd_escmae2010 <- survdiff(Surv(tempo, status_fd) ~ escmae2010, data = dados_eda_clean)
print(logrank_fd_escmae2010)

# Log-rank para nascidos vivos como evento de interesse (censura óbito fetal)
logrank_fd_escmae2010 <- survdiff(Surv(tempo, status_lv) ~ escmae2010, data = dados_eda_clean)
print(logrank_fd_escmae2010)

### Teste de Gray e Incidência acumulada (CIF) 

# Teste de Gray e CIF para a escolaridade da mãe
ci_escmae2010 <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus,
  group   = dados_eda_clean$escmae2010,
  cencode = 0
)

# Visualização dos resultados do teste de Gray
print(ci_escmae2010)

# Ajustar os nomes para separar grupo e evento com um símbolo único (|)
names(ci_escmae2010) <- sub(" ([12])$", "|\\1", names(ci_escmae2010))

p_cif_esc <- ggcompetingrisks(
  fit             = ci_escmae2010,
  gsep            = "\\|",                 
  multiple_panels = TRUE,
  ggtheme         = theme_minimal(base_size = 12),
  conf.int        = FALSE
) +
  labs(
    title  = "CIF por escolaridade da mãe",
    x      = "Semanas de gestação",
    y      = "Incidência acumulada",
    color  = "Evento",                     
    linetype = "Evento"
  ) +
  scale_color_manual(
    values = c("1" = "#d62728", "2" = "#1f77b4"),
    labels = c("1" = "Óbito fetal", "2" = "Nascimento vivo")
  ) +
  scale_linetype_manual(
    values = c("1" = "solid", "2" = "dashed"),
    labels = c("1" = "Óbito fetal", "2" = "Nascimento vivo")
  )

print(p_cif_esc)

ci <- ci_escmae2010

# Curvas do cuminc em formato tidy
curve_names <- setdiff(names(ci), "Tests")

cif_df <- map_dfr(curve_names, function(nm){
  tibble(
    tempo = ci[[nm]]$time,
    est   = ci[[nm]]$est,
    var   = ci[[nm]]$var,
    nome  = nm
  )
}) %>%
  # Separa "grupo" (escolaridade) e "evento" a partir do nome "Grupo|Evento"
  mutate(
    grupo  = sub("\\|[12]$", "", nome),
    evento = sub("^.*\\|([12])$", "\\1", nome)
  ) %>%
  # Mantém somente o evento 1 (óbito fetal)
  filter(evento == "1") %>%
  # Garante a ordem dos níveis igual ao seu factor original
  mutate(
    grupo = factor(grupo, levels = levels(dados_eda_clean$escmae2010)),
    lo = pmax(0, est - 1.96*sqrt(var)),  # IC inferior
    hi = pmin(1, est + 1.96*sqrt(var))   # IC superior
  ) %>%
  arrange(grupo, tempo)

# Figura única com 6 curvas (uma por escolaridade), legenda por escolaridade
p_cif_fd_esc_tidy <-
  ggplot(cif_df, aes(x = tempo, y = est, colour = grupo)) +
  geom_step(size = 0.9) +
  # Bandas de confiança
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = grupo), alpha = 0.08, colour = NA, show.legend = FALSE) +
  labs(
    title  = "CIF apenas dos óbitos (escolaridade)",
    x      = "Semanas de gestação",
    y      = "Incidência acumulada",
    colour = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p_cif_fd_esc_tidy)

## Quantidade de filhos vivos

### KM

# Definir rótulos desejados (na ordem dos níveis)
rotulos_qtdfilvivo_faixa <- c("Nenhum", "Um", "Múltiplos")

# Óbito fetal como evento
km_fd_qtdfilvivo_faixa <- survfit(Surv(tempo, status_fd) ~ qtdfilvivo_faixa, data = dados_eda_clean)
survminer::ggsurvplot(
  km_fd_qtdfilvivo_faixa, 
  data         = dados_eda_clean, 
  conf.int    = TRUE,
  xlab         = "Semanas",
  ylab         = "S(t) — óbito fetal",
  title        = "KM — Óbito fetal (por quantidade de filhos vivos)",
  legend.title = "",
  legend.labs  = rotulos_qtdfilvivo_faixa,
  ggtheme      = theme_minimal(base_size = 12),
  ylim         = c(0.985, 1.00)
)

# Nascido vivo como evento
km_lv_qtdfilvivo_faixa <- survfit(Surv(tempo, status_lv) ~ qtdfilvivo_faixa, data = dados_eda_clean)
survminer::ggsurvplot(
  km_lv_qtdfilvivo_faixa, data = dados_eda_clean, conf.int = TRUE,
  xlab         = "Semanas",
  ylab         = "S(t) — nascido vivo",
  title        = "KM — Nascido vivo (por quantidade de filhos vivos)",
  legend.title = "",
  legend.labs  = rotulos_qtdfilvivo_faixa,
  ggtheme      = theme_minimal(base_size = 12)
)

### Log-Rank

# Log-rank para óbito fetal como evento de interesse (censura nascidos vivos)
logrank_fd_qtdfilvivo_faixa <- survdiff(Surv(tempo, status_fd) ~ qtdfilvivo_faixa, data = dados_eda_clean)
print(logrank_fd_qtdfilvivo_faixa)

# Log-rank para nascidos vivos como evento de interesse (censura óbito fetal)
logrank_fd_qtdfilvivo_faixa <- survdiff(Surv(tempo, status_lv) ~ qtdfilvivo_faixa, data = dados_eda_clean)
print(logrank_fd_qtdfilvivo_faixa)

### Teste de Gray e Incidência acumulada (CIF) 

# Teste de Gray da variável qtdfilvivo_faixa
ci_qtdfilvivo_faixa <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus,
  group   = dados_eda_clean$qtdfilvivo_faixa,
  cencode = 0
)

print(ci_qtdfilvivo_faixa)

# CIF da variável qtdfilvivo_faixa (1 = óbito fetal, 2 = nascido vivo)
p_cif_qtdfilvivo_faixa <- ggcompetingrisks(
  fit            = ci_qtdfilvivo_faixa,
  multiple_panels = TRUE, # Plota ou não cada grupo em painéis separados
  ggtheme         = theme_minimal(base_size = 12),
  legend.title    = "Evento",
  legend.labs     = c("Óbito fetal", "Nascimento vivo"),
  xlab            = "Semanas de gestação",
  ylab            = "Incidência acumulada",
  title           = "Gráfico de Incidência Acumulada (CIF)"
)

print(p_cif_qtdfilvivo_faixa)

# CIF da variável qtdfilvivo_faixa (olhando mais de perto o evento de óbitos)

ci_idade_e1 <- ci_qtdfilvivo_faixa[c("Nenhum 1", "Um 1", "Múltiplos 1")]
group_names <- sub(" 1$", "", names(ci_idade_e1)[names(ci_idade_e1) != "Tests"])

p_cif_fd <- ggcompetingrisks(
  fit             = ci_idade_e1,
  multiple_panels = FALSE,
  ggtheme         = theme_minimal(base_size = 12)
) +
  # Especifica mapeamento nomeado para cada nível e usa breaks na ordem correta
  scale_linetype_manual(
    name   = "Filhos vivos",
    values = c("Nenhum"   = "solid",
               "Um" = "dotted",
               "Múltiplos"   = "dashed"),
    labels = c("Nenhum", "Um", "Múltiplos"),
    breaks = c("Nenhum", "Um", "Múltiplos")
  ) +
  guides(color = "none",
         linetype = guide_legend(
           override.aes = list(colour = rep("#d62728", length(group_names)))
         )) +
  labs(
    title = "CIF apenas dos óbitos (quantidade de filhos vivos)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  )

print(p_cif_fd)

## Quantidade de filhos vivos (binário)

### KM

# Óbito fetal como evento (censura = nascido vivo) 
km_fd_qtdfilvivo_bin <- survfit(Surv(tempo, status_fd) ~ qtdfilvivo_bin, data = dados_eda_clean)
survminer::ggsurvplot(
  km_fd_qtdfilvivo_bin, data = dados_eda_clean, conf.int = TRUE,
  xlab = "Semanas", ylab = "S(t) — óbito fetal",
  title = "KM — Óbito fetal (tem filho vivo?)",
  ggtheme = theme_minimal(base_size = 12),
  ylim = c(0.987, 1.00)  
)

# Nascido vivo como evento (óbito fetal censurado) 
km_lv_qtdfilvivo_bin <- survfit(Surv(tempo, status_lv) ~ qtdfilvivo_bin, data = dados_eda_clean)
survminer::ggsurvplot(
  km_lv_qtdfilvivo_bin, data = dados_eda_clean, conf.int = TRUE,
  xlab = "Semanas", ylab = "S(t) — nascido vivo",
  title = "KM — Nascido vivo (tem filho vivo?)",
  ggtheme = theme_minimal(base_size = 12)
)

### Log-Rank

# Log-rank para óbito fetal como evento de interesse (censura nascidos vivos)
logrank_fd_qtdfilvivo_bin <- survdiff(Surv(tempo, status_fd) ~ qtdfilvivo_bin, data = dados_eda_clean)
print(logrank_fd_qtdfilvivo_bin)

# Log-rank para nascidos vivos como evento de interesse (censura óbito fetal)
logrank_fd_qtdfilvivo_bin <- survdiff(Surv(tempo, status_lv) ~ qtdfilvivo_bin, data = dados_eda_clean)
print(logrank_fd_qtdfilvivo_bin)

### Teste de Gray e Incidência acumulada (CIF) 

# Teste de Gray da variável qtdfilvivo_bin
ci_qtdfilvivo_bin <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus,
  group   = dados_eda_clean$qtdfilvivo_bin,
  cencode = 0
)

print(ci_qtdfilvivo_bin)

# CIF da variável qtdfilvivo_bin (1 = óbito fetal, 2 = nascido vivo)
p_cif_qtdfilvivo_bin <- ggcompetingrisks(
  fit            = ci_qtdfilvivo_bin,
  multiple_panels = TRUE,   # Plota ou não cada grupo em painéis separados
  ggtheme         = theme_minimal(base_size = 12),
  palette         = c("#d62728", "#1f77b4"),
  legend.title    = "Evento",
  legend.labs     = c("Óbito fetal", "Nascimento vivo"),
  xlab            = "Semanas de gestação",
  ylab            = "Incidência acumulada",
  title           = "Gráfico de Incidência Acumulada (CIF). Tem filho vivo?"
)

print(p_cif_qtdfilvivo_bin)

# CIF da variável qtdfilvivo_bin (olhando mais de perto o evento de óbitos)

# Selecionar apenas as curvas do evento 1 (óbito fetal)
ci_qtdfilvivo_bin_evento1 <- ci_qtdfilvivo_bin[c("Não 1", "Sim 1")]

# Extrair os nomes sem o código do evento
group_names_qtdfilvivo_bin <- sub(" 1$", "", names(ci_qtdfilvivo_bin_evento1)[names(ci_qtdfilvivo_bin_evento1) != "Tests"])

# Construir o gráfico
p_cif_fd_qtdfilvivo_bin <- ggcompetingrisks(
  fit             = ci_qtdfilvivo_bin_evento1,
  multiple_panels = FALSE,
  ggtheme         = theme_minimal(base_size = 12)
) +
  # Mapea cada qtdfilvivo_bin a um tipo de linha de forma explícita
  scale_linetype_manual(
    name   = "Tem filho vivo?",
    values = c("Sim" = "solid",
               "Não" = "dashed"),
    labels = c("Sim", "Não"),
    breaks = c("Sim", "Não")
  ) +
  # Esconde a legenda de cor e pinta as linhas da legenda de vermelho
  guides(
    color = "none",
    linetype = guide_legend(
      override.aes = list(colour = rep("#d62728", length(group_names_qtdfilvivo_bin)))
    )
  ) +
  labs(
    title = "CIF apenas dos óbitos (se tem filho vivo ou não)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  )

print(p_cif_fd_qtdfilvivo_bin)

## Quantidade de filhos mortos

### KM

# Definir rótulos desejados (na ordem dos níveis)
rotulos_qtdfilmort_faixa <- c("Nenhum", "Um", "Múltiplos")

# Óbito fetal como evento
km_fd_qtdfilmort_faixa <- survfit(Surv(tempo, status_fd) ~ qtdfilmort_faixa, data = dados_eda_clean)
survminer::ggsurvplot(
  km_fd_qtdfilmort_faixa, 
  data         = dados_eda_clean, 
  conf.int    = TRUE,
  xlab         = "Semanas",
  ylab         = "S(t) — óbito fetal",
  title        = "KM — Óbito fetal (por quantidade de filhos mortos)",
  legend.title = "",
  legend.labs  = rotulos_qtdfilmort_faixa,
  ggtheme      = theme_minimal(base_size = 12),
  ylim         = c(0.97, 1.00)
)

# Nascido vivo como evento
km_lv_qtdfilmort_faixa <- survfit(Surv(tempo, status_lv) ~ qtdfilmort_faixa, data = dados_eda_clean)
survminer::ggsurvplot(
  km_lv_qtdfilmort_faixa, data = dados_eda_clean, conf.int = TRUE,
  xlab         = "Semanas",
  ylab         = "S(t) — nascido vivo",
  title        = "KM — Nascido vivo (por quantidade de filhos mortos)",
  legend.title = "",
  legend.labs  = rotulos_qtdfilmort_faixa,
  ggtheme      = theme_minimal(base_size = 12)
)

### Log-Rank

# Log-rank para óbito fetal como evento de interesse (censura nascidos vivos)
logrank_fd_qtdfilmort_faixa <- survdiff(Surv(tempo, status_fd) ~ qtdfilmort_faixa, data = dados_eda_clean)
print(logrank_fd_qtdfilmort_faixa)

# Log-rank para nascidos vivos como evento de interesse (censura óbito fetal)
logrank_fd_qtdfilmort_faixa <- survdiff(Surv(tempo, status_lv) ~ qtdfilmort_faixa, data = dados_eda_clean)
print(logrank_fd_qtdfilmort_faixa)

### Teste de Gray e Incidência acumulada (CIF) 

# Teste de Gray da variável qtdfilmort_faixa
ci_qtdfilmort_faixa <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus,
  group   = dados_eda_clean$qtdfilmort_faixa,
  cencode = 0
)

print(ci_qtdfilmort_faixa)

# CIF da variável qtdfilvivo_faixa (1 = óbito fetal, 2 = nascido vivo)
p_cif_qtdfilmort_faixa <- ggcompetingrisks(
  fit            = ci_qtdfilmort_faixa,
  multiple_panels = TRUE, # Plota ou não cada grupo em painéis separados
  ggtheme         = theme_minimal(base_size = 12),
  legend.title    = "Evento",
  legend.labs     = c("Óbito fetal", "Nascimento vivo"),
  xlab            = "Semanas de gestação",
  ylab            = "Incidência acumulada",
  title           = "Gráfico de Incidência Acumulada (CIF)"
)

print(p_cif_qtdfilmort_faixa)

# CIF da variável qtdfilmort_faixa (olhando mais de perto o evento de óbitos)

ci_idade_e1 <- ci_qtdfilmort_faixa[c("Nenhum 1", "Um 1", "Múltiplos 1")]
group_names <- sub(" 1$", "", names(ci_idade_e1)[names(ci_idade_e1) != "Tests"])

p_cif_fd <- ggcompetingrisks(
  fit             = ci_idade_e1,
  multiple_panels = FALSE,
  ggtheme         = theme_minimal(base_size = 12)
) +
  # Especifica mapeamento nomeado para cada nível e usa breaks na ordem correta
  scale_linetype_manual(
    name   = "Filhos mortos",
    values = c("Nenhum"   = "solid",
               "Um" = "dotted",
               "Múltiplos"   = "dashed"),
    labels = c("Nenhum", "Um", "Múltiplos"),
    breaks = c("Nenhum", "Um", "Múltiplos")
  ) +
  guides(color = "none",
         linetype = guide_legend(
           override.aes = list(colour = rep("#d62728", length(group_names)))
         )) +
  labs(
    title = "CIF apenas dos óbitos (quantidade de filhos mortos)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  )

print(p_cif_fd)

## Quantidade de filhos mortos (binário)

### KM

# Óbito fetal como evento (censura = nascido vivo) 
km_fd_qtdfilmort_bin <- survfit(Surv(tempo, status_fd) ~ qtdfilmort_bin, data = dados_eda_clean)
survminer::ggsurvplot(
  km_fd_qtdfilmort_bin, data = dados_eda_clean, conf.int = TRUE,
  xlab = "Semanas", ylab = "S(t) — óbito fetal",
  title = "KM — Óbito fetal (tem filho morto?)",
  ggtheme = theme_minimal(base_size = 12),
  ylim = c(0.987, 1.00)  
)

# Nascido vivo como evento (óbito fetal censurado) 
km_lv_qtdfilmort_bin <- survfit(Surv(tempo, status_lv) ~ qtdfilmort_bin, data = dados_eda_clean)
survminer::ggsurvplot(
  km_lv_qtdfilmort_bin, data = dados_eda_clean, conf.int = TRUE,
  xlab = "Semanas", ylab = "S(t) — nascido vivo",
  title = "KM — Nascido vivo (tem filho morto?)",
  ggtheme = theme_minimal(base_size = 12)
)

### Log-Rank

# Log-rank para óbito fetal como evento de interesse (censura nascidos vivos)
logrank_fd_qtdfilmort_bin <- survdiff(Surv(tempo, status_fd) ~ qtdfilmort_bin, data = dados_eda_clean)
print(logrank_fd_qtdfilmort_bin)

# Log-rank para nascidos vivos como evento de interesse (censura óbito fetal)
logrank_fd_qtdfilmort_bin <- survdiff(Surv(tempo, status_lv) ~ qtdfilmort_bin, data = dados_eda_clean)
print(logrank_fd_qtdfilmort_bin)

### Teste de Gray e Incidência acumulada (CIF) 

# Teste de Gray da variável qtdfilmort_bin
ci_qtdfilmort_bin <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus,
  group   = dados_eda_clean$qtdfilmort_bin,
  cencode = 0
)

print(ci_qtdfilmort_bin)

# CIF da variável qtdfilmort_bin (1 = óbito fetal, 2 = nascido vivo)
p_cif_qtdfilmort_bin <- ggcompetingrisks(
  fit            = ci_qtdfilmort_bin,
  multiple_panels = TRUE,   # Plota ou não cada grupo em painéis separados
  ggtheme         = theme_minimal(base_size = 12),
  palette         = c("#d62728", "#1f77b4"),
  legend.title    = "Evento",
  legend.labs     = c("Óbito fetal", "Nascimento vivo"),
  xlab            = "Semanas de gestação",
  ylab            = "Incidência acumulada",
  title           = "Gráfico de Incidência Acumulada (CIF). Tem filho morto?"
)

print(p_cif_qtdfilmort_bin)

# CIF da variável qtdfilmort_bin (olhando mais de perto o evento de óbitos)

# Selecionar apenas as curvas do evento 1 (óbito fetal)
ci_qtdfilmort_bin_evento1 <- ci_qtdfilmort_bin[c("Não 1", "Sim 1")]

# Extrair os nomes sem o código do evento
group_names_qtdfilmort_bin <- sub(" 1$", "", names(ci_qtdfilmort_bin_evento1)[names(ci_qtdfilmort_bin_evento1) != "Tests"])

# Construir o gráfico
p_cif_fd_qtdfilmort_bin <- ggcompetingrisks(
  fit             = ci_qtdfilmort_bin_evento1,
  multiple_panels = FALSE,
  ggtheme         = theme_minimal(base_size = 12)
) +
  # Mapea cada qtdfilmort_bin a um tipo de linha de forma explícita
  scale_linetype_manual(
    name   = "Tem filho morto?",
    values = c("Sim" = "solid",
               "Não" = "dashed"),
    labels = c("Sim", "Não"),
    breaks = c("Sim", "Não")
  ) +
  # Esconde a legenda de cor e pinta as linhas da legenda de vermelho
  guides(
    color = "none",
    linetype = guide_legend(
      override.aes = list(colour = rep("#d62728", length(group_names_qtdfilmort_bin)))
    )
  ) +
  labs(
    title = "CIF apenas dos óbitos (se tem filho morto ou não)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  )

print(p_cif_fd_qtdfilmort_bin)

## Tipo de gravidez

### KM

# Definir rótulos desejados (na ordem dos níveis)
rotulos_gravidez <- c("Única", "Dupla", "Tripla+")

# Óbito fetal como evento
km_fd_gravidez <- survfit(Surv(tempo, status_fd) ~ gravidez, data = dados_eda_clean)
survminer::ggsurvplot(
  km_fd_gravidez, 
  data         = dados_eda_clean, 
  conf.int    = TRUE,
  xlab         = "Semanas",
  ylab         = "S(t) — óbito fetal",
  title        = "KM — Óbito fetal (por tipo de gravidez)",
  legend.title = "",
  legend.labs  = rotulos_gravidez,
  ggtheme      = theme_minimal(base_size = 12),
  ylim         = c(0.9, 1.00)
)

# Nascido vivo como evento
km_lv_gravidez <- survfit(Surv(tempo, status_lv) ~ gravidez, data = dados_eda_clean)
survminer::ggsurvplot(
  km_lv_gravidez, data = dados_eda_clean, conf.int = TRUE,
  xlab         = "Semanas",
  ylab         = "S(t) — nascido vivo",
  title        = "KM — Nascido vivo (por tipo de gravidez)",
  legend.title = "",
  legend.labs  = rotulos_gravidez,
  ggtheme      = theme_minimal(base_size = 12)
)

### Log-Rank

# Log-rank para óbito fetal como evento de interesse (censura nascidos vivos)
logrank_fd_gravidez <- survdiff(Surv(tempo, status_fd) ~ gravidez, data = dados_eda_clean)
print(logrank_fd_gravidez)

# Log-rank para nascidos vivos como evento de interesse (censura óbito fetal)
logrank_fd_gravidez <- survdiff(Surv(tempo, status_lv) ~ gravidez, data = dados_eda_clean)
print(logrank_fd_gravidez)

### Teste de Gray e Incidência acumulada (CIF) 

# Teste de Gray da variável gravidez
ci_gravidez <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus,
  group   = dados_eda_clean$gravidez,
  cencode = 0
)

print(ci_gravidez)

# CIF da variável gravidez (1 = óbito fetal, 2 = nascido vivo)
p_cif_gravidez <- ggcompetingrisks(
  fit            = ci_gravidez,
  multiple_panels = TRUE, # Plota ou não cada grupo em painéis separados
  ggtheme         = theme_minimal(base_size = 12),
  legend.title    = "Evento",
  legend.labs     = c("Óbito fetal", "Nascimento vivo"),
  xlab            = "Semanas de gestação",
  ylab            = "Incidência acumulada",
  title           = "Gráfico de Incidência Acumulada (CIF)"
)

print(p_cif_gravidez)

# CIF da variável gravidez (olhando mais de perto o evento de óbitos)

ci_idade_e1 <- ci_gravidez[c("Única 1", "Dupla 1", "Tripla+ 1")]
group_names <- sub(" 1$", "", names(ci_idade_e1)[names(ci_idade_e1) != "Tests"])

p_cif_fd <- ggcompetingrisks(
  fit             = ci_idade_e1,
  multiple_panels = FALSE,
  ggtheme         = theme_minimal(base_size = 12)
) +
  # Especifica mapeamento nomeado para cada nível e usa breaks na ordem correta
  scale_linetype_manual(
    name   = "Tipo de gravidez",
    values = c("Única"   = "solid",
               "Dupla" = "dotted",
               "Tripla+"   = "dashed"),
    labels = c("Única", "Dupla", "Tripla+"),
    breaks = c("Única", "Dupla", "Tripla+")
  ) +
  guides(color = "none",
         linetype = guide_legend(
           override.aes = list(colour = rep("#d62728", length(group_names)))
         )) +
  labs(
    title = "CIF apenas dos óbitos (Tipo de gravidez)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  )

print(p_cif_fd)

## Tipo de parto

### KM

# Óbito fetal como evento (censura = nascido vivo) 
km_fd_parto <- survfit(Surv(tempo, status_fd) ~ parto, data = dados_eda_clean)
survminer::ggsurvplot(
  km_fd_parto, data = dados_eda_clean, conf.int = TRUE,
  xlab = "Semanas", ylab = "S(t) — óbito fetal",
  title = "KM — Óbito fetal (por tipo de parto)",
  ggtheme = theme_minimal(base_size = 12),
  ylim = c(0.95, 1.00)  
)

# Nascido vivo como evento (óbito fetal censurado) 
km_lv_parto <- survfit(Surv(tempo, status_lv) ~ parto, data = dados_eda_clean)
survminer::ggsurvplot(
  km_lv_parto, data = dados_eda_clean, conf.int = TRUE,
  xlab = "Semanas", ylab = "S(t) — nascido vivo",
  title = "KM — Nascido vivo (por tipo de parto)",
  ggtheme = theme_minimal(base_size = 12)
)

### Log-Rank

# Log-rank para óbito fetal como evento de interesse (censura nascidos vivos)
logrank_fd_parto <- survdiff(Surv(tempo, status_fd) ~ parto, data = dados_eda_clean)
print(logrank_fd_parto)

# Log-rank para nascidos vivos como evento de interesse (censura óbito fetal)
logrank_fd_parto<- survdiff(Surv(tempo, status_lv) ~ parto, data = dados_eda_clean)
print(logrank_fd_parto)

### Teste de Gray e Incidência acumulada (CIF) 

# Teste de Gray da variável parto
ci_parto <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus,
  group   = dados_eda_clean$parto,
  cencode = 0
)

print(ci_parto)

# CIF da variável parto (1 = óbito fetal, 2 = nascido vivo)
p_cif_parto <- ggcompetingrisks(
  fit            = ci_parto,
  multiple_panels = TRUE,   # Plota ou não cada grupo em painéis separados
  ggtheme         = theme_minimal(base_size = 12),
  palette         = c("#d62728", "#1f77b4"),
  legend.title    = "Evento",
  legend.labs     = c("Óbito fetal", "Nascimento vivo"),
  xlab            = "Semanas de gestação",
  ylab            = "Incidência acumulada",
  title           = "Gráfico de Incidência Acumulada (CIF)"
)

print(p_cif_parto)

# CIF da variável parto (olhando mais de perto o evento de óbitos)

# Selecionar apenas as curvas do evento 1 (óbito fetal)
ci_parto_evento1 <- ci_parto[c("Vaginal 1", "Cesáreo 1")]

# Extrair os nomes sem o código do evento
group_names_parto <- sub(" 1$", "", names(ci_sexo_evento1)[names(ci_sexo_evento1) != "Tests"])
# Isso gera c("Vaginal", "Cesáreo")

# Construir o gráfico
p_cif_fd_parto <- ggcompetingrisks(
  fit             = ci_parto_evento1,
  multiple_panels = FALSE,
  ggtheme         = theme_minimal(base_size = 12)
) +
  # Mapea cada sexo a um tipo de linha de forma explícita
  scale_linetype_manual(
    name   = "Tipo de parto",
    values = c("Cesáreo" = "solid",
               "Vaginal" = "dashed"),
    labels = c("Cesáreo", "Vaginal"),
    breaks = c("Cesáreo", "Vaginal")
  ) +
  # Esconde a legenda de cor e pinta as linhas da legenda de vermelho
  guides(
    color = "none",
    linetype = guide_legend(
      override.aes = list(colour = rep("#d62728", length(group_names_parto)))
    )
  ) +
  labs(
    title = "CIF apenas dos óbitos (tipo de parto)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  )

print(p_cif_fd_parto)

## Peso

### KM

# Definir rótulos desejados (na ordem dos níveis)
rotulos_peso_faixa <- c("<2500g", "2500g-3999g", "4000g+")

# Óbito fetal como evento
km_fd_peso_faixa <- survfit(Surv(tempo, status_fd) ~ peso_faixa, data = dados_eda_clean)
survminer::ggsurvplot(
  km_fd_peso_faixa, 
  data         = dados_eda_clean, 
  conf.int    = TRUE,
  xlab         = "Semanas",
  ylab         = "S(t) — óbito fetal",
  title        = "KM — Óbito fetal (por peso em gramas)",
  legend.title = "",
  legend.labs  = rotulos_peso_faixa,
  ggtheme      = theme_minimal(base_size = 12),
  ylim         = c(0.98, 1.00)
)

# Nascido vivo como evento
km_lv_peso_faixa <- survfit(Surv(tempo, status_lv) ~ peso_faixa, data = dados_eda_clean)
survminer::ggsurvplot(
  km_lv_peso_faixa, data = dados_eda_clean, conf.int = TRUE,
  xlab         = "Semanas",
  ylab         = "S(t) — nascido vivo",
  title        = "KM — Nascido vivo (por peso em gramas)",
  legend.title = "",
  legend.labs  = rotulos_peso_faixa,
  ggtheme      = theme_minimal(base_size = 12)
)

### Log-Rank

# Log-rank para óbito fetal como evento de interesse (censura nascidos vivos)
logrank_fd_peso_faixa <- survdiff(Surv(tempo, status_fd) ~ peso_faixa, data = dados_eda_clean)
print(logrank_fd_peso_faixa)

# Log-rank para nascidos vivos como evento de interesse (censura óbito fetal)
logrank_fd_peso_faixa <- survdiff(Surv(tempo, status_lv) ~ peso_faixa, data = dados_eda_clean)
print(logrank_fd_peso_faixa)

### Teste de Gray e Incidência acumulada (CIF) 

# Teste de Gray da variável peso_faixa
ci_peso_faixa <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus,
  group   = dados_eda_clean$peso_faixa,
  cencode = 0
)

print(ci_peso_faixa)

# CIF da variável peso_faixa (1 = óbito fetal, 2 = nascido vivo)
p_cif_peso_faixa <- ggcompetingrisks(
  fit            = ci_peso_faixa,
  multiple_panels = TRUE, # Plota ou não cada grupo em painéis separados
  ggtheme         = theme_minimal(base_size = 12),
  legend.title    = "Evento",
  legend.labs     = c("Óbito fetal", "Nascimento vivo"),
  xlab            = "Semanas de gestação",
  ylab            = "Incidência acumulada",
  title           = "Gráfico de Incidência Acumulada (CIF)"
)

print(p_cif_peso_faixa)

# CIF da variável peso_faixa (olhando mais de perto o evento de óbitos)

ci_idade_e1 <- ci_peso_faixa[c("<2500g 1", "2500g-3999g 1", "4000g+ 1")]
group_names <- sub(" 1$", "", names(ci_idade_e1)[names(ci_idade_e1) != "Tests"])

p_cif_fd <- ggcompetingrisks(
  fit             = ci_idade_e1,
  multiple_panels = FALSE,
  ggtheme         = theme_minimal(base_size = 12)
) +
  # Especifica mapeamento nomeado para cada nível e usa breaks na ordem correta
  scale_linetype_manual(
    name   = "Peso (g)",
    values = c("<2500g"   = "solid",
               "2500g-3999g" = "dotted",
               "4000g+"   = "dashed"),
    labels = c("<2500g", "2500g-3999g", "4000g+"),
    breaks = c("<2500g", "2500g-3999g", "4000g+")
  ) +
  guides(color = "none",
         linetype = guide_legend(
           override.aes = list(colour = rep("#d62728", length(group_names)))
         )) +
  labs(
    title = "CIF apenas dos óbitos (por peso em gramas)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  )

print(p_cif_fd)

## Local de ocorrência

### KM

# Óbito fetal como evento (censura = nascido vivo)
km_fd_lococornasc <- survfit(Surv(tempo, status_fd) ~ lococornasc, data = dados_eda_clean)

# Rótulos na ordem exata das curvas
labs_fd <- sub("^.*=", "", names(km_fd_lococornasc$strata))

# Plotando sem os intervalos para enxergar melhor o comportamento
survminer::ggsurvplot(
  km_fd_lococornasc, data = dados_eda_clean, 
  # conf.int = TRUE, conf.int.style = "step", linetype = 1, size = 0.7,
  xlab = "Semanas", ylab = "S(t) — óbito fetal",
  title = "KM — Óbito fetal (por local de ocorrência)",
  ggtheme = theme_minimal(base_size = 12),
  ylim = c(0.95, 1.00),
  legend.title = NULL,     # sem título
  legend.labs  = labs_fd   # apenas o nome da categoria
)

# Nascido vivo como evento (óbito fetal censurado)
km_lv_lococornasc <- survfit(Surv(tempo, status_lv) ~ lococornasc, data = dados_eda_clean)

labs_lv <- sub("^.*=", "", names(km_lv_lococornasc$strata))

survminer::ggsurvplot(
  km_lv_lococornasc, data = dados_eda_clean, 
  # conf.int = TRUE, conf.int.style = "step", linetype = 1, size = 0.7,
  xlab = "Semanas", ylab = "S(t) — nascido vivo",
  title = "KM — Nascido vivo (por local de ocorrência)",
  ggtheme = theme_minimal(base_size = 12),
  legend.title = NULL,
  legend.labs  = labs_lv
)

### Log-Rank

# Log-rank para óbito fetal como evento de interesse (censura nascidos vivos)
logrank_fd_lococornasc <- survdiff(Surv(tempo, status_fd) ~ lococornasc, data = dados_eda_clean)
print(logrank_fd_lococornasc)

# Log-rank para nascidos vivos como evento de interesse (censura óbito fetal)
logrank_fd_lococornasc <- survdiff(Surv(tempo, status_lv) ~ lococornasc, data = dados_eda_clean)
print(logrank_fd_lococornasc)

### Teste de Gray e Incidência acumulada (CIF) 

# Teste de Gray e CIF para lococornasc
ci_lococornasc <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus,
  group   = dados_eda_clean$lococornasc,
  cencode = 0
)

# Visualização dos resultados do teste de Gray
print(ci_lococornasc)

# Ajustar os nomes para separar grupo e evento com um símbolo único (|)
names(ci_lococornasc) <- sub(" ([12])$", "|\\1", names(ci_lococornasc))

p_cif_lococornasc <- ggcompetingrisks(
  fit             = ci_lococornasc,
  gsep            = "\\|",                 
  multiple_panels = TRUE,
  ggtheme         = theme_minimal(base_size = 12),
  conf.int        = FALSE
) +
  labs(
    title  = "CIF por local de ocorrência",
    x      = "Semanas de gestação",
    y      = "Incidência acumulada",
    color  = "Evento",                     
    linetype = "Evento"
  ) +
  scale_color_manual(
    values = c("1" = "#d62728", "2" = "#1f77b4"),
    labels = c("1" = "Óbito fetal", "2" = "Nascimento vivo")
  ) +
  scale_linetype_manual(
    values = c("1" = "solid", "2" = "dashed"),
    labels = c("1" = "Óbito fetal", "2" = "Nascimento vivo")
  )

print(p_cif_lococornasc)

ci <- ci_lococornasc

# Curvas do cuminc em formato tidy
curve_names <- setdiff(names(ci), "Tests")

cif_df <- map_dfr(curve_names, function(nm){
  tibble(
    tempo = ci[[nm]]$time,
    est   = ci[[nm]]$est,
    var   = ci[[nm]]$var,
    nome  = nm
  )
}) %>%
  # Separa "grupo" (lococornasc) e "evento" a partir do nome "Grupo|Evento"
  mutate(
    grupo  = sub("\\|[12]$", "", nome),
    evento = sub("^.*\\|([12])$", "\\1", nome)
  ) %>%
  # Mantém somente o evento 1 (óbito fetal)
  filter(evento == "1") %>%
  # Garante a ordem dos níveis igual ao seu factor original
  mutate(
    grupo = factor(grupo, levels = levels(dados_eda_clean$lococornasc)),
    lo = pmax(0, est - 1.96*sqrt(var)),  # IC inferior
    hi = pmin(1, est + 1.96*sqrt(var))   # IC superior
  ) %>%
  arrange(grupo, tempo)

# Figura única com 6 curvas (uma por escolaridade), legenda por lococornasc
p_cif_fd_lococornasc_tidy <-
  ggplot(cif_df, aes(x = tempo, y = est, colour = grupo)) +
  geom_step(size = 0.9) +
  # Bandas de confiança
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = grupo), alpha = 0.08, colour = NA, show.legend = FALSE) +
  labs(
    title  = "CIF apenas dos óbitos (por local de ocorrência)",
    x      = "Semanas de gestação",
    y      = "Incidência acumulada",
    colour = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p_cif_fd_lococornasc_tidy)

# Variáveis únicas do SIM e SINASC

# SIM-DOFET
dim(dados_simsp)
dplyr::glimpse(dados_simsp, width = 80)
table(dados_simsp$obitoparto, useNA = 'always')

# SINASC
dim(dados_sinascsp)
dplyr::glimpse(dados_sinascsp, width = 80)

# Recodificação essencial para análises seguintes
dados_sinasc2 <- dados_sinascsp %>%
  mutate(
    estcivmae = case_when(
      estcivmae == "1" ~ "Solteira",
      estcivmae == "2" ~ "Casada",
      estcivmae == "3" ~ "Viúva",
      estcivmae == "4" ~ "Divorciada",
      estcivmae == "5" ~ "União estável",
      TRUE ~ NA_character_) %>% 
      factor(levels = c("Solteira","Casada","Viúva", "Divorciada", "União estável")),
    consultas = case_when(
      consultas == "1" ~ "Nenhuma",
      consultas == "2" ~ "De 1 a 3",
      consultas == "3" ~ "De 4 a 6",
      consultas == "4" ~ "7 e mais",
      TRUE ~ NA_character_) %>% 
      factor(levels = c("Nenhuma","De 1 a 3","De 4 a 6", "7 e mais")),
    apgar5 = case_when(
      apgar5 > 7 ~ "Maior que 7",
      apgar5 <= 7 ~ "7 ou menor",
      TRUE ~ NA_character_) %>% 
      factor(levels = c("Maior que 7","7 ou menor")),
    idanomal = case_when(
      idanomal == 1 ~ "Sim",
      idanomal == 2 ~ "Não",
      TRUE ~ NA_character_) %>% 
      factor(levels = c("Sim","Não")),
    qtdgestant_binario = case_when(
      qtdgestant != 0 & is.na(qtdgestant) == FALSE ~ "Sim",
      qtdgestant == 0 ~ "Não",
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
  ) 

# Selecionar apenas os casos completos nessas variáveis
dados_sinasc2_clean <- dados_sinasc2[complete.cases(dados_sinasc2[, ]), ]
dim(dados_sinasc2_clean)
# > dim(dados_sinasc2_clean)
# [1] 486508     13

# Identificar variáveis categóricas (factor ou character)
cat_vars <- c("estcivmae",
              "consultas",
              "apgar5",
              "idanomal",
              "qtdgestant_binario",
              "racacormae")
cat_vars <- intersect(cat_vars, names(dados_sinasc2_clean))

cat_summary_all <- map_dfr(cat_vars, function(v){
  tab <- janitor::tabyl(dados_sinasc2_clean[[v]], show_na = TRUE)  # n e percent prontos
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