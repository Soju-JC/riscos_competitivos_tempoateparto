.libPaths(c("r_libs", .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(survival)
})

dir.create("scripts/analysis_outputs", recursive = TRUE, showWarnings = FALSE)

summarize_by_group <- function(df, vars, time_var = "tempo", extra_group = NULL) {
  pieces <- lapply(vars, function(v) {
    data_use <- df
    if (!is.null(extra_group)) {
      data_use %>%
        filter(!is.na(.data[[v]]), !is.na(.data[[time_var]]), !is.na(.data[[extra_group]])) %>%
        group_by(
          variavel = v,
          grupo_extra = as.character(.data[[extra_group]]),
          nivel = as.character(.data[[v]])
        ) %>%
        summarise(
          n = dplyr::n(),
          media = mean(.data[[time_var]], na.rm = TRUE),
          mediana = median(.data[[time_var]], na.rm = TRUE),
          q1 = quantile(.data[[time_var]], 0.25, na.rm = TRUE, names = FALSE),
          q3 = quantile(.data[[time_var]], 0.75, na.rm = TRUE, names = FALSE),
          .groups = "drop"
        )
    } else {
      data_use %>%
        filter(!is.na(.data[[v]]), !is.na(.data[[time_var]])) %>%
        group_by(variavel = v, nivel = as.character(.data[[v]])) %>%
        summarise(
          n = dplyr::n(),
          media = mean(.data[[time_var]], na.rm = TRUE),
          mediana = median(.data[[time_var]], na.rm = TRUE),
          q1 = quantile(.data[[time_var]], 0.25, na.rm = TRUE, names = FALSE),
          q3 = quantile(.data[[time_var]], 0.75, na.rm = TRUE, names = FALSE),
          .groups = "drop"
        )
    }
  })

  bind_rows(pieces)
}

logrank_summary <- function(df, vars, time_var = "tempo", status_var = "status_evento") {
  bind_rows(lapply(vars, function(v) {
    data_use <- df %>% filter(!is.na(.data[[v]]), !is.na(.data[[time_var]]), !is.na(.data[[status_var]]))
    fit <- survdiff(as.formula(paste0("Surv(", time_var, ", ", status_var, ") ~ ", v)), data = data_use)
    tibble(
      variavel = v,
      chisq = unname(fit$chisq),
      df = length(fit$n) - 1L,
      p_valor = 1 - pchisq(unname(fit$chisq), df = length(fit$n) - 1L)
    )
  }))
}

# ---------------------------------------------------------------------------
# Base combinada: variáveis comuns
# ---------------------------------------------------------------------------
dados_simsp <- readRDS("dataset_sim_df.rds")
dados_sinascsp <- readRDS("dataset_sinasc_df.rds")
vars_comuns <- intersect(names(dados_sinascsp), names(dados_simsp))

dados_sp_cln <- rbind(
  dados_sinascsp[, vars_comuns],
  dados_simsp[, vars_comuns]
)

dados_ambos <- dados_sp_cln %>%
  mutate(
    semagestac = suppressWarnings(as.numeric(semagestac)),
    idademae = suppressWarnings(as.numeric(idademae)),
    peso = suppressWarnings(as.numeric(peso)),
    qtdfilvivo = suppressWarnings(as.numeric(qtdfilvivo)),
    qtdfilmort = suppressWarnings(as.numeric(qtdfilmort)),
    evento = suppressWarnings(as.integer(evento)),
    sexo = case_when(sexo %in% c("1", "2") ~ sexo, TRUE ~ NA_character_) %>%
      factor(levels = c("1", "2"), labels = c("Masculino", "Feminino")),
    gravidez = case_when(
      gravidez == "1" ~ "Única",
      gravidez %in% c("2", "3") ~ "Múltipla",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Única", "Múltipla")),
    parto = factor(parto, levels = c("1", "2"), labels = c("Vaginal", "Cesáreo")),
    lococornasc = case_when(
      lococornasc == "1" ~ "Hospital",
      lococornasc %in% c("2", "3", "4", "5") ~ "Outros",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Hospital", "Outros")),
    escmae2010 = case_when(
      as.character(escmae2010) %in% c("0", "1", "2") ~ "Baixa escolaridade",
      as.character(escmae2010) == "3" ~ "Média escolaridade",
      as.character(escmae2010) %in% c("4", "5") ~ "Alta escolaridade",
      TRUE ~ NA_character_
    ) %>% factor(
      levels = c("Baixa escolaridade", "Média escolaridade", "Alta escolaridade"),
      ordered = TRUE
    ),
    idademae_categorico = case_when(
      idademae < 20 ~ "<20",
      idademae >= 20 & idademae < 35 ~ "20-34",
      idademae >= 35 ~ "35+",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("<20", "20-34", "35+")),
    peso_categorico = case_when(
      peso < 2500 ~ "<2500g",
      peso >= 2500 & peso < 4000 ~ "2500g-3999g",
      peso >= 4000 ~ "4000g+",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("<2500g", "2500g-3999g", "4000g+")),
    qtdfilvivo_categorico = case_when(
      qtdfilvivo == 0 ~ "Não",
      qtdfilvivo > 0 ~ "Sim",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Não", "Sim")),
    qtdfilmort_categorico = case_when(
      qtdfilmort == 0 ~ "Não",
      qtdfilmort > 0 ~ "Sim",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Não", "Sim")),
    tempo = semagestac,
    evento_lab = factor(evento, levels = c(1, 0), labels = c("Óbito fetal", "Nascimento vivo"))
  )

common_vars <- c(
  "idademae_categorico",
  "sexo",
  "escmae2010",
  "qtdfilvivo_categorico",
  "qtdfilmort_categorico",
  "gravidez",
  "parto",
  "peso_categorico",
  "lococornasc"
)

common_summary <- summarize_by_group(dados_ambos, common_vars, extra_group = "evento_lab")
write.csv(common_summary, "scripts/analysis_outputs/common_summary.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# SIM: variável exclusiva obitoparto
# ---------------------------------------------------------------------------
dados_sim <- readRDS("dataset_sim_df.rds") %>%
  mutate(
    semagestac = suppressWarnings(as.numeric(semagestac)),
    obitoparto = case_when(
      obitoparto == 1 ~ "Antes",
      obitoparto == 2 ~ "Durante",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Antes", "Durante")),
    tempo = semagestac,
    status_evento = 1L
  )

sim_summary <- summarize_by_group(dados_sim, c("obitoparto"))
sim_tests <- logrank_summary(dados_sim, c("obitoparto"))

write.csv(sim_summary, "scripts/analysis_outputs/sim_summary.csv", row.names = FALSE)
write.csv(sim_tests, "scripts/analysis_outputs/sim_logrank.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# SINASC: variáveis exclusivas
# ---------------------------------------------------------------------------
dados_sinasc <- readRDS("dataset_sinasc_df.rds") %>%
  mutate(
    semagestac = suppressWarnings(as.numeric(semagestac)),
    qtdgestant = suppressWarnings(as.numeric(qtdgestant)),
    qtdpartnor = suppressWarnings(as.numeric(qtdpartnor)),
    qtdpartces = suppressWarnings(as.numeric(qtdpartces)),
    apgar5 = suppressWarnings(as.numeric(apgar5)),
    mesprenat = suppressWarnings(as.numeric(mesprenat)),
    consultas = case_when(
      consultas == "1" ~ "Nenhuma",
      consultas == "2" ~ "De 1 a 3",
      consultas == "3" ~ "De 4 a 6",
      consultas == "4" ~ "7 e mais",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Nenhuma", "De 1 a 3", "De 4 a 6", "7 e mais")),
    idanomal = case_when(
      idanomal == 1 ~ "Sim",
      idanomal == 2 ~ "Não",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Sim", "Não")),
    racacormae = case_when(
      racacormae == 1 ~ "Branca",
      racacormae == 2 ~ "Preta",
      racacormae == 3 ~ "Amarela",
      racacormae == 4 ~ "Parda",
      racacormae == 5 ~ "Indígena",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Branca", "Preta", "Amarela", "Parda", "Indígena")),
    sttrabpart = case_when(
      sttrabpart == 1 ~ "Sim",
      sttrabpart == 2 ~ "Não",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Sim", "Não")),
    stcesparto = case_when(
      stcesparto == 1 ~ "Sim",
      stcesparto == 2 ~ "Não",
      stcesparto == 3 ~ "Não se aplica",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Sim", "Não", "Não se aplica")),
    paridade = case_when(
      paridade == 0 ~ "Nulípara",
      paridade == 1 ~ "Multípara",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Nulípara", "Multípara")),
    apgar5_categorico = case_when(
      apgar5 > 7 ~ "7+",
      apgar5 <= 7 ~ "7 ou menos",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("7+", "7 ou menos")),
    qtdgestant_categorico = case_when(
      qtdgestant == 0 ~ "Não",
      qtdgestant > 0 ~ "Sim",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Não", "Sim")),
    qtdpartnor_categorico = case_when(
      qtdpartnor == 0 ~ "Não",
      qtdpartnor > 0 ~ "Sim",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Não", "Sim")),
    qtdpartces_categorico = case_when(
      qtdpartces == 0 ~ "Não",
      qtdpartces > 0 ~ "Sim",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Não", "Sim")),
    mesprenat_categorico = case_when(
      mesprenat %in% c(1, 2, 3) ~ "Primeiro trimestre",
      mesprenat %in% c(4, 5, 6) ~ "Segundo trimestre",
      mesprenat %in% c(7, 8, 9) ~ "Terceiro trimestre",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Primeiro trimestre", "Segundo trimestre", "Terceiro trimestre")),
    tempo = semagestac,
    status_evento = 1L
  )

sinasc_vars <- c(
  "consultas",
  "idanomal",
  "racacormae",
  "sttrabpart",
  "stcesparto",
  "paridade",
  "apgar5_categorico",
  "qtdgestant_categorico",
  "qtdpartnor_categorico",
  "qtdpartces_categorico",
  "mesprenat_categorico"
)

sinasc_summary <- summarize_by_group(dados_sinasc, sinasc_vars)
sinasc_tests <- logrank_summary(dados_sinasc, sinasc_vars)

write.csv(sinasc_summary, "scripts/analysis_outputs/sinasc_summary.csv", row.names = FALSE)
write.csv(sinasc_tests, "scripts/analysis_outputs/sinasc_logrank.csv", row.names = FALSE)

cat("Arquivos gravados em scripts/analysis_outputs\n")
