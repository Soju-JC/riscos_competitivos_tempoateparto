.libPaths(c("r_libs", .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(cmprsk)
  library(purrr)
})

dir.create("scripts/analysis_outputs", recursive = TRUE, showWarnings = FALSE)

target_times <- c(30, 35, 40)

extract_curve_at_times <- function(curve, times) {
  sapply(times, function(t) {
    idx <- max(which(curve$time <= t))
    if (is.finite(idx)) curve$est[idx] else NA_real_
  })
}

extract_cif_for_var <- function(df, var_name, event_code, event_label, times = target_times) {
  data_use <- df %>%
    filter(!is.na(.data[[var_name]]), !is.na(tempo), !is.na(fstatus))

  ci_obj <- cmprsk::cuminc(
    ftime = data_use$tempo,
    fstatus = data_use$fstatus,
    group = data_use[[var_name]],
    cencode = 0
  )

  curve_names <- names(ci_obj)
  curve_names <- curve_names[grepl(paste0(" ", event_code, "$"), curve_names)]

  bind_rows(lapply(curve_names, function(curve_name) {
    ests <- extract_curve_at_times(ci_obj[[curve_name]], times)
    grupo <- sub(paste0(" ", event_code, "$"), "", curve_name)

    tibble(
      variavel = var_name,
      evento = event_label,
      grupo = grupo,
      tempo = times,
      cif = as.numeric(ests)
    )
  }))
}

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
    sexo = case_when(
      sexo %in% c("1", "2") ~ sexo,
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("1", "2"), labels = c("Masculino", "Feminino")),
    gravidez = case_when(
      gravidez == "1" ~ "Única",
      gravidez %in% c("2", "3") ~ "Múltipla",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Única", "Múltipla")),
    parto = factor(parto, levels = c("1", "2"), labels = c("Vaginal", "Cesáreo")),
    lococornasc = case_when(
      lococornasc == "1" ~ "Hospital",
      lococornasc %in% c("2", "3", "4", "5") ~ "Outros",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Hospital", "Outros")),
    escmae2010 = case_when(
      as.character(escmae2010) %in% c("0", "1", "2") ~ "Baixa escolaridade",
      as.character(escmae2010) == "3" ~ "Média escolaridade",
      as.character(escmae2010) %in% c("4", "5") ~ "Alta escolaridade",
      TRUE ~ NA_character_
    ) %>%
      factor(
        levels = c("Baixa escolaridade", "Média escolaridade", "Alta escolaridade"),
        ordered = TRUE
      ),
    idademae_categorico = case_when(
      idademae < 20 ~ "<20",
      idademae >= 20 & idademae < 35 ~ "20-34",
      idademae >= 35 ~ "35+",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("<20", "20-34", "35+")),
    peso_categorico = case_when(
      peso < 2500 ~ "<2500g",
      peso >= 2500 & peso < 4000 ~ "2500g-3999g",
      peso >= 4000 ~ "4000g+",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("<2500g", "2500g-3999g", "4000g+")),
    qtdfilvivo_categorico = case_when(
      qtdfilvivo == 0 ~ "Não",
      qtdfilvivo > 0 ~ "Sim",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Não", "Sim")),
    qtdfilmort_categorico = case_when(
      qtdfilmort == 0 ~ "Não",
      qtdfilmort > 0 ~ "Sim",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Não", "Sim")),
    tempo = semagestac,
    fstatus = case_when(
      evento == 1L ~ 1L,
      evento == 0L ~ 2L,
      TRUE ~ 0L
    )
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

cif_common <- bind_rows(lapply(common_vars, function(var_name) {
  bind_rows(
    extract_cif_for_var(dados_ambos, var_name, 1, "Óbito fetal"),
    extract_cif_for_var(dados_ambos, var_name, 2, "Nascimento vivo")
  )
}))

write.csv(cif_common, "scripts/analysis_outputs/common_cif_snapshots.csv", row.names = FALSE)

cat("Arquivo gerado: scripts/analysis_outputs/common_cif_snapshots.csv\n")
