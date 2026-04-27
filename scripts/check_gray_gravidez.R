.libPaths(c("r_libs", .libPaths()))

library(dplyr)
library(cmprsk)

dados_simsp <- readRDS("dataset_sim_df.rds")
dados_sinascsp <- readRDS("dataset_sinasc_df.rds")

vars_comuns <- intersect(names(dados_sinascsp), names(dados_simsp))

dados_sp_cln <- rbind(
  dados_sinascsp[, vars_comuns],
  dados_simsp[, vars_comuns]
)

dados_eda_clean <- dados_sp_cln %>%
  mutate(
    semagestac = suppressWarnings(as.numeric(semagestac)),
    evento = suppressWarnings(as.integer(evento)),
    gravidez = case_when(
      gravidez == "1" ~ "Única",
      gravidez %in% c("2", "3") ~ "Múltipla",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Única", "Múltipla")),
    tempo = semagestac,
    fstatus = case_when(
      evento == 1L ~ 1L,
      evento == 0L ~ 2L,
      TRUE ~ 0L
    ),
    fstatus_swap = case_when(
      evento == 0L ~ 1L,
      evento == 1L ~ 2L,
      TRUE ~ 0L
    ),
    fstatus_cr = factor(
      fstatus,
      levels = c(0, 1, 2),
      labels = c("Censura", "Óbito fetal", "Nascimento vivo")
    )
  ) %>%
  filter(!is.na(gravidez), !is.na(tempo), !is.na(fstatus))

ci_num <- cmprsk::cuminc(
  ftime = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus,
  group = dados_eda_clean$gravidez,
  cencode = 0
)

ci_fac <- cmprsk::cuminc(
  ftime = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus_cr,
  group = dados_eda_clean$gravidez,
  cencode = "Censura"
)

cat("NUMERIC\n")
print(ci_num$Tests)
cat("\nFACTOR\n")
print(ci_fac$Tests)

ci_swap <- cmprsk::cuminc(
  ftime = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus_swap,
  group = dados_eda_clean$gravidez,
  cencode = 0
)

cat("\nSWAPPED CODING\n")
print(ci_swap$Tests)
