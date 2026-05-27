# Analise a partir da base unificada final
# Base de entrada: datasets_finais/simsinasc_todas_variaveis_pre_recodificacao.rds

# 1) ler uma unica base unificada com todas as variaveis;
# 2) gerar as descritivas, os indicadores por RRAS e as funcoes de KM/CIF;

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(scales)
  library(survival)
  library(survminer)
  library(ggplot2)
  library(readxl)
})

options(scipen = 999)

# =============================================================================
# 1. Carregamento da base unificada
# =============================================================================

caminho_base_unificada <- file.path(
  "datasets_finais",
  "simsinasc_todas_variaveis_pre_recodificacao.rds"
)

dados_unificados_pre <- readRDS(caminho_base_unificada)

cat("\nBase unificada carregada:\n")
dplyr::glimpse(dados_unificados_pre, width = 80)

# evento = 1 identifica os registros de obito fetal do SIM-DOFET.
# evento = 0 identifica os registros de nascidos vivos do SINASC.
table(dados_unificados_pre$evento, useNA = "ifany")

# =============================================================================
# 2. Variáveis usadas na dissertacao
# =============================================================================

vars_comuns <- c(
  "codmunres",
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
  "evento",
  "municipio_residencia",
  "rras_id",
  "rras_nome",
  "regiao_de_saude",
  "drs"
)

vars_sim <- c(
  setdiff(vars_comuns, "evento"),
  "obitoparto"
)

vars_sinasc <- c(
  setdiff(vars_comuns, "evento"),
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
)

vars_sinasc_exclusivas <- c(
  "consultas",
  "apgar5_categorico",
  "idanomal",
  "racacormae",
  "qtdgestant_categorico",
  "qtdpartnor_categorico",
  "qtdpartces_categorico",
  "mesprenat_categorico",
  "sttrabpart",
  "stcesparto",
  "paridade"
)

vars_sim_exclusivas <- c("obitoparto")

vars_km_comuns <- c(
  "sexo",
  "idademae_categorico",
  "escmae2010",
  "qtdfilvivo_categorico",
  "qtdfilmort_categorico",
  "gravidez",
  "parto",
  "peso_categorico",
  "lococornasc"
)

# =============================================================================
# 3. Bases de trabalho
# =============================================================================

dados_sim_pre <- dados_unificados_pre %>%
  filter(evento == 1) %>%
  select(all_of(vars_sim))

dados_sinasc_pre <- dados_unificados_pre %>%
  filter(evento == 0) %>%
  select(all_of(vars_sinasc))

dados_simsinasc_comum_pre <- dados_unificados_pre %>%
  select(all_of(vars_comuns))

dados_simsinasc_todas_variaveis_pre <- dados_unificados_pre

cat("\nDimensoes das bases reconstruidas:\n")
print(tibble::tibble(
  base = c("SIM-DOFET", "SINASC", "SIM+SINASC comuns", "SIM+SINASC todas"),
  linhas = c(
    nrow(dados_sim_pre),
    nrow(dados_sinasc_pre),
    nrow(dados_simsinasc_comum_pre),
    nrow(dados_simsinasc_todas_variaveis_pre)
  ),
  colunas = c(
    ncol(dados_sim_pre),
    ncol(dados_sinasc_pre),
    ncol(dados_simsinasc_comum_pre),
    ncol(dados_simsinasc_todas_variaveis_pre)
  )
))

# =============================================================================
# 4. Recodificacoes usadas nas análises
# =============================================================================

recodificar_variaveis_comuns <- function(df) {
  df %>%
    mutate(
      semagestac = suppressWarnings(as.numeric(semagestac)),
      idademae = suppressWarnings(as.numeric(idademae)),
      peso = suppressWarnings(as.numeric(peso)),
      qtdfilvivo = suppressWarnings(as.numeric(qtdfilvivo)),
      qtdfilmort = suppressWarnings(as.numeric(qtdfilmort)),
      evento = if ("evento" %in% names(.)) suppressWarnings(as.integer(evento)) else NA_integer_
    ) %>%
    mutate(
      sexo = case_when(
        as.character(sexo) %in% c("1", "2") ~ as.character(sexo),
        TRUE ~ NA_character_
      ) %>%
        factor(levels = c("1", "2"), labels = c("Masculino", "Feminino")),
      gravidez = case_when(
        as.character(gravidez) == "1" ~ "Única",
        as.character(gravidez) == "2" ~ "Múltipla",
        as.character(gravidez) == "3" ~ "Múltipla",
        TRUE ~ NA_character_
      ) %>%
        factor(levels = c("Única", "Múltipla")),
      parto = factor(
        as.character(parto),
        levels = c("1", "2"),
        labels = c("Vaginal", "Cesáreo")
      ),
      lococornasc = case_when(
        as.character(lococornasc) == "1" ~ "Hospital",
        as.character(lococornasc) == "2" ~ "Outros",
        as.character(lococornasc) == "3" ~ "Outros",
        as.character(lococornasc) == "4" ~ "Outros",
        as.character(lococornasc) == "5" ~ "Outros",
        TRUE ~ NA_character_
      ) %>%
        factor(levels = c("Hospital", "Outros")),
      escmae2010 = case_when(
        as.character(escmae2010) == "0" ~ "Baixa escolaridade",
        as.character(escmae2010) == "1" ~ "Baixa escolaridade",
        as.character(escmae2010) == "2" ~ "Baixa escolaridade",
        as.character(escmae2010) == "3" ~ "Média escolaridade",
        as.character(escmae2010) == "4" ~ "Alta escolaridade",
        as.character(escmae2010) == "5" ~ "Alta escolaridade",
        TRUE ~ NA_character_
      ) %>%
        factor(
          levels = c("Baixa escolaridade", "Média escolaridade", "Alta escolaridade"),
          ordered = TRUE
        ),
      idademae_categorico = case_when(
        idademae < 20 ~ "<20",
        idademae >= 20 & idademae < 35 ~ "20–34",
        idademae >= 35 ~ "35+",
        TRUE ~ NA_character_
      ) %>%
        factor(levels = c("<20", "20–34", "35+")),
      peso_categorico = case_when(
        peso < 2500 ~ "<2500g",
        peso >= 2500 & peso < 4000 ~ "2500g-3999g",
        peso >= 4000 ~ "4000g+",
        TRUE ~ NA_character_
      ) %>%
        factor(levels = c("<2500g", "2500g-3999g", "4000g+")),
      qtdfilvivo_categorico = case_when(
        qtdfilvivo == 0 ~ "Não",
        qtdfilvivo >= 1 ~ "Sim",
        TRUE ~ NA_character_
      ) %>%
        factor(levels = c("Não", "Sim")),
      qtdfilmort_categorico = case_when(
        qtdfilmort == 0 ~ "Não",
        qtdfilmort >= 1 ~ "Sim",
        TRUE ~ NA_character_
      ) %>%
        factor(levels = c("Não", "Sim")),
      tempo = semagestac
    )
}

recodificar_variaveis_sinasc <- function(df) {
  df %>%
    mutate(
      qtdgestant = suppressWarnings(as.numeric(qtdgestant)),
      qtdpartnor = suppressWarnings(as.numeric(qtdpartnor)),
      qtdpartces = suppressWarnings(as.numeric(qtdpartces)),
      apgar5 = suppressWarnings(as.numeric(apgar5)),
      mesprenat = suppressWarnings(as.numeric(mesprenat))
    ) %>%
    mutate(
      consultas = case_when(
        as.character(consultas) == "1" ~ "Nenhuma",
        as.character(consultas) == "2" ~ "De 1 a 3",
        as.character(consultas) == "3" ~ "De 4 a 6",
        as.character(consultas) == "4" ~ "7 e mais",
        TRUE ~ NA_character_
      ) %>%
        factor(levels = c("Nenhuma", "De 1 a 3", "De 4 a 6", "7 e mais")),
      idanomal = case_when(
        as.character(idanomal) == "1" ~ "Sim",
        as.character(idanomal) == "2" ~ "Não",
        TRUE ~ NA_character_
      ) %>%
        factor(levels = c("Sim", "Não")),
      racacormae = case_when(
        as.character(racacormae) == "1" ~ "Branca",
        as.character(racacormae) == "2" ~ "Preta",
        as.character(racacormae) == "3" ~ "Amarela",
        as.character(racacormae) == "4" ~ "Parda",
        as.character(racacormae) == "5" ~ "Indígena",
        TRUE ~ NA_character_
      ) %>%
        factor(levels = c("Branca", "Preta", "Amarela", "Parda", "Indígena")),
      sttrabpart = case_when(
        as.character(sttrabpart) == "1" ~ "Sim",
        as.character(sttrabpart) == "2" ~ "Não",
        TRUE ~ NA_character_
      ) %>%
        factor(levels = c("Sim", "Não")),
      stcesparto = case_when(
        as.character(stcesparto) == "1" ~ "Sim",
        as.character(stcesparto) == "2" ~ "Não",
        as.character(stcesparto) == "3" ~ "Não se aplica",
        TRUE ~ NA_character_
      ) %>%
        factor(levels = c("Sim", "Não", "Não se aplica")),
      paridade = case_when(
        as.character(paridade) == "0" ~ "Nulípara",
        as.character(paridade) == "1" ~ "Multípara",
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
        mesprenat %in% c(1, 2, 3) ~ "Primeiro trimestre",
        mesprenat %in% c(4, 5, 6) ~ "Segundo trimestre",
        mesprenat %in% c(7, 8, 9) ~ "Terceiro trimestre",
        TRUE ~ NA_character_
      ) %>%
        factor(levels = c("Primeiro trimestre", "Segundo trimestre", "Terceiro trimestre"))
    )
}

recodificar_variaveis_sim <- function(df) {
  df %>%
    mutate(
      obitoparto = case_when(
        as.character(obitoparto) == "1" ~ "Antes",
        as.character(obitoparto) == "2" ~ "Durante",
        TRUE ~ NA_character_
      ) %>%
        factor(levels = c("Antes", "Durante"))
    )
}

incluir_status_evento_isolado <- function(df) {
  df %>% mutate(status_evento = 1L)
}

incluir_desfechos_competitivos <- function(df) {
  df %>%
    mutate(
      evento = suppressWarnings(as.integer(evento)),
      status_fd = ifelse(evento == 1L, 1L, 0L),
      status_lv = ifelse(evento == 0L, 1L, 0L),
      fstatus = case_when(
        evento == 1L ~ 1L,
        evento == 0L ~ 2L,
        TRUE ~ 0L
      ),
      evento_lab = factor(
        evento,
        levels = c(1, 0),
        labels = c("Óbito fetal", "Nascimento vivo")
      ),
      fstatus_cr = factor(
        fstatus,
        levels = c(0, 1, 2),
        labels = c("Censura", "Óbito fetal", "Nascimento vivo")
      )
    )
}

# Dados das análises descritivas e sobrevivência
dados_sim_pos <- dados_sim_pre %>%
  recodificar_variaveis_comuns() %>%
  recodificar_variaveis_sim() %>%
  incluir_status_evento_isolado()

dados_sinasc_pos <- dados_sinasc_pre %>%
  recodificar_variaveis_comuns() %>%
  recodificar_variaveis_sinasc() %>%
  incluir_status_evento_isolado()

dados_simsinasc_comum_pos <- dados_simsinasc_comum_pre %>%
  recodificar_variaveis_comuns() %>%
  incluir_desfechos_competitivos()

# Dados para aplicação do modelo
dados_simsinasc_todas_variaveis_pos <- dados_simsinasc_todas_variaveis_pre %>%
  recodificar_variaveis_comuns() %>%
  recodificar_variaveis_sim() %>%
  recodificar_variaveis_sinasc() %>%
  incluir_desfechos_competitivos()

# =============================================================================
# 5. Funcoes simples para descritiva
# =============================================================================

resumir_numericas <- function(df, vars) {
  vars <- intersect(vars, names(df))

  map_dfr(vars, function(v) {
    x <- suppressWarnings(as.numeric(df[[v]]))

    tibble::tibble(
      variavel = v,
      n = sum(!is.na(x)),
      media = mean(x, na.rm = TRUE),
      dp = sd(x, na.rm = TRUE),
      mediana = median(x, na.rm = TRUE),
      q1 = quantile(x, 0.25, na.rm = TRUE, names = FALSE),
      q3 = quantile(x, 0.75, na.rm = TRUE, names = FALSE)
    )
  })
}

resumir_categoricas <- function(df, vars) {
  vars <- intersect(vars, names(df))

  map_dfr(vars, function(v) {
    df %>%
      count(nivel = as.character(.data[[v]]), name = "n") %>%
      mutate(
        variavel = v,
        percentual = scales::percent(n / sum(n), accuracy = 0.1),
        .before = 1
      )
  })
}

resumir_tempo_por_grupo <- function(df, var) {
  df %>%
    filter(!is.na(tempo), !is.na(.data[[var]])) %>%
    group_by(grupo = .data[[var]]) %>%
    summarise(
      n = n(),
      media = mean(tempo, na.rm = TRUE),
      dp = sd(tempo, na.rm = TRUE),
      mediana = median(tempo, na.rm = TRUE),
      q1 = quantile(tempo, 0.25, na.rm = TRUE, names = FALSE),
      q3 = quantile(tempo, 0.75, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )
}

resumir_rras <- function(df) {
  df %>%
    filter(!is.na(rras_id)) %>%
    count(rras_id, rras_nome, name = "n") %>%
    arrange(rras_id) %>%
    mutate(percentual = scales::percent(n / sum(n), accuracy = 0.1))
}

# =============================================================================
# 6. Descritivas do SIM-DOFET
# =============================================================================

num_sim <- c("idademae", "qtdfilvivo", "qtdfilmort", "semagestac", "peso")
cat_sim <- c(vars_km_comuns, vars_sim_exclusivas, "rras_nome")

desc_num_sim <- resumir_numericas(dados_sim_pos, num_sim)
desc_cat_sim <- resumir_categoricas(dados_sim_pos, cat_sim)
rras_sim <- resumir_rras(dados_sim_pos)

cat("\nDescritiva numerica - SIM-DOFET\n")
print(desc_num_sim)

cat("\nFrequencias por RRAS - SIM-DOFET\n")
print(tibble::as_tibble(rras_sim), n = Inf)

# =============================================================================
# 7. Descritivas do SINASC
# =============================================================================

num_sinasc <- c(
  "idademae", "qtdfilvivo", "qtdfilmort", "qtdgestant", "qtdpartnor",
  "qtdpartces", "apgar5", "mesprenat", "semagestac", "peso"
)
cat_sinasc <- c(vars_km_comuns, vars_sinasc_exclusivas, "rras_nome")

desc_num_sinasc <- resumir_numericas(dados_sinasc_pos, num_sinasc)
desc_cat_sinasc <- resumir_categoricas(dados_sinasc_pos, cat_sinasc)
rras_sinasc <- resumir_rras(dados_sinasc_pos)

cat("\nDescritiva numerica - SINASC\n")
print(desc_num_sinasc)

cat("\nFrequencias por RRAS - SINASC\n")
print(tibble::as_tibble(rras_sinasc), n = Inf)

# =============================================================================
# 8. Descritiva da idade gestacional por causa
# =============================================================================

desc_num_comum <- resumir_numericas(
  dados_simsinasc_comum_pos,
  c("idademae", "qtdfilvivo", "qtdfilmort", "semagestac", "peso")
)

desc_cat_comum <- resumir_categoricas(
  dados_simsinasc_comum_pos,
  c(vars_km_comuns, "evento_lab", "rras_nome")
)

tempo_por_desfecho <- dados_simsinasc_comum_pos %>%
  group_by(evento_lab) %>%
  summarise(
    n = sum(!is.na(tempo)),
    media = mean(tempo, na.rm = TRUE),
    dp = sd(tempo, na.rm = TRUE),
    mediana = median(tempo, na.rm = TRUE),
    q1 = quantile(tempo, 0.25, na.rm = TRUE, names = FALSE),
    q3 = quantile(tempo, 0.75, na.rm = TRUE, names = FALSE),
    max = max(tempo),
    .groups = "drop"
  )

cat("\nTempo gestacional por desfecho - SIM + SINASC\n")
print(tempo_por_desfecho)

# =============================================================================
# 9. Indicadores por RRAS
# =============================================================================

criar_indicadores_rras <- function(df) {
  obitos_rras <- df %>%
    filter(evento == 1L, !is.na(rras_id)) %>%
    count(rras_id, rras_nome, name = "obitos_fetais")

  nascidos_rras <- df %>%
    filter(evento == 0L, !is.na(rras_id)) %>%
    count(rras_id, rras_nome, name = "nascidos_vivos")

  prematuridade_rras <- df %>%
    filter(evento == 0L, !is.na(rras_id)) %>%
    group_by(rras_id, rras_nome) %>%
    summarise(
      n_lv_semagestac = sum(!is.na(semagestac)),
      prematuros_lv = sum(semagestac < 37, na.rm = TRUE),
      perc_prematuridade_lv = 100 * prematuros_lv / n_lv_semagestac,
      .groups = "drop"
    )

  indicadores <- full_join(obitos_rras, nascidos_rras, by = c("rras_id", "rras_nome")) %>%
    full_join(prematuridade_rras, by = c("rras_id", "rras_nome")) %>%
    mutate(
      obitos_fetais = coalesce(obitos_fetais, 0L),
      nascidos_vivos = coalesce(nascidos_vivos, 0L),
      razao_mortalidade_fetal_1000_lv = 1000 * obitos_fetais / nascidos_vivos,
      perc_obitos_total = 100 * obitos_fetais / sum(obitos_fetais),
      perc_nascidos_total = 100 * nascidos_vivos / sum(nascidos_vivos)
    )

  if (file.exists("POP2022_Municipios_20230622.xlsx")) {
    pop_rras <- readxl::read_excel("POP2022_Municipios_20230622.xlsx") %>%
      filter(UF == "SP") %>%
      rename(
        cod_uf = `COD. UF`,
        cod_munic = `COD. MUNIC`,
        pop_txt = `POPULAÇÃO`
      ) %>%
      mutate(
        cod_uf = str_pad(as.character(cod_uf), 2, pad = "0"),
        cod_munic = str_pad(as.character(cod_munic), 5, pad = "0"),
        codmunres = str_sub(paste0(cod_uf, cod_munic), 1, 6),
        pop = suppressWarnings(as.numeric(pop_txt))
      ) %>%
      select(codmunres, pop) %>%
      left_join(
        df %>% distinct(codmunres, rras_id, rras_nome),
        by = "codmunres"
      ) %>%
      filter(!is.na(rras_id)) %>%
      group_by(rras_id, rras_nome) %>%
      summarise(pop_rras = sum(pop, na.rm = TRUE), .groups = "drop")

    indicadores <- indicadores %>%
      left_join(pop_rras, by = c("rras_id", "rras_nome")) %>%
      mutate(nascidos_vivos_1000_pop = 1000 * nascidos_vivos / pop_rras)
  }

  indicadores %>% arrange(rras_id)
}

indicadores_rras <- criar_indicadores_rras(dados_simsinasc_comum_pos)

cat("\nIndicadores por RRAS\n")
print(tibble::as_tibble(indicadores_rras), n = Inf)

# =============================================================================
# 10. Kaplan-Meier para bases isoladas
# =============================================================================

# Replica dos gráficos gerados em selecao_dados_SINASC e selecao_dados_SIM
configurar_km_isolado <- function(dados_modelo, var, titulo, ylab) {
  max_tempo <- max(dados_modelo$tempo, na.rm = TRUE)

  configs_comuns_sim <- list(
    sexo = list(
      title = "Curva empírica do tempo até o óbito fetal por sexo",
      ylab = "S(t) empírico (óbitos fetais)"
    ),
    escmae2010 = list(
      title = "Curva empírica por escolaridade materna",
      ylab = "S(t) empírico (óbitos fetais)"
    ),
    gravidez = list(
      title = "Curva empírica por tipo de gravidez",
      ylab = "S(t) empírico (óbitos fetais)"
    ),
    parto = list(
      title = "Curva empírica por tipo de parto",
      ylab = "S(t) empírico (óbitos fetais)"
    ),
    lococornasc = list(
      title = "Curva empírica por local de ocorrência",
      ylab = "S(t) empírico (óbitos fetais)"
    ),
    idademae_categorico = list(
      title = "Curva empírica por faixa etária da mãe",
      ylab = "S(t) empírico (óbitos fetais)"
    ),
    peso_categorico = list(
      title = "Curva empírica por faixa de peso ao nascer",
      ylab = "S(t) empírico (óbitos fetais)"
    ),
    qtdfilvivo_categorico = list(
      title = "Curva empírica por presença de filhos vivos",
      ylab = "S(t) empírico (óbitos fetais)"
    ),
    qtdfilmort_categorico = list(
      title = "Curva empírica por presença de filhos mortos",
      ylab = "S(t) empírico (óbitos fetais)"
    ),
    obitoparto = list(
      title = "KM (sem censura) - óbito fetal (por momento do óbito em relação ao parto)",
      ylab = "S(t)",
      xlim = c(19, 45),
      break.time.by = 5,
      ggtheme = theme_minimal(base_size = 12)
    )
  )

  configs_comuns_sinasc <- list(
    sexo = list(
      title = "Curva empírica do tempo até o parto por sexo",
      ylab = "S(t) empírico (nascidos vivos)"
    ),
    escmae2010 = list(
      title = "Curva empírica por escolaridade materna",
      ylab = "S(t) empírico (nascidos vivos)"
    ),
    gravidez = list(
      title = "Curva empírica por tipo de gravidez",
      ylab = "S(t) empírico (nascidos vivos)"
    ),
    parto = list(
      title = "Curva empírica por tipo de parto",
      ylab = "S(t) empírico (nascidos vivos)"
    ),
    lococornasc = list(
      title = "Curva empírica por local de ocorrência",
      ylab = "S(t) empírico (nascidos vivos)"
    ),
    idademae_categorico = list(
      title = "Curva empírica por faixa etária da mãe",
      ylab = "S(t) empírico (nascidos vivos)"
    ),
    peso_categorico = list(
      title = "Curva empírica por faixa de peso ao nascer",
      ylab = "S(t) empírico (nascidos vivos)"
    ),
    qtdfilvivo_categorico = list(
      title = "Curva empírica por presença de filhos vivos prévios",
      ylab = "S(t) empírico (nascidos vivos)"
    ),
    qtdfilmort_categorico = list(
      title = "Curva empírica por presença de filhos mortos prévios",
      ylab = "S(t) empírico (nascidos vivos)"
    )
  )

  configs_exclusivas_sinasc <- list(
    consultas = list(
      title = "KM (sem censura) - nascido vivo (por número de consultas pré-natais)",
      ylab = "S(t)",
      xlim = c(24, max_tempo),
      break.time.by = 5,
      ggtheme = theme_minimal(base_size = 12)
    ),
    idanomal = list(
      title = "KM (sem censura) - nascido vivo (por presença de anomalias congênitas)",
      ylab = "S(t)",
      xlim = c(24, max_tempo),
      break.time.by = 5,
      ggtheme = theme_minimal(base_size = 12)
    ),
    racacormae = list(
      title = "KM (sem censura) - nascido vivo (por raça/cor da mãe)",
      ylab = "S(t)",
      xlim = c(24, max_tempo),
      break.time.by = 5,
      ggtheme = theme_minimal(base_size = 12)
    ),
    sttrabpart = list(
      title = "KM (sem censura) - nascido vivo (por indução do trabalho de parto)",
      ylab = "S(t) empírico",
      xlim = c(24, max_tempo),
      break.time.by = 5,
      ggtheme = theme_minimal(base_size = 12)
    ),
    stcesparto = list(
      title = "KM (sem censura) - nascido vivo (por realização de cesárea programada)",
      ylab = "S(t)",
      xlim = c(24, max_tempo),
      break.time.by = 5,
      ggtheme = theme_minimal(base_size = 12)
    ),
    paridade = list(
      title = "KM (sem censura) - nascido vivo (por paridade)",
      ylab = "S(t)",
      xlim = c(24, max_tempo),
      break.time.by = 5,
      ggtheme = theme_minimal(base_size = 12)
    ),
    apgar5_categorico = list(
      title = "KM (sem censura) - nascido vivo (por escore Apgar aos 5 minutos)",
      ylab = "S(t)",
      xlim = c(20, max_tempo),
      break.time.by = 5,
      ggtheme = theme_minimal(base_size = 12)
    ),
    qtdgestant_categorico = list(
      title = "KM (sem censura) - nascido vivo (por número de gestações anteriores)",
      ylab = "S(t)",
      xlim = c(24, max_tempo),
      break.time.by = 5,
      ggtheme = theme_minimal(base_size = 12)
    ),
    qtdpartnor_categorico = list(
      title = "KM (sem censura) - nascido vivo (por número de partos vaginais anteriores)",
      ylab = "S(t)",
      xlim = c(29, max_tempo),
      break.time.by = 5,
      ggtheme = theme_minimal(base_size = 12)
    ),
    qtdpartces_categorico = list(
      title = "KM (sem censura) - nascido vivo (por número de partos cesáreos anteriores)",
      ylab = "S(t)",
      xlim = c(24, max_tempo),
      break.time.by = 5,
      ggtheme = theme_minimal(base_size = 12)
    ),
    mesprenat_categorico = list(
      title = "KM (sem censura) - nascido vivo (por trimestre de início do pré-natal)",
      ylab = "S(t)",
      xlim = c(24, max_tempo),
      break.time.by = 5,
      ggtheme = theme_minimal(base_size = 12)
    )
  )

  configs_base <- if ("obitoparto" %in% names(dados_modelo)) {
    configs_comuns_sim
  } else if ("consultas" %in% names(dados_modelo)) {
    c(configs_comuns_sinasc, configs_exclusivas_sinasc)
  } else {
    list()
  }

  config <- configs_base[[var]]
  if (is.null(config)) {
    config <- list(title = titulo, ylab = ylab)
  }

  config$legend.title <- ""
  config$legend.labs <- if (is.factor(dados_modelo[[var]])) {
    levels(dados_modelo[[var]])
  } else {
    sort(unique(as.character(dados_modelo[[var]])))
  }
  if (is.null(config$ggtheme)) {
    config$ggtheme <- theme_minimal()
  }
  config
}

# Função para chamar cada variável por vez
ajustar_km_isolado <- function(df, var, titulo = var, ylab = "S(t)") {
  dados_modelo <- df %>%
    filter(!is.na(tempo), !is.na(status_evento), !is.na(.data[[var]]))

  if (n_distinct(dados_modelo[[var]]) < 2) {
    message("Variavel ignorada por ter menos de duas categorias: ", var)
    return(invisible(NULL))
  }

  proporcoes_categoria <- dados_modelo %>%
    count(categoria = as.character(.data[[var]]), name = "n") %>%
    mutate(
      proporcao = n / sum(n),
      percentual = scales::percent(proporcao, accuracy = 0.1)
    )

  formula_km <- as.formula(paste0("Surv(tempo, status_evento) ~ ", var))
  ajuste <- survfit(formula_km, data = dados_modelo)
  # O ggsurvplot precisa encontrar a formula expandida dentro do objeto survfit.
  # Sem esta linha, ele pode tentar ler apenas o nome "formula_km" e falhar.
  ajuste$call$formula <- formula_km
  teste_logrank <- survdiff(formula_km, data = dados_modelo)
  p_valor <- 1 - pchisq(teste_logrank$chisq, df = length(teste_logrank$n) - 1)

  config_grafico <- configurar_km_isolado(dados_modelo, var, titulo, ylab)
  args_grafico <- c(
    list(
      ajuste,
      data = dados_modelo,
      conf.int = FALSE,
      xlab = "Semanas de gestação"
    ),
    config_grafico
  )

  grafico <- do.call(ggsurvplot, args_grafico)

  print(grafico)
  cat("\nProporcao das categorias - ", var, "\n", sep = "")
  print(proporcoes_categoria)
  print(resumir_tempo_por_grupo(dados_modelo, var))
  print(teste_logrank)
  cat("p-valor:", signif(p_valor, 3), "\n")

  invisible(list(
    ajuste = ajuste,
    grafico = grafico,
    proporcoes = proporcoes_categoria,
    logrank = teste_logrank,
    p_valor = p_valor
  ))
}

# Para visualizar uma curva isolada por vez, veja o exemplo abaixo:
# ajustar_km_isolado(dados_sim_pos, "obitoparto")
# ajustar_km_isolado(dados_sinasc_pos, "consultas")

# =============================================================================
# 11. Kaplan-Meier e riscos competitivos na base combinada
# =============================================================================

km_global_fd <- survfit(Surv(tempo, status_fd) ~ 1, data = dados_simsinasc_comum_pos)
km_global_lv <- survfit(Surv(tempo, status_lv) ~ 1, data = dados_simsinasc_comum_pos)

# Replica dos gráficos gerados em selecao_dados_SIMSINASC
configurar_km_competitivo <- function(var) {
  configs <- list(
    sexo = list(
      rotulo = "sexo",
      legend.title = "",
      ylim_fd = c(0.987, 1.00)
    ),
    idademae_categorico = list(
      rotulo = "faixa etária materna",
      legend.title = "Faixa etária materna",
      ylim_fd = c(0.985, 1.00)
    ),
    escmae2010 = list(
      rotulo = "escolaridade materna",
      legend.title = "Escolaridade materna",
      ylim_fd = c(0.975, 1.00)
    ),
    qtdfilvivo_categorico = list(
      rotulo = "presença de filhos vivos previamente",
      legend.title = "Filhos vivos previamente",
      ylim_fd = c(0.987, 1.00)
    ),
    qtdfilmort_categorico = list(
      rotulo = "presença de filhos mortos previamente",
      legend.title = "Filhos mortos previamente",
      ylim_fd = c(0.975, 1.00)
    ),
    gravidez = list(
      rotulo = "tipo de gravidez",
      legend.title = "Tipo de gravidez",
      ylim_fd = c(0.950, 1.00)
    ),
    parto = list(
      rotulo = "tipo de parto",
      legend.title = "Tipo de parto",
      ylim_fd = c(0.979, 1.00)
    ),
    peso_categorico = list(
      rotulo = "faixa de peso ao nascer",
      legend.title = "Peso ao nascer",
      ylim_fd = c(0.900, 1.00)
    ),
    lococornasc = list(
      rotulo = "local de ocorrência do parto",
      legend.title = "Local de ocorrência",
      ylim_fd = c(0.950, 1.00)
    )
  )

  config <- configs[[var]]
  if (is.null(config)) {
    config <- list(
      rotulo = var,
      legend.title = var,
      ylim_fd = c(0.985, 1.00)
    )
  }
  config
}

# Função para chamar cada variável por vez
ajustar_km_competitivo <- function(df, var) {
  dados_modelo <- df %>%
    filter(!is.na(tempo), !is.na(.data[[var]]), !is.na(status_fd), !is.na(status_lv))

  if (n_distinct(dados_modelo[[var]]) < 2) {
    message("Variavel ignorada por ter menos de duas categorias: ", var)
    return(invisible(NULL))
  }

  proporcoes_categoria <- dados_modelo %>%
    count(categoria = as.character(.data[[var]]), name = "n") %>%
    mutate(
      proporcao = n / sum(n),
      percentual = scales::percent(proporcao, accuracy = 0.1)
    )

  formula_fd <- as.formula(paste0("Surv(tempo, status_fd) ~ ", var))
  formula_lv <- as.formula(paste0("Surv(tempo, status_lv) ~ ", var))

  km_fd <- survfit(formula_fd, data = dados_modelo)
  km_lv <- survfit(formula_lv, data = dados_modelo)
  # O ggsurvplot precisa encontrar as formulas expandidas dentro dos objetos.
  km_fd$call$formula <- formula_fd
  km_lv$call$formula <- formula_lv

  config_grafico <- configurar_km_competitivo(var)
  max_tempo <- max(dados_modelo$tempo, na.rm = TRUE)
  legend_labs <- if (is.factor(dados_modelo[[var]])) {
    levels(dados_modelo[[var]])
  } else {
    sort(unique(as.character(dados_modelo[[var]])))
  }

  grafico_fd <- ggsurvplot(
    km_fd,
    data = dados_modelo,
    conf.int = FALSE,
    xlab = "Semanas de gestação",
    ylab = "S(t)",
    title = paste0("KM - Óbito fetal (censura: nascido vivo)\npor ", config_grafico$rotulo),
    legend.title = config_grafico$legend.title,
    legend.labs = legend_labs,
    ylim = config_grafico$ylim_fd,
    xlim = c(19, max_tempo),
    break.time.by = 5,
    ggtheme = theme_minimal(base_size = 12)
  )

  grafico_lv <- ggsurvplot(
    km_lv,
    data = dados_modelo,
    conf.int = FALSE,
    xlab = "Semanas de gestação",
    ylab = "S(t)",
    title = paste0("KM - Nascido vivo (censura: óbito fetal)\npor ", config_grafico$rotulo),
    legend.title = config_grafico$legend.title,
    legend.labs = legend_labs,
    ylim = c(0, 1),
    xlim = c(24, max_tempo),
    break.time.by = 5,
    ggtheme = theme_minimal(base_size = 12)
  )

  p1 <- grafico_fd$plot +
    theme(plot.margin = grid::unit(c(5.5, 10, 5.5, 5.5), "pt"))
  p2 <- grafico_lv$plot +
    theme(plot.margin = grid::unit(c(5.5, 5.5, 5.5, 10), "pt"))

  painel <- ggpubr::ggarrange(
    p1, p2,
    ncol = 2,
    nrow = 1,
    common.legend = TRUE,
    legend = "bottom"
  )

  print(grafico_fd)
  print(grafico_lv)
  print(painel)
  cat("\nProporcao das categorias - ", var, "\n", sep = "")
  print(proporcoes_categoria)
  print(resumir_tempo_por_grupo(dados_modelo, var))

  logrank_fd <- survdiff(formula_fd, data = dados_modelo)
  logrank_lv <- survdiff(formula_lv, data = dados_modelo)

  print(logrank_fd)
  print(logrank_lv)

  if (requireNamespace("cmprsk", quietly = TRUE)) {
    teste_gray <- cmprsk::cuminc(
      ftime = dados_modelo$tempo,
      fstatus = dados_modelo$fstatus_cr,
      group = dados_modelo[[var]],
      cencode = "Censura"
    )
    print(teste_gray)
  } else {
    teste_gray <- NULL
    message("Pacote cmprsk nao instalado; teste de Gray nao executado.")
  }

  invisible(list(
    km_fd = km_fd,
    km_lv = km_lv,
    grafico_fd = grafico_fd,
    grafico_lv = grafico_lv,
    painel = painel,
    proporcoes = proporcoes_categoria,
    logrank_fd = logrank_fd,
    logrank_lv = logrank_lv,
    gray = teste_gray
  ))
}

# Para visualizar as analises combinadas uma por vez, siga os exemplos abaixo:
# ajustar_km_competitivo(dados_simsinasc_comum_pos, "sexo")
# ajustar_km_competitivo(dados_simsinasc_comum_pos, "escmae2010")
# ajustar_km_competitivo(dados_simsinasc_comum_pos, "lococornasc")

# =============================================================================
# 12. Funções de incidencia acumulada
# =============================================================================

# CIFs
montar_df_cif <- function(ajuste_cif, evento, niveis_grupo) {
  curvas_evento <- ajuste_cif[grep(paste0(evento, "$"), names(ajuste_cif))]

  purrr::map_dfr(
    names(curvas_evento),
    function(nm) {
      curva <- curvas_evento[[nm]]
      grupo <- sub(paste0(" ", evento, "$"), "", nm)

      tibble::tibble(
        tempo = curva$time,
        cif = curva$est,
        grupo = grupo
      )
    }
  ) %>%
    mutate(
      grupo = factor(grupo, levels = niveis_grupo)
    ) %>%
    arrange(grupo, tempo)
}

# Replica dos gráficos gerados em selecao_dados_SIMSINASC
configurar_cif_variavel <- function(var) {
  configs <- list(
    sexo = list(
      niveis = c("Masculino", "Feminino"),
      legenda = "Sexo",
      linhas = c("Masculino" = "dashed", "Feminino" = "solid"),
      titulo_fd = "Óbito fetal (por sexo)",
      titulo_lv = "Nascido vivo (por sexo)",
      sobrescrever_linhas = c("dashed", "solid")
    ),
    idademae_categorico = list(
      niveis = c("<20", "20–34", "35+"),
      legenda = "Faixa etária materna",
      linhas = c("<20" = "solid", "20–34" = "dashed", "35+" = "dotted"),
      titulo_fd = "Óbito fetal (por faixa etária materna)",
      titulo_lv = "Nascido vivo (por faixa etária materna)",
      sobrescrever_linhas = c("solid", "dashed", "dotted")
    ),
    escmae2010 = list(
      niveis = c("Baixa escolaridade", "Média escolaridade", "Alta escolaridade"),
      legenda = "Escolaridade materna",
      linhas = c(
        "Baixa escolaridade" = "solid",
        "Média escolaridade" = "dashed",
        "Alta escolaridade" = "dotted"
      ),
      titulo_fd = "Óbito fetal (por escolaridade materna)",
      titulo_lv = "Nascido vivo (por escolaridade materna)"
    ),
    qtdfilvivo_categorico = list(
      niveis = c("Não", "Sim"),
      legenda = "Filhos vivos previamente",
      linhas = c("Não" = "solid", "Sim" = "dashed"),
      titulo_fd = "Óbito fetal (por presença de filhos\nvivos previamente)",
      titulo_lv = "Nascido vivo (por presença de filhos\nvivos previamente)"
    ),
    qtdfilmort_categorico = list(
      niveis = c("Não", "Sim"),
      legenda = "Filhos mortos previamente",
      linhas = c("Não" = "solid", "Sim" = "dashed"),
      titulo_fd = "Óbito fetal (por presença de filhos\nmortos previamente)",
      titulo_lv = "Nascido vivo (por presença de filhos\nmortos previamente)"
    ),
    gravidez = list(
      niveis = c("Única", "Múltipla"),
      legenda = "Tipo de gravidez",
      linhas = c("Única" = "solid", "Múltipla" = "dashed"),
      titulo_fd = "Óbito fetal (por tipo de gravidez)",
      titulo_lv = "Nascido vivo (por tipo de gravidez)"
    ),
    parto = list(
      niveis = c("Vaginal", "Cesáreo"),
      legenda = "Tipo de parto",
      linhas = c("Vaginal" = "solid", "Cesáreo" = "dashed"),
      titulo_fd = "Óbito fetal (por tipo de parto)",
      titulo_lv = "Nascido vivo (por tipo de parto)"
    ),
    peso_categorico = list(
      niveis = c("<2500g", "2500g-3999g", "4000g+"),
      legenda = "Peso ao nascer",
      linhas = c(
        "<2500g" = "solid",
        "2500g-3999g" = "dashed",
        "4000g+" = "dotted"
      ),
      titulo_fd = "Óbito fetal (por faixa de peso)",
      titulo_lv = "Nascido vivo (por faixa de peso)"
    ),
    lococornasc = list(
      niveis = c("Hospital", "Outros"),
      legenda = "Local de ocorrência",
      linhas = c("Hospital" = "solid", "Outros" = "dashed"),
      titulo_fd = "Óbito fetal (por local de ocorrência)",
      titulo_lv = "Nascido vivo (por local de ocorrência)"
    )
  )

  config <- configs[[var]]
  if (is.null(config)) {
    stop("Variável sem configuração de CIF definida: ", var)
  }
  config
}

criar_plot_cif <- function(df_cif, config, titulo, cor, xlim_inicio, max_tempo_cif) {
  guia_linhas <- list(colour = "grey20")
  if (!is.null(config$sobrescrever_linhas)) {
    guia_linhas$linetype <- config$sobrescrever_linhas
  }

  ggplot(
    df_cif,
    aes(
      x = tempo,
      y = cif,
      linetype = grupo
    )
  ) +
    geom_step(
      color = cor,
      linewidth = 0.9
    ) +
    scale_linetype_manual(
      name = config$legenda,
      values = config$linhas,
      breaks = config$niveis
    ) +
    scale_x_continuous(
      breaks = seq(20, max_tempo_cif, by = 5)
    ) +
    coord_cartesian(
      xlim = c(xlim_inicio, max_tempo_cif)
    ) +
    labs(
      title = titulo,
      x = "Semanas de gestação",
      y = "Incidência acumulada"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.key.width = grid::unit(1.5, "cm")
    ) +
    guides(
      linetype = guide_legend(
        override.aes = guia_linhas
      )
    )
}

# Função para chamar output para uma variável escolhida
ajustar_cif_global <- function(df) {
  if (!requireNamespace("cmprsk", quietly = TRUE)) {
    message("Pacote cmprsk nao instalado; CIF nao executada.")
    return(invisible(NULL))
  }

  dados_modelo <- df %>%
    filter(!is.na(tempo), !is.na(fstatus_cr))

  ajuste_cif <- cmprsk::cuminc(
    ftime = dados_modelo$tempo,
    fstatus = dados_modelo$fstatus_cr,
    cencode = "Censura"
  )

  print(names(ajuste_cif))

  ci_fd <- ajuste_cif[grep("Óbito fetal", names(ajuste_cif))][[1]]
  ci_lv <- ajuste_cif[grep("Nascimento vivo", names(ajuste_cif))][[1]]

  df_fd <- tibble::tibble(tempo = ci_fd$time, cif = ci_fd$est)
  df_lv <- tibble::tibble(tempo = ci_lv$time, cif = ci_lv$est)

  max_tempo_cif <- max(df_fd$tempo, df_lv$tempo, na.rm = TRUE)
  breaks_tempo_cif <- seq(20, max_tempo_cif, by = 5)

  p_fd <- ggplot(df_fd, aes(x = tempo, y = cif)) +
    geom_step(
      color = "#d62728",
      linewidth = 0.9
    ) +
    scale_x_continuous(
      breaks = breaks_tempo_cif
    ) +
    scale_y_continuous(
      limits = c(0, max(df_fd$cif, na.rm = TRUE) * 1.05),
      expand = expansion(mult = c(0, 0.02))
    ) +
    coord_cartesian(xlim = c(19, max_tempo_cif)) +
    labs(
      title = "Óbito fetal",
      x = "Semanas de gestação",
      y = "Incidência acumulada"
    ) +
    theme_minimal(base_size = 12)

  p_lv <- ggplot(df_lv, aes(x = tempo, y = cif)) +
    geom_step(
      color = "#1f77b4",
      linewidth = 0.9
    ) +
    scale_x_continuous(
      breaks = breaks_tempo_cif
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      expand = expansion(mult = c(0, 0.02))
    ) +
    coord_cartesian(xlim = c(24, max_tempo_cif)) +
    labs(
      title = "Nascido vivo",
      x = "Semanas de gestação",
      y = "Incidência acumulada"
    ) +
    theme_minimal(base_size = 12)

  painel <- ggpubr::ggarrange(
    p_fd,
    p_lv,
    ncol = 2,
    nrow = 1,
    common.legend = FALSE
  )

  print(painel)

  invisible(list(
    cif = ajuste_cif,
    grafico_fd = p_fd,
    grafico_lv = p_lv,
    painel = painel
  ))
}

# Função para chamar output para uma variável escolhida
ajustar_cif_variavel <- function(df, var) {
  if (!requireNamespace("cmprsk", quietly = TRUE)) {
    message("Pacote cmprsk nao instalado; CIF nao executada.")
    return(invisible(NULL))
  }

  dados_modelo <- df %>%
    filter(!is.na(tempo), !is.na(fstatus_cr), !is.na(.data[[var]]))

  if (n_distinct(dados_modelo[[var]]) < 2) {
    message("Variavel ignorada por ter menos de duas categorias: ", var)
    return(invisible(NULL))
  }

  config <- configurar_cif_variavel(var)

  ajuste_cif <- cmprsk::cuminc(
    ftime = dados_modelo$tempo,
    fstatus = dados_modelo$fstatus_cr,
    group = dados_modelo[[var]],
    cencode = "Censura"
  )

  print(ajuste_cif)
  print(ajuste_cif$Tests)

  df_fd <- montar_df_cif(ajuste_cif, "Óbito fetal", config$niveis)
  df_lv <- montar_df_cif(ajuste_cif, "Nascimento vivo", config$niveis)

  print(table(df_fd$grupo, useNA = "ifany"))
  print(table(df_lv$grupo, useNA = "ifany"))

  max_tempo_cif <- max(df_fd$tempo, df_lv$tempo, na.rm = TRUE)

  p_fd <- criar_plot_cif(
    df_fd,
    config,
    config$titulo_fd,
    "#d62728",
    19,
    max_tempo_cif
  )

  p_lv <- criar_plot_cif(
    df_lv,
    config,
    config$titulo_lv,
    "#1f77b4",
    24,
    max_tempo_cif
  )

  print(p_fd)
  print(p_lv)

  painel <- ggpubr::ggarrange(
    p_fd,
    p_lv,
    ncol = 2,
    nrow = 1,
    common.legend = TRUE,
    legend = "bottom"
  )

  print(painel)

  invisible(list(
    cif = ajuste_cif,
    grafico_fd = p_fd,
    grafico_lv = p_lv,
    painel = painel
  ))
}

# Para visualizar uma CIF por vez, veja os exemplos abaixo:
# ajustar_cif_global(dados_simsinasc_comum_pos)
# ajustar_cif_variavel(dados_simsinasc_comum_pos, "sexo")
# ajustar_cif_variavel(dados_simsinasc_comum_pos, "escmae2010")
# ajustar_cif_variavel(dados_simsinasc_comum_pos, "lococornasc")