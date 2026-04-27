library(microdatasus)
library(dplyr)
library(purrr)
library(survival)
library(survminer)
library(RColorBrewer)
library(scales)
library(sf)
library(ggplot2)
library(stringr)
library(readxl)
library(ggrepel)

# Padronização de código municipal (6 dígitos)
padronizar_codmun6 <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_pad(x, width = 6, pad = "0")
  x
}

ler_mapeamento_rras_oficial <- function(caminho_excel) {
  mapa_raw <- readxl::read_excel(caminho_excel)

  if (ncol(mapa_raw) < 5) {
    stop("O arquivo RRAS-MUNICIPIO.xlsx não possui as colunas esperadas.", call. = FALSE)
  }

  names(mapa_raw)[1:5] <- c("cod_ibge", "municipio", "rras", "regiao_de_saude", "drs")

  mapa_raw %>%
    transmute(
      cod_mun6 = padronizar_codmun6(cod_ibge),
      municipio_residencia = str_trim(as.character(municipio)),
      rras_nome = str_trim(as.character(rras)),
      regiao_de_saude = str_trim(as.character(regiao_de_saude)),
      drs = str_trim(as.character(drs)),
      rras_id = suppressWarnings(as.integer(str_extract(as.character(rras), "\\d+")))
    ) %>%
    filter(!is.na(cod_mun6), cod_mun6 != "", !is.na(rras_id), !is.na(rras_nome)) %>%
    distinct(cod_mun6, .keep_all = TRUE)
}

anexar_rras_oficial <- function(df, mapa_rras, codmun_col = "codmunres") {
  stopifnot(codmun_col %in% names(df))

  df %>%
    mutate(codmunres = padronizar_codmun6(.data[[codmun_col]])) %>%
    select(-any_of(c("municipio_residencia", "rras_id", "rras_nome", "regiao_de_saude", "drs"))) %>%
    left_join(mapa_rras, by = c("codmunres" = "cod_mun6"))
}

caminho_rras_oficial <- "suporte/shinyremap/inst/app/data/RRAS-MUNICIPIO.xlsx"
mapa_rras_oficial <- ler_mapeamento_rras_oficial(caminho_rras_oficial)

dados_simsp <- readRDS("dataset_sim_df.rds") # selecao_dados_SIM
dados_simsp <- anexar_rras_oficial(dados_simsp, mapa_rras_oficial)

dados_sinascsp <- readRDS("dataset_sinasc_df.rds") # selecao_dados_SINASC
dados_sinascsp <- anexar_rras_oficial(dados_sinascsp, mapa_rras_oficial)

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

#-------------------------------- RRAS -----------------------------------------
# FUNÇÕES DE APOIO

# Função para frequências por RRAS 
contar_rras <- function(df, id_col = "rras_id", nome_col = "rras_nome") {
  stopifnot(id_col %in% names(df))
  has_nome <- nome_col %in% names(df)
  
  df <- df %>%
    mutate(
      .rras_id   = as.integer(.data[[id_col]]),
      .rras_nome = if (has_nome) as.character(.data[[nome_col]]) else NA_character_
    )
  
  total <- nrow(df)
  
  out <- df %>%
    filter(!is.na(.rras_id)) %>%
    group_by(.rras_id, .rras_nome) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(.rras_id) %>%
    mutate(
      freq = n / total,
      perc = 100 * freq
    )
  
  if (!has_nome) {
    out <- out %>% select(rras_id = .rras_id, n, freq, perc)
  } else {
    out <- out %>% select(rras_id = .rras_id, rras_nome = .rras_nome, n, freq, perc)
  }
  
  attr(out, "total_linhas_df") <- total
  out
}

formatar_tabela_rras <- function(tab) {
  tab %>%
    arrange(desc(n)) %>%
    mutate(
      freq = round(freq, 6),
      perc = round(perc, 2)
    )
}

# =============================================================================
# CONFIGURAÇÃO 
# =============================================================================
caminho_shape_mun_sp <- "SP_Municipios_2024/SP_Municipios_2024.shp"
id_mun_shape         <- "CD_MUN"     # coluna de município no shapefile
dir_saida_analise    <- file.path("scripts", "analysis_outputs")
dir_saida_figuras    <- file.path("trabalho_LaTeX", "figuras")

dir.create(dir_saida_analise, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_saida_figuras, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# TABELAS SIMPLES POR RRAS
# =============================================================================
tabela_rras_sim    <- contar_rras(dados_simsp_comum)
tabela_rras_sinasc <- contar_rras(dados_sinascsp_comum)

print(formatar_tabela_rras(tabela_rras_sim), n = Inf)
print(formatar_tabela_rras(tabela_rras_sinasc), n = Inf)

# =============================================================================
# LER SHAPE MUNICIPAL
# =============================================================================
mun_sp_sf <- st_read(caminho_shape_mun_sp, quiet = TRUE)
stopifnot(id_mun_shape %in% names(mun_sp_sf))

mun_sp_sf <- mun_sp_sf %>%
  mutate(
    cod_mun7 = str_pad(as.character(.data[[id_mun_shape]]), width = 7, pad = "0"),
    cod_mun6 = str_sub(cod_mun7, 1, 6)
  )

# =============================================================================
# CONSTRUIR TABELA MUNICÍPIO -> RRAS A PARTIR DA TABELA OFICIAL
# =============================================================================
map_mun_rras <- mapa_rras_oficial %>%
  select(cod_mun6, municipio_residencia, rras_id, rras_nome, regiao_de_saude, drs)

write.csv(
  map_mun_rras,
  file = file.path(dir_saida_analise, "mapa_municipio_rras_oficial.csv"),
  row.names = FALSE
)

# =============================================================================
# VERIFICAR COBERTURA DO MAPEAMENTO
# =============================================================================
mun_sem_rras <- mun_sp_sf %>%
  st_drop_geometry() %>%
  distinct(cod_mun6) %>%
  left_join(map_mun_rras, by = "cod_mun6") %>%
  filter(is.na(rras_id))

if (nrow(mun_sem_rras) > 0) {
  message("ATENÇÃO: Existem municípios no shapefile sem RRAS no mapeamento oficial.")
  message("Quantidade de municípios sem RRAS no join: ", nrow(mun_sem_rras))
} else {
  message("OK: todos os municípios do shapefile receberam RRAS via mapeamento oficial.")
}

# =============================================================================
# JUNTAR SHAPE MUNICIPAL COM RRAS E DISSOLVER
# =============================================================================
mun_sp_rras_sf <- mun_sp_sf %>%
  left_join(map_mun_rras, by = "cod_mun6")

mun_sp_rras_sf <- st_make_valid(mun_sp_rras_sf)

rras_sf <- mun_sp_rras_sf %>%
  filter(!is.na(rras_id)) %>%
  group_by(rras_id) %>%
  summarise(
    rras_nome = dplyr::first(na.omit(rras_nome)),
    geometry  = st_union(geometry),
    .groups   = "drop"
  ) %>%
  mutate(
    rras_nome = ifelse(is.na(rras_nome), paste0("RRAS ", rras_id), rras_nome)
  )

print(rras_sf)

# =============================================================================
# CONTAGENS: ÓBITO FETAL (SIM) E NASCIDO VIVO (SINASC)
# =============================================================================
obitos_rras <- dados_simsp_comum %>%
  filter(!is.na(rras_id)) %>%
  count(rras_id, rras_nome, name = "n_of")

nascidos_rras <- dados_sinascsp_comum %>%
  filter(!is.na(rras_id)) %>%
  count(rras_id, rras_nome, name = "n_lv")

prematuridade_rras <- dados_sinascsp_comum %>%
  filter(!is.na(rras_id)) %>%
  mutate(semagestac = suppressWarnings(as.numeric(semagestac))) %>%
  group_by(rras_id, rras_nome) %>%
  summarise(
    n_lv_semagestac = sum(!is.na(semagestac)),
    n_prematuro_lv = sum(semagestac < 37, na.rm = TRUE),
    perc_prematuridade_lv = 100 * n_prematuro_lv / n_lv_semagestac,
    .groups = "drop"
  )

# =============================================================================
# POPULAÇÃO POR RRAS (CENSO 2022 agregada)
# =============================================================================
pop_raw <- readxl::read_excel("POP2022_Municipios_20230622.xlsx") %>%
  filter(UF == "SP")

pop_df <- pop_raw %>%
  rename(
    uf        = UF,
    cod_uf    = `COD. UF`,
    cod_munic = `COD. MUNIC`,
    pop_txt   = POPULAÇÃO
  ) %>%
  mutate(
    cod_uf    = str_pad(as.character(cod_uf), 2, pad = "0"),
    cod_munic = str_pad(as.character(cod_munic), 5, pad = "0"),
    cod_mun7  = paste0(cod_uf, cod_munic),
    cod_mun6  = str_sub(cod_mun7, 1, 6),
    pop       = suppressWarnings(as.numeric(pop_txt))
  ) %>%
  select(cod_mun6, pop)

map_mun_rras_std <- map_mun_rras %>%
  mutate(cod_mun6 = str_pad(as.character(cod_mun6), 6, pad = "0")) %>%
  select(cod_mun6, rras_id, rras_nome)

pop_rras <- pop_df %>%
  left_join(map_mun_rras_std, by = "cod_mun6") %>%
  group_by(rras_id, rras_nome) %>%
  summarise(pop_rras = sum(pop, na.rm = TRUE), .groups = "drop") %>%
  arrange(rras_id)

# =============================================================================
# INDICADORES FINAIS POR RRAS
# =============================================================================
rras_indicadores <- pop_rras %>%
  left_join(obitos_rras,   by = c("rras_id", "rras_nome")) %>%
  left_join(nascidos_rras, by = c("rras_id", "rras_nome")) %>%
  left_join(prematuridade_rras, by = c("rras_id", "rras_nome")) %>%
  mutate(
    n_of = ifelse(is.na(n_of), 0L, n_of),
    n_lv = ifelse(is.na(n_lv), 0L, n_lv),
    n_lv_semagestac = ifelse(is.na(n_lv_semagestac), 0L, n_lv_semagestac),
    n_prematuro_lv = ifelse(is.na(n_prematuro_lv), 0L, n_prematuro_lv),
    n_total_nasc = n_of + n_lv
  ) %>%
  mutate(
    # Participação no total do estado
    perc_fd_total = 100 * n_of / sum(n_of, na.rm = TRUE),
    perc_lv_total = 100 * n_lv / sum(n_lv, na.rm = TRUE),
    
    # Taxa adotada na dissertação: obitos fetais por 1000 nascidos vivos.
    # O denominador fica explicito para evitar confusao com "nascimentos",
    # que poderia incluir tanto nascidos vivos quanto obitos fetais.
    taxa_mortalidade_fetal_1000_lv = 1000 * n_of / ifelse(n_lv == 0, NA_real_, n_lv),
    
    # Razao classica usando total de nascimentos, mantida apenas para auditoria.
    taxa_fd_1000_total_nasc = 1000 * n_of / n_total_nasc,
    
    # Taxa demográfica de NV por 1000 habitantes
    taxa_lv_1000_pop  = 1000 * n_lv / pop_rras
  )

# =============================================================================
# UNTAR COM O SHAPE DAS RRAS
# =============================================================================
map_rras <- rras_sf %>%
  left_join(rras_indicadores, by = c("rras_id", "rras_nome"))

# =============================================================================
# RÓTULOS COM REPEL (RESOLVE RRAS 1-6 COM LABEL MUITO PRÓXIMAS)
# =============================================================================
# Trabalhar rótulos em CRS projetado melhora muito o posicionamento
map_rras_proj <- st_transform(map_rras, 31983)  # SIRGAS 2000 / UTM 23S

rotulos_rras_proj <- map_rras_proj %>%
  st_point_on_surface() %>%
  mutate(
    rras_nome_lab = str_wrap(rras_nome, width = 18)
  )

# Extrair coordenadas para usar com ggrepel
coords <- st_coordinates(rotulos_rras_proj)

rotulos_rras_df <- rotulos_rras_proj %>%
  st_drop_geometry() %>%
  mutate(
    x = coords[, 1],
    y = coords[, 2]
  )

# =============================================================================
# TEMA
# =============================================================================
tema_mapa <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(linewidth = 0.15),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8),
    
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9),
    
    plot.margin = margin(5.5, 5.5, 5.5, 5.5)
  )

# =============================================================================
# MAPAS LIMPOS (CORES MAIS OPACAS)
# =============================================================================
# Paletas sequenciais com contraste suficiente para leitura impressa e digital.
pal_fd <- "Reds"
pal_lv <- "Blues"

# Mapa A
p_fd <- ggplot() +
  geom_sf(
    data = map_rras_proj,
    aes(fill = perc_fd_total),
    color = "white",
    linewidth = 0.2
  ) +
  geom_label_repel(
    data = rotulos_rras_df,
    aes(x = x, y = y, label = rras_nome_lab),
    size = 2.5,
    fontface = "bold",
    color = "black",
    fill = "white",
    alpha = 0.85,
    label.size = 0.15,
    min.segment.length = 0,
    seed = 123,
    box.padding = 0.35,
    point.padding = 0.25,
    max.overlaps = Inf
  ) +
  scale_fill_distiller(
    name = "% do total\nestadual de óbitos fetais",
    palette = pal_fd,
    direction = 1,
    na.value = "grey90",
    guide = guide_colorbar(barwidth = 6, barheight = 0.45)
  ) +
  labs(
    title    = "Óbitos fetais por RRAS",
    subtitle = "Participação percentual no total do estado"
  ) +
  coord_sf(crs = 31983) +
  tema_mapa

# Mapa B
p_lv <- ggplot() +
  geom_sf(
    data = map_rras_proj,
    aes(fill = perc_lv_total),
    color = "white",
    linewidth = 0.2
  ) +
  geom_label_repel(
    data = rotulos_rras_df,
    aes(x = x, y = y, label = rras_nome_lab),
    size = 2.5,
    fontface = "bold",
    color = "black",
    fill = "white",
    alpha = 0.85,
    label.size = 0.15,
    min.segment.length = 0,
    seed = 123,
    box.padding = 0.35,
    point.padding = 0.25,
    max.overlaps = Inf
  ) +
  scale_fill_distiller(
    name = "% do total\nestadual de nascidos vivos",
    palette = pal_lv,
    direction = 1,
    na.value = "grey90",
    guide = guide_colorbar(barwidth = 6, barheight = 0.45)
  ) +
  labs(
    title    = "Nascidos vivos por RRAS",
    subtitle = "Participação percentual no total do estado"
  ) +
  coord_sf(crs = 31983) +
  tema_mapa

# Painel final
painel <- ggpubr::ggarrange(
  p_fd + theme(legend.position = "bottom"),
  p_lv + theme(legend.position = "bottom"),
  ncol = 2,
  common.legend = FALSE,
  legend = "bottom"
)

print(painel)

dir.create(dir_saida_analise, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_saida_figuras, recursive = TRUE, showWarnings = FALSE)

ggplot2::ggsave(
  filename = file.path(dir_saida_figuras, "mapas_RRAS.png"),
  plot = painel,
  width = 10,
  height = 5,
  units = "in",
  dpi = 300
)

ggplot2::ggsave(
  filename = "mapas_RRAS.png",
  plot = painel,
  width = 10,
  height = 5,
  units = "in",
  dpi = 300
)

# Painel complementar: intensidade relativa dos desfechos por RRAS.
p_taxa_fd <- ggplot() +
  geom_sf(
    data = map_rras_proj,
    aes(fill = taxa_mortalidade_fetal_1000_lv),
    color = "white",
    linewidth = 0.2
  ) +
  geom_label_repel(
    data = rotulos_rras_df,
    aes(x = x, y = y, label = rras_nome_lab),
    size = 2.5,
    fontface = "bold",
    color = "black",
    fill = "white",
    alpha = 0.85,
    label.size = 0.15,
    min.segment.length = 0,
    seed = 123,
    box.padding = 0.35,
    point.padding = 0.25,
    max.overlaps = Inf
  ) +
  scale_fill_distiller(
    name = "Taxa de mortalidade fetal\npor 1000 nascidos vivos",
    palette = "Reds",
    direction = 1,
    na.value = "grey90"
  ) +
  labs(
    title = "Taxa de mortalidade fetal por RRAS",
    subtitle = "Obitos fetais por 1000 nascidos vivos"
  ) +
  coord_sf(crs = 31983) +
  tema_mapa

p_prematuridade <- ggplot() +
  geom_sf(
    data = map_rras_proj,
    aes(fill = perc_prematuridade_lv),
    color = "white",
    linewidth = 0.2
  ) +
  geom_label_repel(
    data = rotulos_rras_df,
    aes(x = x, y = y, label = rras_nome_lab),
    size = 2.5,
    fontface = "bold",
    color = "black",
    fill = "white",
    alpha = 0.85,
    label.size = 0.15,
    min.segment.length = 0,
    seed = 123,
    box.padding = 0.35,
    point.padding = 0.25,
    max.overlaps = Inf
  ) +
  scale_fill_distiller(
    name = "% prematuridade\n(<37 semanas)",
    palette = "YlOrBr",
    direction = 1,
    na.value = "grey90"
  ) +
  labs(
    title = "Prematuridade entre nascidos vivos por RRAS",
    subtitle = "Percentual de nascidos vivos com idade gestacional < 37 semanas"
  ) +
  coord_sf(crs = 31983) +
  tema_mapa

painel_rras_taxas <- ggpubr::ggarrange(
  p_taxa_fd + theme(legend.position = "bottom"),
  p_prematuridade + theme(legend.position = "bottom"),
  ncol = 2,
  common.legend = FALSE,
  legend = "bottom"
)

print(painel_rras_taxas)

ggplot2::ggsave(
  filename = file.path(dir_saida_figuras, "mapas_RRAS_taxas_prematuridade.png"),
  plot = painel_rras_taxas,
  width = 10,
  height = 5,
  units = "in",
  dpi = 300
)

ggplot2::ggsave(
  filename = "mapas_RRAS_taxas_prematuridade.png",
  plot = painel_rras_taxas,
  width = 10,
  height = 5,
  units = "in",
  dpi = 300
)

# =============================================================================
# TABELA COMPLETA
# =============================================================================
tabela_rras_compacta <- map_rras %>%
  st_drop_geometry() %>%
  transmute(
    rras_id,
    rras_nome,
    pop_rras,
    obitos_fetais = n_of,
    perc_fd_total = round(perc_fd_total, 2),
    taxa_mortalidade_fetal_1000_lv = round(taxa_mortalidade_fetal_1000_lv, 2),
    nascidos_vivos = n_lv,
    perc_lv_total  = round(perc_lv_total, 2),
    taxa_lv_1000_pop = round(taxa_lv_1000_pop, 2),
    prematuros_lv = n_prematuro_lv,
    perc_prematuridade_lv = round(perc_prematuridade_lv, 2)
  ) %>%
  arrange(rras_id) %>%
  select(-rras_id)

write.csv(
  tabela_rras_compacta,
  file = file.path(dir_saida_analise, "rras_indicadores_oficiais.csv"),
  row.names = FALSE
)

knitr::kable(
  tabela_rras_compacta,
  caption = paste0(
    "Indicadores essenciais por RRAS (SP), organizados pela ordem numérica do nome da RRAS. ",
    "População agregada com base no Censo 2022. ",
    "Óbitos fetais por 1.000 nascidos vivos = 1.000 × óbitos fetais/nascidos vivos. ",
    "Nascidos vivos por 1.000 habitantes = 1.000 × nascidos vivos/população. ",
    "Prematuridade = percentual de nascidos vivos com idade gestacional inferior a 37 semanas. ",
    "Percentuais referem-se à participação de cada RRAS no total estadual."
  )
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

#--------------------- Tamanho, taxa bruta de eventos
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

#-------------------------------------------------------------------------------
# Painel estilizado: histograma da idade gestacional por desfecho (evento)
#   evento = 1 -> óbito fetal
#   evento = 0 -> nascimento vivo
#-------------------------------------------------------------------------------

# Cria rótulos legíveis para o artigo
dados_eda_clean <- dados_eda_clean %>%
  dplyr::mutate(
    evento_lab = factor(
      evento,
      levels = c(1, 0),
      labels = c("Óbito fetal", "Nascimento vivo")
    )
  )

# Tema "padrão artigo" reaproveitável
tema_artigo <- theme_minimal(base_size = 12) +
  theme(
    # grade horizontal suave, sem grade vertical
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, colour = "grey80"),
    
    # título mais forte e centralizado
    plot.title = element_text(
      face  = "bold",
      hjust = 0.5,
      size  = 12
    ),
    
    # eixos
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 9, colour = "black"),  # um pouco menor para caber melhor
    
    # faixas dos facets
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey90", colour = NA),
    
    # margens compactas
    plot.margin = margin(5.5, 5.5, 5.5, 5.5, unit = "pt")
  )

# Intervalo de idade gestacional
range_tempo <- range(dados_eda_clean$tempo, na.rm = TRUE)

# Queremos ticks de 5 em 5, começando em 0
break_by <- 5

# Máximo observado
max_tempo <- max(dados_eda_clean$tempo, na.rm = TRUE)

# Opcional: arredondar para o múltiplo de 5 imediatamente acima do máximo
limite_sup <- ceiling(max_tempo / break_by) * break_by

# sequência de ticks: 0, 5, 10, ..., limite_sup
breaks_tempo <- seq(0, limite_sup, by = break_by)

p_hist_tempo_evento <- ggplot(dados_eda_clean, aes(x = tempo)) +
  geom_histogram(
    binwidth = 1,
    boundary = 0,
    closed   = "right",
    colour   = "grey30",
    fill     = "grey75"
  ) +
  facet_wrap(
    ~ evento_lab,
    nrow   = 1,
    scales = "free_y"
  ) +
  scale_x_continuous(
    limits = c(0, limite_sup),   # eixo de 0 até o máximo arredondado
    breaks = breaks_tempo        # 0, 5, 10, 15, ...
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "",
    x     = "Semanas de gestação",
    y     = "Frequência (n)"
  ) +
  tema_artigo

print(p_hist_tempo_evento)

# #-------------------  SALVA GRÁFICO LOCALMENTE
# png("histograma_ambas_causas.png", units = "in", width = 10, height = 5, res = 300)
# print(p_hist_tempo_evento)
# dev.off()

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

# Variáveis numéricas
knitr::kable(
  num_summary,
  digits  = c(0, 0, 2, 2, 2, 2, 2),
  caption = "Medidas: n, média, DP, mediana, Q1, Q3"
)

#-------------------------------------------------------------------------------
# Tabela: idade gestacional (tempo) por causa do desfecho (evento)
#-------------------------------------------------------------------------------

# Garante que temos um rótulo legível da causa
dados_eda_clean <- dados_eda_clean %>%
  dplyr::mutate(
    evento_lab = factor(
      evento,
      levels = c(1, 0),
      labels = c("Óbito fetal", "Nascimento vivo")
    )
  )

# Resumo numérico da idade gestacional por causa
tempo_por_causa <- dados_eda_clean %>%
  dplyr::group_by(evento_lab) %>%
  dplyr::summarise(
    n       = sum(!is.na(tempo)),
    minimo  = min(tempo, na.rm = TRUE),
    media   = mean(tempo, na.rm = TRUE),
    mediana = median(tempo, na.rm = TRUE),
    q1      = quantile(tempo, 0.25, na.rm = TRUE, names = FALSE),
    q3      = quantile(tempo, 0.75, na.rm = TRUE, names = FALSE),
    maximo  = max(tempo, na.rm = TRUE),
    dp      = sd(tempo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::rename(causa = evento_lab)

tempo_por_causa

# Tabela em LaTeX/HTML com kable
knitr::kable(
  tempo_por_causa,
  digits  = c(0, 0, 2, 2, 2, 2, 2, 2, 2),
  caption = "Medidas descritivas da idade gestacional (semanas) segundo o desfecho da gestação"
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

# Variáveis categóricas
knitr::kable(cat_summary_all,
             caption = "Categóricas — n e % do total"
)

#-------------------------------------------------------------------------------
#--------------------------- Kaplan-meier e Log-Rank ---------------------------
#-------------------------------------------------------------------------------
min_tempo1 <- 19
min_tempo2 <- 24

# Limites de tempo para os eixos x dos gráficos
max_tempo <- max(dados_eda_clean$tempo, na.rm = TRUE)

# Transformar fstatus em fator com rótulos desejados
dados_eda_clean$fstatus_cr <- factor(
  dados_eda_clean$fstatus,
  levels = c(0, 1, 2),
  labels = c("Censura", "Óbito fetal", "Nascimento vivo")
)

#-------------------------------------------------------------------------------
# Saídas auxiliares para auditoria dos resultados apresentados no texto
#-------------------------------------------------------------------------------

vars_comuns_auditoria <- c(
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

vars_comuns_auditoria <- intersect(vars_comuns_auditoria, names(dados_eda_clean))

# Estes resumos não são estimativas da curva de Kaplan-Meier.
# Eles descrevem diretamente a distribuição observada da idade gestacional
# (tempo = semagestac) dentro de cada categoria e de cada desfecho.
# A mediana e o intervalo interquartilico sao as medidas principais porque a
# idade gestacional pode ser assimetrica em varios grupos, especialmente nos
# obitos fetais e nos grupos de prematuridade. A media e o desvio padrao ficam
# como complemento quando ajudam a indicar deslocamentos gerais da distribuicao.
criterios_resumo_tempo_variaveis_comuns <- tibble::tibble(
  variavel = vars_comuns_auditoria,
  medida_principal = "mediana; intervalo_interquartilico",
  medidas_complementares = "media; desvio_padrao",
  justificativa = paste(
    "Mediana e intervalo interquartilico usados como medidas principais por",
    "serem menos sensiveis a assimetria e valores extremos da idade gestacional;",
    "media e desvio padrao mantidos como resumos complementares."
  )
)

frequencias_variaveis_comuns <- purrr::map_dfr(
  vars_comuns_auditoria,
  function(var) {
    dados_eda_clean %>%
      filter(!is.na(.data[[var]]), !is.na(tempo), !is.na(evento_lab)) %>%
      count(
        variavel = var,
        desfecho = evento_lab,
        nivel = as.character(.data[[var]]),
        name = "n"
      ) %>%
      group_by(variavel, desfecho) %>%
      mutate(percentual = 100 * n / sum(n)) %>%
      ungroup()
  }
)

resumo_tempo_variaveis_comuns <- purrr::map_dfr(
  vars_comuns_auditoria,
  function(var) {
    dados_eda_clean %>%
      filter(!is.na(.data[[var]]), !is.na(tempo), !is.na(evento_lab)) %>%
      group_by(
        variavel = var,
        desfecho = evento_lab,
        nivel = as.character(.data[[var]])
      ) %>%
      summarise(
        n = dplyr::n(),
        media = mean(tempo, na.rm = TRUE),
        dp = sd(tempo, na.rm = TRUE),
        mediana = median(tempo, na.rm = TRUE),
        q1 = quantile(tempo, 0.25, na.rm = TRUE, names = FALSE),
        q3 = quantile(tempo, 0.75, na.rm = TRUE, names = FALSE),
        minimo = min(tempo, na.rm = TRUE),
        maximo = max(tempo, na.rm = TRUE),
        .groups = "drop"
      )
  }
)

resumo_tempo_km_global <- function(df) {
  # Resumo observado da idade gestacional por desfecho usado no KM global.
  resumo <- df %>%
    filter(!is.na(tempo), !is.na(evento_lab)) %>%
    group_by(desfecho = evento_lab) %>%
    summarise(
      n = dplyr::n(),
      media = round(mean(tempo, na.rm = TRUE), 2),
      dp = round(sd(tempo, na.rm = TRUE), 2),
      mediana = round(median(tempo, na.rm = TRUE), 2),
      q1 = round(quantile(tempo, 0.25, na.rm = TRUE, names = FALSE), 2),
      q3 = round(quantile(tempo, 0.75, na.rm = TRUE, names = FALSE), 2),
      .groups = "drop"
    )

  cat("\nResumo da idade gestacional - KM global\n")
  print(resumo)
  invisible(resumo)
}

resumo_tempo_km_comum <- function(df, var, rotulo = var) {
  # Resumo observado por desfecho e grupo usado nos paineis de KM.
  resumo <- df %>%
    filter(!is.na(tempo), !is.na(evento_lab), !is.na(.data[[var]])) %>%
    group_by(desfecho = evento_lab, grupo = .data[[var]]) %>%
    summarise(
      n = dplyr::n(),
      media = round(mean(tempo, na.rm = TRUE), 2),
      dp = round(sd(tempo, na.rm = TRUE), 2),
      mediana = round(median(tempo, na.rm = TRUE), 2),
      q1 = round(quantile(tempo, 0.25, na.rm = TRUE, names = FALSE), 2),
      q3 = round(quantile(tempo, 0.75, na.rm = TRUE, names = FALSE), 2),
      .groups = "drop"
    )

  cat("\nResumo da idade gestacional - KM por ", rotulo, "\n", sep = "")
  print(resumo)
  invisible(resumo)
}

calcular_logrank <- function(var, status_var, desfecho) {
  dados_teste <- dados_eda_clean %>%
    filter(!is.na(.data[[var]]), !is.na(tempo), !is.na(.data[[status_var]]))

  ajuste <- survival::survdiff(
    stats::as.formula(paste0("Surv(tempo, ", status_var, ") ~ ", var)),
    data = dados_teste
  )

  tibble::tibble(
    variavel = var,
    desfecho = desfecho,
    teste = "Log-rank",
    estatistica = unname(ajuste$chisq),
    gl = length(ajuste$n) - 1L,
    valor_p = 1 - stats::pchisq(unname(ajuste$chisq), df = length(ajuste$n) - 1L)
  )
}

calcular_gray <- function(var) {
  dados_teste <- dados_eda_clean %>%
    filter(!is.na(.data[[var]]), !is.na(tempo), !is.na(fstatus))

  ajuste <- cmprsk::cuminc(
    ftime = dados_teste$tempo,
    fstatus = dados_teste$fstatus,
    group = dados_teste[[var]],
    cencode = 0
  )

  testes <- as.data.frame(ajuste$Tests)
  testes$desfecho_codigo <- rownames(testes)

  testes %>%
    tibble::as_tibble() %>%
    transmute(
      variavel = var,
      desfecho = dplyr::recode(
        as.character(desfecho_codigo),
        "1" = "Óbito fetal",
        "2" = "Nascimento vivo",
        .default = as.character(desfecho_codigo)
      ),
      teste = "Gray",
      estatistica = stat,
      gl = df,
      valor_p = pv
    )
}

testes_variaveis_comuns <- dplyr::bind_rows(
  purrr::map_dfr(
    vars_comuns_auditoria,
    ~ dplyr::bind_rows(
      calcular_logrank(.x, "status_fd", "Óbito fetal"),
      calcular_logrank(.x, "status_lv", "Nascimento vivo")
    )
  ),
  purrr::map_dfr(vars_comuns_auditoria, calcular_gray)
)

# As semanas 30, 35 e 40 foram usadas no texto por terem leitura obstétrica
# direta: 30 semanas representa prematuridade importante, 35 semanas aproxima a
# faixa de prematuridade tardia e 40 semanas representa o termo. A semana 20 é
# mantida nesta saída apenas para conferência, pois os valores tendem a ser muito
# baixos e pouco informativos para a maioria das variáveis.
semanas_cif_auditoria <- c(20, 30, 35, 40)

extrair_cif_variavel <- function(var) {
  dados_teste <- dados_eda_clean %>%
    filter(!is.na(.data[[var]]), !is.na(tempo), !is.na(fstatus))

  ajuste <- cmprsk::cuminc(
    ftime = dados_teste$tempo,
    fstatus = dados_teste$fstatus,
    group = dados_teste[[var]],
    cencode = 0
  )

  pontos <- cmprsk::timepoints(ajuste, times = semanas_cif_auditoria)$est

  as.data.frame(as.table(pontos), stringsAsFactors = FALSE) %>%
    transmute(
      variavel = var,
      curva = as.character(Var1),
      semana = suppressWarnings(as.numeric(as.character(Var2))),
      cif = Freq,
      nivel = sub(
        " 2$", "",
        sub(
          " 1$", "",
          sub(" Nascimento vivo$", "", sub(" Óbito fetal$", "", curva))
        )
      ),
      desfecho = dplyr::case_when(
        grepl("Óbito fetal$", curva) ~ "Óbito fetal",
        grepl("Nascimento vivo$", curva) ~ "Nascimento vivo",
        grepl(" 1$", curva) ~ "Óbito fetal",
        grepl(" 2$", curva) ~ "Nascimento vivo",
        TRUE ~ NA_character_
      )
    ) %>%
    select(variavel, desfecho, nivel, semana, cif)
}

cif_variaveis_comuns <- purrr::map_dfr(vars_comuns_auditoria, extrair_cif_variavel)

write.csv(
  criterios_resumo_tempo_variaveis_comuns,
  file = file.path(dir_saida_analise, "criterios_resumo_tempo_variaveis_comuns.csv"),
  row.names = FALSE
)

write.csv(
  resumo_tempo_variaveis_comuns,
  file = file.path(dir_saida_analise, "resumo_tempo_variaveis_comuns.csv"),
  row.names = FALSE
)

write.csv(
  frequencias_variaveis_comuns,
  file = file.path(dir_saida_analise, "frequencias_variaveis_comuns.csv"),
  row.names = FALSE
)

write.csv(
  testes_variaveis_comuns,
  file = file.path(dir_saida_analise, "testes_lr_gray_variaveis_comuns.csv"),
  row.names = FALSE
)

write.csv(
  cif_variaveis_comuns,
  file = file.path(dir_saida_analise, "cif_variaveis_comuns_semanas.csv"),
  row.names = FALSE
)

#-------------------------------- KM global

# Óbito fetal como evento (censura = nascido vivo) 
km_fd <- survfit(Surv(tempo, status_fd) ~ 1, data = dados_eda_clean)
grafico_km_fd <- ggsurvplot(
  km_fd,
  data       = dados_eda_clean,
  conf.int   = FALSE,         # Há censura
  xlab       = "Semanas de gestação",
  ylab       = "S(t)",
  title      = "KM - Óbito fetal (nascido vivo censurado)",
  legend     = "none",       # remove a legenda
  ylim       = c(0.985, 1.00), # zoom
  xlim       = c(min_tempo1, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme     = theme_minimal(base_size = 12)
)
print(grafico_km_fd)

# #-------------------  SALVA GRÁFICO LOCALMENTE
# png("km_global_fd.png", units="in", width = 10, height = 5, res=300)
# print(grafico_km_fd)
# dev.off()

# Nascido vivo como evento (óbito fetal censurado) 
km_lv <- survfit(Surv(tempo, status_lv) ~ 1, data = dados_eda_clean)
grafico_km_lv <- ggsurvplot(
  km_lv,
  data       = dados_eda_clean,
  conf.int   = FALSE,         # Há censura
  xlab       = "Semanas de gestação",
  ylab       = "S(t)",
  title      = "KM - Nascido vivo (óbito fetal censurado)",
  legend     = "none",       # remove a legenda
  ylim       = c(0, 1.00), # zoom
  xlim       = c(min_tempo2, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme     = theme_minimal(base_size = 12),
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
resumo_tempo_km_global(dados_eda_clean)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("0_ambos_km_global.png", units="in", width=10, height=5, res=112)
print(painel_km_global)
dev.off()

# CIF global separada por causa (óbito fetal vs nascimento vivo) ---------------

# Objeto de incidência acumulada (CIF para competing risks)
ci_global <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus_cr,  # fator com níveis: Censura, Óbito fetal, Nascimento vivo
  cencode = "Censura"
)

# Conferir nomes das curvas para saber como extrair cada causa
print(names(ci_global))

# Extrair curvas de CIF de cada evento separadamente -----------------------

# Óbito fetal
ci_fd_global <- ci_global[grep("Óbito fetal", names(ci_global))][[1]]

# Nascimento vivo
ci_lv_global <- ci_global[grep("Nascimento vivo", names(ci_global))][[1]]

# Transformar em data.frames longos ----------------------------------------
df_cif_fd_global <- data.frame(
  tempo = ci_fd_global$time,
  cif   = ci_fd_global$est
)

df_cif_lv_global <- data.frame(
  tempo = ci_lv_global$time,
  cif   = ci_lv_global$est
)

# Definir limite superior comum e sequência de ticks
max_tempo_cif <- max(
  df_cif_fd_global$tempo,
  df_cif_lv_global$tempo,
  na.rm = TRUE
)
breaks_tempo_cif <- seq(20, max_tempo_cif, by = 5)

# CIF global – óbito fetal
p_cif_fd_global <- ggplot(df_cif_fd_global, aes(x = tempo, y = cif)) +
  geom_step(
    color     = "#d62728",
    linewidth = 0.9
  ) +
  scale_x_continuous(
    breaks = breaks_tempo_cif
  ) +
  scale_y_continuous(
    limits = c(0, max(df_cif_fd_global$cif, na.rm = TRUE) * 1.05),
    expand = expansion(mult = c(0, 0.02))
  ) +
  coord_cartesian(xlim = c(min_tempo1, max_tempo_cif)) +  # zoom a partir da semana 19
  labs(
    title = "Óbito fetal",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12)

# CIF global – nascimento vivo
p_cif_lv_global <- ggplot(df_cif_lv_global, aes(x = tempo, y = cif)) +
  geom_step(
    color     = "#1f77b4",
    linewidth = 0.9
  ) +
  scale_x_continuous(
    breaks = breaks_tempo_cif
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  coord_cartesian(xlim = c(min_tempo2, max_tempo_cif)) +  # mesmo zoom
  labs(
    title = "Nascido vivo",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12)

# Painel 
painel_cif_global <- ggpubr::ggarrange(
  p_cif_fd_global,
  p_cif_lv_global,
  ncol   = 2,
  nrow   = 1,
  # labels = c("A", "B"),   
  common.legend = FALSE
)

print(painel_cif_global)

#-------------------  SALVA GRÁFICO LOCALMENTE
png("cif_global_painel.png", units = "in", width = 10, height = 5, res = 112)
print(painel_cif_global)
dev.off()

# painel_km_cif_global <- painel_km_global / painel_cif_global +
#   patchwork::plot_layout(heights = c(2, 1)) +  # KM ocupa mais altura
#   plot_annotation(
#     title = "Curvas de sobrevivência e incidência acumulada por desfecho",
#     theme = theme(
#       plot.title = element_text(
#         face = "bold",
#         hjust = 0.5,
#         size = 13
#       )
#     )
#   )
# 
# # Visualizar no R
# print(painel_km_cif_global)

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
  conf.int   = FALSE,        
  xlab       = "Semanas de gestação",
  ylab       = "S(t)",
  title      = "KM - Óbito fetal (censura: nascido vivo)\npor sexo",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$sexo),
  ylim = c(0.987, 1.00), # zoom
  xlim       = c(min_tempo1, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme = theme_minimal(base_size = 12)
)
print(grafico_kmglobal_sexo_fd)

# Nascido vivo como evento (óbito fetal censurado) 
km_lv_sexo <- survfit(Surv(tempo, status_lv) ~ sexo, data = dados_eda_clean)
grafico_kmglobal_sexo_lv <- ggsurvplot(
  km_lv_sexo,
  data       = dados_eda_clean,
  conf.int   = FALSE,        
  xlab       = "Semanas de gestação",
  ylab       = "S(t)",
  title      = "KM - Nascido vivo (censura: óbito fetal)\npor sexo",
  legend.title = "",
  legend.labs  = levels(dados_eda_clean$sexo),
  ylim = c(0, 1), # zoom
  xlim       = c(min_tempo2, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme    = theme_minimal(base_size = 12)
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
resumo_tempo_km_comum(dados_eda_clean, "sexo", "sexo")

#-------------------  SALVA GRÁFICO LOCALMENTE
png("kmglobal_sexo.png", units="in", width=10, height=5, res=112)
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
# Teste de Gray da variável sexo
ci_sexo <- cmprsk::cuminc(
  ftime   = dados_eda_clean$tempo,
  fstatus = dados_eda_clean$fstatus_cr,
  group   = dados_eda_clean$sexo,
  cencode = "Censura"   # nível que representa censura
)
print(ci_sexo)

# Teste de Gray da variável sexo (sem informações extras)
ci_sexo$Tests

#------------------------------- CIF por sexo
# Partimos de 'ci_sexo' já criado com cmprsk::cuminc()

# Separar curvas do evento 1 (Óbito fetal) e evento 2 (Nascimento vivo)
ci_sexo_evento1 <- ci_sexo[grep("Óbito fetal$", names(ci_sexo))]        # evento 1
ci_sexo_evento2 <- ci_sexo[grep("Nascimento vivo$", names(ci_sexo))]   # evento 2

#--------------------------- Data frame longo – ÓBITO FETAL (evento 1)

df_cif_fd_sexo <- purrr::map_dfr(
  names(ci_sexo_evento1),
  function(nm) {
    comp <- ci_sexo_evento1[[nm]]
    
    # Remove o sufixo " Óbito fetal" para obter o rótulo do sexo
    sexo_grupo <- sub(" Óbito fetal$", "", nm)
    
    tibble::tibble(
      tempo = comp$time,
      cif   = comp$est,
      sexo  = sexo_grupo
    )
  }
) %>%
  dplyr::mutate(
    sexo = factor(sexo, levels = c("Masculino", "Feminino"))
  ) %>%
  dplyr::arrange(sexo, tempo)

print(table(df_cif_fd_sexo$sexo, useNA = "ifany"))

#--------------------------- Data frame longo – NASCIMENTO VIVO (evento 2)

df_cif_lv_sexo <- purrr::map_dfr(
  names(ci_sexo_evento2),
  function(nm) {
    comp <- ci_sexo_evento2[[nm]]
    
    # Remove o sufixo " Nascimento vivo" para obter o rótulo do sexo
    sexo_grupo <- sub(" Nascimento vivo$", "", nm)
    
    tibble::tibble(
      tempo = comp$time,
      cif   = comp$est,
      sexo  = sexo_grupo
    )
  }
) %>%
  dplyr::mutate(
    sexo = factor(sexo, levels = c("Masculino", "Feminino"))
  ) %>%
  dplyr::arrange(sexo, tempo)

print(table(df_cif_lv_sexo$sexo, useNA = "ifany"))

#--------------------------- Definição de limite de tempo e breaks (zoom a partir da semana 19)

max_tempo_cif_sexo <- max(
  df_cif_fd_sexo$tempo,
  df_cif_lv_sexo$tempo,
  na.rm = TRUE
)

breaks_tempo_cif_sexo <- seq(20, max_tempo_cif_sexo, by = 5)

#--------------------------- CIF apenas ÓBITOS FETAIS por sexo
p_cif_fd_sexo <- ggplot(
  df_cif_fd_sexo,
  aes(x = tempo,
      y = cif,
      linetype = sexo)
) +
  geom_step(
    color     = "#d62728",   # vermelho para óbito fetal
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Sexo",
    values = c(
      "Masculino" = "dashed",
      "Feminino"  = "solid"
    )
  ) +
  scale_x_continuous(
    breaks = breaks_tempo_cif_sexo
  ) +
  coord_cartesian(
    xlim = c(min_tempo1, max_tempo_cif_sexo)
  ) +
  labs(
    title = "Óbito fetal (por sexo)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    # aumenta o espaço horizontal do símbolo da legenda
    legend.key.width = grid::unit(1.5, "cm")
  ) +
  # legenda neutra, mas preservando o padrão de linha
  guides(
    linetype = guide_legend(
      override.aes = list(
        colour   = "grey20",
        linetype = c("dashed", "solid")  # na ordem dos níveis
      )
    )
  )

print(p_cif_fd_sexo)

#--------------------------- CIF apenas NASCIDOS VIVOS por sexo

p_cif_lv_sexo <- ggplot(
  df_cif_lv_sexo,
  aes(x = tempo,
      y = cif,
      linetype = sexo)
) +
  geom_step(
    color     = "#1f77b4",   # azul para nascimento vivo
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Sexo",
    values = c(
      "Masculino" = "dashed",
      "Feminino"  = "solid"
    )
  ) +
  scale_x_continuous(
    breaks = breaks_tempo_cif_sexo
  ) +
  coord_cartesian(
    xlim = c(min_tempo2, max_tempo_cif_sexo)
  ) +
  labs(
    title = "Nascido vivo (por sexo)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(
        colour   = "grey20",
        linetype = c("dashed", "solid")
      )
    )
  )

print(p_cif_lv_sexo)

#--------------------------- Painel: CIF Nascidos vivos (esq.) e Óbitos fetais (dir.)

painel_cif_sexo <- ggpubr::ggarrange(
  p_cif_fd_sexo,
  p_cif_lv_sexo,
  ncol          = 2,
  nrow          = 1,
  common.legend = TRUE,
  legend        = "bottom"
)

print(painel_cif_sexo)

#-------------------  SALVA PAINEL LOCALMENTE
png(
  "ambos_cif_sexo.png",
  units = "in",
  width = 10,
  height = 5,
  res = 112
)
print(painel_cif_sexo)
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
  conf.int    = FALSE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t)",
  title       = "KM - Óbito fetal (censura: nascido vivo)\npor faixa etária materna",
  legend.title = "Faixa etária materna",
  legend.labs  = levels(dados_eda_clean$idademae_categorico),
  ylim        = c(0.985, 1.00), # zoom
  xlim       = c(min_tempo1, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme    = theme_minimal(base_size = 12)
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
  conf.int    = FALSE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t)",
  title       = "KM - Nascido vivo (censura: óbito fetal)\npor faixa etária materna",
  legend.title = "Faixa etária materna",
  legend.labs  = levels(dados_eda_clean$idademae_categorico),
  ylim        = c(0, 1), # zoom
  xlim       = c(min_tempo2, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme    = theme_minimal(base_size = 12)
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
resumo_tempo_km_comum(dados_eda_clean, "idademae_categorico", "faixa etária materna")

#-------------------  SALVA GRÁFICO LOCALMENTE
png("kmglobal_idade_mae.png", units = "in", width = 10, height = 5, res = 112)
print(painel_kmglobal_idade)
dev.off()

#------------------------------- Log-Rank Faixa etária da mãe

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

# Teste de Gray para a variável Faixa etária da mãe (idademae_categorico)
ci_idade$Tests

#--------------------------------------------------------------
# CIF – Faixa etária da mãe (idademae_categorico)
# Painel: Nascidos vivos (evento 2) x Óbitos fetais (evento 1)
#--------------------------------------------------------------

# Separar curvas do evento 1 (Óbito fetal) e evento 2 (Nascimento vivo)
ci_idade_evento1 <- ci_idade[grep("Óbito fetal$",     names(ci_idade))]  # evento 1
ci_idade_evento2 <- ci_idade[grep("Nascimento vivo$", names(ci_idade))]  # evento 2

#--------------------------- Data frame longo – ÓBITO FETAL (evento 1)

df_cif_fd_idade <- purrr::map_dfr(
  names(ci_idade_evento1),
  function(nm) {
    comp <- ci_idade_evento1[[nm]]
    
    # nm vem no formato "<20 Óbito fetal", "20–34 Óbito fetal", "35+ Óbito fetal"
    faixa <- sub(" Óbito fetal$", "", nm)
    
    tibble::tibble(
      tempo = comp$time,
      cif   = comp$est,
      faixa = faixa
    )
  }
) %>%
  dplyr::mutate(
    faixa = factor(
      faixa,
      levels = c("<20", "20–34", "35+")
    )
  ) %>%
  dplyr::arrange(faixa, tempo)

print(table(df_cif_fd_idade$faixa, useNA = "ifany"))

#--------------------------- Data frame longo – NASCIMENTO VIVO (evento 2)

df_cif_lv_idade <- purrr::map_dfr(
  names(ci_idade_evento2),
  function(nm) {
    comp <- ci_idade_evento2[[nm]]
    
    # nm vem no formato "<20 Nascimento vivo", etc.
    faixa <- sub(" Nascimento vivo$", "", nm)
    
    tibble::tibble(
      tempo = comp$time,
      cif   = comp$est,
      faixa = faixa
    )
  }
) %>%
  dplyr::mutate(
    faixa = factor(
      faixa,
      levels = c("<20", "20–34", "35+")
    )
  ) %>%
  dplyr::arrange(faixa, tempo)

print(table(df_cif_lv_idade$faixa, useNA = "ifany"))

#--------------------------- Limites de tempo e breaks (zoom a partir da semana 19)

max_tempo_cif_idade <- max(
  df_cif_fd_idade$tempo,
  df_cif_lv_idade$tempo,
  na.rm = TRUE
)

breaks_tempo_cif_idade <- seq(20, max_tempo_cif_idade, by = 5)

#--------------------------- CIF apenas ÓBITOS FETAIS por faixa etária

p_cif_fd_idade <- ggplot(
  df_cif_fd_idade,
  aes(x = tempo,
      y = cif,
      linetype = faixa)
) +
  geom_step(
    color     = "#d62728",   # vermelho para óbito fetal
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Faixa etária materna",
    values = c(
      "<20"   = "solid",
      "20–34" = "dashed",
      "35+"   = "dotted"
    )
  ) +
  scale_x_continuous(
    breaks = breaks_tempo_cif_idade
  ) +
  coord_cartesian(
    xlim = c(min_tempo1, max_tempo_cif_idade)
  ) +
  labs(
    title = "Óbito fetal (por faixa etária materna)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  ) +
  # Legenda neutra (apenas linetype informa a categoria)
  guides(
    linetype = guide_legend(
      override.aes = list(
        colour   = "grey20",
        linetype = c("solid", "dashed", "dotted")
      )
    )
  )

print(p_cif_fd_idade)

#--------------------------- CIF apenas NASCIDOS VIVOS por faixa etária

p_cif_lv_idade <- ggplot(
  df_cif_lv_idade,
  aes(x = tempo,
      y = cif,
      linetype = faixa)
) +
  geom_step(
    color     = "#1f77b4",   # azul para nascimento vivo
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Faixa etária materna",
    values = c(
      "<20"   = "solid",
      "20–34" = "dashed",
      "35+"   = "dotted"
    )
  ) +
  scale_x_continuous(
    breaks = breaks_tempo_cif_idade
  ) +
  coord_cartesian(
    xlim = c(min_tempo2, max_tempo_cif_idade)
  ) +
  labs(
    title = "Nascido vivo (por faixa etária materna)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(
        colour   = "grey20",
        linetype = c("solid", "dashed", "dotted")
      )
    )
  )

print(p_cif_lv_idade)

#--------------------------- Painel: CIF Nascidos vivos (esq.) x Óbitos fetais (dir.)

painel_cif_idade <- ggpubr::ggarrange(
  p_cif_fd_idade,
  p_cif_lv_idade,
  ncol          = 2,
  nrow          = 1,
  common.legend = TRUE,
  legend        = "bottom"
)

print(painel_cif_idade)

#-------------------  SALVA PAINEL LOCALMENTE
png(
  "cif_idademae_painel_ambos.png",
  units = "in",
  width = 10,
  height = 5,
  res   = 112
)
print(painel_cif_idade)
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
  conf.int    = FALSE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t)",
  title       = "KM - Óbito fetal (censura: nascido vivo)\npor escolaridade materna",
  legend.title = "Escolaridade materna",
  legend.labs  = levels(dados_eda_clean$escmae2010),
  ylim        = c(0.975, 1.00), # zoom
  xlim       = c(min_tempo1, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme    = theme_minimal(base_size = 12)
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
  conf.int    = FALSE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t)",
  title       = "KM - Nascido vivo (censura: óbito fetal)\npor escolaridade materna",
  legend.title = "Escolaridade materna",
  legend.labs  = levels(dados_eda_clean$escmae2010),
  ylim        = c(0, 1), # zoom
  xlim       = c(min_tempo2, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme    = theme_minimal(base_size = 12)
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
resumo_tempo_km_comum(dados_eda_clean, "escmae2010", "escolaridade materna")

#-------------------  SALVA GRÁFICO LOCALMENTE
png("kmglobal_escmae2010.png", units = "in", width = 10, height = 5, res = 112)
print(painel_kmglobal_esc)
dev.off()

#------------------------------- Log-Rank Escolaridade da mãe

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

# Teste de Gray para escmae2010
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

# #-------------------  SALVA GRÁFICO LOCALMENTE
# png("cif_escmae2010.png", units = "in", width = 10, height = 5, res = 112)
# print(p_cif_esc)
# dev.off()

# CIF apenas para o evento óbito fetal (1) por escolaridade materna

cif_df_fd <- cif_df_all %>%
  dplyr::filter(evento == "Óbito fetal") %>%
  dplyr::arrange(grupo, tempo)

# CIF apenas para o evento Nascimento vivo (2) por escolaridade materna
cif_df_lv <- cif_df_all %>%
  dplyr::filter(evento == "Nascimento vivo") %>%
  dplyr::arrange(grupo, tempo)

# Limite superior comum e escala de tempo (zoom a partir da semana 19)
max_tempo_cif_esc <- max(
  cif_df_fd$tempo,
  cif_df_lv$tempo,
  na.rm = TRUE
)

breaks_tempo_cif_esc <- seq(20, max_tempo_cif_esc, by = 5)

p_cif_fd_esc <- ggplot(
  cif_df_fd,
  aes(x = tempo,
      y = est,
      linetype = grupo)
) +
  geom_step(
    color     = "#d62728",  # vermelho para óbito fetal
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
  scale_x_continuous(
    breaks = breaks_tempo_cif_esc
  ) +
  coord_cartesian(
    xlim = c(min_tempo1, max_tempo_cif_esc)
  ) +
  labs(
    title = "Óbito fetal (por escolaridade materna)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(
        colour = "grey20"   # legenda neutra (cinza)
      )
    )
  )

print(p_cif_fd_esc)

p_cif_lv_esc <- ggplot(
  cif_df_lv,
  aes(x = tempo,
      y = est,
      linetype = grupo)
) +
  geom_step(
    color     = "#1f77b4",  # azul para nascimento vivo
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
  scale_x_continuous(
    breaks = breaks_tempo_cif_esc
  ) +
  coord_cartesian(
    xlim = c(min_tempo2, max_tempo_cif_esc)
  ) +
  labs(
    title = "Nascido vivo (por escolaridade materna)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(
        colour = "grey20"   # legenda neutra
      )
    )
  )

print(p_cif_lv_esc)

painel_cif_esc <- ggpubr::ggarrange(
  p_cif_fd_esc,
  p_cif_lv_esc,
  ncol          = 2,
  nrow          = 1,
  common.legend = TRUE,
  legend        = "bottom"
)

print(painel_cif_esc)

#-------------------  SALVA PAINEL LOCALMENTE
png(
  "cif_escmae2010_painel_ambos.png",
  units = "in",
  width = 10,
  height = 5,
  res   = 112
)
print(painel_cif_esc)
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
  conf.int    = FALSE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t)",
  title       = "KM - Óbito fetal (censura: nascido vivo)\npor presença de filhos vivos previamente",
  legend.title = "Filhos vivos previamente",
  legend.labs  = levels(dados_eda_clean$qtdfilvivo_categorico),
  ylim        = c(0.987, 1.00), # zoom
  xlim       = c(min_tempo1, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme    = theme_minimal(base_size = 12)
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
  conf.int    = FALSE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t)",
  title       = "KM - Nascido vivo (censura: óbito fetal)\npor presença de filhos vivos previamente",
  legend.title = "Filhos vivos previamente",
  legend.labs  = levels(dados_eda_clean$qtdfilvivo_categorico),
  ylim        = c(0, 1), # zoom
  xlim       = c(min_tempo2, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme    = theme_minimal(base_size = 12)
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
resumo_tempo_km_comum(dados_eda_clean, "qtdfilvivo_categorico", "presença de filhos vivos previamente")

#-------------------  SALVA GRÁFICO LOCALMENTE
png("kmglobal_filvivo.png", units = "in", width = 10, height = 5, res = 112)
print(painel_kmglobal_filvivo)
dev.off()

#------------------------------- Log-Rank Presença de filhos vivos previamente

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

# Teste de Gray para qtdfilvivo_categorico
ci_filvivo$Tests

#------------------------------- CIF
# 
# p_cif_filvivo <- ggcompetingrisks(
#   fit             = ci_filvivo,
#   multiple_panels = TRUE,   # um painel para cada categoria ("Não", "Sim")
#   ggtheme         = theme_minimal(base_size = 12),
#   palette         = c("#1f77b4", "#d62728"),   # 1 = óbito fetal, 2 = nascido vivo
#   legend.title    = "Evento",
#   legend.labs     = c("Óbito fetal", "Nascimento vivo"),
#   xlab            = "Semanas de gestação",
#   ylab            = "Incidência acumulada",
#   title           = "Gráfico de Incidência Acumulada (CIF) por presença de filhos vivos previamente"
# )
# print(p_cif_filvivo)
# 
# #-------------------  SALVA GRÁFICO LOCALMENTE
# png("cif_filvivo.png", units = "in", width = 10, height = 5, res = 300)
# print(p_cif_filvivo)
# dev.off()

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

# Selecionar apenas as curvas cujo nome termina em "Nascimento vivo"
ci_filvivo_evento2 <- ci_filvivo[grep("Nascimento vivo$", names(ci_filvivo))]

# Converter para data.frame longo: tempo, CIF, grupo ("Não"/"Sim")
df_cif_lv_filvivo <- purrr::map_dfr(
  names(ci_filvivo_evento2),
  function(nm) {
    comp <- ci_filvivo_evento2[[nm]]
    
    # Extrai o rótulo do grupo removendo o sufixo " Nascimento vivo"
    grupo <- sub(" Nascimento vivo$", "", nm)
    
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
  ) %>%
  dplyr::arrange(grupo, tempo)

print(table(df_cif_lv_filvivo$grupo, useNA = "ifany"))

# Limite superior comum e escala de tempo (zoom a partir da semana 19)
max_tempo_cif_filvivo <- max(
  df_cif_fd_filvivo$tempo,
  df_cif_lv_filvivo$tempo,
  na.rm = TRUE
)

breaks_tempo_cif_filvivo <- seq(20, max_tempo_cif_filvivo, by = 5)

p_cif_fd_filvivo <- ggplot(
  df_cif_fd_filvivo,
  aes(x = tempo,
      y = cif,
      linetype = grupo)
) +
  geom_step(
    color     = "#d62728",  # vermelho para óbito fetal
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Filhos vivos previamente",
    values = c(
      "Não" = "solid",
      "Sim" = "dashed"
    ),
    breaks = c("Não", "Sim")
  ) +
  scale_x_continuous(
    breaks = breaks_tempo_cif_filvivo
  ) +
  coord_cartesian(
    xlim = c(min_tempo1, max_tempo_cif_filvivo)
  ) +
  labs(
    title = "Óbito fetal (por presença de filhos\nvivos previamente)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  ) +
  # Legenda neutra (cor cinza, válida para ambos os gráficos do painel)
  guides(
    linetype = guide_legend(
      override.aes = list(
        colour = "grey20"
      )
    )
  )

print(p_cif_fd_filvivo)

p_cif_lv_filvivo <- ggplot(
  df_cif_lv_filvivo,
  aes(x = tempo,
      y = cif,
      linetype = grupo)
) +
  geom_step(
    color     = "#1f77b4",  # azul para nascimento vivo
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Filhos vivos previamente",
    values = c(
      "Não" = "solid",
      "Sim" = "dashed"
    ),
    breaks = c("Não", "Sim")
  ) +
  scale_x_continuous(
    breaks = breaks_tempo_cif_filvivo
  ) +
  coord_cartesian(
    xlim = c(min_tempo2, max_tempo_cif_filvivo)
  ) +
  labs(
    title = "Nascido vivo (por presença de filhos\nvivos previamente)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(
        colour = "grey20"
      )
    )
  )

print(p_cif_lv_filvivo)

painel_cif_filvivo <- ggpubr::ggarrange(
  p_cif_fd_filvivo,
  p_cif_lv_filvivo,
  ncol          = 2,
  nrow          = 1,
  common.legend = TRUE,
  legend        = "bottom"
)

print(painel_cif_filvivo)

#-------------------  SALVA PAINEL LOCALMENTE
png(
  "cif_filvivo_painel_ambos.png",
  units = "in",
  width = 10,
  height = 5,
  res   = 112
)
print(painel_cif_filvivo)
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
  conf.int    = FALSE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t)",
  title       = "KM - Óbito fetal (censura: nascido vivo)\npor presença de filhos mortos previamente",
  legend.title = "Filhos mortos previamente",
  legend.labs  = levels(dados_eda_clean$qtdfilmort_categorico),
  ylim        = c(0.975, 1.00), # zoom
  xlim       = c(min_tempo1, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme    = theme_minimal(base_size = 12)
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
  conf.int    = FALSE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t)",
  title       = "KM - Nascido vivo (censura: óbito fetal)\npor presença de filhos mortos previamente",
  legend.title = "Filhos mortos previamente",
  legend.labs  = levels(dados_eda_clean$qtdfilmort_categorico),
  ylim        = c(0, 1), # zoom
  xlim       = c(min_tempo2, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme    = theme_minimal(base_size = 12)
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
resumo_tempo_km_comum(dados_eda_clean, "qtdfilmort_categorico", "presença de filhos mortos previamente")

#-------------------  SALVA GRÁFICO LOCALMENTE
png("kmglobal_filmort.png", units = "in", width = 10, height = 5, res = 112)
print(painel_kmglobal_filmort)
dev.off()

#------------------------------- Log-Rank Presença de filhos mortos previamente

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

# Teste de Gray para qtdfilmort_categorico
ci_filmort$Tests

#------------------------------- CIF
# 
# p_cif_filmort <- ggcompetingrisks(
#   fit             = ci_filmort,
#   multiple_panels = TRUE,   # painel separado para "Não" e "Sim"
#   ggtheme         = theme_minimal(base_size = 12),
#   palette         = c("#1f77b4", "#d62728"),   # 1 = óbito fetal, 2 = nascido vivo
#   legend.title    = "Evento",
#   legend.labs     = c("Óbito fetal", "Nascimento vivo"),
#   xlab            = "Semanas de gestação",
#   ylab            = "Incidência acumulada",
#   title           = "Gráfico de Incidência Acumulada (CIF) por presença de filhos mortos previamente"
# )
# print(p_cif_filmort)
# 
# #-------------------  SALVA GRÁFICO LOCALMENTE
# png("cif_filmort.png", units = "in", width = 10, height = 5, res = 300)
# print(p_cif_filmort)
# dev.off()

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

# Selecionar apenas as curvas cujo nome termina em "Nascimento vivo"
ci_filmort_evento2 <- ci_filmort[grep("Nascimento vivo$", names(ci_filmort))]

# Converter em data.frame longo: tempo, CIF, grupo ("Não"/"Sim")
df_cif_lv_filmort <- purrr::map_dfr(
  names(ci_filmort_evento2),
  function(nm) {
    comp <- ci_filmort_evento2[[nm]]
    
    # Extrai o rótulo do grupo removendo o sufixo " Nascimento vivo"
    grupo <- sub(" Nascimento vivo$", "", nm)
    
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
  ) %>%
  dplyr::arrange(grupo, tempo)

print(table(df_cif_lv_filmort$grupo, useNA = "ifany"))

# Limite superior comum e escala de tempo (zoom a partir da semana 19)
max_tempo_cif_filmort <- max(
  df_cif_fd_filmort$tempo,
  df_cif_lv_filmort$tempo,
  na.rm = TRUE
)

breaks_tempo_cif_filmort <- seq(20, max_tempo_cif_filmort, by = 5)

p_cif_fd_filmort <- ggplot(
  df_cif_fd_filmort,
  aes(x = tempo,
      y = cif,
      linetype = grupo)
) +
  geom_step(
    color     = "#d62728",  # vermelho para óbito fetal
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Filhos mortos previamente",
    values = c(
      "Não" = "solid",
      "Sim" = "dashed"
    ),
    breaks = c("Não", "Sim")
  ) +
  scale_x_continuous(
    breaks = breaks_tempo_cif_filmort
  ) +
  coord_cartesian(
    xlim = c(min_tempo1, max_tempo_cif_filmort)
  ) +
  labs(
    title = "Óbito fetal (por presença de filhos\nmortos previamente)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  ) +
  # Legenda neutra (cinza), compartilhada entre os dois gráficos do painel
  guides(
    linetype = guide_legend(
      override.aes = list(
        colour = "grey20"
      )
    )
  )

print(p_cif_fd_filmort)

p_cif_lv_filmort <- ggplot(
  df_cif_lv_filmort,
  aes(x = tempo,
      y = cif,
      linetype = grupo)
) +
  geom_step(
    color     = "#1f77b4",  # azul para nascimento vivo
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Filhos mortos previamente",
    values = c(
      "Não" = "solid",
      "Sim" = "dashed"
    ),
    breaks = c("Não", "Sim")
  ) +
  scale_x_continuous(
    breaks = breaks_tempo_cif_filmort
  ) +
  coord_cartesian(
    xlim = c(min_tempo2, max_tempo_cif_filmort)
  ) +
  labs(
    title = "Nascido vivo (por presença de filhos\nmortos previamente)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(
        colour = "grey20"
      )
    )
  )

print(p_cif_lv_filmort)

painel_cif_filmort <- ggpubr::ggarrange(
  p_cif_fd_filmort,
  p_cif_lv_filmort,
  ncol          = 2,
  nrow          = 1,
  common.legend = TRUE,
  legend        = "bottom"
)

print(painel_cif_filmort)

#-------------------  SALVA PAINEL LOCALMENTE
png(
  "cif_filmort_painel_ambos.png",
  units = "in",
  width = 10,
  height = 5,
  res   = 112
)
print(painel_cif_filmort)
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
  conf.int    = FALSE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t)",
  title       = "KM - Óbito fetal (censura: nascido vivo)\npor tipo de gravidez",
  legend.title = "Tipo de gravidez",
  legend.labs  = levels(dados_eda_clean$gravidez),
  ylim        = c(0.950, 1.00), # zoom
  xlim       = c(min_tempo1, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme    = theme_minimal(base_size = 12)
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
  conf.int    = FALSE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t)",
  title       = "KM - Nascido vivo (censura: óbito fetal)\npor tipo de gravidez",
  legend.title = "Tipo de gravidez",
  legend.labs  = levels(dados_eda_clean$gravidez),
  ylim        = c(0, 1), # zoom
  xlim       = c(min_tempo2, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme    = theme_minimal(base_size = 12)
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
resumo_tempo_km_comum(dados_eda_clean, "gravidez", "tipo de gravidez")

#-------------------  SALVA GRÁFICO LOCALMENTE
png("kmglobal_gravidez.png", units = "in", width = 10, height = 5, res = 112)
print(painel_kmglobal_grav)
dev.off()

#------------------------------- Log-Rank Tipo de gravidez

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

# Teste de Gray e CIF para tipo de gravidez
ci_grav$Tests

#------------------------------- CIF
# 
# p_cif_grav <- ggcompetingrisks(
#   fit             = ci_grav,
#   multiple_panels = TRUE,   # um painel para Única e outro para Múltipla
#   ggtheme         = theme_minimal(base_size = 12),
#   palette         = c("#1f77b4", "#d62728"),   # 1 = Óbito fetal, 2 = Nascido vivo
#   legend.title    = "Evento",
#   legend.labs     = c("Óbito fetal", "Nascimento vivo"),
#   xlab            = "Semanas de gestação",
#   ylab            = "Incidência acumulada",
#   title           = "CIF por tipo de gravidez"
# )
# print(p_cif_grav)
# 
# #-------------------  SALVA GRÁFICO LOCALMENTE
# png("cif_gravidez.png", units = "in", width = 10, height = 5, res = 300)
# print(p_cif_grav)
# dev.off()

# CIF apenas do evento óbito fetal por tipo de gravidez

# Selecionar apenas as curvas cujo nome termina em "Óbito fetal"
ci_grav_evento1 <- ci_grav[grep("Óbito fetal$", names(ci_grav))]

# Converter em data.frame longo
df_cif_fd_grav <- purrr::map_dfr(
  names(ci_grav_evento1),
  function(nm) {
    comp <- ci_grav_evento1[[nm]]
    
    # nm vem no formato "Única Óbito fetal" ou "Múltipla Óbito fetal"
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

# Selecionar apenas as curvas cujo nome termina em "Nascimento vivo"
ci_grav_evento2 <- ci_grav[grep("Nascimento vivo$", names(ci_grav))]

df_cif_lv_grav <- purrr::map_dfr(
  names(ci_grav_evento2),
  function(nm) {
    comp <- ci_grav_evento2[[nm]]
    
    # nm vem no formato "Única Nascimento vivo" ou "Múltipla Nascimento vivo"
    grupo <- sub(" Nascimento vivo$", "", nm)
    
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

print(table(df_cif_lv_grav$grupo, useNA = "ifany"))

#--------------------------- Limites de tempo e escala (zoom a partir da semana 19)

max_tempo_cif_grav <- max(
  df_cif_fd_grav$tempo,
  df_cif_lv_grav$tempo,
  na.rm = TRUE
)

breaks_tempo_cif_grav <- seq(20, max_tempo_cif_grav, by = 5)

#--------------------------- CIF – ÓBITO FETAL por tipo de gravidez

p_cif_fd_grav <- ggplot(
  df_cif_fd_grav,
  aes(x = tempo,
      y = cif,
      linetype = grupo)
) +
  geom_step(
    color     = "#d62728",  # vermelho para óbito fetal
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Tipo de gravidez",
    values = c(
      "Única"    = "solid",
      "Múltipla" = "dashed"
    ),
    breaks = c("Única", "Múltipla")
  ) +
  scale_x_continuous(
    breaks = breaks_tempo_cif_grav
  ) +
  coord_cartesian(
    xlim = c(min_tempo1, max_tempo_cif_grav)
  ) +
  labs(
    title = "Óbito fetal (por tipo de gravidez)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(
        colour = "grey20"   # legenda neutra
      )
    )
  )

print(p_cif_fd_grav)

#--------------------------- CIF – NASCIMENTO VIVO por tipo de gravidez

p_cif_lv_grav <- ggplot(
  df_cif_lv_grav,
  aes(x = tempo,
      y = cif,
      linetype = grupo)
) +
  geom_step(
    color     = "#1f77b4",  # azul para nascimento vivo
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Tipo de gravidez",
    values = c(
      "Única"    = "solid",
      "Múltipla" = "dashed"
    ),
    breaks = c("Única", "Múltipla")
  ) +
  scale_x_continuous(
    breaks = breaks_tempo_cif_grav
  ) +
  coord_cartesian(
    xlim = c(min_tempo2, max_tempo_cif_grav)
  ) +
  labs(
    title = "Nascido vivo (por tipo de gravidez)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(
        colour = "grey20"
      )
    )
  )

print(p_cif_lv_grav)

#--------------------------- Painel: Nascidos vivos (esq.) × Óbitos fetais (dir.)

painel_cif_grav <- ggpubr::ggarrange(
  p_cif_fd_grav,
  p_cif_lv_grav,
  ncol          = 2,
  nrow          = 1,
  common.legend = TRUE,
  legend        = "bottom"
)

print(painel_cif_grav)

#-------------------  SALVA PAINEL LOCALMENTE
png(
  "cif_gravidez_painel_ambos.png",
  units = "in",
  width = 10,
  height = 5,
  res   = 112
)
print(painel_cif_grav)
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
  conf.int    = FALSE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t)",
  title       = "KM - Óbito fetal (censura: nascido vivo)\npor tipo de parto",
  legend.title = "Tipo de parto",
  legend.labs  = levels(dados_eda_clean$parto),
  ylim        = c(0.979, 1.00), # zoom
  xlim       = c(min_tempo1, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme    = theme_minimal(base_size = 12)
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
  conf.int    = FALSE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t)",
  title       = "KM - Nascido vivo (censura: óbito fetal)\npor tipo de parto",
  legend.title = "Tipo de parto",
  legend.labs  = levels(dados_eda_clean$parto),
  ylim        = c(0, 1), # zoom
  xlim       = c(min_tempo2, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme    = theme_minimal(base_size = 12)
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
resumo_tempo_km_comum(dados_eda_clean, "parto", "tipo de parto")

#-------------------  SALVA GRÁFICO LOCALMENTE
png("kmglobal_parto.png", units = "in", width = 10, height = 5, res = 112)
print(painel_kmglobal_parto)
dev.off()

#------------------------------- Log-Rank Tipo de parto

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

# Teste de Gray e CIF para tipo de parto
ci_parto$Tests

#------------------------------- CIF
# 
# p_cif_parto <- ggcompetingrisks(
#   fit             = ci_parto,
#   multiple_panels = TRUE,   # um painel para Vaginal e outro para Cesáreo
#   ggtheme         = theme_minimal(base_size = 12),
#   palette         = c("#1f77b4", "#d62728"),   # 1 = Óbito fetal, 2 = Nascido vivo
#   legend.title    = "Evento",
#   legend.labs     = c("Óbito fetal", "Nascimento vivo"),
#   xlab            = "Semanas de gestação",
#   ylab            = "Incidência acumulada",
#   title           = "CIF por tipo de parto"
# )
# print(p_cif_parto)
# 
# #-------------------  SALVA GRÁFICO LOCALMENTE
# png("cif_parto.png", units = "in", width = 10, height = 5, res = 300)
# print(p_cif_parto)
# dev.off()

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

# Evento 2 = Nascimento vivo
ci_parto_evento2 <- ci_parto[grep("Nascimento vivo$", names(ci_parto))]

df_cif_lv_parto <- purrr::map_dfr(
  names(ci_parto_evento2),
  function(nm) {
    comp  <- ci_parto_evento2[[nm]]
    grupo <- sub(" Nascimento vivo$", "", nm)
    
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

print(table(df_cif_lv_parto$grupo, useNA = "ifany"))

#--------------------------- Limites de tempo e escala (zoom a partir da semana 19)

max_tempo_cif_parto <- max(
  df_cif_fd_parto$tempo,
  df_cif_lv_parto$tempo,
  na.rm = TRUE
)

breaks_tempo_cif_parto <- seq(20, max_tempo_cif_parto, by = 5)

#--------------------------- CIF – ÓBITO FETAL por tipo de parto

p_cif_fd_parto <- ggplot(
  df_cif_fd_parto,
  aes(x = tempo,
      y = cif,
      linetype = grupo)
) +
  geom_step(
    color     = "#d62728",   # vermelho para óbito fetal
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Tipo de parto",
    values = c(
      "Vaginal" = "solid",
      "Cesáreo" = "dashed"
    ),
    breaks = c("Vaginal", "Cesáreo")
  ) +
  scale_x_continuous(
    breaks = breaks_tempo_cif_parto
  ) +
  coord_cartesian(
    xlim = c(min_tempo1, max_tempo_cif_parto)
  ) +
  labs(
    title = "Óbito fetal (por tipo de parto)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  ) +
  # Legenda neutra (cinza), compartilhada com o painel
  guides(
    linetype = guide_legend(
      override.aes = list(
        colour = "grey20"
      )
    )
  )

print(p_cif_fd_parto)

#--------------------------- CIF – NASCIMENTO VIVO por tipo de parto

p_cif_lv_parto <- ggplot(
  df_cif_lv_parto,
  aes(x = tempo,
      y = cif,
      linetype = grupo)
) +
  geom_step(
    color     = "#1f77b4",   # azul para nascimento vivo
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Tipo de parto",
    values = c(
      "Vaginal" = "solid",
      "Cesáreo" = "dashed"
    ),
    breaks = c("Vaginal", "Cesáreo")
  ) +
  scale_x_continuous(
    breaks = breaks_tempo_cif_parto
  ) +
  coord_cartesian(
    xlim = c(min_tempo2, max_tempo_cif_parto)
  ) +
  labs(
    title = "Nascido vivo (por tipo de parto)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(
        colour = "grey20"
      )
    )
  )

print(p_cif_lv_parto)

#--------------------------- Painel: Nascidos vivos (esq.) × Óbitos fetais (dir.)

painel_cif_parto <- ggpubr::ggarrange(
  p_cif_fd_parto,
  p_cif_lv_parto,
  ncol          = 2,
  nrow          = 1,
  common.legend = TRUE,
  legend        = "bottom"
)

print(painel_cif_parto)

#-------------------  SALVA PAINEL LOCALMENTE
png(
  "cif_parto_painel_ambos.png",
  units = "in",
  width = 10,
  height = 5,
  res   = 112
)
print(painel_cif_parto)
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
  conf.int    = FALSE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t)",
  title       = "KM - Óbito fetal (censura: nascido vivo)\npor faixa de peso ao nascer",
  legend.title = "Peso ao nascer",
  legend.labs  = levels(dados_eda_clean$peso_categorico),
  ylim        = c(0.900, 1.00), # zoom
  xlim       = c(min_tempo1, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme    = theme_minimal(base_size = 12)
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
  conf.int    = FALSE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t)",
  title       = "KM - Nascido vivo (censura: óbito fetal)\npor faixa de peso ao nascer",
  legend.title = "Peso ao nascer",
  legend.labs  = levels(dados_eda_clean$peso_categorico),
  ylim        = c(0, 1), # zoom
  xlim       = c(min_tempo2, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme    = theme_minimal(base_size = 12)
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
resumo_tempo_km_comum(dados_eda_clean, "peso_categorico", "faixa de peso ao nascer")

#-------------------  SALVA GRÁFICO LOCALMENTE
png("kmglobal_peso_categorico.png", units = "in", width = 10, height = 5, res = 112)
print(painel_kmglobal_peso)
dev.off()

#------------------------------- Log-Rank Faixa de peso ao nascer

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

# Teste de Gray e CIF para peso_categorico
ci_peso$Tests

#------------------------------- CIF
# 
# p_cif_peso <- ggcompetingrisks(
#   fit             = ci_peso,
#   multiple_panels = TRUE,   # um painel para cada faixa de peso
#   ggtheme         = theme_minimal(base_size = 12),
#   palette         = c("#1f77b4", "#d62728"),   # 1 = Óbito fetal, 2 = Nascido vivo
#   legend.title    = "Evento",
#   legend.labs     = c("Óbito fetal", "Nascimento vivo"),
#   xlab            = "Semanas de gestação",
#   ylab            = "Incidência acumulada",
#   title           = "CIF por faixa de peso ao nascer"
# )
# print(p_cif_peso)
# 
# #-------------------  SALVA GRÁFICO LOCALMENTE
# png("cif_peso_categorico.png", units = "in", width = 10, height = 5, res = 300)
# print(p_cif_peso)
# dev.off()

# CIF apenas do evento óbito fetal por faixa de peso ao nascer (sem IC)

# Evento 1 (Óbito fetal)
ci_peso_evento1 <- ci_peso[grep("Óbito fetal$", names(ci_peso))]
# Evento 2 (Nascimento vivo)
ci_peso_evento2 <- ci_peso[grep("Nascimento vivo$", names(ci_peso))]

# 3. Construir data.frames longos para cada evento
#    Cada linha terá: tempo, cif, e grupo (faixa de peso)

# Data frame para óbito fetal (evento 1)
df_cif_fd_peso <- purrr::map_dfr(
  names(ci_peso_evento1),
  function(nm) {
    comp  <- ci_peso_evento1[[nm]]
    grupo <- sub(" Óbito fetal$", "", nm)  # remove sufixo do nome
    tibble::tibble(
      tempo = comp$time,
      cif   = comp$est,
      grupo = grupo
    )
  }
) %>%
  dplyr::mutate(
    # Ajusta a ordem dos níveis das categorias de peso
    grupo = factor(grupo,
                   levels = c("<2500g", "2500g-3999g", "4000g+"))
  ) %>%
  dplyr::arrange(grupo, tempo)

# Data frame para nascimento vivo (evento 2)
df_cif_lv_peso <- purrr::map_dfr(
  names(ci_peso_evento2),
  function(nm) {
    comp  <- ci_peso_evento2[[nm]]
    grupo <- sub(" Nascimento vivo$", "", nm)
    tibble::tibble(
      tempo = comp$time,
      cif   = comp$est,
      grupo = grupo
    )
  }
) %>%
  dplyr::mutate(
    grupo = factor(grupo,
                   levels = c("<2500g", "2500g-3999g", "4000g+"))
  ) %>%
  dplyr::arrange(grupo, tempo)

# 4. Definir limites e pontos de corte do eixo x
#    (Zoom a partir da semana 19 e intervalos de 5 em 5 semanas)
max_tempo_cif_peso <- max(
  df_cif_fd_peso$tempo,
  df_cif_lv_peso$tempo,
  na.rm = TRUE
)
breaks_tempo_cif_peso <- seq(20, max_tempo_cif_peso, by = 5)

# 5. Plotar a CIF apenas para ÓBITOS FETAIS (evento 1)
p_cif_fd_peso <- ggplot(
  df_cif_fd_peso,
  aes(x = tempo,
      y = cif,
      linetype = grupo)
) +
  geom_step(
    color     = "#d62728",      # vermelho para óbito fetal
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Peso ao nascer",
    values = c(
      "<2500g"      = "solid",
      "2500g-3999g" = "dashed",
      "4000g+"      = "dotted"
    )
  ) +
  scale_x_continuous(
    breaks = breaks_tempo_cif_peso
  ) +
  coord_cartesian(
    xlim = c(min_tempo1, max_tempo_cif_peso)
  ) +
  labs(
    title = "Óbito fetal (por faixa de peso)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  ) +
  # Legenda neutra (cinza), mas com o padrão de linha preservado
  guides(
    linetype = guide_legend(
      override.aes = list(
        colour = "grey20"
      )
    )
  )

# 6. Plotar a CIF apenas para NASCIDOS VIVOS (evento 2)
p_cif_lv_peso <- ggplot(
  df_cif_lv_peso,
  aes(x = tempo,
      y = cif,
      linetype = grupo)
) +
  geom_step(
    color     = "#1f77b4",      # azul para nascimento vivo
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Peso ao nascer",
    values = c(
      "<2500g"      = "solid",
      "2500g-3999g" = "dashed",
      "4000g+"      = "dotted"
    )
  ) +
  scale_x_continuous(
    breaks = breaks_tempo_cif_peso
  ) +
  coord_cartesian(
    xlim = c(min_tempo2, max_tempo_cif_peso)
  ) +
  labs(
    title = "Nascido vivo (por faixa de peso)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(
        colour = "grey20"
      )
    )
  )

# 7. Montar o painel com os dois gráficos lado a lado
painel_cif_peso <- ggpubr::ggarrange(
  p_cif_fd_peso,
  p_cif_lv_peso,
  ncol          = 2,
  nrow          = 1,
  common.legend = TRUE,
  legend        = "bottom"
)

print(painel_cif_peso)

# 8. Opcional: salvar o painel em arquivo PNG
png(
  "cif_peso_categorico_painel_ambos.png",
  units = "in",
  width = 10,
  height = 5,
  res   = 112
)
print(painel_cif_peso)
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
  conf.int    = FALSE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t)",
  title       = "KM - Óbito fetal (censura: nascido vivo)\npor local de ocorrência do parto",
  legend.title = "Local de ocorrência",
  legend.labs  = levels(dados_eda_clean$lococornasc),
  ylim        = c(0.950, 1.00), # zoom
  xlim       = c(min_tempo1, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme    = theme_minimal(base_size = 12)
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
  conf.int    = FALSE,
  xlab        = "Semanas de gestação",
  ylab        = "S(t)",
  title       = "KM - Nascido vivo (censura: óbito fetal)\npor local de ocorrência do parto",
  legend.title = "Local de ocorrência",
  legend.labs  = levels(dados_eda_clean$lococornasc),
  ylim        = c(0, 1), # zoom
  xlim       = c(min_tempo2, max_tempo), # zoom (comportamento ocorre a partir da semana 20)
  break.time.by = 5,
  ggtheme    = theme_minimal(base_size = 12)
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
resumo_tempo_km_comum(dados_eda_clean, "lococornasc", "local de ocorrência")

#-------------------  SALVA GRÁFICO LOCALMENTE
png("kmglobal_lococornasc.png", units = "in", width = 10, height = 5, res = 112)
print(painel_kmglobal_loc)
dev.off()

#------------------------------- Log-Rank Local de ocorrência do parto

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

# Teste de Gray e CIF para local de ocorrência
ci_loc$Tests

#------------------------------- CIF
# 
# p_cif_loc <- ggcompetingrisks(
#   fit             = ci_loc,
#   multiple_panels = TRUE,   # um painel para Hospital e outro para Outros
#   ggtheme         = theme_minimal(base_size = 12),
#   palette         = c("#1f77b4", "#d62728"),   # 1 = Óbito fetal, 2 = Nascido vivo
#   legend.title    = "Evento",
#   legend.labs     = c("Óbito fetal", "Nascimento vivo"),
#   xlab            = "Semanas de gestação",
#   ylab            = "Incidência acumulada",
#   title           = "CIF por local de ocorrência do parto"
# )
# print(p_cif_loc)
# 
# #-------------------  SALVA GRÁFICO LOCALMENTE
# png("cif_lococornasc.png", units = "in", width = 10, height = 5, res = 300)
# print(p_cif_loc)
# dev.off()

# CIF apenas do evento óbito fetal por local de ocorrência (sem IC)

# 2. Separar as curvas por tipo de evento
#    - Evento 1 = "Óbito fetal"
#    - Evento 2 = "Nascimento vivo"
ci_loc_evento1 <- ci_loc[grep("Óbito fetal$",     names(ci_loc))]
ci_loc_evento2 <- ci_loc[grep("Nascimento vivo$", names(ci_loc))]

# 3. Construir data.frames longos para cada evento (tempo, cif, grupo)
# Data frame para óbito fetal (evento 1)
df_cif_fd_loc <- purrr::map_dfr(
  names(ci_loc_evento1),
  function(nm) {
    comp  <- ci_loc_evento1[[nm]]
    grupo <- sub(" Óbito fetal$", "", nm)  # remove o sufixo
    tibble::tibble(
      tempo = comp$time,
      cif   = comp$est,
      grupo = grupo
    )
  }
) %>%
  dplyr::mutate(
    # Garante que “Hospital” aparece antes de “Outros”
    grupo = factor(grupo, levels = c("Hospital", "Outros"))
  ) %>%
  dplyr::arrange(grupo, tempo)

# Data frame para nascimento vivo (evento 2)
df_cif_lv_loc <- purrr::map_dfr(
  names(ci_loc_evento2),
  function(nm) {
    comp  <- ci_loc_evento2[[nm]]
    grupo <- sub(" Nascimento vivo$", "", nm)
    tibble::tibble(
      tempo = comp$time,
      cif   = comp$est,
      grupo = grupo
    )
  }
) %>%
  dplyr::mutate(
    grupo = factor(grupo, levels = c("Hospital", "Outros"))
  ) %>%
  dplyr::arrange(grupo, tempo)

# 4. Definir limites e pontos de corte do eixo x
max_tempo_cif_loc <- max(
  df_cif_fd_loc$tempo,
  df_cif_lv_loc$tempo,
  na.rm = TRUE
)
breaks_tempo_cif_loc <- seq(20, max_tempo_cif_loc, by = 5)

# 5. Plotar a CIF apenas para ÓBITOS FETAIS (evento 1)
#    - Vermelho (#d62728) para óbitos fetais
#    - Linhas sólidas/dash para Hospital/Outros
p_cif_fd_loc <- ggplot(
  df_cif_fd_loc,
  aes(x = tempo,
      y = cif,
      linetype = grupo)
) +
  geom_step(
    color     = "#d62728",
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Local de ocorrência",
    values = c(
      "Hospital" = "solid",
      "Outros"   = "dashed"
    )
  ) +
  scale_x_continuous(
    breaks = breaks_tempo_cif_loc
  ) +
  coord_cartesian(
    xlim = c(min_tempo1, max_tempo_cif_loc)
  ) +
  labs(
    title = "Óbito fetal (por local de ocorrência)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  ) +
  # Legenda neutra (cinza) para diferenciar o tipo de linha
  guides(
    linetype = guide_legend(
      override.aes = list(
        colour = "grey20"
      )
    )
  )

# 6. Plotar a CIF apenas para NASCIDOS VIVOS (evento 2)
#    - Azul (#1f77b4) para nascimentos vivos
p_cif_lv_loc <- ggplot(
  df_cif_lv_loc,
  aes(x = tempo,
      y = cif,
      linetype = grupo)
) +
  geom_step(
    color     = "#1f77b4",
    linewidth = 0.9
  ) +
  scale_linetype_manual(
    name   = "Local de ocorrência",
    values = c(
      "Hospital" = "solid",
      "Outros"   = "dashed"
    )
  ) +
  scale_x_continuous(
    breaks = breaks_tempo_cif_loc
  ) +
  coord_cartesian(
    xlim = c(min_tempo2, max_tempo_cif_loc)
  ) +
  labs(
    title = "Nascido vivo (por local de ocorrência)",
    x     = "Semanas de gestação",
    y     = "Incidência acumulada"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(
        colour = "grey20"
      )
    )
  )

# 7. Combinar ambos os gráficos em um painel
painel_cif_loc <- ggpubr::ggarrange(
  p_cif_fd_loc,
  p_cif_lv_loc,
  ncol          = 2,
  nrow          = 1,
  common.legend = TRUE,
  legend        = "bottom"
)

print(painel_cif_loc)

# 8. (Opcional) Salvar o painel como PNG
png(
  "cif_lococornasc_painel_ambos.png",
  units = "in",
  width = 10,
  height = 5,
  res   = 112
)
print(painel_cif_loc)
dev.off()
