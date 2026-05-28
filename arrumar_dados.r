library(tidyverse)
library(readxl)
library(magrittr)
library(janitor)
library(glue)

Quantitativo_2024 <- read_excel("quant_2024.xlsx")
Quantitativo_2025 <- read_excel("quant_2025.xlsx")


Quantitativo_2024 %<>%
  filter(
    `DEP. ADMINISTRATIVA` == "ESTADUAL",
    `CONVENIO` == "NÃO POSSUI"
  ) %>%
  mutate(
    `CÓD. TURMA` = as.character(`CÓD. TURMA`),
    `SÉRIE` = case_when(
      `ENSINO NÍVEL` == "Ensino Médio" & `SÉRIE` == "1º Ano" ~ "1ª Série",
      `ENSINO NÍVEL` == "Ensino Médio" & `SÉRIE` == "2º Ano" ~ "2ª Série",
      `ENSINO NÍVEL` == "Ensino Médio" & `SÉRIE` == "3º Ano" ~ "3ª Série",
      TRUE ~ `SÉRIE`
    )
  ) %>%
  filter(
    `SÉRIE` %in%
      c(glue("{seq(1,9,1)} º Ano"), glue("{seq(1,3,1)}ª Série"))
  ) %>%
  select(
    `MUNICÍPIO`,
    `COD. ESCOLA`,
    ESCOLA,
    `CARACTERÍSTICA`,
    TURNO,
    `ENSINO MODALIDADE`,
    `SÉRIE`,
    `CÓD. TURMA`,
    TURMA,
    INTEGRAL,
    CAPACIDADE,
    `QTDE. ALUNOS FREQUENTES`
  ) %>%
  mutate(
    `CÓD. TURMA` = glue("{`COD. ESCOLA`} - {`CÓD. TURMA`}"),
    ANO = 2024
  ) %>%
  clean_names()

Quantitativo_2025 %<>%
  filter(
    `DEP. ADMINISTRATIVA` == "ESTADUAL",
    `CONVENIO` == "NÃO POSSUI"
  ) %>%
  mutate(
    `CÓD. TURMA` = as.character(`CÓD. TURMA`),
    `SÉRIE` = case_when(
      `ENSINO NÍVEL` == "Ensino Médio" & `SÉRIE` == "1º Ano" ~ "1ª Série",
      `ENSINO NÍVEL` == "Ensino Médio" & `SÉRIE` == "2º Ano" ~ "2ª Série",
      `ENSINO NÍVEL` == "Ensino Médio" & `SÉRIE` == "3º Ano" ~ "3ª Série",
      TRUE ~ `SÉRIE`
    )
  ) %>%
  filter(
    `SÉRIE` %in%
      c(glue("{seq(1,9,1)} º Ano"), glue("{seq(1,3,1)}ª Série"))
  ) %>%
  select(
    `MUNICÍPIO`,
    `COD. ESCOLA`,
    ESCOLA,
    `CARACTERÍSTICA`,
    TURNO,
    `ENSINO MODALIDADE`,
    `SÉRIE`,
    `CÓD. TURMA`,
    TURMA,
    INTEGRAL,
    CAPACIDADE,
    `QTDE. ALUNOS FREQUENTES`
  ) %>%
  mutate(
    `CÓD. TURMA` = glue("{`COD. ESCOLA`} - {`CÓD. TURMA`}"),
    ANO = 2025
  ) %>%
  clean_names()

Quantitativo <- bind_rows(Quantitativo_2024, Quantitativo_2025)

Quantitativo_limpo <- Quantitativo %>%
  group_by(municipio, cod_escola, escola, serie, ano) %>%
  summarise(
    caracteristica = first(caracteristica),
    capacidade = sum(capacidade, na.rm = TRUE),
    qtde_alunos_frequentes = sum(qtde_alunos_frequentes, na.rm = TRUE),
    .groups = "drop"
  )

Quantitativo_limpo <- Quantitativo_limpo %>%
  mutate(
    serie_anterior = case_when(
      serie == "1 º Ano" ~ NA_character_,
      serie == "2 º Ano" ~ "1 º Ano",
      serie == "3 º Ano" ~ "2 º Ano",
      serie == "4 º Ano" ~ "3 º Ano",
      serie == "5 º Ano" ~ "4 º Ano",
      serie == "6 º Ano" ~ "5 º Ano",
      serie == "7 º Ano" ~ "6 º Ano",
      serie == "8 º Ano" ~ "7 º Ano",
      serie == "9 º Ano" ~ "8 º Ano",
      serie == "1ª Série" ~ "9 º Ano",
      serie == "2ª Série" ~ "1ª Série",
      serie == "3ª Série" ~ "2ª Série",
      TRUE ~ NA_character_
    )
  )

df_2025 <- Quantitativo_limpo %>% filter(ano == 2025)
df_2024 <- Quantitativo_limpo %>%
  filter(ano == 2024) %>%
  select(cod_escola, serie, qtde_anterior = qtde_alunos_frequentes)

df_model <- df_2025 %>%
  left_join(
    df_2024,
    by = c("cod_escola" = "cod_escola", "serie_anterior" = "serie")
  ) %>%
  mutate(Y_anterior = replace_na(qtde_anterior, 0))

df_model <- df_model %>%
  mutate(
    id_escola_num = as.integer(as.factor(cod_escola)),
    id_serie_num = as.integer(as.factor(serie)),
    id_tipo_num = as.integer(as.factor(caracteristica)),
    id_cidades_num = as.integer(as.factor(municipio))
  )

df_model <- df_model %>%
  mutate(
    capacidade = pmax(capacidade, pmax(qtde_alunos_frequentes, 1))
  )
mapa_escola_cidade <- df_model %>%
  group_by(id_escola_num) %>%
  summarise(id_cidades_num = first(id_cidades_num), .groups = "drop") %>%
  arrange(id_escola_num) %>%
  pull(id_cidades_num)

dat.6 <- list(
  N = nrow(df_model),
  Y = as.integer(df_model$qtde_alunos_frequentes),
  Y_anterior = as.integer(df_model$Y_anterior),
  C_serie = as.integer(df_model$capacidade),
  E = max(df_model$id_escola_num),
  escola_id = df_model$id_escola_num,
  K = max(df_model$id_serie_num),
  serie_id = df_model$id_serie_num,
  F = max(df_model$id_tipo_num),
  tipo_id = df_model$id_tipo_num,
  C_cidades = max(df_model$id_cidades_num),
  escola_cidade = mapa_escola_cidade
)

saveRDS(dat.6,file="dat6.rds")
