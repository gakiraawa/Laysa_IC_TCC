########################## preparação de dados #################################
### Guilherme Akira Awane
list.files()

library(tidyverse)
library(readxl)
library(janitor)

db_1 <- read_xlsx("Planilha ic_tcc.xlsx", sheet = 1)
db_2 <- read_xlsx("Planilha ic_tcc.xlsx", sheet = 2)
db_3 <- read_xlsx("Planilha ic_tcc.xlsx", sheet = 3)

setdiff(names(db_1), names(db_2))
setdiff(names(db_1), names(db_3))
setdiff(names(db_2), names(db_3))

compare_df_cols(db_1, db_2)
compare_df_cols(db_1, db_3)
compare_df_cols(db_2, db_3)

colnames(db_1)
colnames(db_2)
colnames(db_3)

unique(db_1$"alim disp")
unique(db_1$"Alim esco (macaco)")
unique(db_1$"Alim esco (macaco)")
unique(db_1$"classe sex age")

db_1_renamed <- db_1 %>% 
  rename(
  data = Data,
  horario = hora,
  id_video = `id video`,
  id_plataforma = plat,
  alim_disp = `alim disp`,
  alim_esco_capuchin = `Alim esco (macaco)`,
  alim = comendo,
  id_animal = `Animal np`,
  id_especie = especie,
  sex_age = `classe sex age`,
  id_capuchin = id,
  vocalizacao = vocalização,
  agonismo_disputa_alimento = `disp alim (intra)`,
  agonismo_ameaca = ameaça,
  agonismo_deslocar = deslocar,
  agonismo_deslocado = deslocado,
  agonismo_int_intra = `int intra agonis`,
  agonismo_int_inter = `int inter agonis`,
  positiva_int_intra = `int intra social`,
  positiva_int_inter = `int inter social`,
  neutra_int_intra = `int intra neutra`,
  neutra_int_inter = `int inter neutra`,
  )

colnames(db_1_renamed)
db_1_renamed <- db_1_renamed %>% 
  select(
    data,
    horario,
    id_video,
    id_plataforma,
    alim_disp,
    alim_esco_capuchin,
    id_animal,
    id_especie,
    sex_age,
    n_classe,
    n_total,
    id_capuchin,
    alim,
    vocalizacao,
    agonismo_disputa_alimento,
    agonismo_deslocar,
    agonismo_deslocado,
    agonismo_ameaca,
    agonismo_int_intra,
    agonismo_int_inter,
    positiva_int_intra,
    positiva_int_inter,
    neutra_int_intra,
    neutra_int_inter
    )
colnames(db_1_renamed)


db_2_renamed <- db_2 %>% 
  rename(
    data = Data,
    horario = hora,
    id_video = `id video`,
    id_plataforma = plat,
    alim_disp = `alim disp`,
    alim_esco_capuchin = `Alim esco (macaco)`,
    alim = comendo,
    id_animal = `Animal np`,
    id_especie = especie,
    sex_age = `classe sex age`,
    id_capuchin = id,
    vocalizacao = vocalização,
    agonismo_disputa_alimento = `disp alim (intra)`,
    agonismo_ameaca = ameaça,
    agonismo_deslocar = deslocar,
    agonismo_deslocado = deslocado,
    agonismo_int_intra = `int intra agonis`,
    agonismo_int_inter = `int inter agonis`,
    positiva_int_intra = `int intra social`,
    positiva_int_inter = `int inter social`,
    neutra_int_intra = `int intra neutra`,
    neutra_int_inter = `int inter neutra`,
  )

colnames(db_2_renamed)
db_2_renamed <- db_2_renamed %>% 
  select(
    data,
    horario,
    id_video,
    id_plataforma,
    alim_disp,
    alim_esco_capuchin,
    id_animal,
    id_especie,
    sex_age,
    n_classe,
    n_total,
    id_capuchin,
    alim,
    vocalizacao,
    agonismo_disputa_alimento,
    agonismo_deslocar,
    agonismo_deslocado,
    agonismo_ameaca,
    agonismo_int_intra,
    agonismo_int_inter,
    positiva_int_intra,
    positiva_int_inter,
    neutra_int_intra,
    neutra_int_inter
  )
colnames(db_2_renamed)


db_3_renamed <- db_3 %>% 
  rename(
    data = Data,
    horario = hora,
    id_video = `id video`,
    id_plataforma = plat,
    alim_disp = `alim disp`,
    alim_esco_capuchin = `Alim esco (macaco)`,
    alim = comendo,
    id_animal = `Animal np`,
    id_especie = especie,
    sex_age = `classe sex age`,
    id_capuchin = id,
    vocalizacao = vocalização,
    agonismo_disputa_alimento = `disp alim (intra)`,
    agonismo_ameaca = ameaça,
    agonismo_deslocar = deslocar,
    agonismo_deslocado = deslocado,
    agonismo_int_intra = `int intra agonis`,
    agonismo_int_inter = `int inter agonis`,
    positiva_int_intra = `int intra social`,
    positiva_int_inter = `int inter social`,
    neutra_int_intra = `int intra neutra`,
    neutra_int_inter = `int inter neutra`,
  )

colnames(db_3_renamed)
db_3_renamed <- db_3_renamed %>% 
  select(
    data,
    horario,
    id_video,
    id_plataforma,
    alim_disp,
    alim_esco_capuchin,
    id_animal,
    id_especie,
    sex_age,
    n_classe,
    n_total,
    id_capuchin,
    alim,
    vocalizacao,
    agonismo_disputa_alimento,
    agonismo_deslocar,
    agonismo_deslocado,
    agonismo_ameaca,
    agonismo_int_intra,
    agonismo_int_inter,
    positiva_int_intra,
    positiva_int_inter,
    neutra_int_intra,
    neutra_int_inter
  )
colnames(db_3_renamed)

  glimpse(db_1_renamed)



unique(db_1_renamed$alim_disp)

db_1_renamed <- db_1_renamed %>%
  mutate(
    alim_disp = case_when(
      str_detect(alim_disp, "mamão") ~ "mamao",
      str_detect(alim_disp, "nada \\(alim caiu no chao\\)") ~ NA,
      str_detect(alim_disp, "nada") ~ NA,
      str_detect(alim_disp, "banana, manga, mamão, abacate") ~ "abacate_banana_mamao_manga",
      str_detect(alim_disp, "banana, manga, mamão, abacate") ~ "abacate_banana_mamao_manga",
      str_detect(alim_disp, "manga, abacate") ~ "abacate_manga",
      str_detect(alim_disp, "mamão, banana, manga") ~ "banana_mamao_manga",
      str_detect(alim_disp, "mamão, manga, banana") ~ "banana_mamao_manga",
      str_detect(alim_disp, "manga, banana") ~ "banana_manga",
      str_detect(alim_disp, "mamão, melão, mexirica") ~ "mamao_melao_mexirica",
      str_detect(alim_disp, "mamão, melão") ~ "mamao_melao",
      str_detect(alim_disp, "mamão, melão") ~ "mamao_melao",
      str_detect(alim_disp, "melão, mexirica") ~ "melao_mexirica",
      str_detect(alim_disp, "melão") ~ "melao",
      TRUE ~ alim_disp
    )
  )

unique(db_1_renamed$alim_esco_capuchin)

db_1_renamed <- db_1_renamed %>%
  mutate(
    alim_esco_capuchin = case_when(
      str_detect(alim_esco_capuchin, "mamão") ~ "mamao",
      str_detect(alim_esco_capuchin, "0.0") ~ NA,
      str_detect(alim_esco_capuchin, "casca ni \\(pega do chao\\)") ~ "restos",
      str_detect(alim_esco_capuchin, "casca ni") ~ "restos",
      str_detect(alim_esco_capuchin, "nada") ~ NA,
      str_detect(alim_esco_capuchin, "trigo \\(da plantação\\)") ~ "trigo",
      str_detect(alim_esco_capuchin, "ni") ~ NA,
      str_detect(alim_esco_capuchin, "abacate \\(prov do chao\\)") ~ "abacate",
      str_detect(alim_esco_capuchin, "casca velha") ~ "restos",
      str_detect(alim_esco_capuchin, "trigo plant") ~ "trigo",
      str_detect(alim_esco_capuchin, "melao \\(casca\\)") ~ "melao",
      TRUE ~ alim_esco_capuchin
    )
  )
a

unique(db_1_renamed$id_animal)

db_1_renamed <- db_1_renamed %>%
  mutate(
    id_animal = case_when(
      str_detect(id_animal, "macaco prego") ~ "macaco_prego",
      str_detect(id_animal, "macaco-prego") ~ "macaco_prego",
      str_detect(id_animal, "ouriço-cacheiro") ~ "ourico_cacheiro",
      str_detect(id_animal, "sarue  \\(gambá de orelh bran\\)") ~ "sarue",
      str_detect(id_animal, "sarue \\(gamba de orelh branc") ~ "sarue",
      str_detect(id_animal, "ouriço cacheiro") ~ "ourico_cacheiro",
      TRUE ~ id_animal
    )
  )

db_1_renamed <- db_1_renamed %>%
  mutate(
    id_animal = case_when(
      str_detect(id_animal, "macaco prego") ~ "macaco_prego",
      str_detect(id_animal, "macaco-prego") ~ "macaco_prego",
      str_detect(id_animal, "ouriço-cacheiro") ~ "ourico_cacheiro",
      str_detect(id_animal, "sarue  \\(gambá de orelh bran\\)") ~ "sarue",
      str_detect(id_animal, "sarue \\(gamba de orelh branc") ~ "sarue",
      str_detect(id_animal, "ouriço cacheiro") ~ "ourico_cacheiro",
      TRUE ~ id_animal
    )
  )

unique(db_1_renamed$sex_age)
db_1_renamed <- db_1_renamed %>%
  mutate(
    sex_age = case_when(
      str_detect(sex_age, "JUV") ~ "JU",
      str_detect(sex_age, "ni") ~ "NI",
      TRUE ~ sex_age
    )
  )

db_1_renamed <- db_1_renamed %>% 
  mutate(sex_age = replace_na(sex_age, "NI"))

unique(db_1_renamed$alim)

# 1. Defina quais colunas vão virar fator (coloque os nomes exatos aqui)
colunas_para_fator <- c("coluna_A", "coluna_B", "coluna_C")

# 2. Agrupe os dataframes em uma lista temporária
meus_dfs <- list(db_1 = db_1, db_2 = db_2, db_3 = db_3)

# 3. O Loop: itera sobre cada dataframe da lista
for (nome in names(meus_dfs)) {
  meus_dfs[[nome]] <- meus_dfs[[nome]] |> 
    mutate(across(all_of(colunas_para_fator), as.factor))
}