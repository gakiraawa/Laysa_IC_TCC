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

# 1. Defina quais colunas vão virar fator (coloque os nomes exatos aqui)
colunas_para_fator <- c("coluna_A", "coluna_B", "coluna_C")

# 2. Agrupe os dataframes em uma lista temporária
meus_dfs <- list(db_1 = db_1, db_2 = db_2, db_3 = db_3)

# 3. O Loop: itera sobre cada dataframe da lista
for (nome in names(meus_dfs)) {
  meus_dfs[[nome]] <- meus_dfs[[nome]] |> 
    mutate(across(all_of(colunas_para_fator), as.factor))
}