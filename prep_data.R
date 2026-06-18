########################## Preparação de Dados #################################
### Guilherme Akira Awane



###list.files() <- usado para verificar quais arquivos estão no diretório
list.files()


### library() <- carregamento de pacotes #######################################
### observação: Se os pacotes não estiverem instalados, use a função:
### install.packages(), exemplo install.packages ("tidyverse")
### depois de instalados, carregue com o library
library(tidyverse)
library(readxl)
library(janitor)

### Carregamento dos dados######################################################
### read_xlsx() função do pacote readxl, utilizada porque os arquivos originais
### estão em .xlsx, sempre de preferência por salvar em .csv, são arquivos mais
### leves e simples, aqui poderíamos ter carregado eles comm função do R base
### read.csv(). Repare que estou criando objetos db_1; db_2; db_3; Cada objeto
### é uma aba da planilha original, isso é evidenciado por sheet = e o número
### identificando a aba. db_x = dadosbrutos_numeroaba

db_1 <- read_xlsx("Planilha ic_tcc.xlsx", sheet = 1)
db_2 <- read_xlsx("Planilha ic_tcc.xlsx", sheet = 2)
db_3 <- read_xlsx("Planilha ic_tcc.xlsx", sheet = 3)

### Verficicação padrão para observar se o nome no cabeçalho das colunas difere
### entre os objetos.

setdiff(names(db_1), names(db_2))
setdiff(names(db_1), names(db_3))
setdiff(names(db_2), names(db_3))

### copare_df_cols() Comando do pacote janitor utilizado para comparar colunas 
### de dataframes diferentes. Aqui já observamos problemáticas no seu banco de
### dados, uma vez que, uma mesma coluna adota tipo de dados diferentes em abas
### diferentes.

compare_df_cols(db_1, db_2)
compare_df_cols(db_1, db_3)
compare_df_cols(db_2, db_3)

### colnames() utilizado para exibir o nome das colunas de um dataframe

colnames(db_1)

### Correção de nomes problemáticos do dataframe ###############################
### criação do objeto db_1_renamed depositando nele o db_1, mas depois de fazer
### a modificação nos nomes, evidênciado pelo pip %>%  <- sempre para realizar
### opreações seguidas. rename() <- renomear as colunas. Se fossemos traduzir o
### que foi escrito para a nossa lingua, seria algo como: "Quero criar o objeto
### db_1_renamed utilizando o objeto db_1, mas após ( %>% ) renomear as colunas.
db_1_renamed <- db_1 %>% 
  rename(
  data = Data,
  horario = hora,
  id_video = `id video`, #repare a problemática que são os espaços
  id_plataforma = plat,
  alim_disp = `alim disp`,#ao invés de espaços use _ (underscore)
  alim_esco_capuchin = `Alim esco (macaco)`,#evite letras maísculas
  alim = comendo,
  id_animal = `Animal np`,
  id_especie = especie,
  sex_age = `classe sex age`,
  id_capuchin = id,
  vocalizacao = vocalização,#evite caractéres especiais coo acentos
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



### colnames usado aqui para verificar se os nomes realmente foram modificados

colnames(db_1_renamed)

colnames(db_2_renamed)

colnames(db_3_renamed)

### modificando a ordem das colunas do dataframe. Existem várias formas de se
### fazer isso, aqui usamos a função select() do tidyverse

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


colnames(db_1_renamed) ### colnames usado agora para verificar a ordem

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

### Após saber os nomesm utilizamos a função unique(), essa função mostra quais
### os valores estão presentes na coluna de um dataframe sem repetí-los.
### unique(dados$"coluna_desejada") <-  note que o $ está mostrando que a
### "coluna_desejada" está dentro de dados. 

unique(db_1_renamed$alim_disp)

### Modificação dos valores errados dentro das colunas #########################
### db_1_renamed <- db_renamed %>%  criar objeto db_1_renamed  com db_1_renamed
### realizando as seguintes %>% operações:
### mutate() modificar coluna
### casewhen() identificar alguma condição
### str_detect() detectar a string (sequência de caracteres)

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

unique(db_1_renamed$sex_age)
db_1_renamed <- db_1_renamed %>%
  mutate(
    sex_age = case_when(
      str_detect(sex_age, "JUV") ~ "JU",
      str_detect(sex_age, "ni") ~ "NI",
      TRUE ~ sex_age
    )
  )

### Substituindo todos os NA sem sex_age por NI para manter um padrão.

db_1_renamed <- db_1_renamed %>% 
  mutate(sex_age = replace_na(sex_age, "NI"))

unique(db_1_renamed$alim)
db_1_renamed <- db_1_renamed %>%
  mutate(
    alim = case_when(
      str_detect(alim, "0.0") ~ "0",
      str_detect(alim, "1.0") ~ "1",
      str_detect(alim, "ni") ~ "0",
      TRUE ~ alim
    )
  )

#conferindo valor de todas as outras colunas

unique(db_1_renamed$vocalizacao)
unique(db_1_renamed$agonismo_disputa_alimento)
unique(db_1_renamed$agonismo_deslocar)
unique(db_1_renamed$agonismo_deslocado)
unique(db_1_renamed$agonismo_ameaca)
unique(db_1_renamed$agonismo_int_intra)
unique(db_1_renamed$agonismo_int_inter)
unique(db_1_renamed$positiva_int_intra)
unique(db_1_renamed$positiva_int_inter)
unique(db_1_renamed$neutra_int_intra)
unique(db_1_renamed$neutra_int_inter)

### modificando a coluna data e horário para formato de data e hora

db_1_renamed$data <- format(db_1_renamed$data, format = "%Y-%m-%d", tz = "UTC")
db_1_renamed$horario <- format(db_1_renamed$horario, format = "%H:%M:%S", tz = "UTC")

### transfomrando váriaveis em fatores afim de prevenir erro na análise.

db_1_renamed$alim <- as.factor(db_1_renamed$alim)
db_1_renamed$sex_age <- as.factor(db_1_renamed$sex_age)
db_1_renamed$id_plataforma <- as.factor(db_1_renamed$id_plataforma)
db_1_renamed$id_video <- as.factor(db_1_renamed$id_video)
glimpse(db_1_renamed) # conferindo se as váriaveis foram transformadas

#salvando dataframe
write.csv(db_1_renamed, "dados_brutos1.csv")

#filtrando dataframe apenas para macaco_prego.
dados_1_macaco <- db_1_renamed %>% 
  filter(id_animal == "macaco_prego")

#salvando dataframe de macaco
write.csv(dados_1_macaco, "dados_1_macacos.csv")

