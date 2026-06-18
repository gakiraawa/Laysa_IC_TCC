library(tidyverse)
library(glmmTMB)
library(DHARMa)

# 1. Carregar os dados
df <- read.csv("dados_1_macacos.csv")

# 2. Limpeza e Transformação
df_long <- df %>%
  # Remove linhas onde a disponibilidade de alimento é NA
  # E também onde o alimento escolhido é NA, caso queira focar apenas em escolhas concretas
  filter(!is.na(alim_disp)) %>% 
  
  # Agora, se você quiser remover casos onde não houve escolha (se o NA em alim_esco_capuchin significar "nada foi escolhido")
  # Use: filter(!is.na(alim_esco_capuchin))
  
  # Separa os alimentos disponíveis que estão separados por "_"
  separate_rows(alim_disp, sep = "_") %>%
  
  # Cria a coluna is_chosen
  # Nota: Se alim_esco_capuchin for NA, o is_chosen será 0 para todos os itens, 
  # o que é correto se nenhum alimento foi escolhido.
  mutate(is_chosen = ifelse(!is.na(alim_esco_capuchin) & alim_disp == alim_esco_capuchin, 1, 0))

# 3. Rodar o modelo
modelo <- glmmTMB(is_chosen ~ alim_disp + sex_age + (1 | id_plataforma) + (1 | id_video) +  (1 | data), 
                  family = binomial, 
                  data = df_long)

summary(modelo)
plot(simulateResiduals(modelo))
