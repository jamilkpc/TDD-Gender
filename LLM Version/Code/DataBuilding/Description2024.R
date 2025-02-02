library(tidyverse)

dfCandidato <- read.csv('~/Dropbox/DiversityBrazil/data/BaseDosDados/PrefeitosDados2008_2024.csv') %>% 
  filter(ano==2024,
         tipo_eleicao == 'eleicao ordinaria') %>% 
  select(id_municipio, ano, numero_partido, ocupacao, idade, genero) %>% 
  drop_na %>%
  mutate(mulher = if_else(genero == 'feminino',1,0)) %>% 
  summarise(gender = sum(mulher)/n())