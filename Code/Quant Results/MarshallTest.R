library(tidyverse)
library(rdlocrand)

dfCandidato <- read.csv('CandidatoPrefeito.csv')
dfVotos <- read.csv('VotosMunicipioPrefeito.csv')

pop <- read.csv('Populacao.csv', skip = 1) %>% 
  select(-Sigla,-Município) %>% 
  rename(id_municipio = Código) %>% 
  tidyr::pivot_longer(!id_municipio, names_to = "ano", values_to = "pop") %>% 
  mutate(ano = as.numeric(sub('.', '', ano))) %>% 
  mutate(logPop = log(pop)) %>% 
  tidyr::drop_na()

dfVotos <- dfVotos %>% left_join(pop) %>% filter(pop<200000)

dfEleitos <- dfVotos %>% 
  rename(eleito = resultado) %>% 
  select(id_municipio, ano, id_candidato_bd, eleito) %>% 
  filter(eleito == 'eleito')

dfId <- dfCandidato %>% 
  left_join(dfEleitos) %>% 
  filter(eleito == 'eleito') %>% 
  mutate(esquerda = if_else(numero_partido %in% c(12,13,40,50,65),1,0),
         educado = if_else(instrucao == 'ensino superior completo',1,0)) %>% 
  select(id_municipio, ano, idade, educado, esquerda)

dfRDD <- read.csv('dataCovariates.csv') %>% select(-X) %>% 
  left_join(dfId) %>% drop_na

R <- dfRDD$vote_margin
Y <- dfRDD$educado

summary(rdrobust(Y,R))

R <- dfRDD$vote_margin
Y <- dfRDD$esquerda

summary(rdrobust(Y,R))

R <- dfRDD$vote_margin
Y <- dfRDD$idade

summary(rdrobust(Y,R))
