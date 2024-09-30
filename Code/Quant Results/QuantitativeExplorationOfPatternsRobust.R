library(tidyverse)
library(rdrobust)

dfVotos <- read.csv('VotosMunicipioReeleicao.csv')

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
  filter(eleito == 'eleito') %>% 
  mutate(ano = ano+4)

dfCandidato <- read.csv('CandidatoPrefeito.csv')

dfChallengers <- dfCandidato %>% anti_join(dfEleitos)

dfPool <- dfChallengers %>% 
  mutate(esquerda = if_else(numero_partido %in% c(12,13,40,50,65),1,0),
         educado = if_else(instrucao == 'ensino superior completo',1,0),
         homem = if_else(genero == 'masculino',1,0)) %>% 
  group_by(id_municipio, ano) %>% 
  summarise(esquerda = mean(esquerda),
            educado = mean(educado),
            homem = mean(homem),
            tamanho = n()) %>% 
  ungroup

pop <- read.csv('Populacao.csv', skip = 1) %>% 
  select(-Sigla,-Município) %>% 
  rename(id_municipio = Código) %>% 
  tidyr::pivot_longer(!id_municipio, names_to = "ano", values_to = "pop") %>% 
  mutate(ano = as.numeric(sub('.', '', ano))) %>% 
  mutate(logPop = log(pop)) %>% 
  tidyr::drop_na()

dfPool <- pop %>% 
  filter(ano %in% c(2008,2012,2016,2020)) %>%
  select(id_municipio, ano) %>% 
  left_join(dfPool)

dfRDD <- read.csv('dataCovariates.csv') %>% select(-X) %>% 
  left_join(dfPool)

# Testing Size of Opposition

R <- dfRDD$vote_margin
Y <- dfRDD$tamanho

summary(rdrobust(Y, R, 0))

# Testing Gender Composition of Opposition
R <- dfRDD$vote_margin
Y <- dfRDD$homem

summary(rdrobust(Y, R, 0))

# Testing Qualification of Opposition
R <- dfRDD$vote_margin
Y <- dfRDD$educado

summary(rdrobust(Y, R, 0))

# Testing Ideology of Opposition
R <- dfRDD$vote_margin
Y <- dfRDD$esquerda

summary(rdrobust(Y, R, 0))

# Now, testing if it is important for the incumbent to be running for reelection
dfReelection <- dfCandidato %>% left_join(dfEleitos) %>% 
  mutate(incumbent_running = if_else(is.na(eleito),0,1)) %>% 
  group_by(id_municipio, ano) %>% 
  summarise(reelection = sum(incumbent_running)>0) %>% 
  ungroup %>% 
  filter(reelection==1) %>% 
  select(-reelection)

dfReelection <- dfReelection %>% left_join(dfRDD) %>% drop_na

# Testing Size of Opposition

R <- dfReelection$vote_margin
Y <- dfReelection$tamanho

summary(rdrobust(Y, R, 0))

# Testing Gender Composition of Opposition
R <- dfReelection$vote_margin
Y <- dfReelection$homem

summary(rdrobust(Y, R, 0))

# Testing Qualification of Opposition
R <- dfReelection$vote_margin
Y <- dfReelection$educado

summary(rdrobust(Y, R, 0))

# Testing Ideology of Opposition
R <- dfReelection$vote_margin
Y <- dfReelection$esquerda

summary(rdrobust(Y, R, 0))

# Now, placebo test
dfOpenSeat <- dfCandidato %>% left_join(dfEleitos) %>% 
  mutate(incumbent_running = if_else(is.na(eleito),0,1)) %>% 
  group_by(id_municipio, ano) %>% 
  summarise(reelection = sum(incumbent_running)>0) %>% 
  ungroup %>% 
  filter(reelection==0) %>% 
  select(-reelection)

dfOpenSeat <- dfOpenSeat %>% left_join(dfRDD) %>% drop_na

# Testing Size of Opposition - Still positive, but smaller result. Consistent with the idea that it is more difficult for women to coordinate local politics (political networks from Brollo).

R <- dfOpenSeat$vote_margin
Y <- dfOpenSeat$tamanho

summary(rdrobust(Y, R, 0))

# Testing Gender Composition of Opposition
R <- dfOpenSeat$vote_margin
Y <- dfOpenSeat$homem

summary(rdrobust(Y, R, 0))

# Testing Qualification of Opposition
R <- dfOpenSeat$vote_margin
Y <- dfOpenSeat$educado

summary(rdrobust(Y, R, 0))

# Testing Ideology of Opposition
R <- dfOpenSeat$vote_margin
Y <- dfOpenSeat$esquerda

summary(rdrobust(Y, R, 0))
