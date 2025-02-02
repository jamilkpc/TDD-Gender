library(tidyverse)
library(tidylog)

dfCandidato <- read.csv('~/Dropbox/DiversityBrazil/data/BaseDosDados/PrefeitosDados2008_2024.csv') %>% 
  filter(tipo_eleicao == 'eleicao ordinaria',
         situacao == 'deferido'| ano == 2024) %>% 
  select(id_municipio, sigla_uf, ano, sequencial, titulo_eleitoral, numero_partido, instrucao, idade, genero)

dfCandidatoLag <- dfCandidato %>% 
  mutate(ano = ano + 4,
         veterano = 1) %>% 
  select(sigla_uf, ano, titulo_eleitoral, veterano)

dfVeterano <- dfCandidato %>% 
  left_join(dfCandidatoLag) %>% 
  mutate(veterano = replace_na(veterano,0)) %>% 
  select(id_municipio, ano, sequencial, veterano, genero)

dfOpenSeats <- readRDS('~/Documents/GitHub/TDD-gender/LLM Version/Data/DataOpenSeat.RDS')

DictCounts <- readRDS('~/Documents/GitHub/TDD-gender/LLM Version/Data/DataCounts.RDS') %>% 
  left_join(dfOpenSeats) %>% 
  left_join(dfVeterano) %>% 
  filter(veterano == 1) %>% 
  drop_na

dummy_matrix <- model.matrix(~ as.factor(sigla_uf) + as.factor(ano) + openSeat, data = DictCounts)
X <- dummy_matrix[,-1]


R <- DictCounts$vote_margin
Y <- DictCounts$care_count

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio)
summary(model)
rdpower(data = cbind(Y,R), tau = sd(Y[abs(R)<model$bws[1]], na.rm = T)*0.5, covs = X, cluster = DictCounts$id_municipio, alpha = 0.05)


DictCounts <- readRDS('~/Documents/GitHub/TDD-gender/LLM Version/Data/DataCounts.RDS') %>% 
  left_join(dfOpenSeats) %>% 
  left_join(dfVeterano) %>% 
  filter(veterano == 0) %>% 
  drop_na

dummy_matrix <- model.matrix(~ as.factor(sigla_uf) + as.factor(ano) + openSeat, data = DictCounts)
X <- dummy_matrix[,-1]


R <- DictCounts$vote_margin
Y <- DictCounts$care_count

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio)
summary(model)
rdpower(data = cbind(Y,R), tau = sd(Y[abs(R)<model$bws[1]], na.rm = T)*0.2, covs = X, cluster = DictCounts$id_municipio, alpha = 0.05)

dfVeteranoMunip <- dfVeterano %>% 
  filter(genero == 'masculino') %>% 
  group_by(id_municipio, ano) %>% 
  summarise(veterano = sum(veterano)) %>% 
  ungroup

dfRDD <- read.csv('~/Documents/GitHub/FemaleFunding/CleanData/dataRDD.csv') %>% 
  select(-X) %>% 
  mutate(ano = ano + 4)

dfTest <- dfRDD %>% left_join(dfVeteranoMunip)

R <- dfTest$vote_margin
Y <- dfTest$veterano

dummy_matrix <- model.matrix(~ as.factor(sigla_uf) + as.factor(ano) + openSeat, data = dfTest)
X <- dummy_matrix[,-1]

model <- rdrobust(Y, R, 0)
summary(model)
