library(tidyverse)
library(tidylog)
library(rdrobust)

dfCandidato <- read.csv('~/Dropbox/DiversityBrazil/data/BaseDosDados/PrefeitosDados2008_2024.csv') %>% 
  filter(tipo_eleicao == 'eleicao ordinaria',
         situacao == 'deferido'| ano == 2024) %>% 
  select(id_municipio, ano, titulo_eleitoral, numero_partido, instrucao, idade, genero)

dfVotos <- read.csv('~/Dropbox/DiversityBrazil/data/BaseDosDados/ResultadosPrefeito2008_2024.csv') %>% 
  filter(tipo_eleicao == 'eleicao ordinaria')

pop <- read.csv('~/Dropbox/DiversityBrazil/data/BaseDosDados/PopulationMunicipalities.csv', skip = 1) %>% 
  select(-Sigla,-Município) %>% 
  rename(id_municipio = Código) %>% 
  tidyr::pivot_longer(!id_municipio, names_to = "ano", values_to = "pop") %>% 
  mutate(ano = as.numeric(sub('.', '', ano))) %>% 
  mutate(logPop = log(pop)) %>% 
  tidyr::drop_na()

dfIncumbentes <- dfVotos %>% 
  filter(resultado == 'eleito') %>% 
  select(id_municipio, ano, numero_partido) %>% 
  left_join(pop) %>% 
  filter(pop<200000) %>% 
  left_join(dfCandidato) %>% 
  select(id_municipio, ano, titulo_eleitoral) %>% 
  mutate(ano = ano + 4) %>% 
  drop_na

dfOpenSeat <- dfIncumbentes %>% 
  semi_join(dfCandidato %>% select(id_municipio, ano, titulo_eleitoral)) %>% 
  mutate(reelection = 1) %>% 
  select(-titulo_eleitoral)

dfGuide <- expand.grid(unique(pop$id_municipio), seq(2012,2024,4)) %>% 
  rename(id_municipio = Var1,
         ano = Var2)

dfOpenSeat <- dfGuide %>% 
  full_join(dfOpenSeat) %>% 
  mutate(openSeat = 1 - replace_na(reelection, 0)) %>% 
  select(-reelection)

dfMarshall <- dfVotos %>% 
  filter(resultado == 'eleito',
         ano < 2024) %>% 
  select(id_municipio, ano, numero_partido) %>% 
  left_join(pop) %>% 
  filter(pop<200000) %>% 
  left_join(dfCandidato) %>% 
  mutate(esquerda = if_else(numero_partido %in% c(12,13,40,50,65),1,0),
         educado = if_else(instrucao == 'ensino superior completo',1,0),
         homem = if_else(genero == 'masculino',1,0)) %>% 
  select(id_municipio, ano, numero_partido, esquerda, educado, idade) %>% 
  drop_na

dfRDD <- read.csv('~/Documents/GitHub/FemaleFunding/CleanData/dataRDD.csv') %>% 
  select(-X)

dfMarshall <- dfRDD %>% left_join(dfMarshall) %>% mutate(ano = ano + 4) %>% left_join(dfOpenSeat) %>% drop_na

dummy_matrix <- model.matrix(~ as.factor(floor(id_municipio/1000000)) + as.factor(ano) + openSeat, data = dfMarshall)
X <- dummy_matrix[,-1]

R <- dfMarshall$vote_margin
Y <- dfMarshall$educado

model <- rdrobust(Y, R, 0, covs = X, cluster = dfMarshall$id_municipio)
summary(model)

R <- dfMarshall$vote_margin
Y <- dfMarshall$esquerda

model <- rdrobust(Y, R, 0, covs = X, cluster = dfMarshall$id_municipio)
summary(model)

R <- dfMarshall$vote_margin
Y <- dfMarshall$idade

model <- rdrobust(Y, R, 0, covs = X, cluster = dfMarshall$id_municipio)
summary(model)

dfComposition <- dfCandidato %>% 
  filter(ano>2008) %>% 
  anti_join(dfIncumbentes) %>% 
  select(id_municipio, ano, numero_partido, instrucao, idade, genero) %>% 
  mutate(esquerda = if_else(numero_partido %in% c(12,13,40,50,65),1,0),
         educado = if_else(instrucao == 'ensino superior completo',1,0),
         homem = if_else(genero == 'masculino',1,0)) %>% 
  group_by(id_municipio, ano) %>% 
  summarise(esquerda_comp = mean(esquerda),
            educado_comp = mean(educado),
            homem_comp = sum(homem),
            challenger_comp = n()) %>% 
  ungroup %>% 
  select(id_municipio, ano, esquerda_comp, educado_comp, homem_comp, challenger_comp) %>% 
  drop_na

dfComposition <- dfRDD %>% mutate(ano = ano + 4) %>% left_join(dfComposition) %>% left_join(dfOpenSeat) %>% drop_na

dummy_matrix <- model.matrix(~ as.factor(floor(id_municipio/1000000)) + as.factor(ano) + openSeat, data = dfComposition)
X <- dummy_matrix[,-1]

R <- dfComposition$vote_margin
Y <- dfComposition$challenger_comp

model <- rdrobust(Y, R, 0, covs = X, cluster = dfComposition$id_municipio)
summary(model)
mean(Y[abs(R)<model$bws[1]&R<0], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)

R <- dfComposition$vote_margin
Y <- dfComposition$homem_comp

model <- rdrobust(Y, R, 0, covs = X, cluster = dfComposition$id_municipio)
summary(model)
mean(Y[abs(R)<model$bws[1]&R<0], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)

R <- dfComposition$vote_margin
Y <- dfComposition$esquerda_comp

model <- rdrobust(Y, R, 0, covs = X, cluster = dfComposition$id_municipio)
summary(model)
mean(Y[abs(R)<model$bws[1]&R<0], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)

R <- dfComposition$vote_margin
Y <- dfComposition$educado_comp

model <- rdrobust(Y, R, 0, covs = X, cluster = dfComposition$id_municipio)
summary(model)
mean(Y[abs(R)<model$bws[1]&R<0], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)

dfComposition0 <- dfComposition %>% filter(openSeat == 1)

dummy_matrix <- model.matrix(~ as.factor(floor(id_municipio/1000000)) + as.factor(ano), data = dfComposition0)
X <- dummy_matrix[,-1]

R <- dfComposition0$vote_margin
Y <- dfComposition0$challenger_comp

model <- rdrobust(Y, R, 0, covs = X, cluster = dfComposition0$id_municipio)
summary(model)

R <- dfComposition0$vote_margin
Y <- dfComposition0$homem_comp

model <- rdrobust(Y, R, 0, covs = X, cluster = dfComposition0$id_municipio)
summary(model)

R <- dfComposition0$vote_margin
Y <- dfComposition0$esquerda_comp

model <- rdrobust(Y, R, 0, covs = X, cluster = dfComposition0$id_municipio)
summary(model)

R <- dfComposition0$vote_margin
Y <- dfComposition0$educado_comp

model <- rdrobust(Y, R, 0, covs = X, cluster = dfComposition0$id_municipio)
summary(model)

dfComposition1 <- dfComposition %>% filter(openSeat == 0)

dummy_matrix <- model.matrix(~ as.factor(floor(id_municipio/1000000)) + as.factor(ano), data = dfComposition1)
X <- dummy_matrix[,-1]

R <- dfComposition1$vote_margin
Y <- dfComposition1$challenger_comp

model <- rdrobust(Y, R, 0, covs = X, cluster = dfComposition1$id_municipio)
summary(model)

R <- dfComposition1$vote_margin
Y <- dfComposition1$homem_comp

model <- rdrobust(Y, R, 0, covs = X, cluster = dfComposition1$id_municipio)
summary(model)

R <- dfComposition1$vote_margin
Y <- dfComposition1$esquerda_comp

model <- rdrobust(Y, R, 0, covs = X, cluster = dfComposition1$id_municipio)
summary(model)

R <- dfComposition1$vote_margin
Y <- dfComposition1$educado_comp

model <- rdrobust(Y, R, 0, covs = X, cluster = dfComposition1$id_municipio)
summary(model)

dfCompositionH <- dfComposition %>% filter(challenger_comp > 1)

dummy_matrix <- model.matrix(~ as.factor(floor(id_municipio/1000000)) + as.factor(ano) + openSeat, data = dfCompositionH)
X <- dummy_matrix[,-1]

R <- dfCompositionH$vote_margin
Y <- dfCompositionH$challenger_comp

model <- rdrobust(Y, R, 0, covs = X, cluster = dfCompositionH$id_municipio)
summary(model)
mean(Y[abs(R)<model$bws[1]&R<0], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)

R <- dfCompositionH$vote_margin
Y <- dfCompositionH$homem_comp

model <- rdrobust(Y, R, 0, covs = X, cluster = dfCompositionH$id_municipio)
summary(model)
mean(Y[abs(R)<model$bws[1]&R<0], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)

R <- dfCompositionH$vote_margin
Y <- dfCompositionH$esquerda_comp

model <- rdrobust(Y, R, 0, covs = X, cluster = dfCompositionH$id_municipio)
summary(model)
mean(Y[abs(R)<model$bws[1]&R<0], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)

R <- dfCompositionH$vote_margin
Y <- dfCompositionH$educado_comp

model <- rdrobust(Y, R, 0, covs = X, cluster = dfCompositionH$id_municipio)
summary(model)
mean(Y[abs(R)<model$bws[1]&R<0], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)

saveRDS(dfOpenSeat, '~/Documents/GitHub/TDD-gender/LLM Version/Data/DataOpenSeat.RDS')
