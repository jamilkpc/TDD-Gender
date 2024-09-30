library(tidyverse)

dfCandidato <- read.csv('CandidatoPrefeito.csv')
dfVotos <- read.csv('VotosMunicipioPrefeito.csv')

gdp <- read.csv('PIB.csv', skip = 1) %>% 
  select(-Sigla,-Município) %>% 
  rename(id_municipio = Código) %>% 
  tidyr::pivot_longer(!id_municipio, names_to = "ano", values_to = "GDP") %>% 
  mutate(ano = as.numeric(sub('.', '', ano))) %>% 
  filter(GDP>0) %>% 
  tidyr::drop_na()

pop <- read.csv('Populacao.csv', skip = 1) %>% 
  select(-Sigla,-Município) %>% 
  rename(id_municipio = Código) %>% 
  tidyr::pivot_longer(!id_municipio, names_to = "ano", values_to = "pop") %>% 
  mutate(ano = as.numeric(sub('.', '', ano))) %>% 
  mutate(logPop = log(pop)) %>% 
  tidyr::drop_na()

dfVoteShare <- dfVotos %>% 
  select(id_municipio, ano, numero_partido, votos) %>% 
  group_by(id_municipio,ano) %>% 
  drop_na %>% 
  mutate(voteshare = votos/sum(votos, na.rm=T)) %>% 
  ungroup()

dfCandidatoFR <- dfCandidato %>% 
  filter(genero %in% c('masculino', 'feminino')) %>%
  left_join(pop) %>%
  filter(pop<200000) %>% 
  left_join(dfVoteShare) %>% 
  select(id_municipio, ano, genero, voteshare) %>% 
  arrange(id_municipio, ano, genero) %>% 
  group_by(ano, id_municipio, genero) %>% 
  summarise(voteshare = max(voteshare)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = genero, values_from = voteshare, values_fill = list(voteshare = NA)) %>% 
  mutate(vote_margin = feminino - masculino) %>% 
  select(id_municipio, ano, vote_margin) %>% 
  drop_na

dfOpenSeat <- read.csv('openSeat.csv') %>% select(-X)

dfMunicipio <- read.csv('dataDeputados.csv') %>% 
  left_join(dfOpenSeat) %>% 
  left_join(gdp) %>% 
  left_join(pop) %>% 
  mutate(gdppc = GDP/pop) %>% 
  left_join(dfCandidatoFR) %>%
  select(-X, -GDP, -pop) %>% 
  drop_na

write.csv(dfMunicipio, 'dataCovariates.csv')

library(rdlocrand)
R <- dfMunicipio$vote_margin
X <- cbind(dfMunicipio$share_feminino, 
       dfMunicipio$share_esquerda, 
       dfMunicipio$gdppc, 
       dfMunicipio$logPop,
       dfMunicipio$open)
colnames(X) <- c("share_feminino", 
       "share_esquerda", 
       "gdppc", 
       "logPop",
       'open_seat')

tmp <- rdwinselect(R,X,wmin=.0125,wstep=.0125,reps=10000)

