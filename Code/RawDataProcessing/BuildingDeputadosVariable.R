library(tidyverse)

dfCandidato <- read.csv('CandidatosDeputado.csv')
dfVotos1 <- read.csv('VotosDeputado1.csv')
dfVotos2 <- read.csv('VotosDeputado2.csv')
dfVotos <- rbind(dfVotos1,dfVotos2)

dfCandidato <- dfCandidato %>% 
  select(sigla_uf, ano, numero, id_candidato_bd, genero) %>% 
  rename(numero_candidato = numero)

dfGender <- dfVotos %>% left_join(dfCandidato) %>% drop_na %>% 
  group_by(id_municipio, ano, genero) %>% 
  summarise(voto_genero = sum(votos)) %>% 
  ungroup %>% 
  group_by(id_municipio, ano) %>% 
  mutate(share_feminino = voto_genero/sum(voto_genero)) %>% 
  ungroup %>% 
  filter(genero == 'feminino') %>% 
  select(-voto_genero, -genero)

dfParty <- dfVotos %>% drop_na %>% 
  select(id_municipio, ano, numero_partido, votos) %>% 
  mutate(esquerda = if_else(numero_partido %in% c(13,40,50,65,12),1,0)) %>% 
  group_by(id_municipio, ano, esquerda) %>% 
  summarise(voto_ideologia = sum(votos)) %>% 
  ungroup %>% 
  group_by(id_municipio, ano) %>% 
  mutate(share_esquerda = voto_ideologia/sum(voto_ideologia)) %>% 
  ungroup %>% 
  filter(esquerda==1) %>% 
  select(-voto_ideologia, -esquerda)

pop <- read.csv('Populacao.csv', skip = 1) %>% 
  select(-Sigla,-Município) %>% 
  rename(id_municipio = Código) %>% 
  tidyr::pivot_longer(!id_municipio, names_to = "ano", values_to = "pop") %>% 
  mutate(ano = as.numeric(sub('.', '', ano))) %>% 
  mutate(logPop = log(pop)) %>% 
  tidyr::drop_na()

dfGuide <- pop %>% 
  filter(ano %in% c(2008,2012,2016,2020)) %>%
  mutate(ano = ano - 2) %>% 
  select(id_municipio, ano) %>% 
  left_join(dfGender) %>% 
  left_join(dfParty) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(ano = ano + 2)

write.csv(dfGuide,'dataDeputados.csv')
