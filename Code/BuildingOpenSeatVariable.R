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
dfOpenSeat <- dfCandidato %>% 
  left_join(dfEleitos) %>%
  mutate(eleito = if_else(is.na(eleito),0,1)) %>% 
  group_by(id_municipio,ano) %>% 
  summarise(open = 1 - max(eleito)) %>% 
  ungroup

dfGuide <- pop %>% 
  filter(ano %in% c(2008,2012,2016,2020)) %>%
  select(id_municipio, ano) %>% 
  left_join(dfOpenSeat)

write.csv(dfGuide,'openSeat.csv')
