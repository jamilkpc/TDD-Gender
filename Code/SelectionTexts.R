dfRDD <- read.csv('dataCovariates.csv') %>% select(-X)
dfManifestos <- read.csv('20240923_br_mayors_proposal.csv')
dfCandidato <- read.csv('CandidatoPrefeito.csv')
dfVotos <- read.csv('VotosMunicipioPrefeito.csv')

dfManifestosClean <- dfManifestos %>% 
  rename(X = Unnamed..0,
         sequencial = file_name,
         ano = year,
         sigla_uf = state) %>% 
  select(-X)

dfMen <- dfCandidato %>% 
  select(id_municipio, ano, sigla_uf, sequencial, numero_partido, genero) %>% 
  left_join(dfManifestosClean) %>% 
  filter(genero == 'masculino') 

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
  select(id_municipio, ano, numero_partido, eleito) %>% 
  filter(eleito == 'eleito') %>% 
  mutate(ano = ano + 4)

dfMenChallengers <- dfMen %>% left_join(dfEleitos) %>% 
  mutate(eleito = if_else(is.na(eleito),0,1)) %>% 
  filter(eleito == 0)

dfData <- dfMenChallengers %>% left_join(dfRDD) %>% drop_na %>% 
  filter(vote_margin > -0.0375,
         vote_margin < 0.0375)
