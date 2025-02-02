library(tidyverse)

# Load Close Elections from Previous Electoral Cycle
dfRDD <- read.csv('~/Documents/GitHub/FemaleFunding/CleanData/dataRDD.csv') %>% 
  select(-X) %>% 
  mutate(ano = ano + 4)

# Load unique ID for each candidate
dfId <- read.csv('~/Dropbox/DiversityBrazil/data/BaseDosDados/PrefeitosDados2008_2024.csv') %>% 
  filter(tipo_eleicao == 'eleicao ordinaria') %>% 
  select(id_municipio, ano, sigla_partido, sequencial, titulo_eleitoral)

# Load Manifestos, merge unique IDs and margin of victory from previous elections
dfData <- readRDS('~/Documents/GitHub/GenderIssueOwnership/Data/ManifestosClean.RDS') %>% 
  select(id_municipio,ano,sequencial,sigla_partido,sigla_uf,genero,idade,manifesto) %>% 
  left_join(dfId) %>% 
  left_join(dfRDD) %>% 
  drop_na

# Load Voting Results and Create a List of Winners from Previous Elections
dfVotos <- read.csv('~/Dropbox/DiversityBrazil/data/BaseDosDados/ResultadosPrefeito2008_2024.csv') %>% 
  filter(ano<2024) %>% 
  left_join(dfId %>% select(-sequencial))

dfEleitos <- dfVotos %>% 
  rename(eleito = resultado) %>% 
  select(id_municipio, ano, titulo_eleitoral, eleito) %>% 
  filter(eleito == 'eleito') %>% 
  mutate(ano = ano + 4) %>% 
  select(-eleito)

# Select Only Men that did not win Previous Elections
dfMen <- dfData %>% filter(genero == 'masculino') %>% 
  anti_join(dfEleitos) %>% 
  select(-titulo_eleitoral)

# Save Dataset
saveRDS(dfMen, '~/Documents/GitHub/TDD-gender/LLM Version/Data/DataManifestosRDD.RDS')

