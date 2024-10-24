library(tidyverse)
library(stringi)
library(tidytext)
library(stopwords)

dfRDD <- read.csv('dataCovariates.csv') %>% select(-X)
dfManifestos <- read.csv('20240927_br_mayors_proposal.csv')
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
  filter(vote_margin > -0.05,
         vote_margin < 0.05)

dfClean <- dfData %>% 
  mutate(content = tolower(content),
         content = stri_trans_general(content, "Latin-ASCII"),
         content = str_replace_all(content,'\n',' '),
         content = str_replace_all(content, "[^a-zA-Z\\s]", " "))

dfTokens <- dfClean %>%
  select(id_municipio, ano, sequencial, content) %>% 
  unnest_tokens(word, content)

brazilian_states_lower <- c("acre", "alagoas", "amapa", "amazonas", "bahia", "ceara", "distrito federal",
                            "espirito santo", "goias", "maranhao", "mato grosso", "mato grosso do sul",
                            "minas gerais", "para", "paraiba", "parana", "pernambuco", "piaui", "rio de janeiro",
                            "rio grande do norte", "rio grande do sul", "rondonia", "roraima", "santa catarina",
                            "sao paulo", "sergipe", "tocantins")
post_tokenization_patterns <- unique(c(tolower(dfCandidato$sigla_partido),
                                       tolower(dfCandidato$sigla_uf), 
                                       'b',
                                       'cid', 
                                       letters,
                                       brazilian_states_lower,
                                       'prefeito',
                                       'prefeita'))
unwanted_tokens <- tibble(word = post_tokenization_patterns)

# Use anti_join to remove unwanted tokens
cleaned_tokens <- dfTokens %>%
  anti_join(unwanted_tokens, by = "word")

stopwords_pt <- stopwords("pt")

# Create a dataframe of stopwords
stopwords_df <- tibble(word = stopwords_pt) %>% 
  mutate(word = tolower(word),
         word = stri_trans_general(word, "Latin-ASCII"))

# Use anti_join to remove stopwords
cleaned_tokens <- cleaned_tokens %>%
  anti_join(stopwords_df, by = "word")

names_pt <- readRDS('names_BR.rds')
cleaned_tokens <- cleaned_tokens %>%
  anti_join(names_pt, by = "word")

dfCount <- cleaned_tokens %>% group_by(word) %>% 
  summarise(w_count = n()) %>% 
  ungroup %>% 
  filter(w_count < 30) %>% 
  select(word)

cleaned_tokens <- cleaned_tokens %>% anti_join(dfCount, by = 'word')

word_counts <- cleaned_tokens %>%
  distinct(id_municipio, ano, sequencial, word) %>%
  count(word, name = "unique_texts") %>% 
  filter(unique_texts >= floor(964*0.01),
         unique_texts < floor(964*0.7)) %>%
  pull(word) %>% 
  as.tibble %>% 
  rename(word = value)

cleaned_tokens <- cleaned_tokens %>% semi_join(word_counts, by = 'word')

non_null <- cleaned_tokens %>%
  group_by(id_municipio, ano, sequencial) %>% 
  summarise(wcount = n()) %>% 
  ungroup %>% 
  filter(wcount>=50) %>% 
  select(-wcount)

dfManifestos <- cleaned_tokens %>% 
  group_by(id_municipio, ano, sequencial) %>% 
  summarise(manifesto = str_c(word, collapse = ' ')) %>% 
  ungroup()

dfManifestos <- non_null %>% left_join(dfManifestos)

dfData <- dfManifestos %>% left_join(dfRDD)

write.csv(dfData, 'dataManifestosRobustness.csv')