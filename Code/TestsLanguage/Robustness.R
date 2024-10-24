library(tidyverse)
library(stringi)
library(tidytext)
library(stopwords)

### This code is messy, I know
### Here, I do text length across men challengers responding to women and men incumbents.
### I also do sentiment and lexical diversity
### In the end of the text, I also check the distance between the past platform of the incumbent and platforms of the challengers
### I use RDRobust in all analyses

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
  filter(eleito == 0) %>% 
  drop_na

dfData <- dfMenChallengers %>% left_join(dfRDD) %>% drop_na

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
                                       brazilian_states_lower))
unwanted_tokens <- tibble(word = post_tokenization_patterns)
cleaned_tokens <- dfTokens %>%
  anti_join(unwanted_tokens, by = "word")

stopwords_pt <- stopwords("pt")
stopwords_df <- tibble(word = stopwords_pt) %>% 
  mutate(word = tolower(word),
         word = stri_trans_general(word, "Latin-ASCII"))
cleaned_tokens <- cleaned_tokens %>%
  anti_join(stopwords_df, by = "word")

names_pt <- readRDS('names_BR.rds')
cleaned_tokens <- cleaned_tokens %>%
  anti_join(names_pt, by = "word")

dfManifestosClean <- cleaned_tokens %>% 
  group_by(id_municipio, ano, sequencial) %>% 
  summarise(manifesto = str_c(word, collapse = ' ')) %>% 
  ungroup()

dfRDD2 <- dfData %>% select(-content) %>% left_join(dfManifestosClean)

dfRDD2 <- dfRDD2 %>%
  mutate(word_count = str_count(manifesto, '\\w+'))

sentimento <- function(manifesto){
  df <- syuzhet::get_nrc_sentiment(manifesto, language = "portuguese")
  sentimento <- df$positive/(df$positive+df$negative)
}

calculate_lexical_diversity <- function(text) {
  words <- str_split(text, "\\s+")[[1]]  # Split text into words
  unique_words <- length(unique(words))  # Count unique words
  total_words <- length(words)           # Count total words
  return(unique_words / total_words)     # Return type-token ratio
}

# Create a sentiment column (in Portuguese)
dfRDD2 <- dfRDD2 %>%
  mutate(sentiment = sentimento(manifesto),
         riqueza = sapply(manifesto, calculate_lexical_diversity))

library(rdrobust)

R <- dfRDD2$vote_margin
Y <- log(dfRDD2$word_count)

summary(rdrobust(Y, R, 0, cluster = dfRDD2$id_municipio))
rdr <- rdrobust(Y, R, 0, cluster = dfRDD2$id_municipio)
h_l <- rdr$bws[1]  # bandwidth

rdplot(y = log(dfRDD2$word_count), x = dfRDD2$vote_margin, c = 0, x.lim = c(-h_l, h_l), x.label = 'Running Variable', y.label = 'Log(Length)')

R <- dfRDD2$vote_margin
Y <- dfRDD2$sentiment

summary(rdrobust(Y, R, 0, cluster = dfRDD2$id_municipio))

R <- dfRDD2$vote_margin
Y <- dfRDD2$riqueza

summary(rdrobust(Y, R, 0, cluster = dfRDD2$id_municipio))
rdr <- rdrobust(Y, R, 0, cluster = dfRDD2$id_municipio)
h_l <- rdr$bws[1]  # bandwidth

rdplot(y = log(dfRDD2$word_count), x = dfRDD2$vote_margin, c = 0, x.lim = c(-h_l, h_l), x.label = 'Running Variable', y.label = 'Lexical Diversity')

##### Continuation to do a credit claming dataframe

dfManifestosClean <- dfManifestos %>% 
  rename(X = Unnamed..0,
         sequencial = file_name,
         ano = year,
         sigla_uf = state) %>% 
  select(-X)

dfEleitosI <- dfVotos %>% 
  rename(eleito = resultado) %>% 
  select(id_municipio, ano, numero_partido, eleito) %>% 
  filter(eleito == 'eleito')

dfIncumbents <- dfCandidato %>% 
  select(id_municipio, ano, sigla_uf, sequencial, numero_partido, genero) %>% 
  left_join(dfManifestosClean) %>% 
  left_join(dfEleitosI) %>% 
  filter(eleito == 'eleito') %>% 
  mutate(ano = ano + 4) %>% 
  drop_na %>% 
  select(id_municipio, ano,content)

dfIncumbentData <- dfIncumbents %>% left_join(dfRDD) %>% drop_na

dfClean <- dfIncumbentData %>% 
  mutate(content = tolower(content),
         content = stri_trans_general(content, "Latin-ASCII"),
         content = str_replace_all(content,'\n',' '),
         content = str_replace_all(content, "[^a-zA-Z\\s]", " "))

dfTokens <- dfClean %>%
  select(id_municipio, ano, content) %>% 
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
                                       brazilian_states_lower))
unwanted_tokens <- tibble(word = post_tokenization_patterns)
cleaned_tokens <- dfTokens %>%
  anti_join(unwanted_tokens, by = "word")

stopwords_pt <- stopwords("pt")
stopwords_df <- tibble(word = stopwords_pt) %>% 
  mutate(word = tolower(word),
         word = stri_trans_general(word, "Latin-ASCII"))
cleaned_tokens <- cleaned_tokens %>%
  anti_join(stopwords_df, by = "word")

names_pt <- readRDS('names_BR.rds')
cleaned_tokens <- cleaned_tokens %>%
  anti_join(names_pt, by = "word")

dfManifestosClean <- cleaned_tokens %>% 
  group_by(id_municipio, ano) %>% 
  summarise(manifesto = str_c(word, collapse = ' ')) %>% 
  ungroup()

dfRDD3 <- dfIncumbentData %>% 
  select(-content) %>% 
  left_join(dfManifestosClean) %>% 
  select(id_municipio, ano, manifesto) %>% 
  rename(manifesto_incumbente = manifesto) %>% 
  distinct()

dfRDDcomp <- dfRDD2 %>% left_join(dfRDD3) %>% drop_na

library(tm)
library(text2vec)

# Function to compute cosine similarity between two texts
calculate_cosine_similarity <- function(text1, text2) {
  # Create a document-term matrix
  corpus <- Corpus(VectorSource(c(text1, text2)))
  dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(1, Inf)))
  
  # Convert to matrix and calculate cosine similarity
  dtm_matrix <- as.matrix(dtm)
  cosine_sim <- sim2(dtm_matrix, method = "cosine")
  
  return(cosine_sim[1, 2])  # Return cosine similarity between text1 and text2
}

calculate_jaccard_similarity <- function(text1, text2) {
  words1 <- unique(strsplit(text1, "\\s+")[[1]])  # Split text1 into words
  words2 <- unique(strsplit(text2, "\\s+")[[1]])  # Split text2 into words
  intersection <- length(intersect(words1, words2))  # Intersection of word sets
  union <- length(union(words1, words2))  # Union of word sets
  
  return(intersection / union)  # Return Jaccard similarity
}


# Apply cosine similarity function to the manifestos
dfRDDcomp <- dfRDDcomp %>%
  mutate(cosine_similarity = mapply(calculate_cosine_similarity, manifesto_incumbente, manifesto),
         jaccard_similarity = mapply(calculate_jaccard_similarity, manifesto_incumbente, manifesto))

R <- dfRDDcomp$vote_margin
Y <- dfRDDcomp$cosine_similarity

summary(rdrobust(Y, R, 0))

R <- dfRDDcomp$vote_margin
Y <- dfRDDcomp$jaccard_similarity

summary(rdrobust(Y, R, 0))
