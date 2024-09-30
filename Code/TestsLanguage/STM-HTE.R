library(tidyverse)
library(tm)
library(stm)

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

dfReelection <- dfCandidato %>% left_join(dfEleitos) %>% 
  mutate(incumbent_running = if_else(is.na(eleito),0,1)) %>% 
  group_by(id_municipio, ano) %>% 
  summarise(reelection = sum(incumbent_running)>0) %>% 
  ungroup %>% 
  filter(reelection==1)

dfManifestos <- read.csv('dataManifestos.csv') %>% 
  select(-X)

dfManifestos <- dfManifestos %>% 
  mutate(treatment = vote_margin>0) %>% 
  left_join(dfReelection) %>% 
  mutate(reelection = if_else(is.na(reelection), 0, 1),
         treatment = as.integer(treatment),
         moderation = reelection*treatment)


corpus <- Corpus(VectorSource(dfManifestos$manifesto))
dtm <- DocumentTermMatrix(corpus)

dtm <- DocumentTermMatrix(corpus)

# Step 2: Remove documents with no words (optional, if preprocessing leads to empty docs)
dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]

# Step 3: Prepare the input for STM

# Extract the vocabulary (terms)
vocab <- colnames(as.matrix(dtm))

# Extract the documents as a list where each document is represented by term indices and counts
documents <- apply(as.matrix(dtm), 1, function(x) {
  # Get the indices of non-zero entries
  terms <- which(x > 0)
  # Return a list with term indices and their counts
  rbind(terms, x[terms])
})

# Prepare metadata (make sure to adjust the metadata to match the non-empty documents)
meta_data <- dfManifestos[rowSums(as.matrix(dtm)) > 0, c("treatment", "reelection", "moderation"), drop = FALSE]

# Step 4: Fit the STM model
stm_model <- stm(documents = documents,
                 vocab = vocab,
                 K = 10,  # Number of topics
                 prevalence = ~ treatment + reelection + moderation,  # Use binary treatment as a covariate
                 data = meta_data,
                 seed = 123)

topic_effects <- estimateEffect(1:10 ~ treatment + reelection + moderation, stm_model, metadata = meta_data, uncertainty = "Global")

# Step 2: View the summary of the estimated effects
summary(stm_model)
summary(topic_effects)
