library(tidyverse)
library(stringi)
library(tidytext)
library(stopwords)
library(SnowballC)
library(text2vec)
library(tm)
library(hdm)

dfManifestos <- read.csv('dataManifestos.csv') %>% 
  select(-X)

dfTokens <- dfManifestos %>%
  unnest_tokens(word, manifesto) %>% 
  #mutate(word = SnowballC::wordStem(word, language = 'portuguese')) %>% 
  group_by(word) %>% 
  summarise(wcount = n()) %>% 
  ungroup %>% 
  arrange(-wcount) %>% 
  select(-wcount)
dfFeatures <- dfTokens[1:600,'word']

dfClean <- dfManifestos %>%
  select(id_municipio, ano, sequencial, manifesto) %>% 
  unnest_tokens(word, manifesto) %>% 
  #mutate(word = SnowballC::wordStem(word, language = 'portuguese')) %>% 
  semi_join(dfFeatures, by = 'word') %>% 
  group_by(id_municipio, ano, sequencial) %>% 
  summarise(manifesto = str_c(word, collapse = ' ')) %>% 
  ungroup()
  
dfManifestos <- dfManifestos %>% 
  select(-manifesto) %>% 
  left_join(dfClean)

texts <- dfManifestos$manifesto
treatment <- dfManifestos$vote_margin>0

tokens <- word_tokenizer(texts)
it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it, vectorizer)

X <- as.matrix(dtm)
colnames(X) <- vocab$term
sorted_order <- order(colnames(X))
X <- X[, sorted_order]

lasso_fit_rd <- rlassologit(y = treatment, x = X, post = FALSE, rigorous = TRUE)
summary(lasso_fit_rd)

lasso_fit_rd$coefficients[lasso_fit_rd$coefficients>0]
lasso_fit_rd$coefficients[lasso_fit_rd$coefficients<0]
