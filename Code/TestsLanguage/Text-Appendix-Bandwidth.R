dfManifestos <- read.csv('dataManifestosRobustness.csv') %>% 
  select(-X)

dfManifestos <- dfManifestos %>% mutate(treatment = vote_margin>0)

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
meta_data <- dfManifestos[rowSums(as.matrix(dtm)) > 0, "treatment", drop = FALSE]

stm_model <- stm(documents = documents,
                 vocab = vocab,
                 K = 10,  # Number of topics
                 prevalence = ~ treatment,  # Use binary treatment as a covariate
                 data = meta_data,
                 seed = 123)

topic_effects <- estimateEffect(1:10 ~ treatment, stm_model, metadata = meta_data, uncertainty = "Global")

summary(stm_model)

summary_effects <- summary(topic_effects)


dfTokens <- dfManifestos %>%
  unnest_tokens(word, manifesto) %>% 
  mutate(word = SnowballC::wordStem(word, language = 'portuguese')) %>% 
  group_by(word) %>% 
  summarise(wcount = n()) %>% 
  ungroup %>% 
  arrange(-wcount) %>% 
  select(-wcount)
dfFeatures <- dfTokens[1:600,'word']

dfClean <- dfManifestos %>%
  select(id_municipio, ano, sequencial, manifesto) %>% 
  unnest_tokens(word, manifesto) %>% 
  mutate(word = SnowballC::wordStem(word, language = 'portuguese')) %>% 
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
