library(stm)

dfManifestos <- read.csv('dataManifestos.csv') %>% 
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

# Step 2: View the summary of the estimated effects
summary(stm_model)
summary(topic_effects)

labelTopics(stm_model, n = 30, frexweight = 0.5)
plot.STM(stm_model, type = "summary")

topicCorr <- topicCorr(stm_model)
plot(topicCorr)

findThoughts(stm_model, texts = dfManifestos$manifesto, topics = 1, n = 1)

library(wordcloud)
cloud(stm_model, topic = 1)
cloud(stm_model, topic = 6)
