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

summary(stm_model)

summary_effects <- summary(topic_effects)

# Convert the summary to a data frame
effects_df <- data.frame()

# Loop through the topics to extract estimates and CIs
for (i in 1:length(summary_effects$tables)) {
  topic_df <- as.data.frame(summary_effects$tables[[i]])
  topic_df$Topic <- i  # Add a column for topic numbers
  effects_df <- rbind(effects_df, topic_df)  # Combine all topics' data into one data frame
}

effects_df_real <- effects_df[2*(1:10),c(1,2, 5)]
colnames(effects_df_real) <- c("estimate", 'std', 'topic')
effects_df_real <- effects_df_real %>% 
  mutate(lci = estimate - 1.96*std,
         hci = estimate + 1.96*std,
         topic = as.character(topic)) %>% 
  mutate(topic = if_else(topic == '1', '01. Tourism', topic),
         topic = if_else(topic == '2', '02. Social Action', topic),
         topic = if_else(topic == '3', '03. Formatation', topic),
         topic = if_else(topic == '4', '04. Civic Participation', topic),
         topic = if_else(topic == '5', '05. Environment', topic),
         topic = if_else(topic == '6', '06. Commitment', topic),
         topic = if_else(topic == '7', '07. Prosocial Policies', topic),
         topic = if_else(topic == '8', '08. Community Support', topic),
         topic = if_else(topic == '9', '09. Infrastructure', topic),
         topic = if_else(topic == '10','10. Policy Implementation', topic))

ggplot(effects_df_real, aes(x = factor(topic), y = estimate, ymin = lci, ymax = hci)) +
  geom_pointrange() +
  coord_flip() +
  labs(title = "Causal Impact of Treatment on Topics", 
       x = "Topics", 
       y = "Effect Size") +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed")

summary(topic_effects)

# Robustness Check (15 topics)

stm_model <- stm(documents = documents,
                 vocab = vocab,
                 K = 15,  # Number of topics
                 prevalence = ~ treatment,  # Use binary treatment as a covariate
                 data = meta_data,
                 seed = 123)

topic_effects <- estimateEffect(1:15 ~ treatment, stm_model, metadata = meta_data, uncertainty = "Global")

summary(stm_model)
summary(topic_effects)


k_search <- searchK(documents, vocab, K = c(5, 10, 15, 20), prevalence = ~ treatment, data = meta_data)
plot(k_search)
