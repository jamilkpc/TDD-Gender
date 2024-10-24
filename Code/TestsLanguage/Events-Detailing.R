library(tidyverse)
library(stringi)
library(stringr)
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
  mutate(word = if_else(word=='eventos','evento',word)) %>% 
  filter(word %in% c('evento', 'eventos')) %>% 
  distinct() %>% 
  select(id_municipio, ano, sequencial)

dfRaw <- read.csv('20240927_br_mayors_proposal.csv')
dfRaw <- dfRaw[ ,2:5]
dfRaw <- dfRaw %>% 
  rename(ano = year,
         sequencial = file_name)

dfTokens <- dfTokens %>% left_join(dfRaw)

dfClean <- dfTokens %>%
  mutate(content = tolower(content),
         content = stri_trans_general(content, "Latin-ASCII"),
         content = str_replace_all(content,'\n',' '),
         content = str_replace_all(content, "[^a-zA-Z\\s]", " "))

names_pt <- readRDS('names_BR.rds')
brazilian_states_lower <- c("acre", "alagoas", "amapa", "amazonas", "bahia", "ceara", "distrito federal",
                            "espirito santo", "goias", "maranhao", "mato grosso", "mato grosso do sul",
                            "minas gerais", "para", "paraiba", "parana", "pernambuco", "piaui", "rio de janeiro",
                            "rio grande do norte", "rio grande do sul", "rondonia", "roraima", "santa catarina",
                            "sao paulo", "sergipe", "tocantins")
post_tokenization_patterns <- unique(c(letters,
                                       brazilian_states_lower,
                                       'prefeito',
                                       'prefeita',
                                       stri_trans_general(stopwords("pt"), "Latin-ASCII"),
                                       names_pt$word))
pattern <- paste0("\\b(", paste(post_tokenization_patterns, collapse = "|"), ")\\b")
dfClean$text_cleaned <- str_replace_all(dfClean$content, pattern, " ")


# Function to extract 50-word windows around "evento" and "eventos"
extract_window <- function(text, keyword, window_size = 6) {
  # Create a regular expression for both singular and plural forms of "evento"
  pattern <- paste0("\\b", keyword, "\\b")
  
  # Split text into words
  words <- str_split(text, "\\s+")[[1]]
  
  # Find positions of occurrences of the word (word index)
  match_positions <- which(str_detect(words, pattern))
  
  # Initialize an empty list to store the extracted windows
  windows <- list()
  
  # Loop over each occurrence and extract the 50-word window
  for (match_index in match_positions) {
    # Define start and end indices for the window (50 words before and after the match)
    start <- max(1, match_index - window_size)
    end <- min(length(words), match_index + window_size)
    
    # Extract the words in the window and collapse them back into a sentence
    window_text <- paste(words[start:end], collapse = " ")
    
    # Store the result
    windows <- append(windows, window_text)
  }
  
  return(windows)
}


# Apply the function to your dataframe
df_windows <- dfClean %>%
  rowwise() %>%
  mutate(context_evento = list(extract_window(text_cleaned, "evento")),
         context_eventos = list(extract_window(text_cleaned, "eventos")))

# To inspect the results:
df_window_singular <- df_windows %>%
  select(- context_eventos) %>% 
  unnest(context_evento) %>% 
  mutate(context_evento = unlist(context_evento)) %>% 
  select(-content)# Unnest lists for easier reading

df_window_plural <- df_windows %>%
  select(- context_evento) %>% 
  unnest(context_eventos) %>% 
  mutate(context_eventos = unlist(context_eventos)) %>% 
  select(-content)  # Unnest lists for easier reading

write.csv(df_window_singular, 'ValidationEvents1.csv')
write.csv(df_window_plural, 'ValidationEvents2.csv')
write.csv(rbind(df_window_singular, df_window_plural %>% rename(context_evento = context_eventos)), 'ValidationEvents.csv')

df_window = read.csv('ValidationEvents.csv') %>% select(-X)

#pattern <- paste0("\\b(", paste(c("evento", "eventos"), collapse = "|"), ")\\b")
#df_window$context_evento <- str_replace_all(df_window$context_evento, pattern, " ")

texts <- df_window$context_evento

processed <- textProcessor(texts, metadata = NULL)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

docs <- out$documents
vocab <- out$vocab

k_search <- searchK(docs, vocab, K = c(4, 6, 8, 10))
plot(k_search)

stm_model <- stm(documents = docs, vocab = vocab, K = 6, data = NULL)


labelTopics(stm_model, n = 8)


average_topic_proportions <- colMeans(stm_model$theta)

# Convert to a data frame for ggplot
topic_proportions_df <- data.frame(
  Topic = c("Sport", "Big Events", "Cultural Party", "Construction", "Business", "Incentives to Culture"),
  Proportion = average_topic_proportions
)

# Create the bar plot
ggplot(topic_proportions_df, aes(x = reorder(Topic, Proportion), y = Proportion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Average Topic Proportions",
       x = "",
       y = "Average Proportion") +
  theme_minimal()
