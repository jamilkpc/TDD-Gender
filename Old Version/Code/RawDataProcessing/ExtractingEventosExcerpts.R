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



# Function to extract 50-word windows around "evento" and "eventos"
extract_window <- function(text, keyword, window_size = 20) {
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
  mutate(context_evento = list(extract_window(content, "evento")),
         context_eventos = list(extract_window(content, "eventos")))

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
