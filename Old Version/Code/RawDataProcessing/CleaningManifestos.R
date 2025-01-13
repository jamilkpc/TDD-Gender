library(stringi)
library(tidytext)
library(stopwords)

dfCandidatos <- dfCandidatos %>% 
  left_join(dfProposals) %>% 
  tidyr::drop_na() %>% 
  mutate(proposta = tolower(proposta),
         sigla_partido = tolower(sigla_partido),
         sigla_uf = tolower(sigla_uf),
         proposta = stri_trans_general(proposta, "Latin-ASCII"),
         proposta = str_replace_all(proposta,'\n',' '),
         proposta = str_replace_all(proposta, "[^a-zA-Z\\s]", " "))
rm(dfProposals)

dfTokens <- dfCandidatos %>%
  select(id_municipio, ano, sigla_uf, sequencial, proposta) %>% 
  unnest_tokens(word, proposta)

# Unique patterns to remove after tokenizing
brazilian_states_lower <- c("acre", "alagoas", "amapa", "amazonas", "bahia", "ceara", "distrito federal",
                            "espirito santo", "goias", "maranhao", "mato grosso", "mato grosso do sul",
                            "minas gerais", "para", "paraiba", "parana", "pernambuco", "piaui", "rio de janeiro",
                            "rio grande do norte", "rio grande do sul", "rondonia", "roraima", "santa catarina",
                            "sao paulo", "sergipe", "tocantins")
post_tokenization_patterns <- unique(c(dfCandidatos$sigla_partido,
                                       dfCandidatos$sigla_uf, 
                                       'b',
                                       'cid', 
                                       letters,
                                       brazilian_states_lower))

unwanted_tokens <- tibble(word = post_tokenization_patterns)

# Use anti_join to remove unwanted tokens
cleaned_tokens <- dfTokens %>%
  anti_join(unwanted_tokens, by = "word")

# Get Portuguese stopwords
stopwords_pt <- stopwords("pt")

# Create a dataframe of stopwords
stopwords_df <- tibble(word = stopwords_pt) %>% 
  mutate(word = tolower(word),
         word = stri_trans_general(word, "Latin-ASCII"))

# Use anti_join to remove stopwords
cleaned_tokens <- cleaned_tokens %>%
  anti_join(stopwords_df, by = "word")

# Create list of names and cleaning the dataset
names_pt <- readRDS('names_BR.rds')
cleaned_tokens <- cleaned_tokens %>%
  anti_join(names_pt, by = "word")