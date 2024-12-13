library(tidyverse)
library(stringi)
library(tidytext)
library(stopwords)
library(rdrobust)
library(rddensity)
library(rdpower)

dfRDD <- read.csv('dataCovariates.csv') %>% select(-X)
dfManifestos <- read.csv('~/Downloads/20240927_br_mayors_proposal.csv')
dfCandidato <- read.csv('CandidatoPrefeito.csv')
dfVotos <- read.csv('VotosMunicipioPrefeito.csv')

dentest <- rddensity(dfRDD$vote_margin, all = T)
rdplotdensity(dentest, dfRDD$vote_margin, lcol = c("black", "black"), xlabel = "margin",
              plotRange = c(-1, 1), plotN = 100)

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

dfData <- dfMenChallengers %>% left_join(dfRDD) %>% drop_na

dfClean <- dfData %>% 
  mutate(content = tolower(content),
         content = stri_trans_general(content, "Latin-ASCII"),
         content = str_replace_all(content,'\n',' '),
         content = str_replace_all(content, "[^a-zA-Z\\s]", " "))

dfTokens <- dfClean %>%
  select(id_municipio, ano, sequencial, content) %>% 
  unnest_tokens(word, content)

tourismDict <- c('turisticos', 'turismo', 'turistico', 'turistica', 'turista', 'turistas', 
                'evento', 'eventos', 'vista')

TourismTokens <- dfTokens %>% 
  filter(word %in% tourismDict) %>% 
  group_by(id_municipio, ano, sequencial) %>% 
  summarise(tourism_count = n()) %>% 
  ungroup()

funDict <- c('esportivo', 'esporte', 'esportiva',
             'esportivos', 'esportivas', 'futebol', 'cultura', 
             'cultural','culturais', 'musica', 'musical', 'artista', 'artistas')

FunTokens <- dfTokens %>% 
  filter(word %in% funDict) %>% 
  group_by(id_municipio, ano, sequencial) %>% 
  summarise(fun_count = n()) %>% 
  ungroup()

agroDict <- c('agricultura', 'agricola', 'agricolas', 'agronegocio', 'rural', 
                 'rurais', 'acquisicao', 'maquinas', 'equipamentos')

AgroTokens <- dfTokens %>% 
  filter(word %in% agroDict) %>% 
  group_by(id_municipio, ano, sequencial) %>% 
  summarise(agro_count = n()) %>% 
  ungroup()

saudeDict <- c('hospital', 'agua', 'saude', 'vacinacao', 'vacinas', 'vacina',
              'posto', 'esgoto')

SaudeTokens <- dfTokens %>% 
  filter(word %in% saudeDict) %>% 
  group_by(id_municipio, ano, sequencial) %>% 
  summarise(saude_count = n()) %>% 
  ungroup()

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
unwanted_tokens <- tibble(word = post_tokenization_patterns)

TotalTokens <- dfTokens %>% 
  anti_join(unwanted_tokens, by = "word") %>% 
  group_by(id_municipio, ano, sequencial) %>% 
  summarise(tokens_count = n()) %>% 
  ungroup()

DictCounts <- dfClean %>% 
  select(id_municipio, ano, sequencial) %>% 
  distinct() %>% 
  left_join(TourismTokens, by = c("id_municipio", "ano", "sequencial")) %>% 
  left_join(AgroTokens, by = c("id_municipio", "ano", "sequencial")) %>% 
  left_join(SaudeTokens, by = c("id_municipio", "ano", "sequencial")) %>% 
  left_join(FunTokens, by = c("id_municipio", "ano", "sequencial")) %>% 
  left_join(TotalTokens, by = c("id_municipio", "ano", "sequencial")) %>% 
  mutate(tourism_count = replace_na(tourism_count/tokens_count, 0),
         agro_count = replace_na(agro_count/tokens_count, 0),
         saude_count = replace_na(saude_count/tokens_count, 0),
         fun_count = replace_na(fun_count/tokens_count, 0)) %>% 
  left_join(dfRDD) %>% 
  drop_na

DictCounts <- DictCounts %>% mutate(uf = floor(id_municipio/100000))
dummy_matrix <- model.matrix(~ as.factor(uf) + as.factor(ano) + open, data = DictCounts)
X <- dummy_matrix[,-1]

R <- DictCounts$vote_margin
Y <- DictCounts$tourism_count

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio)
rdpower(data = cbind(Y,R), tau = sd(Y[abs(R)<model$bws[1] & R<0], na.rm = T)*0.2, covs = X, cluster = DictCounts$id_municipio, alpha = 0.05)
rdpower(data = cbind(Y,R), tau = sd(Y[abs(R)<model$bws[1] & R<0], na.rm = T)*0.2, covs = X, cluster = DictCounts$id_municipio, alpha = 0.1)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio)
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio)
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio, kernel = 'epanechnikov')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, kernel = 'epanechnikov')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio, kernel = 'uniform')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, kernel = 'uniform')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio, bwselect = 'cerrd')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, bwselect = 'cerrd')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio, p = 2)
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, p = 2)
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)


R <- DictCounts$vote_margin
Y <- DictCounts$agro_count

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio)
rdpower(data = cbind(Y,R), tau = sd(Y[abs(R)<model$bws[1] & R<0], na.rm = T)*0.2, covs = X, cluster = DictCounts$id_municipio, alpha = 0.05)
rdpower(data = cbind(Y,R), tau = sd(Y[abs(R)<model$bws[1] & R<0], na.rm = T)*0.2, covs = X, cluster = DictCounts$id_municipio, alpha = 0.1)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio)
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio)
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio, kernel = 'epanechnikov')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, kernel = 'epanechnikov')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio, kernel = 'uniform')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, kernel = 'uniform')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio, bwselect = 'cerrd')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, bwselect = 'cerrd')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio, p = 2)
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, p = 2)
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)


R <- DictCounts$vote_margin
Y <- DictCounts$saude_count

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio)
rdpower(data = cbind(Y,R), tau = sd(Y[abs(R)<model$bws[1] & R<0], na.rm = T)*0.2, covs = X, cluster = DictCounts$id_municipio, alpha = 0.05)
rdpower(data = cbind(Y,R), tau = sd(Y[abs(R)<model$bws[1] & R<0], na.rm = T)*0.2, covs = X, cluster = DictCounts$id_municipio, alpha = 0.1)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio)
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1] & R<0], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio)
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio, kernel = 'epanechnikov')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, kernel = 'epanechnikov')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio, kernel = 'uniform')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, kernel = 'uniform')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio, bwselect = 'cerrd')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, bwselect = 'cerrd')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio, p = 2)
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, p = 2)
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)


R <- DictCounts$vote_margin
Y <- DictCounts$fun_count

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio)
rdpower(data = cbind(Y,R), tau = sd(Y[abs(R)<model$bws[1] & R<0], na.rm = T)*0.2, covs = X, cluster = DictCounts$id_municipio, alpha = 0.05)
rdpower(data = cbind(Y,R), tau = sd(Y[abs(R)<model$bws[1] & R<0], na.rm = T)*0.2, covs = X, cluster = DictCounts$id_municipio, alpha = 0.1)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio)
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio)
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio, kernel = 'uniform')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, kernel = 'uniform')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio, kernel = 'epanechnikov')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, kernel = 'epanechnikov')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio, bwselect = 'cerrd')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, bwselect = 'cerrd')
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, cluster = DictCounts$id_municipio, p = 2)
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, p = 2)
mean(Y[abs(R)<model$bws[1]], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
summary(model)

