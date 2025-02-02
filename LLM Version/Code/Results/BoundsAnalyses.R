library(tidyverse)
library(tidytext)
library(rdrobust)
library(rdpower)
library(rddensity)

dfOpenSeats <- readRDS('~/Documents/GitHub/TDD-gender/LLM Version/Data/DataOpenSeat.RDS')

DictCounts <- readRDS('~/Documents/GitHub/TDD-gender/LLM Version/Data/DataCounts.RDS') %>% 
  left_join(dfOpenSeats) %>% 
  drop_na

# I checked the draw; it was on Bananal (SP). The female got elected.
DictFemale <- DictCounts %>% filter(vote_margin>=0)
DictMale <- DictCounts %>% filter(vote_margin<0)

DictFemale <- DictFemale %>% 
  group_by(id_municipio, ano) %>% 
  mutate(obs = n()) %>% 
  ungroup %>%
  filter(obs>1) %>% 
  select(-obs)

DictMale <- DictMale %>% 
  group_by(id_municipio, ano) %>% 
  mutate(obs = n()) %>% 
  ungroup %>%
  filter(obs>1) %>% 
  select(-obs)

### Baseline

dfBounds <- rbind(DictFemale, DictMale)

dummy_matrix <- model.matrix(~ as.factor(sigla_uf) + as.factor(ano) + openSeat, data = dfBounds)
X <- dummy_matrix[,-1]

R <- dfBounds$vote_margin
Y <- dfBounds$care_count

model <- rdrobust(Y, R, 0, covs = X, cluster = dfBounds$id_municipio)
summary(model)

### Eliminating All Most Caring Men

BoundsFemale <- DictFemale %>%
  group_by(id_municipio, ano) %>%
  filter(care_count != max(care_count)) %>%
  ungroup()

dfBounds <- rbind(BoundsFemale, DictMale)

dummy_matrix <- model.matrix(~ as.factor(sigla_uf) + as.factor(ano) + openSeat, data = dfBounds)
X <- dummy_matrix[,-1]

R <- dfBounds$vote_margin
Y <- dfBounds$care_count

model <- rdrobust(Y, R, 0, covs = X, cluster = dfBounds$id_municipio)
summary(model)

### Eliminating All Least Caring Men

BoundsFemale <- DictFemale %>%
  group_by(id_municipio, ano) %>%
  filter(care_count != min(care_count)) %>%
  ungroup()

dfBounds <- rbind(BoundsFemale, DictMale)

dummy_matrix <- model.matrix(~ as.factor(sigla_uf) + as.factor(ano) + openSeat, data = dfBounds)
X <- dummy_matrix[,-1]

R <- dfBounds$vote_margin
Y <- dfBounds$care_count

model <- rdrobust(Y, R, 0, covs = X, cluster = dfBounds$id_municipio)
summary(model)
