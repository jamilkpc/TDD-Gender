library(tidyverse)
library(tidytext)
library(rdrobust)
library(rdpower)

dfRDD <- read.csv('~/Documents/GitHub/FemaleFunding/CleanData/dataRDD.csv') %>% 
  select(-X) %>% 
  mutate(ano = ano + 4)

dentest <- rddensity(dfRDD$vote_margin, all = T)
summary(dentest)
rdplotdensity(dentest, dfRDD$vote_margin, lcol = c("black", "black"), xlabel = "margin",
              plotRange = c(-1, 1), plotN = 100)

DictCounts <- readRDS('~/Documents/GitHub/TDD-gender/LLM Version/Data/DataCounts.RDS')

dummy_matrix <- model.matrix(~ as.factor(sigla_uf) + as.factor(ano), data = DictCounts)
X <- dummy_matrix[,-1]

R <- DictCounts$vote_margin
Y <- DictCounts$care_count

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio)
summary(model)
rdpower(data = cbind(Y,R), tau = sd(Y[abs(R)<model$bws[1]], na.rm = T)*0.2, covs = X, cluster = DictCounts$id_municipio, alpha = 0.05)
rdpower(data = cbind(Y,R), tau = sd(Y[abs(R)<model$bws[1]], na.rm = T)*0.2, covs = X, cluster = DictCounts$id_municipio, alpha = 0.1)
