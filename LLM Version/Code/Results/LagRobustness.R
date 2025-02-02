library(tidyverse)
library(tidytext)
library(rdrobust)
library(rdpower)
library(rddensity)

dfOpenSeats <- readRDS('~/Documents/GitHub/TDD-gender/LLM Version/Data/DataOpenSeat.RDS')

DictCounts <- readRDS('~/Documents/GitHub/TDD-gender/LLM Version/Data/DataCountsLag.RDS') %>% 
  left_join(dfOpenSeats) %>% 
  drop_na

dummy_matrix <- model.matrix(~ as.factor(sigla_uf) + as.factor(ano), data = DictCounts)
X <- dummy_matrix[,-1]

R <- DictCounts$vote_margin
Y <- DictCounts$care_count

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio)
summary(model)
mean(Y[abs(R)<model$bws[1]&R<0], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)

R <- DictCounts$vote_margin
Y <- DictCounts$dev_count

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio)
summary(model)
mean(Y[abs(R)<model$bws[1]&R<0], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)

R <- DictCounts$vote_margin
Y <- DictCounts$env_count

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio)
summary(model)
mean(Y[abs(R)<model$bws[1]&R<0], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)


R <- DictCounts$vote_margin
Y <- DictCounts$leisure_count

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio)
summary(model)
mean(Y[abs(R)<model$bws[1]&R<0], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)

R <- DictCounts$vote_margin
Y <- DictCounts$tax_count

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio)
summary(model)
mean(Y[abs(R)<model$bws[1]&R<0], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)

R <- DictCounts$vote_margin
Y <- DictCounts$urb_count

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio)
summary(model)
mean(Y[abs(R)<model$bws[1]&R<0], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
