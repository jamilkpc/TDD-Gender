library(tidyverse)
library(rdrobust)

pop <- read.csv('Populacao.csv', skip = 1) %>% 
  select(-Sigla,-Município) %>% 
  rename(id_municipio = Código) %>% 
  tidyr::pivot_longer(!id_municipio, names_to = "ano", values_to = "pop") %>% 
  mutate(ano = as.numeric(sub('.', '', ano))) %>% 
  mutate(logPop = log(pop)) %>% 
  tidyr::drop_na()

dfGuide <- pop %>% 
  filter(ano %in% c(2008,2012,2016,2020)) %>%
  select(id_municipio, ano)

dfHealth <- read.csv('healthpolicy.csv') %>% 
  filter(ano %in% c(2011,2015,2019,2023)) %>% 
  mutate(ano = ano - 3,
         prematuros = gestacao_under_38/non_null_semana_gestacao,
         prenatal = 1 - pre_natal_above_0/total_observations) %>%
  rename(id_municipio = id_municipio_nascimento)

dfHealth <- dfGuide %>% left_join(dfHealth)

dfRDD <- read.csv('dataCovariates.csv') %>% select(-X) %>% 
  left_join(dfHealth)

dfRDD <- dfRDD %>% mutate(uf = floor(id_municipio/100000))
dummy_matrix <- model.matrix(~ as.factor(uf) + as.factor(ano) + open, data = dfRDD)
X <- dummy_matrix[,-1]

R <- dfRDD$vote_margin
Y <- dfRDD$prenatal

summary(rdrobust(Y, R, 0, cluster = dfRDD$id_municipio))
summary(rdrobust(Y, R, 0, covs = X, cluster = dfRDD$id_municipio))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

model3 <- rdrobust(Y, R, 0, cluster = dfRDD$id_municipio)
mean(Y[abs(R)<model3$bws[1]], na.rm = T)
sd(Y[abs(R)<model3$bws[1]], na.rm = T)
summary(model3)

model4 <- rdrobust(Y, R, 0, covs = X, cluster = dfRDD$id_municipio)
mean(Y[abs(R)<model4$bws[1]], na.rm = T)
sd(Y[abs(R)<model4$bws[1]], na.rm = T)
summary(model4)


R <- dfRDD$vote_margin
Y <- dfRDD$prematuros

summary(rdrobust(Y, R, 0, cluster = dfRDD$id_municipio))
summary(rdrobust(Y, R, 0, covs = X, cluster = dfRDD$id_municipio))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

model5 <- rdrobust(Y, R, 0, cluster = dfRDD$id_municipio)
mean(Y[abs(R)<model5$bws[1]], na.rm = T)
sd(Y[abs(R)<model5$bws[1]], na.rm = T)
summary(model5)

model6 <- rdrobust(Y, R, 0, covs = X, cluster = dfRDD$id_municipio)
mean(Y[abs(R)<model6$bws[1]], na.rm = T)
sd(Y[abs(R)<model6$bws[1]], na.rm = T)
summary(model6)

dfHealth <- read.csv('healthpolicy2.csv') %>% 
  filter(ano %in% c(2011,2015,2019,2023)) %>% 
  mutate(ano = ano - 3,
         prematuros = gestacao_under_38/non_null_semana_gestacao,
         prenatal = 1 - pre_natal_above_0/total_observations) %>%
  rename(id_municipio = id_municipio_nascimento)


dfHealth <- dfGuide %>% left_join(dfHealth)

dfRDD <- read.csv('dataCovariates.csv') %>% select(-X) %>% 
  left_join(dfHealth)

dfRDD <- dfRDD %>% mutate(uf = floor(id_municipio/100000))
dummy_matrix <- model.matrix(~ as.factor(uf) + as.factor(ano) + open, data = dfRDD)
X <- dummy_matrix[,-1]

R <- dfRDD$vote_margin
Y <- dfRDD$prenatal

model3 <- rdrobust(Y, R, 0, cluster = dfRDD$id_municipio)
mean(Y[abs(R)<model3$bws[1]], na.rm = T)
sd(Y[abs(R)<model3$bws[1]], na.rm = T)
summary(model3)

model4 <- rdrobust(Y, R, 0, covs = X, cluster = dfRDD$id_municipio)
mean(Y[abs(R)<model4$bws[1]], na.rm = T)
sd(Y[abs(R)<model4$bws[1]], na.rm = T)
summary(model4)
