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

R <- dfRDD$vote_margin
Y <- dfRDD$prenatal

summary(rdrobust(Y, R, 0))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

R <- dfRDD$vote_margin
Y <- dfRDD$prematuros

summary(rdrobust(Y, R, 0))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))



dfHealth <- read.csv('healthpolicy2.csv') %>% 
  filter(ano %in% c(2011,2015,2019,2023)) %>% 
  mutate(ano = ano - 3,
         prematuros = gestacao_under_38/non_null_semana_gestacao,
         prenatal = 1 - pre_natal_above_0/total_observations) %>%
  rename(id_municipio = id_municipio_nascimento)


dfHealth <- dfGuide %>% left_join(dfHealth)

dfRDD <- read.csv('dataCovariates.csv') %>% select(-X) %>% 
  left_join(dfHealth)

R <- dfRDD$vote_margin
Y <- dfRDD$prenatal

summary(rdrobust(Y, R, 0))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))