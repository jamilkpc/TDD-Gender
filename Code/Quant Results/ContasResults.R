library(tidyverse)
library(rdrobust)

dfSICONFI <- read.csv('SICONFIdata.csv') %>% select(-X)

dfRDD <- read.csv('dataCovariates.csv') %>% select(-X) %>% 
  left_join(dfSICONFI)

R <- dfRDD$vote_margin
Y <- log(1+dfRDD$saude)

summary(rdrobust(Y, R, 0))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

R <- dfRDD$vote_margin
Y <- log(1+dfRDD$saneamento)

summary(rdrobust(Y, R, 0))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

R <- dfRDD$vote_margin
Y <- log(1+dfRDD$turismo)

summary(rdrobust(Y, R, 0))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

R <- dfRDD$vote_margin
Y <- log(1+dfRDD$comercio)

summary(rdrobust(Y, R, 0))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

R <- dfRDD$vote_margin
Y <- log(1+dfRDD$agro)

summary(rdrobust(Y, R, 0))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

dfRDD2 <- dfRDD %>% 
  mutate(saude = saude/gastos,
         saneamento = saneamento/gastos,
         turismo = turismo/gastos,
         comercio = comercio/gastos,
         agro = agro/gastos)

R <- dfRDD2$vote_margin
Y <- dfRDD2$saude

summary(rdrobust(Y, R, 0))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

R <- dfRDD2$vote_margin
Y <- dfRDD2$saneamento

summary(rdrobust(Y, R, 0))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

R <- dfRDD2$vote_margin
Y <- dfRDD2$turismo

summary(rdrobust(Y, R, 0))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

R <- dfRDD2$vote_margin
Y <- dfRDD2$comercio

summary(rdrobust(Y, R, 0))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

R <- dfRDD2$vote_margin
Y <- dfRDD2$agro

summary(rdrobust(Y, R, 0))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))
