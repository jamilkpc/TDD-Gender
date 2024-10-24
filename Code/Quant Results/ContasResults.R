library(tidyverse)
library(rdrobust)

dfSICONFI <- read.csv('SICONFIdata.csv') %>% select(-X)

dfRDD <- read.csv('dataCovariates.csv') %>% select(-X) %>% 
  left_join(dfSICONFI)

dfRDD <- dfRDD %>% mutate(uf = floor(id_municipio/100000))
dummy_matrix <- model.matrix(~ as.factor(uf) + as.factor(ano) + open, data = dfRDD)
X <- dummy_matrix[,-1]

R <- dfRDD$vote_margin
Y <- log(dfRDD$gastos)


summary(rdrobust(Y, R, 0, cluster = dfRDD$id_municipio))
summary(rdrobust(Y, R, 0, covs = X, cluster = dfRDD$id_municipio))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

model1 <- rdrobust(Y, R, 0, cluster = dfRDD$id_municipio)
mean(Y[abs(R)<model1$bws[1]], na.rm = T)
sd(Y[abs(R)<model1$bws[1]], na.rm = T)
summary(model1)

model2 <- rdrobust(Y, R, 0, covs = X, cluster = dfRDD$id_municipio)
mean(Y[abs(R)<model2$bws[1]], na.rm = T)
sd(Y[abs(R)<model2$bws[1]], na.rm = T)
summary(model2)

R <- dfRDD$vote_margin
Y <- log(1+dfRDD$saude)

summary(rdrobust(Y, R, 0, cluster = dfRDD$id_municipio))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

R <- dfRDD$vote_margin
Y <- log(1+dfRDD$saneamento)

summary(rdrobust(Y, R, 0, cluster = dfRDD$id_municipio))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

R <- dfRDD$vote_margin
Y <- log(1+dfRDD$turismo)

summary(rdrobust(Y, R, 0, cluster = dfRDD$id_municipio))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

R <- dfRDD$vote_margin
Y <- log(1+dfRDD$comercio)

summary(rdrobust(Y, R, 0, cluster = dfRDD$id_municipio))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

R <- dfRDD$vote_margin
Y <- log(1+dfRDD$agro)

summary(rdrobust(Y, R, 0, cluster = dfRDD$id_municipio))
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

dfRDD2 <- dfRDD %>% 
  mutate(saude = saude/gastos,
         saneamento = saneamento/gastos,
         turismo = turismo/gastos,
         comercio = comercio/gastos,
         agro = agro/gastos)

R <- dfRDD2$vote_margin
Y <- dfRDD2$saude

df_plot <- matrix(NA,5,4)

df_plot[1,1] = 'Health'
model1 <- rdrobust(Y, R, 0, cluster = dfRDD2$id_municipio)
summary(rdrobust(Y, R, 0, cluster = dfRDD2$id_municipio))
df_plot[1,2] = model1$coef[1]
df_plot[1,3] <- as.numeric(model1$ci[3,1])
df_plot[1,4] <- as.numeric(model1$ci[3,2])

summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))



R <- dfRDD2$vote_margin
Y <- dfRDD2$saneamento

df_plot[2,1] = 'Sanitation'
model2 <- rdrobust(Y, R, 0, cluster = dfRDD2$id_municipio)
summary(rdrobust(Y, R, 0, cluster = dfRDD2$id_municipio))
df_plot[2,2] = model2$coef[1]
df_plot[2,3] <- as.numeric(model2$ci[3,1])
df_plot[2,4] <- as.numeric(model2$ci[3,2])
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

R <- dfRDD2$vote_margin
Y <- dfRDD2$turismo

df_plot[3,1] = 'Tourism'
model3 <- rdrobust(Y, R, 0, cluster = dfRDD2$id_municipio)
summary(rdrobust(Y, R, 0, cluster = dfRDD2$id_municipio))
df_plot[3,2] = model3$coef[1]
df_plot[3,3] <- as.numeric(model3$ci[3,1])
df_plot[3,4] <- as.numeric(model3$ci[3,2])
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

R <- dfRDD2$vote_margin
Y <- dfRDD2$comercio

df_plot[4,1] = 'Commerce'
model4 <- rdrobust(Y, R, 0, cluster = dfRDD2$id_municipio)
summary(rdrobust(Y, R, 0, cluster = dfRDD2$id_municipio))
df_plot[4,2] = model4$coef[1]
df_plot[4,3] <- as.numeric(model4$ci[3,1])
df_plot[4,4] <- as.numeric(model4$ci[3,2])
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

R <- dfRDD2$vote_margin
Y <- dfRDD2$agro

df_plot[5,1] = 'Agriculture'
model5 <- rdrobust(Y, R, 0, cluster = dfRDD2$id_municipio)
summary(rdrobust(Y, R, 0, cluster = dfRDD2$id_municipio))
df_plot[5,2] = model5$coef[1]
df_plot[5,3] <- as.numeric(model5$ci[3,1])
df_plot[5,4] <- as.numeric(model5$ci[3,2])
summary(rdlocrand::rdrandinf(Y, R, 0, wl = -0.0375, wr = 0.0375))

df_plot <- as.data.frame(df_plot)
colnames(df_plot) <- c('area','beta','lci','hci')
df_plot <- df_plot %>%
  mutate(beta = as.numeric(beta),
         lci = as.numeric(lci),
         hci = as.numeric(hci))

ggplot(df_plot, aes(x = factor(area), y = beta, ymin = lci, ymax = hci)) +
  geom_pointrange() +
  coord_flip() +
  labs(title = "RDD Results on Spending", 
       x = "Area", 
       y = "Effect Size") +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed")
