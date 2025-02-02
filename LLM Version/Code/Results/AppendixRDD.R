library(tidyverse)
library(tidytext)
library(rdrobust)
library(rdpower)
library(rddensity)
library(ggplot2)

dfRDD <- read.csv('~/Documents/GitHub/FemaleFunding/CleanData/dataRDD.csv') %>% 
  select(-X) %>% 
  mutate(ano = ano + 4)

### Density Appendix

dentest <- rddensity(dfRDD$vote_margin, all = T)
summary(dentest)
rdplotdensity(dentest, dfRDD$vote_margin, lcol = c("black", "black"), xlabel = "margin",
              plotRange = c(-1, 1), plotN = 100)

### Loading Controls

dfOpenSeats <- readRDS('~/Documents/GitHub/TDD-gender/LLM Version/Data/DataOpenSeat.RDS')

DictCounts <- readRDS('~/Documents/GitHub/TDD-gender/LLM Version/Data/DataCounts.RDS') %>% 
  left_join(dfOpenSeats) %>% 
  drop_na

dummy_matrix <- model.matrix(~ as.factor(sigla_uf) + as.factor(ano) + openSeat, data = DictCounts)
X <- dummy_matrix[,-1]

R <- DictCounts$vote_margin
Y <- DictCounts$care_count

### Cut-off Appendix

# Define a range of cutoffs to test
cutoffs <- seq(-0.2, 0.2, by = 0.01)

# Create a data frame to store results
results <- data.frame(cutoff = numeric(), estimate = numeric(), 
                      ci_lower = numeric(), ci_upper = numeric())

# Loop through the cutoffs
for (c in cutoffs) {
  model <- rdrobust(Y, R, c, covs = X, cluster = DictCounts$id_municipio)
  estimate <- model$Estimate[1, 1] # Point estimate
  ci_lower <- model$ci[1, 1]       # Lower CI
  ci_upper <- model$ci[1, 2]       # Upper CI
  
  # Append to results
  results <- rbind(results, data.frame(cutoff = c, estimate = estimate, 
                                       ci_lower = ci_lower, ci_upper = ci_upper))
}

# Plot the estimates and confidence intervals
ggplot(results, aes(x = cutoff, y = estimate)) +
  geom_point(aes(y = estimate), color = "blue", size = 1) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.005, color = "black") +
  theme_minimal() +
  labs(title = "Estimate and Confidence Intervals by Cutoff",
       x = "Cutoff Value", y = "Estimate with 95% CI")

### Bandwidth Appendix

# Define a range of cutoffs to test
h <- seq(0.03, 0.35, by = 0.02)

# Create a data frame to store results
results <- data.frame(bandwidth = numeric(), estimate = numeric(), 
                      ci_lower = numeric(), ci_upper = numeric())

# Loop through the cutoffs
for (i in h) {
  model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, h = i)
  estimate <- model$Estimate[1, 1] # Point estimate
  ci_lower <- model$ci[1, 1]       # Lower CI
  ci_upper <- model$ci[1, 2]       # Upper CI
  
  # Append to results
  results <- rbind(results, data.frame(bandwidth = i, estimate = estimate, 
                                       ci_lower = ci_lower, ci_upper = ci_upper))
}

# Plot the estimates and confidence intervals
ggplot(results, aes(x = bandwidth, y = estimate)) +
  geom_point(aes(y = estimate), color = "blue", size = 1) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.005, color = "black") +
  theme_minimal() +
  labs(title = "Estimate and Confidence Intervals by Bandwidth",
       x = "Bandwidth Value", y = "Estimate with 95% CI") +
  ylim(-0.005, 0.015)

### Alternate Kernels, BW and Polynomials

model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, kernel = 'uniform')
summary(model)
mean(Y[abs(R)<model$bws[1]&R<0], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, kernel = 'epanechnikov')
summary(model)
mean(Y[abs(R)<model$bws[1]&R<0], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, bwselect = 'cerrd')
summary(model)
mean(Y[abs(R)<model$bws[1]&R<0], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
model <- rdrobust(Y, R, 0, covs = X, cluster = DictCounts$id_municipio, p = 2)
summary(model)
mean(Y[abs(R)<model$bws[1]&R<0], na.rm = T)
sd(Y[abs(R)<model$bws[1]], na.rm = T)
