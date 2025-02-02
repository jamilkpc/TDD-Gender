library(tidyverse)
library(tidytext)
library(rdrobust)
library(rdpower)
library(rddensity)
library(ggplot2)

dfOpenSeats <- readRDS('~/Documents/GitHub/TDD-gender/LLM Version/Data/DataOpenSeat.RDS')

DictCounts <- readRDS('~/Documents/GitHub/TDD-gender/LLM Version/Data/DataCounts.RDS') %>% 
  left_join(dfOpenSeats) %>% 
  drop_na

# HTE - Not running again

dfHTE0 <- DictCounts %>% filter(openSeat == 1)

dummy_matrix <- model.matrix(~ as.factor(sigla_uf) + as.factor(ano), data = dfHTE0)
X <- dummy_matrix[,-1]

R <- dfHTE0$vote_margin
Y <- dfHTE0$care_count

care0 <- rdrobust(Y, R, 0, covs = X, cluster = dfHTE0$id_municipio)
summary(care0)
rdpower(data = cbind(Y,R), tau = 0.0231447*0.25, covs = X, cluster = dfHTE0$id_municipio, alpha = 0.05)

R <- dfHTE0$vote_margin
Y <- dfHTE0$env_count

env0 <- rdrobust(Y, R, 0, covs = X, cluster = dfHTE0$id_municipio)
summary(env0)

R <- dfHTE0$vote_margin
Y <- dfHTE0$urb_count

urb0 <- rdrobust(Y, R, 0, covs = X, cluster = dfHTE0$id_municipio)
summary(urb0)

R <- dfHTE0$vote_margin
Y <- dfHTE0$dev_count

dev0 <- rdrobust(Y, R, 0, covs = X, cluster = dfHTE0$id_municipio)
summary(dev0)

R <- dfHTE0$vote_margin
Y <- dfHTE0$tax_count

tax0 <- rdrobust(Y, R, 0, covs = X, cluster = dfHTE0$id_municipio)
summary(tax0)

R <- dfHTE0$vote_margin
Y <- dfHTE0$leisure_count

lei0 <- rdrobust(Y, R, 0, covs = X, cluster = dfHTE0$id_municipio)
summary(lei0)

# HTE - Running again

dfHTE1 <- DictCounts %>% filter(openSeat == 0)

dummy_matrix <- model.matrix(~ as.factor(sigla_uf) + as.factor(ano), data = dfHTE1)
X <- dummy_matrix[,-1]

R <- dfHTE1$vote_margin
Y <- dfHTE1$care_count

care1 <- rdrobust(Y, R, 0, covs = X, cluster = dfHTE1$id_municipio)
summary(care1)

R <- dfHTE1$vote_margin
Y <- dfHTE1$env_count

env1 <- rdrobust(Y, R, 0, covs = X, cluster = dfHTE1$id_municipio)
summary(env1)

R <- dfHTE1$vote_margin
Y <- dfHTE1$urb_count

urb1 <- rdrobust(Y, R, 0, covs = X, cluster = dfHTE1$id_municipio)
summary(urb1)

R <- dfHTE1$vote_margin
Y <- dfHTE1$dev_count

dev1 <- rdrobust(Y, R, 0, covs = X, cluster = dfHTE1$id_municipio)
summary(dev1)
rdpower(data = cbind(Y,R), tau = sd(Y[abs(R)<dev1$bws[1]], na.rm = T)*0.2, covs = X, cluster = dfHTE1$id_municipio, alpha = 0.05)

R <- dfHTE1$vote_margin
Y <- dfHTE1$tax_count

tax1 <- rdrobust(Y, R, 0, covs = X, cluster = dfHTE1$id_municipio)
summary(tax1)

R <- dfHTE1$vote_margin
Y <- dfHTE1$leisure_count

lei1 <- rdrobust(Y, R, 0, covs = X, cluster = dfHTE1$id_municipio)
summary(lei1)

# Function to extract results from RDRobust objects
extract_results <- function(model, name) {
  data.frame(
    Model = name,
    Estimate = model$Estimate[2],
    CI_Lower = model$ci[3,1],
    CI_Upper = model$ci[3,2]
  )
}

# Extract results for "0" models
models_0 <- list(care0 = care0, dev0 = dev0, env0 = env0, lei0 = lei0, tax0 = tax0, urb0 = urb0)
results_0 <- bind_rows(lapply(names(models_0), function(name) extract_results(models_0[[name]], name)))

# Extract results for "1" models
models_1 <- list(care1 = care1, dev1 = dev1, env1 = env1, lei1 = lei1, tax1 = tax1, urb1 = urb1)
results_1 <- bind_rows(lapply(names(models_1), function(name) extract_results(models_1[[name]], name)))

# Add a column to distinguish between "0" and "1" models
results_0 <- results_0 %>% mutate(Group = "0")
results_1 <- results_1 %>% mutate(Group = "1")

model_order <- c("care0", "dev0", "env0", "lei0", "tax0", "urb0", 
                 "care1", "dev1", "env1", "lei1", "tax1", "urb1")
model_labels <- c("Social and Care", "Development", "Environmental", 
                  "Leisure", "Public Finance", "Urban Services")

# Combine results into one data frame
results <- bind_rows(results_0, results_1) %>%
  mutate(
    Model = factor(Model, levels = model_order),
    Label = factor(
      case_when(
        grepl("care", Model) ~ "Social and Care",
        grepl("dev", Model) ~ "Development",
        grepl("env", Model) ~ "Environmental",
        grepl("lei", Model) ~ "Leisure",
        grepl("tax", Model) ~ "Public Finance",
        grepl("urb", Model) ~ "Urban Services"
      ),
      levels = rev(model_labels)
    )
  )

# Create plots
plot_0 <- ggplot(results %>% filter(Group == "0"), aes(y = Label, x = Estimate)) +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "RDD Estimates: Open Seat", x = "Estimate", y = "Policy Topic") +
  coord_cartesian(xlim = c(-0.01, 0.01))  +
  theme_minimal()

plot_1 <- ggplot(results %>% filter(Group == "1"), aes(y = Label, x = Estimate)) +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "RDD Estimates: Reelection", x = "Estimate", y = NULL) +
  coord_cartesian(xlim = c(-0.01, 0.01)) +
  theme_minimal()

library(patchwork)

# Combine the plots in a 1x2 layout
combined_plot <- plot_0 + plot_1 + plot_layout(ncol = 2)

# Print the combined plot
print(combined_plot)
