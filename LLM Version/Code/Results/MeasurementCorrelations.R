library(fixest)

df <- readRDS('~/Documents/GitHub/TDD-gender/LLM Version/Data/CorrCount.RDS')

feols(care_count ~ mulher, data = df, cluster = 'id_municipio')
feols(env_count ~ mulher, data = df, cluster = 'id_municipio')
feols(urb_count ~ mulher, data = df, cluster = 'id_municipio')
feols(dev_count ~ mulher, data = df, cluster = 'id_municipio')
feols(tax_count ~ mulher, data = df, cluster = 'id_municipio')
feols(leisure_count ~ mulher, data = df, cluster = 'id_municipio')

feols(care_count ~ mulher | id_municipio + ano, data = df)
feols(env_count ~ mulher | id_municipio + ano, data = df)
feols(urb_count ~ mulher | id_municipio + ano, data = df)
feols(dev_count ~ mulher | id_municipio + ano, data = df)
feols(tax_count ~ mulher | id_municipio + ano, data = df)
feols(leisure_count ~ mulher | id_municipio + ano, data = df)

# Define categories
categories <- c("care_count", "dev_count", "env_count", 
                "leisure_count", "tax_count", "urb_count")

# Custom x-axis labels
x_labels <- c("Social and Care", "Development", "Environmental", 
              "Leisure", "Public Finance", "Urban Services")

# Prepare results storage
results <- data.frame(category = character(),
                      gender = character(),
                      estimate = numeric(),
                      se = numeric(),
                      stringsAsFactors = FALSE)

# Loop through each category and run regressions
for (cat in categories) {
  formula <- as.formula(paste(cat, "~ mulher"))
  model <- feols(formula, data = df, cluster = 'id_municipio')
  
  # Extract estimates and non-clustered SEs
  male_avg <- coef(model)["(Intercept)"]
  female_avg <- male_avg + coef(model)["mulher"]
  male_se <- sqrt(vcov(model)["(Intercept)", "(Intercept)"])
  female_se <- sqrt(vcov(model)["(Intercept)", "(Intercept)"] + 
                      vcov(model)["mulher", "mulher"] +
                      2 * vcov(model)["(Intercept)", "mulher"])
  
  # Store results
  results <- results %>%
    add_row(category = cat, gender = "Male", estimate = male_avg, se = male_se) %>%
    add_row(category = cat, gender = "Female", estimate = female_avg, se = female_se)
}

# Update categories to match x-axis labels
results$category <- factor(results$category, levels = categories, labels = x_labels)

# Plotting
ggplot(results, aes(x = category, y = estimate, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                position = position_dodge(0.9), width = 0.25) +
  theme_minimal() +
  labs(title = "Male and Female Averages per Topic",
       x = "Topic", y = "Average Attention", fill = "Gender") +
  scale_fill_manual(values = c("Female" = "#9ecae1", "Male" = "#fdae6b")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10))
