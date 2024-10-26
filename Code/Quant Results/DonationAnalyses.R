df <- read.csv('Receitas2012Total.csv')

dfGrande <- df %>%
  filter(fonte_receita!='fundo partidario') %>% 
  filter(origem_receita!='recursos de partido politico') %>% 
  group_by(id_municipio, cpf_cnpj_doador) %>%
  summarise(receita = sum(total_valor_receita)) %>% 
  mutate(grande_doador90 = if_else(receita>quantile(receita,0.90, na.rm=T),1,0),
         grande_doador95 = if_else(receita>quantile(receita,0.95, na.rm=T),1,0),
         grande_doador99 = if_else(receita>quantile(receita,0.99, na.rm=T),1,0)) %>% 
  ungroup %>% 
  select(-receita)

dfPrefeitos = df %>% filter(floor(numero_candidato/100)==0) %>% 
  left_join(dfGrande) %>% 
  group_by(id_municipio,numero_candidato) %>% 
  summarise(total_volume = sum(total_valor_receita),
            elite90 = sum(total_valor_receita*grande_doador90, na.rm = T),
            elite95 = sum(total_valor_receita*grande_doador95, na.rm = T),
            elite99 = sum(total_valor_receita*grande_doador99, na.rm = T)) %>% 
  ungroup

# dfCandidato <- read.csv('CandidatoPrefeito.csv')
dfAnalise2012 <- dfCandidato %>% 
  filter(ano == 2012) %>%
  rename(numero_candidato = numero_partido) %>% 
  left_join(dfPrefeitos) %>% 
  mutate(mulher = if_else(genero == 'feminino',1,0))

summary(lm(log(1+elite90) ~ mulher, dfAnalise2012))
summary(lm(log(1+elite95) ~ mulher, dfAnalise2012))
summary(lm(log(1+elite99) ~ mulher, dfAnalise2012))


#### 2016

df <- read.csv('Receitas2016Total.csv')

dfGrande <- df %>%
  filter(fonte_receita!='fundo partidario') %>% 
  filter(origem_receita!='recursos de partido politico') %>% 
  group_by(id_municipio, cpf_cnpj_doador) %>%
  summarise(receita = sum(total_valor_receita)) %>% 
  mutate(grande_doador90 = if_else(receita>quantile(receita,0.90, na.rm=T),1,0),
         grande_doador95 = if_else(receita>quantile(receita,0.95, na.rm=T),1,0),
         grande_doador99 = if_else(receita>quantile(receita,0.99, na.rm=T),1,0)) %>% 
  ungroup %>% 
  select(-receita)

dfPrefeitos = df %>% filter(floor(numero_candidato/100)==0) %>% 
  left_join(dfGrande) %>% 
  group_by(id_municipio,numero_candidato) %>% 
  summarise(total_volume = sum(total_valor_receita),
            elite90 = sum(total_valor_receita*grande_doador90, na.rm = T),
            elite95 = sum(total_valor_receita*grande_doador95, na.rm = T),
            elite99 = sum(total_valor_receita*grande_doador99, na.rm = T)) %>% 
  ungroup

# dfCandidato <- read.csv('CandidatoPrefeito.csv')
dfAnalise2016 <- dfCandidato %>% 
  filter(ano == 2016) %>%
  rename(numero_candidato = numero_partido) %>% 
  left_join(dfPrefeitos) %>% 
  mutate(mulher = if_else(genero == 'feminino',1,0))

summary(lm(log(1+elite90) ~ mulher, dfAnalise2016))
summary(lm(log(1+elite95) ~ mulher, dfAnalise2016))
summary(lm(log(1+elite99) ~ mulher, dfAnalise2016))

### Plot

# Load necessary libraries
library(broom)
library(ggplot2)

# Run models and store in a list
models <- list(
  "2012: 90 Percentile" = lm(log(1 + elite90) ~ mulher, dfAnalise2012),
  "2012: 95 Percentile" = lm(log(1 + elite95) ~ mulher, dfAnalise2012),
  "2012: 99 Percentile" = lm(log(1 + elite99) ~ mulher, dfAnalise2012),
  "2016: 90 Percentile" = lm(log(1 + elite90) ~ mulher, dfAnalise2016),
  "2016: 95 Percentile" = lm(log(1 + elite95) ~ mulher, dfAnalise2016),
  "2016: 99 Percentile" = lm(log(1 + elite99) ~ mulher, dfAnalise2016)
)

# Extract coefficients and confidence intervals using broom::tidy
results <- bind_rows(lapply(models, tidy), .id = "Model") %>%
  filter(term == "mulher")  # Filter to only show results for 'mulher'

# Split the model column into separate columns for year and elite strength
results <- results %>%
  separate(Model, into = c("Year", "EliteStrength"), sep = ": ")

# Plot the results in a 3x2 grid
ggplot(results, aes(x = EliteStrength, y = estimate, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)) +
  geom_pointrange(color = "red", size = 0.5) +
  facet_wrap(~ Year, ncol = 2) +
  # geom_vline(xintercept = , linetype = "dashed", color = "black") +
  labs(title = "Effect of Female Gender on Elite Support Measures",
       x = "Measure of Elite Support",
       y = "Impact on log(1 + Elite Support)") +
  theme_minimal()
