library(tidyverse)
library(tidytext)
library(rdrobust)
library(rdpower)

dfData <- readRDS('~/Documents/GitHub/TDD-gender/LLM Version/Data/DataManifestosRDD.RDS')

dfTokens <- dfData %>%
  select(id_municipio, ano, sequencial, manifesto) %>% 
  unnest_tokens(word, manifesto)

careDict <- c("saude", "educacao", 
              "creche", "crianca", "criancas", "idoso", "idosos", "idosa", "idosas",
              "familia", "familias", "assistencia", "assistencial", "social", "sociais", "cuidados", 
              "vulneraveis", "apoio", "maternidade", "paternidade", "maes", 
              "acolhimento", "acessibilidade", "protecao", "inclusao", 
              "comunidade", "comunidades", "servicos", "igualdade", "direitos", "direito", "cuidado", 
              "autonomia", "psicologico", "mental", "enfermagem", "solidariedade", "enfermeiros", "enfermeiras",
              "prevencao", "seguranca", "alimentacao", "habitacao", "moradia", "medico", "medica", "medicos", "medicas",
              "vacina", "vacinas", "vacinacao", "posto", "atendimento", "upa"
)

careTokens <- dfTokens %>% 
  filter(word %in% careDict) %>% 
  group_by(id_municipio, ano, sequencial) %>% 
  summarise(care_count = n()) %>% 
  ungroup()

TotalTokens <- dfTokens %>% 
  group_by(id_municipio, ano, sequencial) %>% 
  summarise(tokens_count = n()) %>% 
  ungroup()

DictCounts <- dfData %>% 
  select(id_municipio, ano, sequencial, sigla_uf, sigla_partido, vote_margin) %>% 
  left_join(careTokens, by = c("id_municipio", "ano", "sequencial")) %>% 
  left_join(TotalTokens, by = c("id_municipio", "ano", "sequencial")) %>% 
  mutate(care_count = replace_na(care_count/tokens_count, 0)) %>% 
  drop_na

DictCounts %>% write_rds('~/Documents/GitHub/TDD-gender/LLM Version/Data/DataCounts.RDS')
