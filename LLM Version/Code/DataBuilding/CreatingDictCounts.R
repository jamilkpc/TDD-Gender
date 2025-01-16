library(tidyverse)
library(tidytext)
library(rdrobust)
library(rdpower)

dfData <- readRDS('~/Documents/GitHub/TDD-gender/LLM Version/Data/DataManifestosRDD.RDS')

dfTokens <- dfData %>%
  select(id_municipio, ano, sequencial, manifesto) %>% 
  unnest_tokens(word, manifesto)

careDict <- c("saude", "educacao", "escola", "auxilio","auxiliar",
              "creche", "crianca", "criancas", "idoso", "idosos", "idosa", "idosas",
              "familia", "familias", "assistencia", "assistencial", "social", "sociais", "cuidados", 
              "vulneraveis", "apoio", "maternidade", "paternidade", "maes", "vulneravel",
              "acolhimento", "acessibilidade", "protecao",
              "inclusao", "inclusivo", "inclusiva", "inclusivas", "comunitario",
              "inclusivos", "comunidade", "comunidades", "servicos", "igualdade", "direitos", "direito", "cuidado",
              "cuidado", "cuidados",  "autonomia", "psicologico", "mental", "enfermagem", "solidariedade", "enfermeiros", "enfermeiras",
              "prevencao", "seguranca", "alimentacao", "habitacao", "moradia", "medico", "medica", "medicos", "medicas",
              "vacina", "vacinas", "vacinacao", "posto", "atendimento", "upa"
)

leisureDict <- c(
  "lazer", "cultura", "culturais", "cultural", "esporte", "esportivo", "esportiva",
  "esportivos", "esportivas", "recreacao", "recreativo", "recreativa", "recreativos", 
  "recreativas", "parque", "parques", "praça", "evento", "eventos", "atividades", "atividade",
  "festival", "festivais", "festa", "festas", "espetaculos", "espetaculo", "musica", 
  "musical", "musicais", "arte", "artes", "artistico", "artistica", "artisticos", "teatro", 
  "teatral","cinema","atracao", "atracoes", "biblioteca", "quadra", "ginasio", "escolinha",
  "bibliotecas", "espacos", "diversao", 
  "comemoracoes", "tradicoes", "clubes", "escolinhas", 
  "competicao", "ginasios", "quadras", "jogos", "danca", 
  "confraternizacao", "carnaval", "folclore",
  "esportiva", "ludico", "hobby"
)

careTokens <- dfTokens %>% 
  filter(word %in% careDict) %>% 
  group_by(id_municipio, ano, sequencial) %>% 
  summarise(care_count = n()) %>% 
  ungroup()

leisureTokens <- dfTokens %>% 
  filter(word %in% leisureDict) %>% 
  group_by(id_municipio, ano, sequencial) %>% 
  summarise(leisure_count = n()) %>% 
  ungroup()

TotalTokens <- dfTokens %>% 
  group_by(id_municipio, ano, sequencial) %>% 
  summarise(tokens_count = n()) %>% 
  ungroup()

DictCounts <- dfData %>% 
  select(id_municipio, ano, sequencial, sigla_uf, sigla_partido, vote_margin) %>% 
  left_join(careTokens, by = c("id_municipio", "ano", "sequencial")) %>% 
  left_join(leisureTokens, by = c("id_municipio", "ano", "sequencial")) %>% 
  left_join(TotalTokens, by = c("id_municipio", "ano", "sequencial")) %>% 
  mutate(care_count = replace_na(care_count/tokens_count, 0),
         leisure_count = replace_na(leisure_count/tokens_count, 0)) %>% 
  drop_na

DictCounts %>% write_rds('~/Documents/GitHub/TDD-gender/LLM Version/Data/DataCounts.RDS')
