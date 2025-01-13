library(tidyverse)

# Building SICONFI data

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

dfContas <- read.csv('contasSICONFI1.csv')
unique(dfContas$conta_bd)
unique(dfContas$estagio_bd)

dfSaneamento <- dfContas %>% 
  filter(conta_bd %in% c("Saneamento",
                         "Saneamento Básico Rural",
                         "Saneamento Básico Urbano",
                         "Demais Subfunções Saneamento")) %>% 
  group_by(id_municipio, ano, estagio) %>% 
  summarise(valor = sum(valor)) %>% 
  ungroup() %>% 
  filter(estagio %in% c('', 'Despesas Empenhadas')) %>% 
  select(id_municipio, ano, valor) %>% 
  mutate(ano = ano - 3) %>% 
  rename(saneamento = valor)

dfComercio <- dfContas %>% 
  filter(conta_bd %in% c("Comércio e Serviços",
                         "Promoção Comercial",
                         "Comercialização")) %>% 
  group_by(id_municipio, ano, estagio) %>% 
  summarise(valor = sum(valor)) %>% 
  ungroup() %>% 
  filter(estagio %in% c('', 'Despesas Empenhadas')) %>% 
  select(id_municipio, ano, valor) %>% 
  mutate(ano = ano - 3) %>% 
  rename(comercio = valor)

dfTurismo <- dfContas %>% 
  filter(conta_bd %in% c("Turismo")) %>% 
  group_by(id_municipio, ano, estagio) %>% 
  summarise(valor = sum(valor)) %>% 
  ungroup() %>% 
  filter(estagio %in% c('', 'Despesas Empenhadas')) %>% 
  select(id_municipio, ano, valor) %>% 
  mutate(ano = ano - 3) %>% 
  rename(turismo = valor)

dfAgro <- dfContas %>% 
  filter(conta_bd %in% c("Agricultura",
                         "Abastecimento",
                         "Extensão Rural",
                         "Irrigação",
                         "Promoção da Produção Agropecuária",
                         "Defesa Agropecuária",
                         "Demais Subfunções Agricultura")) %>% 
  group_by(id_municipio, ano, estagio) %>% 
  summarise(valor = sum(valor)) %>% 
  ungroup() %>% 
  filter(estagio %in% c('', 'Despesas Empenhadas')) %>% 
  select(id_municipio, ano, valor) %>% 
  mutate(ano = ano - 3) %>% 
  rename(agro = valor)

dfTotal <- dfContas %>% 
  group_by(id_municipio, ano, estagio) %>% 
  summarise(valor = sum(valor)) %>% 
  ungroup() %>% 
  filter(estagio %in% c('', 'Despesas Empenhadas')) %>% 
  select(id_municipio, ano, valor) %>% 
  mutate(ano = ano - 3) %>% 
  rename(gastos = valor)


dfSICONFI <- dfGuide %>%
  left_join(dfSaude) %>% 
  left_join(dfSaneamento) %>% 
  left_join(dfTurismo) %>% 
  left_join(dfComercio) %>% 
  left_join(dfAgro) %>% 
  left_join(dfTotal)

write.csv(dfSICONFI, 'SICONFIdata.csv')
