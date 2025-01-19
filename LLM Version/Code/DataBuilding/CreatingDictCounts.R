library(tidyverse)
library(tidytext)
library(rdrobust)
library(rdpower)

dfData <- readRDS('~/Documents/GitHub/TDD-gender/LLM Version/Data/DataManifestosRDD.RDS')

dfTokens <- dfData %>%
  select(id_municipio, ano, sequencial, manifesto) %>% 
  unnest_tokens(word, manifesto)

careDict <- c("educacao", "escola", "escolas", "escolar", "educacional", 
              "educativo", "educativa", "educativos", "educativas", "creche",
              "crianca", "criancas", "infantil", "infantis", "infancia",
              "alfabetizacao", "alfabetizar", # Suggestions - education - LLama 1
              "ensino", "ensinar", # Suggestions - education - LLama 2
              "aprendizado", "aprendizagem", # Suggestions - education - Llama 3
              "saude", "enfermagem", "enfermaria", "enfermeiros", "enfermeiras",
              "enfermeiro", "enfermeira", "alimentacao",  "posto", "atendimento",
              "upa", "hospital", "medico", "medica", "medicos", "medicas",
              "prevencao", 'preventivo', 'preventiva', 'preventivos', 'preventivas',
              "psicologia", "psicologico", "mental",
              "vacina", "vacinas", "vacinacao",
              'clinica', "assistencia", "assistencial", # Suggestions - saude - Llama
              'ambulatorio', 'ambulatorial', 'ambulancia', # Suggestions - saude - Llama
              'laboratorio', "odontologia", "dentistas", "dental", # Suggestions - saude - Llama
              'pediatria', 'pediatra', # Suggestion - saude - Llama, iteration over crianca
              'acessibilidade', 
              "inclusao", "inclusivo", "inclusiva", "inclusivos", "inclusivas",
              'vulneravel',  'vulneraveis',
              "autonomia", "cuidado", "cuidados",
              'acolhimento', 'acolher', # Suggestion - deficientes - Llama
              'direito', 'direitos',
              "social", "sociais", 
              "comunitario", "comunidade", "comunidades",
              'pobreza', 'desigualdade',
              "auxilio", "auxilios", "auxiliar", "apoio", 
              "familia", "familias", # Suggestion - poverty - Llama
              'moradia', 'habitacao', 'habitacional',
              # Palavras adicionais abaixo
              "idoso", "idosos", "idosa", "idosas",
              "maternidade", "paternidade", "maes", 'prenatal',
              "igualdade"
)

envDict <- c(
  'ambiente', 'ambiental',
  'sustentabilidade', 'sustentavel', 'sustentaveis',
  'ecologico', 'ecologica', 'ecologicos', 'ecologicas', 'ecologia',
  'natureza', 'natural', 'naturais',
  'minerais', 'mineral',
  'poluicao', 'poluente', 'poluentes', 'residuo', 'residuos',
  'contaminacao', 'contaminavel',
  'hidrica', 'rios', 'manancial', 'mananciais', 'margem', 'margens',
  'lago','lagoa', 'lagos', 'praia', 'serra', 'serras',
  'marginal', 'afluente',
  'renovavel', 'renovaveis', 
  'solar', #llama
  'ecossistema', 'ecossistemas',
  'protecao', 'preservacao', 'conservacao', #llama
  'biodiversidade', 'bioma', #llama
  'florestas', 'floresta', 'bosque', 'mata',
  'fauna', 'flora', 'animais', 'plantas', 'silvestres', 'arvores', 'especies',
  'solo', 'terra', 'fertilidade', 'reflorestamento', 'degradacao', 'erosao',
  'revegetacao', 'reciclagem',  'reuso', 'reaproveitamento', 'esgotamento' #llama
)

urbanServDict <- c(
    "urbano", 'urbana', 'urbanos', 'urbana', 'urbanismo', 'urbanizacao',
    'urbanizado', 'urbanizada', 'urbanizados', 'urbanizadas', 'urbanistico',
    'zona', 'zonas', 'ordenamento', 'calcada', 'calcadas', 'rua', 'ruas',
    'avenida', 'avenidas', 'residencial', 'residenciais', 'comercial', 'comerciais',
    'asfalto', 'asfaltamento', 'buraco', 'buracos', 'esburacado', 'pavimentacao',
    'verticalizacao', 
    'perimetral', #llama
    'cabeamento', 'eletricidade', 'eletrica', 'regularizacao', #llama
    'saneamento', 'agua', 'aguas', 'esgoto', 'drenagem', 'efluente', 'efluentes',
    'lixo', 'cloracao', 'cloro', 'limpeza', 'descarte', 'pluvial', 'enchente',
    'enchentes', 'abastecimento', 'coleta',
    'transporte', 'onibus', 'terminal', 'terminais', 'veiculos', 'estrada', 'estradas',
    'metro', 'trem', 'ciclovia', 'bicicleta', 'rodovias', 'ciclistas', 'ciclista', 'ciclofaixa',
    'viario', 'rodoviario', 'metroviario', 'ferroviario', 'hidroviario',
    'barco', 'barca', 'barcos', 'barcas', 'balsa', 'balsas', 
    'mobilidade', 'deslocamento', 'sinalizacao', 'semaforo', 'semaforos',
    'congestionamento', 'transito', 'transitar', 'pedestre', 'pedestres',
    'seguranca', 'violencia', 'guarda', 'guardas', 'patrulhamento', 'patrulha',
    'patrulhas', 'vigilancia', 'arma', 'armas', 'armamentos', 'ordem',
    'multa', 'multas', 'infracao', 'infracoes', 'limite', 'limites', 'trafego',
    'velocidade', 'camera', 'cameras',
    'iluminacao', 'poste', 'postes', 'luminaria', 'luz', 'lampada', 'lampadas',
    'luminosidade', 'noturna'
)

devDict <- c(
  "desenvolvimento", "economico", "economia", "investimento", "investimentos", 
  "emprego", "empregos", "empregabilidade",
  "trabalho", "renda", "agricultura", 'agronegocio', 'agricola', 'agricolas',
  "industria", 'industrial', "comercio", 
  "servicos", "inovacao", 'inovacoes',
  "competitividade", "negocio", "negocios",
  "empreendedorismo", "empreendimentos", "empreendimentos", "empreendedor", "empreendedores",
  "startup", "startups", 
  "financiamento", "microcredito", "produtivo", 'produtividade', 'producao',  
  "exportacao", "importacao", "crescimento", "burocracia", 
  "empresa", "empresas", "cooperativa", "cooperativas", 'subsidio', 'subsidios',
  
  'microempresa', 'microempresario', 'simplificacao', 'consultoria', 'formalizar',
  'licenciamento',
  
  'turismo', 'turista', 'turistas', 'turistico', 'turistica', 'turisticos', 
  'turisticas', 'ecoturismo', 'destinos', 'hospitalidade',
  'gastronomia', 'gastronomico', 'gastronomicos',
  'restaurante', 'restaurantes', 'bar', 'bares', 'guias', 'roteiros', 
  'hotel', 'hoteis', 'pousada', 'pousadas', 'acomodacao', 'acomodacoes',
  'atrativo', 'atrativa', 'atrativos', 'atrativas',
  
  'agricultor', 'agricultores', 'rural', 'rurais', 'campo', 'colheita', 'plantio',
  'sementes', 'fertilizante', 'fertilizantes', 'irrigacao', 'maquinario', 'credito'
)

taxDict <- c(
  "tributacao", 'tributo', 'tributos', 'tributaria', 'tributario',
  'contribuicao', "contribuicoes", 'contribuinte', 'contribuintes',
  "imposto", "impostos", "taxa", "taxas", 'taxar',
  'arrecadar', "arrecadacao", "fiscal", "fiscais", "receita", "desoneracao", "isencao",
  "progressivo", "progressiva", "regressivo", "regressiva",
  "aliquota", "aliquotas", 'financiamento', 'refinanciamento', 'conta', 'contas',
  "carga", "IPTU", "ISS", "ITBI", 'ITR', 'propriedade', 'propriedades', 'territorial',
  "ICMS", "regime", "orcamento", 'orcamentaria', 'orcamentario', 
  'transparencia', 'transparente',  'transparentes',
  'previdencia', 'previdenciario', 'previdenciaria', 'previdenciarios', 'previdenciarias',
  'COSIP', 'custeio', 'lucro', 'prejuizo', 'saldo', 'balanco', 'balancete',
  'demonstrativo', 'demonstrativos', 'caixa', 'custo', 'custos', 'receita', 'receitas',
  'pagamento', 'administrativo', 'administrativos'
)

leisureDict <- c(
  "lazer", "cultura", "culturais", "cultural", "esporte", "esportivo", "esportiva",
  "esportivos", "esportivas", "recreacao", "recreativo", "recreativa", "recreativos", 
  "recreativas", "parque", "parques", "praca", "pracas", "evento", "eventos", "atividades", "atividade",
  "festival", "festivais", "festa", "festas", "espetaculos", "espetaculo", "musica", 
  "musical", "musicais", "arte", "artes", "artistico", "artistica", "artisticos", "artisticas", "teatro", 
  "teatral","cinema","atracao", "atracoes", "biblioteca", "quadra", "quadras", "ginasio", "escolinha",
  "bibliotecas", "espacos", "diversao", "futebol", "futsal", "volei", "basquete",
  "comemoracoes", "tradicoes", "clubes", "escolinhas", 
  "competicao", "ginasios", "quadras", "jogos", "danca", 
  "confraternizacao", "carnaval", "folclore",
  "esportiva", "ludico", "hobby", 'audiovisual', 'cinema',
  'historico', 'historica', 'historicos', 'historicas', 'historia', 'museu', 'museus',
  'popular', 'populares'
)

careTokens <- dfTokens %>% 
  filter(word %in% careDict) %>% 
  group_by(id_municipio, ano, sequencial) %>% 
  summarise(care_count = n()) %>% 
  ungroup()

envTokens <- dfTokens %>% 
  filter(word %in% envDict) %>% 
  group_by(id_municipio, ano, sequencial) %>% 
  summarise(env_count = n()) %>% 
  ungroup()

urbTokens <- dfTokens %>% 
  filter(word %in% urbanServDict) %>% 
  group_by(id_municipio, ano, sequencial) %>% 
  summarise(urb_count = n()) %>% 
  ungroup()

devTokens <- dfTokens %>% 
  filter(word %in% devDict) %>% 
  group_by(id_municipio, ano, sequencial) %>% 
  summarise(dev_count = n()) %>% 
  ungroup()

taxTokens <- dfTokens %>% 
  filter(word %in% taxDict) %>% 
  group_by(id_municipio, ano, sequencial) %>% 
  summarise(tax_count = n()) %>% 
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
  left_join(envTokens, by = c("id_municipio", "ano", "sequencial")) %>% 
  left_join(urbTokens, by = c("id_municipio", "ano", "sequencial")) %>% 
  left_join(devTokens, by = c("id_municipio", "ano", "sequencial")) %>% 
  left_join(taxTokens, by = c("id_municipio", "ano", "sequencial")) %>% 
  left_join(leisureTokens, by = c("id_municipio", "ano", "sequencial")) %>% 
  left_join(TotalTokens, by = c("id_municipio", "ano", "sequencial")) %>% 
  mutate(care_count = replace_na(care_count/tokens_count, 0),
         env_count = replace_na(env_count/tokens_count, 0),
         urb_count = replace_na(urb_count/tokens_count, 0),
         dev_count = replace_na(dev_count/tokens_count, 0),
         tax_count = replace_na(tax_count/tokens_count, 0),
         leisure_count = replace_na(leisure_count/tokens_count, 0)) %>% 
  drop_na

DictCounts %>% write_rds('~/Documents/GitHub/TDD-gender/LLM Version/Data/DataCounts.RDS')
