#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# Ao terminar a ETAPA 2 faça um merge de SIM para main
# Altere as orientações do script e commit (em main) "Script com orientações ETAPA 3 - SIDRA"
# Abra um branch OUTROS
# Na branch OUTROS escreva os comandos da Tarefa 1 abaixo

# Tarefa 1. Acesso aos bancos de dados do SIDRA e obtenção da informação
library(tidyverse)

# Lendo os arquivos que você enviou (usando read.csv pois o separador parece ser vírgula)
dados_estimada       <- read.csv("tabela_6579.csv", encoding = "UTF-8")
dados_censo_geral    <- read.csv("tabela_1552_sexo.csv", encoding = "UTF-8")
dados_idade_uf       <- read.csv("tabela_1552_idade_uf.csv", encoding = "UTF-8")
dados_idade_sexo_mun <- read.csv("tabela_1552_idade_sexo_mun.csv", encoding = "UTF-8")

# --- PROCESSAMENTO DOS DADOS PARA O RIO GRANDE DO NORTE (UF 24) ---

# Criando a base_sidra (Variáveis 1 a 4)
base_sidra <- dados_estimada %>%
  mutate(CODMUNRES = as.character(CODMUNRES)) %>%
  # Filtra apenas o RN (24) e seus municípios
  filter(CODMUNRES == "24" | str_starts(CODMUNRES, "24")) %>%
  transmute(
    ANO = 2015,
    NIVEL = ifelse(nchar(CODMUNRES) == 2, "UF", "MUNICIPIO"),
    CODMUNRES = CODMUNRES,
    POPRE_T = as.numeric(POPRE_T)
  )

# Variáveis 5 a 7 (Censo 2010 Total e Sexo)
base_censo_sexo <- dados_censo_geral %>%
  mutate(CODMUNRES = as.character(CODMUNRES)) %>%
  filter(CODMUNRES == "24" | str_starts(CODMUNRES, "24")) %>%
  select(CODMUNRES, POPRC_T, POPRC_M, POPRC_F)

# Variáveis 8 a 13 (Cálculo de faixas etárias)
idades_calculadas <- bind_rows(dados_idade_uf, dados_idade_sexo_mun) %>%
  mutate(CODMUNRES = as.character(CODMUNRES)) %>%
  filter(CODMUNRES == "24" | str_starts(CODMUNRES, "24")) %>%
  group_by(CODMUNRES) %>%
  summarise(
    POPRC_15      = sum(POP[F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos")], na.rm = TRUE),
    POPRC_15_49   = sum(POP[F_IDADE %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos")], na.rm = TRUE),
    POPRC_50      = sum(POP[!F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos")], na.rm = TRUE),
    POPRC_F_15    = sum(POPF[F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos")], na.rm = TRUE),
    POPRC_F_15_49 = sum(POPF[F_IDADE %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos")], na.rm = TRUE),
    POPRC_F_50    = sum(POPF[!F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos")], na.rm = TRUE)
  )

# UNIR TUDO NO BANCO FINAL SIDRA_UF
SIDRA_UF <- base_sidra %>%
  left_join(base_censo_sexo, by = "CODMUNRES") %>%
  left_join(idades_calculadas, by = "CODMUNRES")

# Exporte o arquivo em formato CSV
write.csv(SIDRA_UF, "SIDRA_UF.csv", row.names = FALSE)

# Faça o commit com a mensagem "Script e dados TAREFA 3 - SIDRA"