##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015
# Nomeie o banco de dados como dados_sim
dados_sim <- read.csv2("Mortalidade_Geral_2015.csv", header = TRUE, sep = ";")

# Verificar se a leitura foi feita corretamente e a estrutura dos dados
dim(dados_sim)
str(dados_sim)


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão: 1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84
dados_sim_1 <- dados_sim[, c(1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84)]

# Nomes das variáveis: CONTADOR, TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO, RACACOR, ESC2010, CODMUNRES, TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO
names(dados_sim_1) <- c("CONTADOR", "TIPOBITO", "DTOBITO", "DTNASC", "IDADE", "SEXO", "RACACOR", "ESC2010", "CODMUNRES", "TPMORTEOCO", "OBITOGRAV", "OBITOPUERP", "CAUSABAS", "TPOBITOCOR", "MORTEPARTO")


# Tarefa 3. Reduzir dados_sim_1 apenas para o estado (Rio Grande do Norte: 24), nomeando como dados_sim_2
dados_sim_2 <- dados_sim_1[substr(dados_sim_1$CODMUNRES, 1, 2) == "24", ]

# Exportar o arquivo com o nome dados_sim_2.csv
write.csv(dados_sim_2, "dados_sim_2.csv", row.names = FALSE)


# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias
table(dados_sim_2$TIPOBITO)
table(dados_sim_2$SEXO)
table(dados_sim_2$RACACOR)
table(dados_sim_2$TPMORTEOCO)
table(dados_sim_2$OBITOGRAV)
table(dados_sim_2$OBITOPUERP)
table(dados_sim_2$CAUSABAS)
table(dados_sim_2$TPOBITOCOR)
table(dados_sim_2$MORTEPARTO)


# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de "Não informado ou Ignorado" (código 9)
dados_sim_2$TIPOBITO[dados_sim_2$TIPOBITO == 9] <- NA
dados_sim_2$SEXO[dados_sim_2$SEXO == 0 | dados_sim_2$SEXO == 9] <- NA
dados_sim_2$RACACOR[dados_sim_2$RACACOR == 9] <- NA
dados_sim_2$TPMORTEOCO[dados_sim_2$TPMORTEOCO == 9] <- NA
dados_sim_2$OBITOGRAV[dados_sim_2$OBITOGRAV == 9] <- NA
dados_sim_2$OBITOPUERP[dados_sim_2$OBITOPUERP == 9] <- NA
dados_sim_2$TPOBITOCOR[dados_sim_2$TPOBITOCOR == 9] <- NA
dados_sim_2$MORTEPARTO[dados_sim_2$MORTEPARTO == 9] <- NA

# Verificar se existem valores como 9999 para NA em IDADE (No SIM costuma ser 999)
dados_sim_2$IDADE[dados_sim_2$IDADE == 999] <- NA


# Tarefa 6. Atribuir legendas para as categorias (apenas primeira letra maiúscula)
dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2), labels = c("Fetal", "Não fetal"))
dados_sim_2$SEXO = factor(dados_sim_2$SEXO, levels = c(1,2), labels = c("Masculino", "Feminino"))
dados_sim_2$RACACOR = factor(dados_sim_2$RACACOR, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))


# Tarefa 7. Crie um banco de dados, de nome SIM_RN.csv, contendo as 41 variáveis do PDF
# RODE O BLOCO ABAIXO INTEIRO (do SIM_RN até o último parêntese)
SIM_RN <- data.frame(
  ANO = 2015,
  NIVEL = "UF",
  CODMUNRES = 24,
  TO = nrow(dados_sim_2),
  TORC = sum(complete.cases(dados_sim[substr(dados_sim$CODMUNRES, 1, 2) == "24", ])),
  TORCR = sum(complete.cases(dados_sim_2)),
  TO_NN = sum(substr(dados_sim_2$CAUSABAS, 1, 1) %in% c("V", "W", "X", "Y"), na.rm = TRUE),
  TO_N = sum(!substr(dados_sim_2$CAUSABAS, 1, 1) %in% c("V", "W", "X", "Y"), na.rm = TRUE),
  TO_CB_I = sum(substr(dados_sim_2$CAUSABAS, 1, 1) %in% c("A", "B"), na.rm = TRUE),
  TO_CB_N = sum(substr(dados_sim_2$CAUSABAS, 1, 1) %in% c("C", "D"), na.rm = TRUE),
  TO_CB_C = sum(substr(dados_sim_2$CAUSABAS, 1, 1) == "I", na.rm = TRUE),
  TO_CB_R = sum(substr(dados_sim_2$CAUSABAS, 1, 1) == "J", na.rm = TRUE),
  TO_CB_O = sum(!substr(dados_sim_2$CAUSABAS, 1, 1) %in% c("A", "B", "C", "D", "I", "J", "V", "W", "X", "Y"), na.rm = TRUE),
  TO_M = sum(dados_sim_2$SEXO == "Masculino", na.rm = TRUE),
  TO_F = sum(dados_sim_2$SEXO == "Feminino", na.rm = TRUE),
  TO_F_IF = sum(dados_sim_2$SEXO == "Feminino" & as.numeric(substr(dados_sim_2$IDADE, 2, 3)) >= 15 & as.numeric(substr(dados_sim_2$IDADE, 2, 3)) <= 49 & substr(dados_sim_2$IDADE, 1, 1) == "4", na.rm = TRUE),
  TO_FT = sum(dados_sim_2$TIPOBITO == "Fetal", na.rm = TRUE),
  TO_NT = sum(substr(dados_sim_2$IDADE, 1, 1) == "2" & as.numeric(substr(dados_sim_2$IDADE, 2, 3)) <= 27, na.rm = TRUE),
  TO_NT_P = sum(substr(dados_sim_2$IDADE, 1, 1) == "2" & as.numeric(substr(dados_sim_2$IDADE, 2, 3)) <= 6, na.rm = TRUE),
  TO_NT_T = sum(substr(dados_sim_2$IDADE, 1, 1) == "2" & as.numeric(substr(dados_sim_2$IDADE, 2, 3)) >= 7 & as.numeric(substr(dados_sim_2$IDADE, 2, 3)) <= 27, na.rm = TRUE),
  TO_PNT = sum((substr(dados_sim_2$IDADE, 1, 1) == "2" & as.numeric(substr(dados_sim_2$IDADE, 2, 3)) >= 28) | substr(dados_sim_2$IDADE, 1, 1) == "3", na.rm = TRUE),
  TO_MT_G = sum(dados_sim_2$OBITOGRAV == 1 | dados_sim_2$TPMORTEOCO == 1, na.rm = TRUE),
  TONT_B = sum(substr(dados_sim_2$IDADE, 1, 1) == "2" & as.numeric(substr(dados_sim_2$IDADE, 2, 3)) <= 27 & dados_sim_2$RACACOR == "Branca", na.rm = TRUE),
  TONT_PT = sum(substr(dados_sim_2$IDADE, 1, 1) == "2" & as.numeric(substr(dados_sim_2$IDADE, 2, 3)) <= 27 & dados_sim_2$RACACOR == "Preta", na.rm = TRUE),
  TONT_A = sum(substr(dados_sim_2$IDADE, 1, 1) == "2" & as.numeric(substr(dados_sim_2$IDADE, 2, 3)) <= 27 & dados_sim_2$RACACOR == "Amarela", na.rm = TRUE),
  TONT_PD = sum(substr(dados_sim_2$IDADE, 1, 1) == "2" & as.numeric(substr(dados_sim_2$IDADE, 2, 3)) <= 27 & dados_sim_2$RACACOR == "Parda", na.rm = TRUE),
  TONT_I = sum(substr(dados_sim_2$IDADE, 1, 1) == "2" & as.numeric(substr(dados_sim_2$IDADE, 2, 3)) <= 27 & dados_sim_2$RACACOR == "Indígena", na.rm = TRUE),
  TO_MT = sum(dados_sim_2$TPMORTEOCO %in% c(1, 2, 3, 4, 5), na.rm = TRUE),
  TO_MT_DG = sum(dados_sim_2$TPMORTEOCO == 1, na.rm = TRUE),
  TO_MT_PT = sum(dados_sim_2$TPMORTEOCO == 2 | dados_sim_2$MORTEPARTO == 2, na.rm = TRUE),
  TO_MT_AB = sum(dados_sim_2$TPMORTEOCO == 3, na.rm = TRUE),
  TO_MT_42 = sum(dados_sim_2$TPMORTEOCO == 4, na.rm = TRUE),
  TO_MT_43 = sum(dados_sim_2$TPMORTEOCO == 5, na.rm = TRUE),
  TO_MT_P = sum(dados_sim_2$TPMORTEOCO %in% c(1, 2, 3, 4), na.rm = TRUE),
  TO_MT_P_I = sum(dados_sim_2$TPMORTEOCO %in% c(1, 2, 3, 4) & as.numeric(substr(dados_sim_2$IDADE, 2, 3)) >= 15 & as.numeric(substr(dados_sim_2$IDADE, 2, 3)) <= 49, na.rm = TRUE),
  TO_MT_P_ES = sum(dados_sim_2$TPMORTEOCO %in% c(1, 2, 3, 4) & dados_sim_2$ESC2010 == 0, na.rm = TRUE),
  TO_MT_P_EFI = sum(dados_sim_2$TPMORTEOCO %in% c(1, 2, 3, 4) & dados_sim_2$ESC2010 == 1, na.rm = TRUE),
  TO_MT_P_EFII = sum(dados_sim_2$TPMORTEOCO %in% c(1, 2, 3, 4) & dados_sim_2$ESC2010 == 2, na.rm = TRUE),
  TO_MT_P_EM = sum(dados_sim_2$TPMORTEOCO %in% c(1, 2, 3, 4) & dados_sim_2$ESC2010 == 3, na.rm = TRUE),
  TO_MT_P_ESI = sum(dados_sim_2$TPMORTEOCO %in% c(1, 2, 3, 4) & dados_sim_2$ESC2010 == 4, na.rm = TRUE),
  TO_MT_P_ESC = sum(dados_sim_2$TPMORTEOCO %in% c(1, 2, 3, 4) & dados_sim_2$ESC2010 == 5, na.rm = TRUE)
)


# Tarefa 8: Exporte o banco de dados com o nome SIM_RN.csv
write.csv(SIM_RN, "SIM_RN.csv", row.names = FALSE)
