################################################################
# ETAPA 1: BANCO DE DADOS DO SINASC
################################################################

# Tarefa 1. Leitura do banco de dados do SINASC 2015
dados_sinasc = read.csv("SINASC_2015.csv", sep=";", header=TRUE)
head(dados_sinasc)
str(dados_sinasc)

# Tarefa 2. Reduzir dados_sinasc apenas para as colunas selecionadas (dados_sinasc_1)
dados_sinasc_1 = dados_sinasc[, c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61)]
summary(dados_sinasc_1)

# Tarefa 3. Reduzir dados_sinasc_1 para o Rio Grande do Norte (UF 24) -> dados_sinasc_2
UF = substr(as.character(dados_sinasc_1$CODMUNRES), 1, 2)
dados_sinasc_2 = dados_sinasc_1[UF == "24",]

write.csv(dados_sinasc_2, "dados_sinasc_2.csv", row.names = FALSE)
rm(dados_sinasc, dados_sinasc_1)
gc()

# Tarefa 4. Verificar frequências das categorias
table(dados_sinasc_2$LOCNASC)
table(dados_sinasc_2$ESTCIVMAE)
table(dados_sinasc_2$GESTACAO)
table(dados_sinasc_2$GRAVIDEZ)
table(dados_sinasc_2$PARTO)
table(dados_sinasc_2$SEXO)
table(dados_sinasc_2$RACACOR)
table(dados_sinasc_2$IDANOMAL)
table(dados_sinasc_2$ESCMAE2010)
table(dados_sinasc_2$RACACORMAE)
table(dados_sinasc_2$TPAPRESENT)
table(dados_sinasc_2$TPROBSON)
table(dados_sinasc_2$PARIDADE)
table(dados_sinasc_2$KOTELCHUCK)

# Tarefa 5. Atribuir NA para "Não informado ou Ignorado"
dados_sinasc_2$LOCNASC[dados_sinasc_2$LOCNASC == 9] = NA
dados_sinasc_2$IDADEMAE[dados_sinasc_2$IDADEMAE == 99] = NA
dados_sinasc_2$ESTCIVMAE[dados_sinasc_2$ESTCIVMAE == 9] = NA
dados_sinasc_2$GESTACAO[dados_sinasc_2$GESTACAO == 9] = NA
dados_sinasc_2$GRAVIDEZ[dados_sinasc_2$GRAVIDEZ == 9] = NA
dados_sinasc_2$PARTO[dados_sinasc_2$PARTO == 9] = NA
dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == 0] = NA
dados_sinasc_2$APGAR5[dados_sinasc_2$APGAR5 == 99] = NA
dados_sinasc_2$PESO[dados_sinasc_2$PESO == 9999] = NA
dados_sinasc_2$IDANOMAL[dados_sinasc_2$IDANOMAL == 9] = NA
dados_sinasc_2$ESCMAE2010[dados_sinasc_2$ESCMAE2010 == 9] = NA
dados_sinasc_2$CONSPRENAT[dados_sinasc_2$CONSPRENAT == 99] = NA
dados_sinasc_2$TPAPRESENT[dados_sinasc_2$TPAPRESENT == 9] = NA
dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON == 11] = NA
dados_sinasc_2$KOTELCHUCK[dados_sinasc_2$KOTELCHUCK == 9] = NA

# Tarefa 6. Atribuir legendas (Fatores)
dados_sinasc_2$LOCNASC = factor(dados_sinasc_2$LOCNASC, levels = c(1,2,3,4), labels = c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Outros"))
dados_sinasc_2$ESTCIVMAE = factor(dados_sinasc_2$ESTCIVMAE, levels = c(1,2,3,4,5), labels = c("Solteira", "Casada", "Viúva", "Separada judicialmente/divorciada", "União estável"))
dados_sinasc_2$GESTACAO = factor(dados_sinasc_2$GESTACAO, levels = c(1,2,3,4,5,6), labels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "37 a 41 semanas", "42 semanas e mais"))
dados_sinasc_2$GRAVIDEZ = factor(dados_sinasc_2$GRAVIDEZ, levels = c(1,2,3), labels = c("Única", "Dupla", "Tripla ou mais"))
dados_sinasc_2$PARTO = factor(dados_sinasc_2$PARTO, levels = c(1,2), labels = c("Vaginal", "Cesário"))
dados_sinasc_2$SEXO = factor(dados_sinasc_2$SEXO, levels = c(1,2), labels = c("Masculino", "Feminino"))
dados_sinasc_2$RACACOR = factor(dados_sinasc_2$RACACOR, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$IDANOMAL = factor(dados_sinasc_2$IDANOMAL, levels = c(1,2), labels = c("Sim", "Não"))
dados_sinasc_2$ESCMAE2010 = factor(dados_sinasc_2$ESCMAE2010, levels = c(0,1,2,3,4,5), labels = c("Sem escolaridade", "Fundamental I", "Fundamental II", "Médio", "Superior incompleto", "Superior completo"))
dados_sinasc_2$RACACORMAE = factor(dados_sinasc_2$RACACORMAE, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$TPAPRESENT = factor(dados_sinasc_2$TPAPRESENT, levels = c(1,2,3), labels = c("Cefálico", "Pélvica ou podálica", "Transversa"))
dados_sinasc_2$TPROBSON = factor(dados_sinasc_2$TPROBSON, levels = 1:10, labels = paste("Grupo", 1:10))
dados_sinasc_2$PARIDADE = factor(dados_sinasc_2$PARIDADE, levels = c(0,1), labels = c("Nulípara", "Multípara"))
dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = 1:5, labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado"))

# Tarefa 7. Categorizar IDADEMAE, PESO, APGAR5 e criar PERIG e ESTCIV
dados_sinasc_2$F_IDADE = cut(dados_sinasc_2$IDADEMAE, breaks = c(0, 14, 19, 24, 29, 34, 39, 44, 49, Inf), labels = c("<15", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+"), include.lowest = TRUE)
dados_sinasc_2$F_PESO = cut(dados_sinasc_2$PESO, breaks = c(0, 2499, 3999, Inf), labels = c("Baixo peso", "Peso normal", "Macrossomia"))
dados_sinasc_2$F_APGAR5 = cut(dados_sinasc_2$APGAR5, breaks = c(0, 6, Inf), labels = c("Baixo", "Normal"), include.lowest = TRUE)
dados_sinasc_2$PERIG = factor(ifelse(dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES, "Não", "Sim"))
dados_sinasc_2$ESTCIV = factor(ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Solteira", "Viúva", "Separada judicialmente/divorciada"), "Sem companheiro", "Com companheiro"))

# Tarefa 8. Agregar informações de PIG/AIG/GIG
tabela_pig = read.csv("Tabela_PIG_Brasil.csv", sep=",", header=TRUE)
dados_sinasc_2 = merge(dados_sinasc_2, tabela_pig, by=c("SEMAGESTAC", "SEXO"), all.x=TRUE)
dados_sinasc_2$F_PIG = factor(ifelse(dados_sinasc_2$GRAVIDEZ == "Única", ifelse(dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10, "PIG", ifelse(dados_sinasc_2$PESO <= dados_sinasc_2$PESO_P90, "AIG", "GIG")), NA))

# Tarefa 9. Medidas descritivas e frequências (Exemplos de variáveis salvas)
freq_SEXO = table(dados_sinasc_2$SEXO)
media_peso = mean(dados_sinasc_2$PESO, na.rm = TRUE)
p_idade = quantile(dados_sinasc_2$IDADEMAE, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
sd_apgar = sd(dados_sinasc_2$APGAR5, na.rm = TRUE)

# Tarefa 10. Criar o banco de dados final (SINASC_RN.csv) com as 103 colunas
gerar_linha = function(df, nivel, cod) {
  c(2015, nivel, cod, nrow(df), sum(complete.cases(df)), sum(complete.cases(df[,1:22])),
    table(factor(df$F_IDADE, levels=levels(df$F_IDADE))),
    sum(df$IDADEMAE >= 15 & df$IDADEMAE <= 49, na.rm=T),
    quantile(df$IDADEMAE, c(.25, .5, .75), na.rm=T), mean(df$IDADEMAE, na.rm=T), sd(df$IDADEMAE, na.rm=T),
    table(factor(df$ESCMAE2010, levels=levels(df$ESCMAE2010))),
    table(factor(df$RACACORMAE, levels=levels(df$RACACORMAE))),
    sum(df$ESTCIV=="Sem companheiro", na.rm=T), sum(df$ESTCIV=="Com companheiro", na.rm=T),
    sum(df$PARIDADE=="Nulípara", na.rm=T), sum(df$PARIDADE=="Multípara", na.rm=T),
    sum(df$GRAVIDEZ=="Única", na.rm=T), sum(df$GRAVIDEZ %in% c("Dupla", "Tripla ou mais"), na.rm=T),
    table(factor(df$GESTACAO, levels=levels(df$GESTACAO))),
    sum(df$SEMAGESTAC < 37, na.rm=T), sum(df$SEMAGESTAC >= 37 & df$SEMAGESTAC <= 41, na.rm=T), sum(df$SEMAGESTAC >= 42, na.rm=T),
    quantile(df$SEMAGESTAC, c(.25, .5, .75), na.rm=T), mean(df$SEMAGESTAC, na.rm=T), sd(df$SEMAGESTAC, na.rm=T),
    table(factor(df$KOTELCHUCK, levels=levels(df$KOTELCHUCK))),
    sum(df$PERIG=="Sim", na.rm=T), sum(df$PERIG=="Não", na.rm=T), sum(df$PARTO=="Vaginal", na.rm=T), sum(df$PARTO=="Cesário", na.rm=T),
    table(factor(df$TPAPRESENT, levels=levels(df$TPAPRESENT))), table(factor(df$TPROBSON, levels=levels(df$TPROBSON))),
    table(factor(df$LOCNASC, levels=levels(df$LOCNASC))), TNLOC_AI = 0,
    sum(df$SEXO=="Masculino", na.rm=T), sum(df$SEXO=="Feminino", na.rm=T), table(factor(df$RACACOR, levels=levels(df$RACACOR))),
    table(factor(df$F_PESO, levels=levels(df$F_PESO))), quantile(df$PESO, c(.25, .5, .75), na.rm=T), mean(df$PESO, na.rm=T), sd(df$PESO, na.rm=T),
    table(factor(df$F_PIG, levels=levels(df$F_PIG))), sum(df$F_APGAR5=="Baixo", na.rm=T), sum(df$F_APGAR5=="Normal", na.rm=T),
    mean(df$APGAR5, na.rm=T), sd(df$APGAR5, na.rm=T), sum(df$IDANOMAL=="Sim", na.rm=T), sum(df$IDANOMAL=="Não", na.rm=T))
}

# Geração das linhas UF + Municípios
linha_UF = gerar_linha(dados_sinasc_2, "UF", "24")
lista_MUN = split(dados_sinasc_2, dados_sinasc_2$CODMUNRES)
tabela_MUN = t(sapply(names(lista_MUN), function(x) gerar_linha(lista_MUN[[x]], "MUNICIPIO", x)))
SINASC_RN = as.data.frame(rbind(linha_UF, tabela_MUN))

# --- CORREÇÃO AQUI: DANDO NOME AS 103 COLUNAS DO DATAFRAME ---
colnames(SINASC_RN) = c(
  "ANO", "NIVEL", "CODMUNRES", "TN", "TNRC", "TNRCR",
  "TGI_15", "TGI_15_19", "TGI_20_24", "TGI_25_29", "TGI_30_34", "TGI_35_39", "TGI_40_44", "TGI_45_49", "TGI_50",
  "TGIF", "IM_P25", "IM_P50", "IM_P75", "IM_MD", "IM_DP",
  "EM_S", "EM_FI", "EM_FII", "EM_M", "EM_SI", "EM_SC",
  "TGRC_B", "TGRC_PT", "TGRC_A", "TGRC_PD", "TGRC_I",
  "TGSC", "TGCC", "TGPRI", "TGNPRI", "TGU", "TGG",
  "TGD_22", "TGD_22_27", "TGD_28_31", "TGD_32_36", "TGD_37_41", "TGD_42",
  "TGD_PRT", "TGD_AT", "TGD_PST",
  "DG_P25", "DG_P50", "DG_P75", "DG_MD", "DG_DP",
  "TKC_NR", "TKC_ID", "TKC_IT", "TKC_AD", "TKC_MAD",
  "TGPRG_S", "TGPRG_N", "TPV", "TPC",
  "TRAP_C", "TRAP_P", "TRAP_T",
  "TGROB_1", "TGROB_2", "TGROB_3", "TGROB_4", "TGROB_5", "TGROB_6", "TGROB_7", "TGROB_8", "TGROB_9", "TGROB_10",
  "TNLOC_H", "TNLOC_ES", "TNLOC_D", "TNLOC_O", "TNLOC_AI",
  "TRS_M", "TRS_F",
  "TRRC_B", "TRRC_PT", "TRRC_A", "TRRC_PD", "TRRC_I",
  "TRP_BP", "TRP_N", "TRP_M",
  "PESO_P25", "PESO_P50", "PESO_P75", "PESO_MD", "PESO_DP",
  "TRPIG_P", "TRPIG_A", "TRPIG_G",
  "TRAPG5_B", "TRAPG5_N", "APG5_MD", "APG5_DP",
  "TRAC", "TRSAC"
)

# Tarefa 11. Exporte o banco de dados com o nome SINASC_RN.csv
write.csv(SINASC_RN, "SINASC_RN.csv", row.names = FALSE)



##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# Altere o script esqueleto nas partes que se refere a ETAPA 2 e envie para o repositório Extensao tendo feito o commit "Esqueleto atualizado na Etapa 2"
# A partir de main crie a branch SIM e vá para ela
# ESTANDO NA BRANCH SIM, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 1 e só insira comandos na ETAPA 2

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1264175 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim
dados_sim <- read.csv2("Mortalidade_Geral_2015.csv", header = TRUE, sep = ";")
dim(dados_sim)
str(dados_sim)

# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão: 1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO, RACACOR, ESC2010, 
# CODMUNRES, TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO
dados_sim_1 <- dados_sim[, c(1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84)]
names(dados_sim_1) <- c("CONTADOR", "TIPOBITO", "DTOBITO", "DTNASC", "IDADE", "SEXO", "RACACOR", "ESC2010", 
                        "CODMUNRES", "TPMORTEOCO", "OBITOGRAV", "OBITOPUERP", "CAUSABAS", "TPOBITOCOR", "MORTEPARTO")

# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de 
# CODMUNRES), nomeando este novo banco de dados como dados_sim_2
# Códigos das UF: 24: RN
dados_sim_2 <- dados_sim_1[substr(dados_sim_1$CODMUNRES, 1, 2) == "24", ]

# Exportar o arquivo com o nome dados_sim_2.csv
write.csv(dados_sim_2, "dados_sim_2.csv", row.names = FALSE)

# Ao concluir a Tarefa 3 da Etapa 2 commite e envie para o repositório REMOTO o script e dados_sim_2.csv com o comentário "Dados do estado RN e script de sua obtenção"


# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes variáveis: TIPOBITO, SEXO, RACACOR,
# TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO
table(dados_sim_2$TIPOBITO)
table(dados_sim_2$SEXO)
table(dados_sim_2$RACACOR)
table(dados_sim_2$TPMORTEOCO)
table(dados_sim_2$OBITOGRAV)
table(dados_sim_2$OBITOPUERP)
table(dados_sim_2$CAUSABAS)
table(dados_sim_2$TPOBITOCOR)
table(dados_sim_2$MORTEPARTO)


# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de "Não informado ou Ignorado", 
# geralmente com código 9
# veja o dicionário do SIM para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADE verificar se existem valores como 9999 para NA
dados_sim_2$TIPOBITO[dados_sim_2$TIPOBITO == 9] <- NA
dados_sim_2$SEXO[dados_sim_2$SEXO == 0 | dados_sim_2$SEXO == 9] <- NA
dados_sim_2$RACACOR[dados_sim_2$RACACOR == 9] <- NA
dados_sim_2$IDADE[dados_sim_2$IDADE == 999] <- NA
dados_sim_2$TPMORTEOCO[dados_sim_2$TPMORTEOCO == 9] <- NA
dados_sim_2$OBITOGRAV[dados_sim_2$OBITOGRAV == 9] <- NA
dados_sim_2$OBITOPUERP[dados_sim_2$OBITOPUERP == 9] <- NA
dados_sim_2$TPOBITOCOR[dados_sim_2$TPOBITOCOR == 9] <- NA
dados_sim_2$MORTEPARTO[dados_sim_2$MORTEPARTO == 9] <- NA


# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2), labels = c("Fetal", "Não fetal")
# ATENÇÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. 
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados
dados_sim_2$TIPOBITO <- factor(dados_sim_2$TIPOBITO, levels = c(1,2), labels = c("Fetal", "Não fetal"))
dados_sim_2$SEXO <- factor(dados_sim_2$SEXO, levels = c(1,2), labels = c("Masculino", "Feminino"))
dados_sim_2$RACACOR <- factor(dados_sim_2$RACACOR, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))


# Tarefa 7. Crie um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv), contendo as 41 variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 da Etapa 2.pdf”
# Atenção:
# 1. Para informações gerais utilize CAUSABAS, SEXO e IDADE
# 2. Para informações fetais utilize TIPOBITO
# 3. Para informações neonatais utilize TIPOBITO não fetal e IDADE entre 0 e 27 dias e RACACOR
# 4. Para informações maternas utilize TPMORTEOCO, ESC e IDADE

library(dplyr)

SIM_RN <- dados_sim_2 %>%
  group_by(CODMUNRES) %>%
  summarise(
    ANO = 2015, 
    NIVEL = "MU",
    TO = n(),
    TORC = n(),
    TORCR = sum(complete.cases(pick(everything()))),
    TO_NN = sum(substr(CAUSABAS, 1, 1) %in% c("V", "W", "X", "Y"), na.rm = TRUE),
    TO_N = sum(!substr(CAUSABAS, 1, 1) %in% c("V", "W", "X", "Y"), na.rm = TRUE),
    TO_CB_I = sum(substr(CAUSABAS, 1, 1) %in% c("A", "B"), na.rm = TRUE),
    TO_CB_N = sum(substr(CAUSABAS, 1, 1) %in% c("C", "D"), na.rm = TRUE),
    TO_CB_C = sum(substr(CAUSABAS, 1, 1) == "I", na.rm = TRUE),
    TO_CB_R = sum(substr(CAUSABAS, 1, 1) == "J", na.rm = TRUE),
    TO_CB_O = sum(!substr(CAUSABAS, 1, 1) %in% c("A","B","C","D","I","J","V","W","X","Y"), na.rm = TRUE),
    TO_M = sum(SEXO == "Masculino", na.rm = TRUE),
    TO_F = sum(SEXO == "Feminino", na.rm = TRUE),
    TO_F_IF = sum(SEXO == "Feminino" & as.numeric(substr(IDADE, 2, 3)) >= 15 & as.numeric(substr(IDADE, 2, 3)) <= 49 & substr(IDADE, 1, 1) == "4", na.rm = TRUE),
    TO_FT = sum(TIPOBITO == "Fetal", na.rm = TRUE),
    TO_NT = sum(substr(IDADE, 1, 1) == "2" & as.numeric(substr(IDADE, 2, 3)) <= 27, na.rm = TRUE),
    TO_NT_P = sum(substr(IDADE, 1, 1) == "2" & as.numeric(substr(IDADE, 2, 3)) <= 6, na.rm = TRUE),
    TO_NT_T = sum(substr(IDADE, 1, 1) == "2" & as.numeric(substr(IDADE, 2, 3)) >= 7 & as.numeric(substr(IDADE, 2, 3)) <= 27, na.rm = TRUE),
    TO_PNT = sum((substr(IDADE, 1, 1) == "2" & as.numeric(substr(IDADE, 2, 3)) >= 28) | substr(IDADE, 1, 1) == "3", na.rm = TRUE),
    TO_MT_G = sum(OBITOGRAV == 1 | TPMORTEOCO == 1, na.rm = TRUE),
    TONT_B = sum(substr(IDADE, 1, 1) == "2" & as.numeric(substr(IDADE, 2, 3)) <= 27 & RACACOR == "Branca", na.rm = TRUE),
    TONT_PT = sum(substr(IDADE, 1, 1) == "2" & as.numeric(substr(IDADE, 2, 3)) <= 27 & RACACOR == "Preta", na.rm = TRUE),
    TONT_A = sum(substr(IDADE, 1, 1) == "2" & as.numeric(substr(IDADE, 2, 3)) <= 27 & RACACOR == "Amarela", na.rm = TRUE),
    TONT_PD = sum(substr(IDADE, 1, 1) == "2" & as.numeric(substr(IDADE, 2, 3)) <= 27 & RACACOR == "Parda", na.rm = TRUE),
    TONT_I = sum(substr(IDADE, 1, 1) == "2" & as.numeric(substr(IDADE, 2, 3)) <= 27 & RACACOR == "Indígena", na.rm = TRUE),
    TO_MT = sum(TPMORTEOCO %in% c(1, 2, 3, 4, 5), na.rm = TRUE),
    TO_MT_DG = sum(TPMORTEOCO == 1, na.rm = TRUE),
    TO_MT_PT = sum(TPMORTEOCO == 2 | MORTEPARTO == 2, na.rm = TRUE),
    TO_MT_AB = sum(TPMORTEOCO == 3, na.rm = TRUE),
    TO_MT_42 = sum(TPMORTEOCO == 4, na.rm = TRUE),
    TO_MT_43 = sum(TPMORTEOCO == 5, na.rm = TRUE),
    TO_MT_P = sum(TPMORTEOCO %in% c(1, 2, 3, 4), na.rm = TRUE),
    TO_MT_P_I = sum(TPMORTEOCO %in% c(1, 2, 3, 4) & as.numeric(substr(IDADE, 2, 3)) >= 15 & as.numeric(substr(IDADE, 2, 3)) <= 49, na.rm = TRUE),
    TO_MT_P_ES = sum(TPMORTEOCO %in% c(1, 2, 3, 4) & ESC2010 == 0, na.rm = TRUE),
    TO_MT_P_EFI = sum(TPMORTEOCO %in% c(1, 2, 3, 4) & ESC2010 == 1, na.rm = TRUE),
    TO_MT_P_EFII = sum(TPMORTEOCO %in% c(1, 2, 3, 4) & ESC2010 == 2, na.rm = TRUE),
    TO_MT_P_EM = sum(TPMORTEOCO %in% c(1, 2, 3, 4) & ESC2010 == 3, na.rm = TRUE),
    TO_MT_P_ESI = sum(TPMORTEOCO %in% c(1, 2, 3, 4) & ESC2010 == 4, na.rm = TRUE),
    TO_MT_P_ESC = sum(TPMORTEOCO %in% c(1, 2, 3, 4) & ESC2010 == 5, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(ANO, NIVEL, CODMUNRES, everything())


# Tarefa 8: Exporte o banco de dados com o nome SIM_UF.csv
write.csv(SIM_RN, "SIM_RN.csv", row.names = FALSE)

# Ao terminar a ETAPA 2 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 2"
# Faça um merge de script de SIM para main



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



################################################################
# ETAPA 4: GERAR BANCO DE DADOS FINAL DO ESTADO COM DADOS DO SIDRA, ATLAS, SINASC, SIM, SINISA E INDICADORES
################################################################

library(dplyr)

# Carregando os bancos de dados criados nas etapas anteriores (ajustados para o RN)
SIDRA_UF  <- read.csv("SIDRA_RN.csv", colClasses = c("CODMUNRES" = "character"))
ATLAS_UF  <- read.csv("ATLAS_RN.csv", colClasses = c("CODMUNRES" = "character"))
SINASC_UF <- read.csv("SINASC_RN.csv", colClasses = c("CODMUNRES" = "character"))
SIM_UF    <- read.csv("SIM_RN.csv", colClasses = c("CODMUNRES" = "character"))
SINISA_UF <- read.csv("SINISA_RN.csv", colClasses = c("CODMUNRES" = "character"))

# Tarefa 1: Fazer o merge dos bancos de dados criados nas etapas anteriores (SIDRA_UF, ATLAS_ UF,  SINASC_UF, SIM_UF e SINISA_UF), 
# sendo que as variáveis deverão seguir a ordem

# ANO, NIVEL, CODMUNRES (uma única vez), variáveis do SIDRA, do ATLAS, do SINASC, do SIM e da SINISA. No merge deve constar qualquer município que esteja em pelo menos um dos bancos
# Chamar o banco de dados de DA_UF

DA_UF <- SIDRA_UF %>%
  full_join(ATLAS_UF,  by = c("ANO", "NIVEL", "CODMUNRES")) %>%
  full_join(SINASC_UF, by = c("ANO", "NIVEL", "CODMUNRES")) %>%
  full_join(SIM_UF,    by = c("ANO", "NIVEL", "CODMUNRES")) %>%
  full_join(SINISA_UF, by = c("ANO", "NIVEL", "CODMUNRES"))

# Após o merge dos bancos, fazer commit “Script e dados agregados da UF”


# Tarefa 2: Acrescentar no banco DA_UF os indicadores TFG, TMG, RMM, TMM, TMM_P, TMN, TMN_P, TMN_T e TMI e chamar o banco de BDEM_UF_2015

BDEM_RN_2015 <- DA_UF %>%
  mutate(
    # Indicadores Demográficos e de Mortalidade Geral/Materna
    TFG   = (TN / POPRC_F_15_49) * 1000,
    TMG   = (TO / POPRE_T) * 1000,
    RMM   = (TO_MT / TN) * 100000,
    TMM   = (TO_MT / POPRC_F_15_49) * 100000,
    TMM_P = (TO_MT_P / POPRC_F_15_49) * 100000,
    
    # Indicadores Neonatais (Precoce, Tardia e Total) e Infantil
    TMN   = (TO_NT / TN) * 1000,
    TMN_P = (TO_NT_P / TN) * 1000,
    TMN_T = (TO_NT_T / TN) * 1000,
    TMI   = ((TO_NT + TO_PNT) / TN) * 1000
  )

# Após a criação do banco, fazer commit “Script e dados BDEM_UF_2015”

# Exporte o arquivo em formato CSV
write.csv(BDEM_RN_2015, "BDEM_RN_2015.csv", row.names = FALSE)

# Faça o commit com a mensagem "Script e dados TAREFA 3 - ATLAS"


