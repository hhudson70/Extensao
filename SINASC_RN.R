####################################
# ETAPA 1: BANCO DE DADOS DO SINASC
####################################

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
  # Cálculo dos indicadores na ordem exata do PDF
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

# Tarefa 11. Exporte o banco de dados com o nome SINASC_RN.csv
write.csv(SINASC_RN, "SINASC_RN.csv", row.names = FALSE)






