
setwd("/home/hyuri/Dropbox/Anna")

save.image(file="dmAlunos_last.RData")
#system.time


library(sqldf)
library(datasets)
library(dplyr)
library(lubridate)
library(ggplot2)

#carrega os dados do csv selecionado apenas as variáveis que seram usadas, com isso reduz o tamanho do data frame a ser carregado

  #2012
dados_alunos_2012 <- read.csv.sql(file="/home/hyuri/Dropbox/Anna/microdados_censo_superior_2012/2012/DADOS/DM_ALUNO.CSV",
                                  sql = "select  CO_CURSO,NO_CURSO, CO_GRAU_ACADEMICO, CO_COR_RACA_ALUNO, DS_COR_RACA_ALUNO, IN_SEXO_ALUNO, DS_SEXO_ALUNO,IN_FINANC_ESTUDANTIL
                                  from file",
                                  header = TRUE, sep ="|")


#2016
#dados_alunos_2016 <- read.csv.sql(file="/home/hyuri/Dropbox/Anna/microdados_censo_superior_2016/DADOS/DM_ALUNO.CSV",
#                                  sql = "select  CO_CURSO,NO_CURSO, CO_COR_RACA_ALUNO, DS_COR_RACA_ALUNO, IN_SEXO_ALUNO, DS_SEXO_ALUNO
#                                  from file",
#                                 header = TRUE, sep ="|")


#2017
dados_alunos_2017 <- read.csv.sql(file="/home/hyuri/Dropbox/Anna/microdados_educacao_superior_2017/Microdados_Educacao_Superior_2017/DADOS/DM_ALUNO.CSV",
                                  sql = "select  CO_CURSO, TP_COR_RACA, TP_SEXO, TP_GRAU_ACADEMICO
                                  from file",header = TRUE, sep ="|")

# Cod. Curso 2017

cod_curso= read.csv.sql("/home/hyuri/Dropbox/Anna/microdados_educacao_superior_2017/Microdados_Educacao_Superior_2017/DADOS/DM_CURSO.CSV", 
                        sql = "select CO_CURSO, NO_CURSO from file" ,header = TRUE, sep ="|")


CO_CURSO <- cod_curso$CO_CURSO

NO_CURSO <- cod_curso$NO_CURSO

#NO_CURSO <- iconv(NO_CURSO, "ASCII","latin1", "")
iconv(NO_CURSO, "latin1", "ASCII//TRANSLIT")

NO_CURSO <- iconv(NO_CURSO, "latin1", "ASCII//TRANSLIT")

NO_CURSO <- as.data.frame(NO_CURSO)

CO_CURSO <- as.data.frame(CO_CURSO)



COD_CURSOS_2017<-bind_cols(CO_CURSO,NO_CURSO)

dados_2017<-left_join(COD_CURSOS_2017, dados_alunos_2017)



NO_CURSO_2012 <- dados_alunos_2012$NO_CURSO

iconv(NO_CURSO_2012, "latin1", "ASCII//TRANSLIT")

NO_CURSO_2012 <- iconv(NO_CURSO_2012, "latin1", "ASCII//TRANSLIT")


NO_CURSO_2012 <- as.data.frame(NO_CURSO_2012)

dados_2012<-bind_cols(NO_CURSO_2012,dados_alunos_2012)


NOME_CURSO <- cod_curso$NO_CURSO

nome<-iconv(NOME_CURSO, "latin1", "ASCII//TRANSLIT")

nome <- as.data.frame(nome)

write.table(nome, "/home/hyuri/Dropbox/Anna/nomeCursos.txt", sep = ";")

write.csv(nome, "/home/hyuri/Dropbox/Anna/nomeCursos.csv")

summary(dados_2012)
#NO_CURSO_2012        CO_CURSO         NO_CURSO         CO_GRAU_ACADEMICO CO_COR_RACA_ALUNO DS_COR_RACA_ALUNO  IN_SEXO_ALUNO    DS_SEXO_ALUNO     
#ADMINISTRACAO     :1177483   Min.   :      1   Length:9565483     Min.   :0.000     Min.   :0.000     Length:9565483     Min.   :0.0000   Length:9565483    
#DIREITO           : 957607   1st Qu.:  21666   Class :character   1st Qu.:1.000     1st Qu.:0.000     Class :character   1st Qu.:0.0000   Class :character  
#PEDAGOGIA         : 769920   Median :  78182   Mode  :character   Median :1.000     Median :1.000     Mode  :character   Median :1.0000   Mode  :character  
#CIENCIAS CONTABEIS: 419268   Mean   : 221083                      Mean   :1.479     Mean   :2.755                        Mean   :0.5585                     
#ENFERMAGEM        : 305420   3rd Qu.: 106739                      3rd Qu.:2.000     3rd Qu.:6.000                        3rd Qu.:1.0000                     
#EDUCACAO FISICA   : 256317   Max.   :5001100                      Max.   :3.000     Max.   :6.000                        Max.   :1.0000                     
#(Other)           :5679468                                                                                                                                  
#IN_FINANC_ESTUDANTIL
#Min.   :0.0000      
#1st Qu.:0.0000      
#Median :0.0000      
#Mean   :0.2118      
#3rd Qu.:0.0000      
#Max.   :1.0000 


summary(dados_2017)

# CO_CURSO         NO_CURSO          TP_COR_RACA       TP_SEXO      TP_GRAU_ACADEMICO
# Min.   :      1   Length:11589194    Min.   :0.000   Min.   :1.000   Min.   :0.00     
# 1st Qu.:  48540   Class :character   1st Qu.:0.000   1st Qu.:1.000   1st Qu.:1.00     
# Median :  98445   Mode  :character   Median :1.000   Median :1.000   Median :1.00     
# Mean   : 497113                      Mean   :1.459   Mean   :1.443   Mean   :1.46     
# 3rd Qu.:1125999                      3rd Qu.:3.000   3rd Qu.:2.000   3rd Qu.:2.00     
# Max.   :5001415                      Max.   :9.000   Max.   :2.000   Max.   :3.00

str(dados_2017$NO_CURSO)


#salva o novo data frame
#write.csv(dados_alunos_2012, "alunos_2012.csv", row.names = FALSE)

#write.csv(dados_alunos_2017, "alunos_2017.csv", row.names = FALSE)

########### ANALISE 2012

## SEXO

sexo_aluno<- table(dados_alunos_2012$IN_SEXO_ALUNO)
sexo_aluno_DS<-table(dados_alunos_2012$DS_SEXO_ALUNO)

sum(dados_alunos_2012$IN_SEXO_ALUNO == "1")
#[1] 5342343
sum(dados_alunos_2012$IN_SEXO_ALUNO == "0")
#[1] 4223140

# Sexo 
# 0. Masculino
# 1. Feminino


prop.table(sexo_aluno_DS)
#   Feminino   Masculino 
# 0.5585022    0.4414978 

# Moda
names(sexo_aluno)[which.max(sexo_aluno)]
# 1

# quartis
quantile(sexo_aluno, na.rm = TRUE)
#   0%     25%     50%     75%    100% 
# 4223140 4502941 4782742 5062542 5342343

barplot(sexo_aluno, main = "Distribuição de homens e Mulheres no Ensino Superiro - 2012", col = c("red", "green"))
legend("topleft", c("Homem", "Mulher"), fill = c("red", "green"))


pie(sexo_aluno_DS, labels = sexo_aluno_DS, main = "Distribuição Homens e Mulheres -2012")

pct <- round(sexo_aluno/sum(sexo_aluno)*100)
nome <- c("Homem", "Mulher")
nome <- paste(nome,pct)
nome <- paste(nome, "%", sep = "")

pie(sexo_aluno, labels = nome, col = rainbow(length(nome)), main = "Porcentagem da Distribuição de Homens e Mulheres 2012")


pie(sexo_aluno, labels = nome, main = "porcentagem de homens e mulheres no ensino superior",col = rainbow(length(nome)))
legend("topright", c("Homem","Mulher"), cex = 0.8,
       fill = rainbow(length(x)))


# COR-RAÇA - 2012

cor_raca <- table(dados_alunos_2012$CO_COR_RACA_ALUNO)

prop.table(cor_raca)

#     0           1           2           3           4           5           6 
# 0.287494317 0.219814514 0.025325015 0.100395662 0.008364031 0.001417597 0.357188863 

# 1. Branca
# 2. Preta
# 3. Parda
# 4. Amarela
# 5. Indígena
# 6. Não dispõe da informação
# 0. Não declarado


cores <- c(1:7)  
barplot(cor_raca, main ="Distribuição de cor-raca - 2012", col = cores )
legend("top", c("Não declarado","Branca","Preta","Parda","Amarela","Indígena","Não dispõe da informação" ), fill = cores)

pct2012 <- round(cor_raca_Mulher/sum(cor_raca)*100)
nomeCR2012 <- c("Nao Declarado", "Branco","Preto", "Pardo","Amarela", "Indígina", "Não dispõe da informação", "Não declarado")
nomeCR2012 <- paste(nomeCR2012,pct2012)
nomeCR2012 <- paste(nomeCR2012, "%", sep = "")

pie(cor_raca, labels = nomeCR2012, main = "porcentagem do cor-raca Mulhres- 2012",col = rainbow(length(nomeCR2012)))


# grau

grau_academico <- table(dados_alunos_2012$CO_GRAU_ACADEMICO)  

prop.table(grau_academico)
#     0           1           2           3 
# 0.006769444 0.653984958 0.192417466 0.146828132

# 1. Bacharelado
# 2. Licenciatura
# 3. Tecnológico
# 0. Não aplicável 

coresG <-(1:4)
barplot(grau_academico, main ="Distribuição do Grau academico - 2012", col = cores )
legend("topleft", c("Não aplicavel","Bacharelado","Licenciatura","Tecnológico" ), fill = cores)

pie(grau_academico, labels = grau_academico, main = "Distribuição Grau Academico -2012")

pctG <- round(grau_academico/sum(grau_academico)*100)
nomeG <- c("Nao Aplicado", "Bacharelado","Licenciatura", "Tecnológico")
nomeG <- paste(nomeG,pctG)
nomeG <- paste(nomeG, "%", sep = "")

pie(grau_academico, labels = nomeG, main = "porcentagem do Grau Academico - 2012",col = rainbow(length(nomeG)))
legend("topright",  c("Nao Aplicado", "Bacharelado","Licenciatura", "Tecnológico"), cex = 0.8,
       fill = rainbow(length(x)))

########## MULHRES 2012

alunos_mulheres <- subset(dados_alunos_2012, dados_alunos_2012$IN_SEXO_ALUNO == "1")


# COR-RAÇA MULHER - 2012

cor_raca_Mulher <- table(alunos_mulheres$CO_COR_RACA_ALUNO)

prop.table(cor_raca_Mulher)

#       0           1           2           3           4           5           6 
#  0.284007597 0.219609074 0.023595078 0.102365760 0.008616444 0.001299991 0.360506055 

# 1. Branca
# 2. Preta
# 3. Parda
# 4. Amarela
# 5. Indígena
# 6. Não dispõe da informação
# 0. Não declarado

# Moda
names(cor_raca_Mulher)[which.max(cor_raca_Mulher)]
# 6

cores <- c(1:7) 
barplot(cor_raca_Mulher, main ="Distribuição de cor-raca Mulheres - 2012", col = cores )
legend("top", c("Não declarado","Branca","Preta","Parda","Amarela","Indígena","Não dispõe da informação" ), fill = cores)

pctRM <- round(cor_raca_Mulher/sum(cor_raca_Mulher)*100)
nomeRM <- c("Nao Declarado", "Branco","Preto", "Pardo","Amarela", "Indígina", "Não dispõe da informação", "Não declarado")
nomeRM <- paste(nomeRM,pctRM)
nomeRM <- paste(nomeRM, "%", sep = "")

pie(cor_raca_Mulher, labels = nomeRM, main = "porcentagem do cor-raca Mulhres- 2012",col = rainbow(length(nomeRM)))

# grau academico

grau_academico_Mulheres <- table(alunos_mulheres$CO_GRAU_ACADEMICO)  

prop.table(grau_academico_Mulheres)
#      0           1           2           3 
# 0.006747414 0.625883812 0.244163843 0.123204931 

# 1. Bacharelado
# 2. Licenciatura
# 3. Tecnológico
# 0. Não aplicável 

coresGM <-(1:4)
barplot(grau_academico_Mulheres, main ="Distribuição Grau academico-Mulhres- 2012", col = cores )
legend("topleft", c("Não aplicavel","Bacharelado","Licenciatura","Tecnológico" ), fill = coresGM)

pie(grau_academico_Mulheres, labels = grau_academico_Mulheres, main = "Distribuição Grau Academico -  Mulhres -2012")

pctGM <- round(grau_academico_Mulheres/sum(grau_academico_Mulheres)*100)
nomeGM <- c("Nao Aplicado", "Bacharelado","Licenciatura", "Tecnológico")
nomeGM <- paste(nomeGM,pctGM)
nomeGM <- paste(nomeGM, "%", sep = "")

pie(grau_academico_Mulheres, labels = nomeGM, main = "porcentagem do Grau Academico - Mulhres- 2012",col = rainbow(length(nomeGM)))
legend("topright",  c("Nao Aplicado", "Bacharelado","Licenciatura", "Tecnológico"), cex = 0.8,
       fill = rainbow(length(x)))


# ######   Homens 2012



alunos_homens <- subset(dados_alunos_2012, dados_alunos_2012$IN_SEXO_ALUNO == "0")

# COR-RAÇA Homens - 2012

cor_raca_homens <- table(alunos_homens$CO_COR_RACA_ALUNO)

prop.table(cor_raca_homens)

#       0           1           2           3           4           5           6 
#  0.291905075 0.220074400 0.027513414 0.097903456 0.008044725 0.001566370 0.352992560 

# 1. Branca
# 2. Preta
# 3. Parda
# 4. Amarela
# 5. Indígena
# 6. Não dispõe da informação
# 0. Não declarado

# Moda
names(cor_raca_homens)[which.max(cor_raca_homens)]
# 6


barplot(cor_raca_homens, main ="Distribuição de cor-raca-Homens - 2012", col = cores )
legend("top", c("Não declarado","Branca","Preta","Parda","Amarela","Indígena","Não dispõe da informação" ), fill = cores)

pie(cor_raca_homens, labels = cor_raca_homens, main = "Distribuição Cor-Raça Mulheres -2012")

pctRH <- round(cor_raca_homens/sum(cor_raca_homens)*100)
nomeRH <- c("Nao Declarado", "Branco","Preto", "Pardo","Amarela", "Indígina", "Não dispõe da informação", "Não declarado")
nomeRH <- paste(nomeRH,pctRH)
nomeRH <- paste(nomeRH, "%", sep = "")

pie(cor_raca_homens, labels = nomeRH, main = "porcentagem do cor-raca homens- 2012",col = rainbow(length(nomeRH)))


# grau academico

grau_academico_homens <- table(alunos_homens$CO_GRAU_ACADEMICO)  

prop.table(grau_academico_homens)
#       0           1           2           3 
#  0.006797312 0.689533380 0.126957430 0.176711878 

# 1. Bacharelado
# 2. Licenciatura
# 3. Tecnológico
# 0. Não aplicável 

barplot(grau_academico_homens, main ="Distribuição Grau academico-Homens- 2012", col = cores )
legend("topleft", c("Não aplicavel","Bacharelado","Licenciatura","Tecnológico" ), fill = coresGM)

pie(grau_academico_homens, labels = grau_academico_homens, main = "Distribuição Grau Academico -  homens -2012")

pctGH <- round(grau_academico_homens/sum(grau_academico_homens)*100)
nomeGH <- c("Nao Aplicado", "Bacharelado","Licenciatura", "Tecnológico")
nomeGH <- paste(nomeGH,pctGH)
nomeGH <- paste(nomeGH, "%", sep = "")

pie(grau_academico_homens, labels = nomeGH, main = "porcentagem Grau Academico Homens 2012",col = rainbow(length(nomeGH)))



####################### DADOS 2017 ################################################

#  CO_CURSO, TP_COR_RACA, TP_SEXO, TP_GRAU_ACADEMICO

## SEXO

sexo_aluno_2017<- table(dados_alunos_2017$TP_SEXO)


sum(dados_alunos_2017$TP_SEXO == "1")
#[1] 6449750
sum(dados_alunos_2017$TP_SEXO == "2")
#[1] 5139444

# Sexo 
# 2. Masculino
# 1. Feminino


prop.table(sexo_aluno_2017)
#   Feminino   Masculino 
#       1         2 
#   0.5565314 0.4434686 

# Moda
names(sexo_aluno_2017)[which.max(sexo_aluno_2017)]
# 1

# quartis
quantile(sexo_aluno_2017, na.rm = TRUE)
#     0%     25%     50%     75%    100% 
#   5139444 5467020 5794597 6122174 6449750 

barplot(sexo_aluno_2017, main = "Distribuição de homens e Mulheres no Ensino Superiro - 2017", col = c("red", "green"))
legend("topright", c("Mulher", "Homen"), fill = c("red", "green"))


pie(sexo_aluno_2017, labels = sexo_aluno_2017, main = "Distribuição Homens e Mulheres -2017")

pct2017 <- round(sexo_aluno_2017/sum(sexo_aluno_2017)*100)
nome2017 <- c("Mulher", "Homem")
nome2017 <- paste(nome2017,pct2017)
nome2017 <- paste(nome2017, "%", sep = "")

pie(sexo_aluno_2017, labels = nome2017, col = rainbow(length(nome2017)), main = "Porcentagem da Distribuição de Homens e Mulheres 2017")



# COR-RAÇA - 2017

cor_raca_2017 <- table(dados_alunos_2017$TP_COR_RACA)

prop.table(cor_raca_2017)

#          0           1           2           3           4           5           9 
#     0.261582988 0.388149944 0.063391811 0.254859743 0.016036232 0.007175219 0.008804064 

# 1. Branca
# 2. Preta
# 3. Parda
# 4. Amarela
# 5. Indígena
# 9. Não dispõe da informação
# 0. Não declarado


barplot(cor_raca_2017, main ="Distribuição de cor-raca - 2017", col = cores )
legend("topright", c("Não declarado","Branca","Preta","Parda","Amarela","Indígena","Não dispõe da informação" ), fill = cores)

pct2017 <- round(cor_raca_2017/sum(cor_raca_2017)*100)
nomeCR2017 <- c("Nao Declarado", "Branco","Preto", "Pardo","Amarela", "Indígina", "Não dispõe da informação", "Não declarado")
nomeCR2017 <- paste(nomeCR2017,pct2017)
nomeCR2017 <- paste(nomeCR2017, "%", sep = "")

pie(cor_raca_2017, labels = nomeCR2017, main = "porcentagem do COR-RAÇA - 2017",col = rainbow(length(nomeCR2017)))


# GRAU ACADEMICO

grau_academico_2017 <- table(dados_alunos_2017$TP_GRAU_ACADEMICO)  

prop.table(grau_academico_2017)
#           0           1           2           3 
#     0.005663811 0.666781314 0.189807850 0.137747025 

# 1. Bacharelado
# 2. Licenciatura
# 3. Tecnológico
# 0. Não aplicável 


barplot(grau_academico_2017, main ="Distribuição do Grau academico - 2017", col = cores)
legend("topright", c("Não aplicavel","Bacharelado","Licenciatura","Tecnológico" ), fill = cores)

pie(grau_academico_2017, labels = grau_academico_2017, main = "Distribuição Grau Acadêmico -2017")

pctG2017 <- round(grau_academico_2017/sum(grau_academico_2017)*100)
nomeG2017 <- c("Nao Aplicado", "Bacharelado","Licenciatura", "Tecnológico")
nomeG2017 <- paste(nomeG2017,pctG2017)
nomeG2017 <- paste(nomeG2017, "%", sep = "")

pie(grau_academico_2017, labels = nomeG2017, main = "porcentagem Grau Academico 2017",col = rainbow(length(nomeG2017)))


########## MULHRES 2017

alunos_mulheres_2017 <- subset(dados_alunos_2017, dados_alunos_2017$TP_SEXO== "1")


# COR-RAÇA MULHER - 2017

cor_raca_Mulher_2017 <- table(alunos_mulheres_2017$TP_COR_RACA)

prop.table(cor_raca_Mulher_2017)

#             0           1           2           3           4           5           9 
#        0.267597039 0.382752820 0.060145897 0.257142835 0.016823753 0.007027559 0.008510097 

# 1. Branca
# 2. Preta
# 3. Parda
# 4. Amarela
# 5. Indígena
# 9. Não dispõe da informação
# 0. Não declarado

# Moda
names(cor_raca_Mulher_2017)[which.max(cor_raca_Mulher_2017)]
# 1

barplot(cor_raca_Mulher_2017, main ="Distribuição de cor-raça Mulheres - 2017", col = cores )
legend("topright", c("Não declarado","Branca","Preta","Parda","Amarela","Indígena","Não dispõe da informação" ), fill = cores)

pctRM2017 <- round(cor_raca_Mulher_2017/sum(cor_raca_Mulher_2017)*100)
nomeRM2017 <- c("Nao Declarado", "Branco","Preto", "Pardo","Amarela", "Indígina", "Não dispõe da informação", "Não declarado")
nomeRM2017 <- paste(nomeRM2017,pctRM2017)
nomeRM2017 <- paste(nomeRM2017, "%", sep = "")

pie(cor_raca_Mulher_2017, labels = nomeRM2017, main = "porcentagem do COR-RAÇA - Mulhres- 2017",col = rainbow(length(nomeRM2017)))

# grau academico

grau_academico_Mulheres_2017 <- table(alunos_mulheres_2017$TP_GRAU_ACADEMICO)  

prop.table(grau_academico_Mulheres_2017)
#             0          1          2          3 
#        0.00504035 0.64492686 0.23538029 0.11465251  

# 1. Bacharelado
# 2. Licenciatura
# 3. Tecnológico
# 0. Não aplicável 

barplot(grau_academico_Mulheres_2017, main ="Distribuição Grau academico Mulhres- 2017", col = cores )
legend("topright", c("Não aplicavel","Bacharelado","Licenciatura","Tecnológico" ), fill = coresGM)

pie(grau_academico_Mulheres_2017, labels = grau_academico_Mulheres_2017, main = "Distribuição Grau Academico -  Mulhres -2017")

pctGM2017 <- round(grau_academico_Mulheres_2017/sum(grau_academico_Mulheres_2017)*100)
nomeGM2017 <- c("Nao Aplicado", "Bacharelado","Licenciatura", "Tecnológico")
nomeGM2017 <- paste(nomeGM2017,pctGM2017)
nomeGM2017 <- paste(nomeGM2017, "%", sep = "")

pie(grau_academico_Mulheres_2017, labels = nomeGM2017, main = "porcentagem do Grau Academico Mulhres- 2017",col = rainbow(length(nomeGM2017)))

# Homens 2017

alunos_homens_2017 <- subset(dados_alunos_2017, dados_alunos_2017$TP_SEXO == "2")

# COR-RAÇA Homens - 2012

cor_raca_homens_2017 <- table(alunos_homens_2017$TP_COR_RACA)

prop.table(cor_raca_homens_2017)

#             0           1           2           3           4           5           9 
#        0.254035651 0.394923069 0.067465274 0.251994574 0.015047931 0.007360524 0.009172977 

# 1. Branca
# 2. Preta
# 3. Parda
# 4. Amarela
# 5. Indígena
# 9. Não dispõe da informação
# 0. Não declarado

# Moda
names(cor_raca_homens_2017)[which.max(cor_raca_homens_2017)]
# 1


barplot(cor_raca_homens_2017, main ="Distribuição de cor-raca Homens - 2017", col = cores )
legend("topright", c("Não declarado","Branca","Preta","Parda","Amarela","Indígena","Não dispõe da informação" ), fill = cores)

pie(cor_raca_homens_2017, labels = cor_raca_homens_2017, main = "Distribuição Cor-Raça Mulheres -2012")

pctRH2017 <- round(cor_raca_homens_2017/sum(cor_raca_homens_2017)*100)
nomeRH2017 <- c("Nao Declarado", "Branco","Preto", "Pardo","Amarela", "Indígina", "Não dispõe da informação", "Não declarado")
nomeRH2017 <- paste(nomeRH2017,pctRH2017)
nomeRH2017 <- paste(nomeRH2017, "%", sep = "")

pie(cor_raca_homens_2017, labels = nomeRH2017, main = "porcentagem cor-raca Homens- 2017",col = rainbow(length(nomeRH2017)))


# grau academico

grau_academico_homens_2017 <- table(alunos_homens_2017$TP_GRAU_ACADEMICO)  

prop.table(grau_academico_homens_2017)
#             0           1           2           3 
#      0.006446223 0.694207584 0.132616680 0.166729514 

# 1. Bacharelado
# 2. Licenciatura
# 3. Tecnológico
# 0. Não aplicável 

barplot(grau_academico_homens_2017, main ="Distribuição do Grau academico Homens- 2017", col = cores )
legend("topright", c("Não aplicavel","Bacharelado","Licenciatura","Tecnológico" ), fill = coresGM)

pie(grau_academico_homens_2017, labels = grau_academico_homens_2017, main = "Distribuição Grau Academico -  homens -2017")

pctGH2017 <- round(grau_academico_homens_2017/sum(grau_academico_homens_2017)*100)
nomeGH2017 <- c("Nao Aplicado", "Bacharelado","Licenciatura", "Tecnológico")
nomeGH2017 <- paste(nomeGH2017,pctGH2017)
nomeGH2017 <- paste(nomeGH2017, "%", sep = "")

pie(grau_academico_homens_2017, labels = nomeGH2017, main = "porcentagem do Grau Academico  Homens 2017",col = rainbow(length(nomeGH2017)))



##### GRUPOS   #####





























