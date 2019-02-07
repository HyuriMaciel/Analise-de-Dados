
setwd("/home/hyuri/Área de Trabalho/Anna")

save.image(file="Alunos_last.RData")
#system.time

library(sqldf)
library(datasets)
library(dplyr)
library(lubridate)
library(ggplot2)

#carrega os dados do csv selecionado apenas as variáveis que seram usadas, com isso reduz o tamanho do data frame a ser carregado

  #2012
dados_alunos_2012 <- read.csv.sql(file="/home/hyuri/Área de Trabalho/Anna/microdados_censo_superior_2012/2012/DADOS/DM_ALUNO.CSV",
                                  sql = "select  CO_CURSO,NO_CURSO, CO_COR_RACA_ALUNO, DS_COR_RACA_ALUNO, IN_SEXO_ALUNO, DS_SEXO_ALUNO
                                  from file",
                                  header = TRUE, sep ="|")

#2017
dados_alunos_2017 <- read.csv.sql(file="/home/hyuri/Área de Trabalho/Anna/microdados_educacao_superior_2017/Microdados_Educacao_Superior_2017/DADOS/DM_ALUNO.CSV",
                                  sql = "select  CO_CURSO, TP_COR_RACA, TP_SEXO
                                  from file",header = TRUE, sep ="|")

# Cod. Curso 2017

cod_curso= read.csv.sql("/home/hyuri/Área de Trabalho/Anna/microdados_educacao_superior_2017/Microdados_Educacao_Superior_2017/DADOS/DM_CURSO.CSV", 
                        sql = "select CO_CURSO, NO_CURSO from file" ,header = TRUE, sep ="|")


CO_CURSO <- cod_curso$CO_CURSO

NO_CURSO <- cod_curso$NO_CURSO

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

nome <- iconv(NOME_CURSO, "latin1", "ASCII//TRANSLIT")

nome <- as.data.frame(nome)

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


######################################## ANALISE 2012 #######################################################################

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



contagem2012 <- table(dados_2012$IN_SEXO_ALUNO)

nomes2012 <- levels(dados_2012$IN_SEXO_ALUNO)

porcent2012 <- round(prop.table(contagem2012)*100,2)

rotulo2012 <- paste(nomes2012," (",porcent2012,"%",")",sep="")

dados2012 <- data.frame(round(prop.table(contagem2012)*100,2))

dados2012 <- within(dados2012, {
  Var1 <- factor(Var1, labels=c('Homem','Mulher'))
})

attach(dados2012)
dados2012 <- dados2012[order(Freq),] 
detach(dados2012)

# Fora das Barras
ggplot(data=dados2012, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill=c(1,2))+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  ylab("Frequência") +
  xlab("Tipo") +
  theme_minimal()


############################### RAÇA - 2012 ################################################################

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
contagemR_2012 <- table(dados_2012$CO_COR_RACA_ALUNO)

nomesR_2012 <- levels(dados_2012$CO_COR_RACA_ALUNO)

porcentR_2012 <- round(prop.table(contagemR_2012)*100,7)

rotuloR_2012 <- paste(nomesR_2012," (",porcentR_2012,"%",")",sep="")

cores <- c(1:7) 

dadosR_2012 <- data.frame(round(prop.table(contagemR_2012)*100,2))

dadosR_2012 <- within(dadosR_2012, {
  Var1 <- factor(Var1, labels=c("Não declarado","Branca","Preta","Parda","Amarela","Indígena","Não dispõe da informação"))
})

attach(dadosR_2012)
dadosR_2012 <- dadosR_2012[order(Freq),] 
detach(dadosR_2012)

# Fora das Barras
ggplot(data=dadosR_2012, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill= cores)+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  ylab("Frequência") +
  xlab("Tipo") +
  theme_minimal()


################################################# NEGROS 2012 ################################################


NEGRO_2012 <- dados_2012 %>%  filter(CO_COR_RACA_ALUNO == 2 )

sum(NEGRO_2012$IN_SEXO_ALUNO == "1")
#[1] 17472
sum(NEGRO_2012$IN_SEXO_ALUNO == "0")
#[1] 116193

# Sexo 
# 0. Masculino
# 1. Feminino

contagemNEGRO_2012 <- table(NEGRO_2012$IN_SEXO_ALUNO)

nomesNEGRO_2012 <- levels(NEGRO_2012$IN_SEXO_ALUNO)

porcentNEGRO_2012 <- round(prop.table(contagemNEGRO_2012)*100,2)

rotuloNEGRO_2012 <- paste(nomesNEGRO_2012," (",porcentNEGRO_2012,"%",")",sep="")

dadosNEGRO_2012 <- data.frame(round(prop.table(contagemNEGRO_2012)*100,2))

dadosNEGRO_2012 <- within(dadosNEGRO_2012, {
  Var1 <- factor(Var1, labels=c('Homem','Mulher'))
})

attach(dadosNEGRO_2012)
dadosNEGRO_2012 <- dadosNEGRO_2012[order(Freq),] 
detach(dadosNEGRO_2012)

# Fora das Barras
ggplot(data=dadosNEGRO_2012, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill=c(1,2))+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  ylab("Frequência") +
  xlab("Tipo") +
  theme_minimal()

################################################## MULHRES 2012 #######################################################################

alunos_mulheres <- subset(dados_alunos_2012, dados_alunos_2012$IN_SEXO_ALUNO == "1")

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


#################################################  Homens 2012 ###########################################################################

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


####################### ANALISE 2017 ################################################

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


contagem2017 <- table(dados_2017$TP_SEXO)

nomes2017 <- levels(dados_2017$TP_SEXO)

porcent2017 <- round(prop.table(contagem2017)*100,2)

rotulo2017 <- paste(nomes2017," (",porcent2017,"%",")",sep="")

dados2017 <- data.frame(round(prop.table(contagem2017)*100,2))

dados2017 <- within(dados2017, {
  Var1 <- factor(Var1, labels=c('Mulher','Homem'))
})

attach(dados2017)
dados2017 <- dados2017[order(Freq),] 
detach(dados2017)

# Fora das Barras
ggplot(data=dados2017, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill=c(1,2))+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  ylab("Frequência") +
  xlab("Tipo") +
  theme_minimal()


############################### RAÇA - 2017 ################################################################

cor_raca_2017 <- table(dados_alunos_2017$TP_COR_RACA)

prop.table(cor_raca_2017)

#       0           1           2           3           4           5           9 
#  0.261582988 0.388149944 0.063391811 0.254859743 0.016036232 0.007175219 0.008804064 

# 1. Branca
# 2. Preta
# 3. Parda
# 4. Amarela
# 5. Indígena
# 6. Não dispõe da informação
# 0. Não declarado


contagemR_2017 <- table(dados_2017$TP_COR_RACA)

nomesR_2017 <- levels(dados_2017$TP_COR_RACA)

porcentR_2017 <- round(prop.table(contagemR_2017)*100,7)

rotuloR_2017 <- paste(nomesR_2017," (",porcentR_2017,"%",")",sep="")

dadosR_2017 <- data.frame(round(prop.table(contagemR_2017)*100,2))

dadosR_2017 <- within(dadosR_2017, {
  Var1 <- factor(Var1, labels=c("Não declarado","Branca","Preta","Parda","Amarela","Indígena","Não dispõe da informação"))
})

attach(dadosR_2017)
dadosR_2017 <- dadosR_2017[order(Freq),] 
detach(dadosR_2017)

# Fora das Barras
ggplot(data=dadosR_2017, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill= cores)+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  ylab("Frequência") +
  xlab("Tipo") +
  theme_minimal()


################################################# NEGROS 2017 ################################################


NEGRO_2017 <- dados_2017 %>%  filter(TP_COR_RACA == 2 )

sum(NEGRO_2017$TP_SEXO == "1")
#[1] 387926
  sum(NEGRO_2017$TP_SEXO == "2")
#[1] 346734

# Sexo 
# 2. Masculino
# 1. Feminino

contagemNEGRO_2017 <- table(NEGRO_2017$TP_SEXO)

nomesNEGRO_2017 <- levels(NEGRO_2017$TP_SEXO)

porcentNEGRO_2017 <- round(prop.table(contagemNEGRO_2017)*100,2)

rotuloNEGRO_2017 <- paste(nomesNEGRO_2017," (",porcentNEGRO_2012,"%",")",sep="")

dadosNEGRO_2017 <- data.frame(round(prop.table(contagemNEGRO_2017)*100,2))

dadosNEGRO_2017 <- within(dadosNEGRO_2017, {
  Var1 <- factor(Var1, labels=c('Mulher','Homem'))
})

attach(dadosNEGRO_2017)
dadosNEGRO_2017 <- dadosNEGRO_2017[order(Freq),] 
detach(dadosNEGRO_2017)

# Fora das Barras
ggplot(data=dadosNEGRO_2017, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill=c(1,2))+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  ylab("Frequência") +
  xlab("Tipo") +
  theme_minimal()




############################################# GRUPOS 2012  ##############################################################
library(ggplot2)
library(readr)
library(DAAG)

G1_HM_2012 <- read.csv('/home/hyuri/Área de Trabalho/Anna/G1_2012.csv', sep = "," ,header=TRUE, stringsAsFactors=FALSE)
G2_HM_2012 <- read.csv('/home/hyuri/Área de Trabalho/Anna/G2_2012.csv', sep = "," ,header=TRUE, stringsAsFactors=FALSE)
G3_HM_2012<- read.csv('/home/hyuri/Área de Trabalho/Anna/G3_2012.csv',  sep = "," ,header=TRUE, stringsAsFactors=FALSE)
G4_HM_2012 <- read.csv('/home/hyuri/Área de Trabalho/Anna/G4_2012.csv', sep = "," ,header=TRUE, stringsAsFactors=FALSE)
G5_HM_2012 <- read.csv('/home/hyuri/Área de Trabalho/Anna/G5_2012.csv', sep = "," ,header=TRUE, stringsAsFactors=FALSE)
G6_HM_2012 <- read.csv('/home/hyuri/Área de Trabalho/Anna/G6_2012.csv', sep = "," ,header=TRUE, stringsAsFactors=FALSE)
G7_HM_2012 <- read.csv('/home/hyuri/Área de Trabalho/Anna/G7_2012.csv', sep = "," ,header=TRUE, stringsAsFactors=FALSE)
G8_HM_2012 <- read.csv('/home/hyuri/Área de Trabalho/Anna/G8_2012.csv', sep = "," ,header=TRUE, stringsAsFactors=FALSE)


G1_HM_2017 <- read.csv('/home/hyuri/Área de Trabalho/Anna/G1_2017.csv', sep = "," ,header=TRUE, stringsAsFactors=FALSE)
G2_HM_2017 <- read.csv('/home/hyuri/Área de Trabalho/Anna/G2_2017.csv', sep = "," ,header=TRUE, stringsAsFactors=FALSE)
G3_HM_2017<- read.csv('/home/hyuri/Área de Trabalho/Anna/G3_2017.csv',  sep = "," ,header=TRUE, stringsAsFactors=FALSE)
G4_HM_2017 <- read.csv('/home/hyuri/Área de Trabalho/Anna/G4_2017.csv', sep = "," ,header=TRUE, stringsAsFactors=FALSE)
G5_HM_2017 <- read.csv('/home/hyuri/Área de Trabalho/Anna/G5_2017.csv', sep = "," ,header=TRUE, stringsAsFactors=FALSE)
G6_HM_2017 <- read.csv('/home/hyuri/Área de Trabalho/Anna/G6_2017.csv', sep = "," ,header=TRUE, stringsAsFactors=FALSE)
G7_HM_2017 <- read.csv('/home/hyuri/Área de Trabalho/Anna/G7_2017.csv', sep = "," ,header=TRUE, stringsAsFactors=FALSE)
G8_HM_2017 <- read.csv('/home/hyuri/Área de Trabalho/Anna/G8_2017.csv', sep = "," ,header=TRUE, stringsAsFactors=FALSE)

str(G1_HM_2012)
#G1_HM_2012 <- as.data.frame(G1_HM_2012)
summary(G1_HM_2012)

#NO_CURSO_2012         CO_CURSO         NO_CURSO         CO_COR_RACA_ALUNO DS_COR_RACA_ALUNO  IN_SEXO_ALUNO    DS_SEXO_ALUNO     
#Length:771063      Min.   :      6   Length:771063      Min.   :0.000     Length:771063      Min.   :0.0000   Length:771063     
#Class :character   1st Qu.:  57707   Class :character   1st Qu.:0.000     Class :character   1st Qu.:1.0000   Class :character  
#Mode  :character   Median :  94994   Mode  :character   Median :3.000     Mode  :character   Median :1.0000   Mode  :character  
#Mean   : 175597                      Mean   :2.945                        Mean   :0.9196                     
#3rd Qu.: 102280                      3rd Qu.:6.000                        3rd Qu.:1.0000                     
#Max.   :5001097                      Max.   :6.000                        Max.   :1.0000

############################################# SEXP ##############################################################

G1_sexo_aluno_12<- table(G8_HM_2017$TP_SEXO)

sum(G8_HM_2017$TP_SEXO == "1")
#[1] 846995
sum(G8_HM_2017$TP_SEXO == "2")
#[1] 256044

# Sexo 
# 2. Masculino
# 1. Feminino

# Sexo  2012
# 0. Masculino
# 1. Feminino


prop.table(G1_sexo_aluno_12)
#     0          1 
# 0.08042793 0.91957207 

# Moda
names(G1_sexo_aluno_12)[which.max(G1_sexo_aluno_12)]
# 1

# quartis
quantile(G1_sexo_aluno_12, na.rm = TRUE)
#   0%      25%      50%      75%     100% 
#  62015.0 223773.2 385531.5 547289.8 709048.0 

contagemG1 <- table(G8_HM_2017$TP_SEXO)

nomesG1 <- levels(G8_HM_2017$TP_SEXO)

porcentG1 <- round(prop.table(contagemG1)*100,2)

rotuloG1 <- paste(nomesG1," (",porcentG1,"%",")",sep="")

dados <- data.frame(round(prop.table(contagemG1)*100,2))

dados <- within(dados, {
  Var1 <- factor(Var1, labels=c('Mulher','Homem'))
})

attach(dados)
dados <- dados[order(Freq),] 
detach(dados)

# Fora das Barras
ggplot(data=dados, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill= c(2,1))+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  ylab("Frequência") +
  xlab("Genero") +
  theme_minimal()

####################################### RACA ###########################################################


G1_cor_raca_2012 <- table(G8_HM_2017$TP_COR_RACA)

prop.table(G1_cor_raca_2012)

#       0           1           2           3           4           5           6 
#  0.270736632 0.196042347 0.025706071 0.111361847 0.005939852 0.001507010 0.388706241  

# 1. Branca
# 2. Preta
# 3. Parda
# 4. Amarela
# 5. Indígena
# 9. Não dispõe da informação
# 0. Não declarado


############### 2012

# 1. Branca
# 2. Preta
# 3. Parda
# 4. Amarela
# 5. Indígena
# 9. Não dispõe da informação
# 0. Não declarado


# Moda
names(G1_cor_raca_2012)[which.max(G1_cor_raca_2012)]
# 6

contagemRG1 <- table(G8_HM_2017$TP_COR_RACA)

nomesRG1 <- levels(G8_HM_2017$TP_COR_RACA)

porcentRG1 <- round(prop.table(contagemRG1)*100,7)

rotuloRG1 <- paste(nomesRG1," (",porcentRG1,"%",")",sep="")

dadosR <- data.frame(round(prop.table(contagemRG1)*100,2))

dadosR <- within(dadosR, {
  Var1 <- factor(Var1, labels=c("Não declarado","Branca","Preta","Parda","Amarela","Indígena","Não dispõe da informação"))
})

attach(dadosR)
dadosR <- dadosR[order(Freq),] 
detach(dadosR)

# Fora das Barras
ggplot(data=dadosR, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill= cores)+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  ylab("Frequência") +
  xlab("Tipo") +
  theme_minimal()

 ################################ MULHERES E HOMENS NEGROS ##############################################

G1_NEGRO_2012 <- G8_HM_2017 %>%  filter(TP_COR_RACA== 2 )

sum(G1_NEGRO_2012$TP_SEXO == "1")
#[1] 17472
sum(G1_NEGRO_2012$TP_SEXO == "2")
#[1] 2349

# Sexo 
# 0. Masculino
# 1. Feminino

contagemPG1 <- table(G1_NEGRO_2012$TP_SEXO)

nomesPG1 <- levels(G1_NEGRO_2012$TP_SEXO)

porcentPG1 <- round(prop.table(contagemPG1)*100,2)

rotuloPG1 <- paste(nomesPG1," (",porcentPG1,"%",")",sep="")

dadosP <- data.frame(round(prop.table(contagemPG1)*100,2))

dadosP <- within(dadosP, {
  Var1 <- factor(Var1, labels=c('Mulher','Homem'))
})

attach(dadosP)
dadosP <- dadosP[order(Freq),] 
detach(dadosP)

# Fora das Barras
ggplot(data=dadosP, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill=c(1,2))+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  ylab("Frequência") +
  xlab("Tipo") +
  theme_minimal()



















































