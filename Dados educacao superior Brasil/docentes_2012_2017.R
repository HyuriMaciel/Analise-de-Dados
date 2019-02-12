

setwd("/home/hyuri/Área de Trabalho/Anna")

save.image(file="Docentes_last.RData")
#system.time

library(sqldf)
library(datasets)
library(dplyr)
library(lubridate)
library(ggplot2)

#carrega os dados do csv selecionado apenas as variáveis que seram usadas, com isso reduz o tamanho do data frame a ser carregado

#2012
docentes_2012 <- read.csv.sql(file="/home/hyuri/Área de Trabalho/Anna/microdados_censo_superior_2012/2012/DADOS/DM_DOCENTE.CSV",
                                  sql = "select  CO_ESCOLARIDADE_DOCENTE,IN_SEXO_DOCENTE, CO_COR_RACA_DOCENTE
                                  from file",
                                  header = TRUE, sep ="|")

#2017
docentes_2017 <- read.csv.sql(file="/home/hyuri/Área de Trabalho/Anna/microdados_educacao_superior_2017/Microdados_Educacao_Superior_2017/DADOS/DM_DOCENTE.CSV",
                                  sql = "select  TP_ESCOLARIDADE, TP_COR_RACA, TP_SEXO
                                  from file",header = TRUE, sep ="|")


summary(docentes_2012)
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


summary(docentes_2017)

# CO_CURSO         NO_CURSO          TP_COR_RACA       TP_SEXO      TP_GRAU_ACADEMICO
# Min.   :      1   Length:11589194    Min.   :0.000   Min.   :1.000   Min.   :0.00     
# 1st Qu.:  48540   Class :character   1st Qu.:0.000   1st Qu.:1.000   1st Qu.:1.00     
# Median :  98445   Mode  :character   Median :1.000   Median :1.000   Median :1.00     
# Mean   : 497113                      Mean   :1.459   Mean   :1.443   Mean   :1.46     
# 3rd Qu.:1125999                      3rd Qu.:3.000   3rd Qu.:2.000   3rd Qu.:2.00     
# Max.   :5001415                      Max.   :9.000   Max.   :2.000   Max.   :3.00


######################################## ANALISE  #######################################################################

## SEXO

sexo_aluno<- table(docentes_2017$TP_SEXO)

sum(docentes_2017$TP_SEXO == "1")

sum(docentes_2017$TP_SEXO == "2")


# Sexo  2012
# 0. Masculino
# 1. Feminino


# Sexo 2017
# 2. Masculino
# 1. Feminino

prop.table(sexo_aluno)
#   Feminino   Masculino 
# 0.5585022    0.4414978 

# Moda
names(sexo_aluno)[which.max(sexo_aluno)]
# 1

# quartis
quantile(sexo_aluno, na.rm = TRUE)
#   0%     25%     50%     75%    100% 
# 4223140 4502941 4782742 5062542 5342343



contagem2012 <- table(docentes_2017$TP_SEXO)

nomes2012 <- levels(docentes_2017$TP_SEXO)

porcent2012 <- round(prop.table(contagem2012)*100,2)

rotulo2012 <- paste(nomes2012," (",porcent2012,"%",")",sep="")

dados2012 <- data.frame(round(prop.table(contagem2012)*100,2))

dados2012 <- within(dados2012, {
  Var1 <- factor(Var1, labels=c('Mulher','Homem'))
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

cor_raca <- table(docentes_2017$TP_COR_RACA)

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

coress <- c(1:6)  
contagemR_2012 <- table(docentes_2017$TP_COR_RACA)

nomesR_2012 <- levels(docentes_2017$TP_COR_RACA)

porcentR_2012 <- round(prop.table(contagemR_2012)*100,7)

rotuloR_2012 <- paste(nomesR_2012," (",porcentR_2012,"%",")",sep="")


dadosR_2012 <- data.frame(round(prop.table(contagemR_2012)*100,2))

dadosR_2012 <- within(dadosR_2012, {
  Var1 <- factor(Var1, labels=c("Não declarado","Branca","Preta","Parda","Amarela","Indígena"))
})

attach(dadosR_2012)
dadosR_2012 <- dadosR_2012[order(Freq),] 
detach(dadosR_2012)

# Fora das Barras
ggplot(data=dadosR_2012, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill= coress)+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  ylab("Frequência") +
  xlab("Tipo") +
  theme_minimal()



######################### negras e pardas

NEGRO_e_Pardos <- docentes_2012 %>%  filter((CO_COR_RACA_DOCENTE == 2 | CO_COR_RACA_DOCENTE  == 3) & IN_SEXO_DOCENTE== 1 )

Brancas <- docentes_2012%>%  filter((CO_COR_RACA_DOCENTE== 2 | CO_COR_RACA_DOCENTE== 3 | CO_COR_RACA_DOCENTE== 1 ) & IN_SEXO_DOCENTE == 1 )

sum(NEGRO_e_Pardos$CO_COR_RACA_DOCENTE == "2")

sum(NEGRO_e_Pardos$CO_COR_RACA_DOCENTE == "3")

sum(Brancas$CO_COR_RACA_DOCENTE == 1)

contagemNEGRO_2012 <- table(NEGRO_e_Pardos$CO_COR_RACA_DOCENTE)

nomesNEGRO_2012 <- levels(NEGRO_e_Pardos$CO_COR_RACA_DOCENTE)

porcentNEGRO_2012 <- round(prop.table(contagemNEGRO_2012)*100,2)

rotuloNEGRO_2012 <- paste(nomesNEGRO_2012," (",porcentNEGRO_2012,"%",")",sep="")

dadosNEGRO <- data.frame(round(prop.table(contagemNEGRO_2012)*100,2))

dadosNEGRO <- within(dadosNEGRO, {
  Var1 <- factor(Var1, labels=c('Negras','Pardas'))
})

attach(dadosNEGRO)
dadosNEGRO <- dadosNEGRO[order(Freq),] 
detach(dadosNEGRO)

# Fora das Barras
ggplot(data=dadosNEGRO, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill=c(1,2))+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  ylab("Frequência") +
  xlab("Tipo") +
  theme_minimal()


##############   BRANCAS
contagembranca<- table(Brancas$CO_COR_RACA_DOCENTE)

nomesbranca <- levels(Brancas$CO_COR_RACA_DOCENTE)

porcentbranca <- round(prop.table(contagembranca)*100,3)

rotulobranca<- paste(nomesbranca," (",porcentbranca,"%",")",sep="")

dadosbranca <- data.frame(round(prop.table(contagembranca)*100,2))

dadosbranca<- within(dadosbranca, {
  Var1 <- factor(Var1, labels=c('Branca', 'Negras','Pardas'))
})

attach(dadosbranca)
dadosbranca <- dadosbranca[order(Freq),] 
detach(dadosbranca)

# Fora das Barras
ggplot(data=dadosbranca, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill=c(2,1,3))+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  ylab("Frequência") +
  xlab("Tipo") +
  theme_minimal()

############################### Escolaridade - 2012 ################################################################

escolaridade_2012 <- table(docentes_2017$TP_ESCOLARIDADE)

prop.table(escolaridade_2012)

#    1            2            3            4            5 
#  0.0002823673 0.0320922365 0.2642588913 0.3902369511 0.3131295538 

#  1. Sem graduação
# 2. Graduação
# 3. Especialização
# 4. Mestrado
# 5. Doutorado

sum(docentes_2017$TP_ESCOLARIDADE== "1")
#107
sum(docentes_2017$TP_ESCOLARIDADE == "2")

sum(docentes_2017$TP_ESCOLARIDADE== "3")

sum(docentes_2017$TP_ESCOLARIDADE == "4")

sum(docentes_2017$TP_ESCOLARIDADE == "5")

cor <- (1:5)
contagemR_2012 <- table(docentes_2017$TP_ESCOLARIDADE)

nomesR_2012 <- levels(docentes_2017$TP_ESCOLARIDADE)

porcentR_2012 <- round(prop.table(contagemR_2012)*100,5)

rotuloR_2012 <- paste(nomesR_2012," (",porcentR_2012,"%",")",sep="")


dadosR_2012 <- data.frame(round(prop.table(contagemR_2012)*100,5))

dadosR_2012 <- within(dadosR_2012, {
  Var1 <- factor(Var1, labels=c("Sem graduação","Graduação","Especialização","Mestrado","Doutorado"))
})

attach(dadosR_2012)
dadosR_2012 <- dadosR_2012[order(Freq),] 
detach(dadosR_2012)

# Fora das Barras
ggplot(data=dadosR_2012, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill= cor)+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  ylab("Frequência") +
  xlab("Tipo") +
  theme_minimal()



############################# escolaridade

#  1. Sem graduação
# 2. Graduação
# 3. Especialização
# 4. Mestrado
# 5. Doutorado


# Sexo  2012
# 0. Masculino
# 1. Feminino

NEGRO_e_Pardos <- docentes_2017 %>%  filter((TP_COR_RACA == 2 | TP_COR_RACA  == 3) & TP_SEXO== 1 & TP_ESCOLARIDADE == 5)

Brancas <- docentes_2017 %>%  filter((TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 1 ) 
                                    & TP_SEXO == 1 & TP_ESCOLARIDADE == 5)

sum(NEGRO_e_Pardos$TP_COR_RACA == "2")

sum(NEGRO_e_Pardos$TP_COR_RACA == "3")

sum(Brancas$TP_COR_RACA == 1)

contagemNEGRO_2012 <- table(NEGRO_e_Pardos$TP_COR_RACA)

nomesNEGRO_2012 <- levels(NEGRO_e_Pardos$TP_COR_RACA)

porcentNEGRO_2012 <- round(prop.table(contagemNEGRO_2012)*100,2)

rotuloNEGRO_2012 <- paste(nomesNEGRO_2012," (",porcentNEGRO_2012,"%",")",sep="")

dadosNEGRO <- data.frame(round(prop.table(contagemNEGRO_2012)*100,2))

dadosNEGRO <- within(dadosNEGRO, {
  Var1 <- factor(Var1, labels=c('Negras','Pardas'))
})

attach(dadosNEGRO)
dadosNEGRO <- dadosNEGRO[order(Freq),] 
detach(dadosNEGRO)

# Fora das Barras
ggplot(data=dadosNEGRO, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill=c(1,2))+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  ylab("Frequência") +
  xlab("Tipo") +
  theme_minimal()


##############   BRANCAS escolaridade
contagembranca<- table(Brancas$TP_COR_RACA)

nomesbranca <- levels(Brancas$TP_COR_RACA)

porcentbranca <- round(prop.table(contagembranca)*100,3)

rotulobranca<- paste(nomesbranca," (",porcentbranca,"%",")",sep="")

dadosbranca <- data.frame(round(prop.table(contagembranca)*100,2))

dadosbranca<- within(dadosbranca, {
  Var1 <- factor(Var1, labels=c('Branca', 'Negras','Pardas'))
})

attach(dadosbranca)
dadosbranca <- dadosbranca[order(Freq),] 
detach(dadosbranca)

# Fora das Barras
ggplot(data=dadosbranca, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill=c(2,1,3))+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  ylab("Frequência") +
  xlab("Tipo") +
  theme_minimal()

################################################# NEGROS 2012 ################################################


NEGRO_2012 <- docentes_2017 %>%  filter(TP_COR_RACA == 2 )

sum(NEGRO_2012$TP_SEXO == "1")
#[1] 17472
sum(NEGRO_2012$TP_SEXO== "2")
#[1] 116193

# Sexo 
# 0. Masculino
# 1. Feminino

contagemNEGRO_2012 <- table(NEGRO_2012$TP_SEXO)

nomesNEGRO_2012 <- levels(NEGRO_2012$TP_SEXO)

porcentNEGRO_2012 <- round(prop.table(contagemNEGRO_2012)*100,2)

rotuloNEGRO_2012 <- paste(nomesNEGRO_2012," (",porcentNEGRO_2012,"%",")",sep="")

dadosNEGRO_2012 <- data.frame(round(prop.table(contagemNEGRO_2012)*100,2))

dadosNEGRO_2012 <- within(dadosNEGRO_2012, {
  Var1 <- factor(Var1, labels=c('Mulher','Homem'))
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





################################################# NEGROS POR ESPECIALIDADES  MULHERES 2012 ################################################


MULHERES_NEGRO_2012 <- docentes_2017 %>%  filter(TP_COR_RACA == 2 , TP_SEXO == 1)

M_escolaridade_2012 <- table(MULHERES_NEGRO_2012$TP_ESCOLARIDADE)

prop.table(M_escolaridade_2012)

#    1            2            3            4            5 
#  0.0002823673 0.0320922365 0.2642588913 0.3902369511 0.3131295538 

#  1. Sem graduação
# 2. Graduação
# 3. Especialização
# 4. Mestrado
# 5. Doutorado

sum(MULHERES_NEGRO_2012$TP_ESCOLARIDADE == "1")

sum(MULHERES_NEGRO_2012$TP_ESCOLARIDADE == "2")

sum(MULHERES_NEGRO_2012$TP_ESCOLARIDADE == "3")

sum(MULHERES_NEGRO_2012$TP_ESCOLARIDADE == "4")

sum(MULHERES_NEGRO_2012$TP_ESCOLARIDADE == "5")



contagemR_2012 <- table(MULHERES_NEGRO_2012$TP_ESCOLARIDADE)

nomesR_2012 <- levels(MULHERES_NEGRO_2012$TP_ESCOLARIDADE)

porcentR_2012 <- round(prop.table(contagemR_2012)*100,5)

rotuloR_2012 <- paste(nomesR_2012," (",porcentR_2012,"%",")",sep="")


dadosR_2012 <- data.frame(round(prop.table(contagemR_2012)*100,5))

dadosR_2012 <- within(dadosR_2012, {
  Var1 <- factor(Var1, labels=c("Graduação","Especialização","Mestrado","Doutorado"))
})

attach(dadosR_2012)
dadosR_2012 <- dadosR_2012[order(Freq),] 
detach(dadosR_2012)

co <- c(1:4)
# Fora das Barras
ggplot(data=dadosR_2012, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill= co)+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  ylab("Frequência") +
  xlab("Tipo") +
  theme_minimal()


################################################# NEGROS POR ESPECIALIDADES  Homens 2012 ################################################


Homens_NEGRO_2012 <- docentes_2017 %>%  filter(TP_COR_RACA == 2 , TP_SEXO == 2)

H_escolaridade_2012 <- table(Homens_NEGRO_2012$TP_ESCOLARIDADE)

prop.table(H_escolaridade_2012)

#    1            2            3            4            5 
#  0.0002823673 0.0320922365 0.2642588913 0.3902369511 0.3131295538 

#  1. Sem graduação
# 2. Graduação
# 3. Especialização
# 4. Mestrado
# 5. Doutorado

sum(Homens_NEGRO_2012$TP_ESCOLARIDADE == "1")

sum(Homens_NEGRO_2012$TP_ESCOLARIDADE == "2")

sum(Homens_NEGRO_2012$TP_ESCOLARIDADE == "3")

sum(Homens_NEGRO_2012$TP_ESCOLARIDADE == "4")

sum(Homens_NEGRO_2012$TP_ESCOLARIDADE == "5")



contagemR_2012 <- table(Homens_NEGRO_2012$TP_ESCOLARIDADE)

nomesR_2012 <- levels(Homens_NEGRO_2012$TP_ESCOLARIDADE)

porcentR_2012 <- round(prop.table(contagemR_2012)*100,5)

rotuloR_2012 <- paste(nomesR_2012," (",porcentR_2012,"%",")",sep="")


dadosR_2012 <- data.frame(round(prop.table(contagemR_2012)*100,5))

dadosR_2012 <- within(dadosR_2012, {
  Var1 <- factor(Var1, labels=c("Graduação","Especialização","Mestrado","Doutorado"))
})

attach(dadosR_2012)
dadosR_2012 <- dadosR_2012[order(Freq),] 
detach(dadosR_2012)

# Fora das Barras
ggplot(data=dadosR_2012, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill= co)+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  ylab("Frequência") +
  xlab("Tipo") +
  theme_minimal()
