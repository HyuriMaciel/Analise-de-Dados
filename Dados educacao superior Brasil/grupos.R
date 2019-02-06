setwd("/home/hyuri/Área de Trabalho/Anna")

save.image(file="Grupos_last.RData")
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
                                  sql = "select  CO_CURSO, TP_COR_RACA, TP_SEXO, TP_GRAU_ACADEMICO
                                  from file",header = TRUE, sep ="|")
# Cod. Curso 2017

cod_curso= read.csv.sql("/home/hyuri/Área de Trabalho/Anna/microdados_educacao_superior_2017/Microdados_Educacao_Superior_2017/DADOS/DM_CURSO.CSV", 
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




G8_2017 <-  dados_2017 %>% filter(NO_CURSO =="HOTELARIA"|
                                    NO_CURSO =="GASTRONOMIA"|
                                    NO_CURSO =="ESTETICA E COSMETICA"|
                                    NO_CURSO =="SEGURANCA PUBLICA"|
                                    NO_CURSO =="SEGURANCA NO TRABALHO"|
                                    NO_CURSO =="LOGISTICA"|
                                    NO_CURSO =="TURISMO"|
                                    NO_CURSO =="EMBELEZAMENTO E IMAGEM PESSOAL"|
                                    NO_CURSO =="PODOLOGIA"|
                                    NO_CURSO =="TRANSPORTE TERRESTRE"|
                                    NO_CURSO =="PILOTAGEM PROFISSIONAL DE AERONAVES"|
                                    NO_CURSO =="ESTETICA E IMAGEM PESSOAL"|
                                    NO_CURSO =="AVIACAO CIVIL"|
                                    NO_CURSO =="SANEAMENTO AMBIENTAL"|
                                    NO_CURSO =="EVENTOS"|
                                    NO_CURSO =="ESTETICA E COSMETOLOGIA"|
                                    NO_CURSO =="CIENCIAS MILITARES"|
                                    NO_CURSO =="TURISMO E HOTELARIA"|
                                    NO_CURSO =="ESTETICA"|
                                    NO_CURSO =="CONSERVACAO E RESTAURACAO DE BENS CULTURAIS MOVEIS"|
                                    NO_CURSO =="QUIROPRAXIA"|
                                    NO_CURSO =="CONSERVACAO E RESTAURO"|
                                    NO_CURSO =="SEGURANCA PRIVADA"|
                                    NO_CURSO =="SEGURANCA E ORDEM PUBLICA"|
                                    NO_CURSO =="SERVICOS PENAIS"|
                                    NO_CURSO =="SERVICOS JURIDICOS"|
                                    NO_CURSO =="TURISMO E LAZER"|
                                    NO_CURSO =="PROCESSOS AMBIENTAIS"|
                                    NO_CURSO =="GEOCIENCIAS E EDUCACAO AMBIENTAL"|
                                    NO_CURSO =="MEIO AMBIENTE"|
                                    NO_CURSO =="BELEZA, ESTETICA E IMAGEM PESSOAL"|
                                    NO_CURSO =="SEGURANCA DO TRABALHO"|
                                    NO_CURSO =="COSMETICOS"|
                                    NO_CURSO =="CIENCIAS MILITARES E SEGURANCA PUBLICA"|
                                    NO_CURSO =="REFRIGERACAO E CLIMATIZACAO"|
                                    NO_CURSO =="CIENCIAS SOCIOAMBIENTAIS"|
                                    NO_CURSO =="FORMACAO DE OFICIAIS BOMBEIRO MILITAR"|
                                    NO_CURSO =="SEGURANCA PUBLICA E DO CIDADAO"|
                                    NO_CURSO =="TOXICOLOGIA AMBIENTAL"|
                                    NO_CURSO =="COSMETOLOGIA E ESTETICA"|
                                    NO_CURSO =="SEGURANCA NO TRANSITO"|
                                    NO_CURSO =="INSPECAO DE EQUIPAMENTOS E DE SOLDAGEM"|
                                    NO_CURSO =="COSMETOLOGIA E ESTESTICA"|
                                    NO_CURSO =="ENERGIAS RENOVAVEIS"|
                                    NO_CURSO =="DECORACAO"|
                                    NO_CURSO =="INVESTIGACAO FORENSE E PERICIA CRIMINAL"|
                                    NO_CURSO =="ORGANIZACAO DE SERVICOS JUDICIARIOS"|
                                    NO_CURSO =="TRANSPORTE AEREO"|
                                    NO_CURSO =="TURISMO PATRIMONIAL E SOCIOAMBIENTAL"|
                                    NO_CURSO =="MEIO AMBIENTE E RECURSOS HIDRICOS"|
                                    NO_CURSO =="PLANEJAMENTO LOGISTICO DE CARGAS"|
                                    NO_CURSO =="TECNOLOGIA EM CONTROLE AMBIENTAL"|
                                    NO_CURSO =="CONSERVACAO E RESTAURACAO"|
                                    NO_CURSO =="FORMACAO ESPECIFICA EM COSMETOLOGIA E ESTETICA"|
                                    NO_CURSO =="VISAGISMO E TERAPIAS CAPILARES"|
                                    NO_CURSO =="SERVICOS JURIDICOS E NOTARIAIS"|
                                    NO_CURSO =="FORMACAO DE OFICIAIS (CCSA)"|
                                    NO_CURSO =="TURISMO E MEIO AMBIENTE"|
                                    NO_CURSO =="LOGISTICA AEROPORTUARIA"|
                                    NO_CURSO =="CONTROLE AMBIENTAL"|
                                    NO_CURSO =="PROCESSOS DE SUSTENTABILIDADE AMBIENTAL"|
                                    NO_CURSO =="ESTETICA, BELEZA E IMAGEM PESSOAL"|
                                    NO_CURSO =="LAZER E TURISMO" )


library(readr)

write_csv(G8_2017, "G8_2017.csv", na = "NA")















































































































































































































































































































































































































































































































































































































































