library(PNADcIBGE)
library(survey)
library(srvyr)
library(tidyr)
library(dplyr)
library(kableExtra)
library(ggplot2)
rm(list = ls())
SM_2019 <- 998

dados_srvyr_2021<-as_survey(pnadc_design(read_pnadc("./2021/PNADC_042021.txt","./input/input_PNADC_trimestral.txt",vars=c(
  'V1022', # Situação do Domicílio: 1- Urbano 2-Rural
  'V2005', #Situação da pessoa no domicílio'01'-Pessoa responsavel pelo domicilio/02-Conjuge/03-Conjuge
  'V2007', # Sexo
  'V2009', #Idade
  'VD4002', # Condição de ocupação 1. Ocupadas/2. Desocupadas/NA
  'VD4010' #Grupramento de atividades
  ))))


dados_srvyr_2021<- dados_srvyr_2021 %>% mutate(PEA = if_else(VD4002 %in% c('1','2'),1,0))

x2021 <- dados_srvyr_2021 %>% group_by(V2009,V2007,V1022) %>% summarise(PEA_Total = survey_total(PEA))

x2021 <-x2021 %>% mutate(PEA_Total=trunc(PEA_Total))
x2021 <- x2021 %>% mutate(PEA_Total=trunc(PEA_Total)) %>% select(!PEA_Total_se)

names(x2021) <- c('idade','sexo','clientela','pea.total')

PEA_H_Urb <-x2021 %>% filter(sexo=='1',clientela=='1')
PEA_H_Rur <-x2021 %>% filter(sexo=='1',clientela=='2')
PEA_M_Urb <-x2021 %>% filter(sexo=='2',clientela=='1')
PEA_M_Rur <-x2021 %>% filter(sexo=='2',clientela=='2')





