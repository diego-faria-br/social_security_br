library(PNADcIBGE)
library(survey)
library(srvyr)
library(tidyr)
library(dplyr)
library(multidplyr)
library(kableExtra)
library(ggplot2)

rm(list = ls())
SM_2019 <- 998


# 2021 --------------------------------------------------------------------



dados_srvyr_2021<-as_survey(pnadc_design(read_pnadc("./PNADC_042021.txt","./input/input_PNADC_trimestral.txt",vars=c(
  'V1022', # Situação do Domicílio: 1- Urbano 2-Rural
  'V2005', #Situação da pessoa no domicílio'01'-Pessoa responsavel pelo domicilio/02-Conjuge/03-Conjuge
  'V2007', # Sexo
  'V2009', #Idade
  'VD4002', # Condição de ocupação 1. Ocupadas/2. Desocupadas/NA
  'VD4010' #Grupramento de atividades
  ))))


# Define PEA como sendo a população ocupada ou desocupada
dados_srvyr_2021<- dados_srvyr_2021 %>% mutate(PEA = if_else(VD4002 %in% c('1','2'),1,0))

PEA_H_Urb_2021 <- PEA_H_Urb_2021 %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))

PEA_H_Urb_2021 <- dados_srvyr_2021 %>% filter(V2007=='1',V1022=='1') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))
PEA_H_Rur_2021 <- dados_srvyr_2021 %>% filter(V2007=='1',V1022=='2') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))
PEA_M_Urb_2021 <- dados_srvyr_2021 %>% filter(V2007=='2',V1022=='1') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))
PEA_M_Rur_2021 <- dados_srvyr_2021 %>% filter(V2007=='2',V1022=='2') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))


mais_de_100 <- function(pea_Data){
  idade_cent <-  pea_Data %>% mutate(idade_cent = if_else(V2009>=100,1,0)) %>% group_by(
    idade_cent) %>% summarise(V2009,PEA_Total,total_cent = sum(PEA_Total)) %>% select(
      V2009,PEA_Total,idade_cent,total_cent)
  
  pea_Data <-  bind_cols(pea_Data,idade_cent$total_cent)
  
  pea_Data <- pea_Data %>% filter(V2009<=100) %>% mutate(PEA_Total = if_else(V2009==100,`...3`,PEA_Total)) %>% select(V2009,PEA_Total)
  return(pea_Data)
}

PEA_H_Urb_2021<- mais_de_100(PEA_H_Rur_2021)
PEA_H_Rur_2021<- mais_de_100(PEA_H_Rur_2021)
PEA_M_Urb_2021<- mais_de_100(PEA_H_Rur_2021)
PEA_M_Rur_2021<- mais_de_100(PEA_H_Rur_2021)


# 2020 --------------------------------------------------------------------

  

