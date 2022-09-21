library(PNADcIBGE)
library(tidyr)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(openxlsx)
library(survey)
library(srvyr)

rm(list = ls())

# Data import --------------------------------------------------------------------



dados_srvyr_2021<-as_survey(pnadc_design(read_pnadc("./PNADC_042021.txt","./input/input_PNADC_trimestral.txt",vars=c(
  'V1022', # Situação do Domicílio: 1- Urbano 2-Rural
  'V2005', #Situação da pessoa no domicílio'01'-Pessoa responsavel pelo domicilio/02-Conjuge/03-Conjuge
  'V2007', # Sexo
  'V2009'#Idade
  ))))

# dados_srvyr_2020<-as_survey(pnadc_design(read_pnadc("./PNADC_042020.txt","./input/input_PNADC_trimestral.txt",vars=c(
#   'V1022', # Situação do Domicílio: 1- Urbano 2-Rural
#   'V2005', #Situação da pessoa no domicílio'01'-Pessoa responsavel pelo domicilio/02-Conjuge/03-Conjuge
#   'V2007', # Sexo
#   'V2009', #Idade
# ))))
# 
# 
# dados_srvyr_2019<-as_survey(pnadc_design(read_pnadc("./PNADC_042019.txt","./input/input_PNADC_trimestral.txt",vars=c(
#   'V1022', # Situação do Domicílio: 1- Urbano 2-Rural
#   'V2005', #Situação da pessoa no domicílio'01'-Pessoa responsavel pelo domicilio/02-Conjuge/03-Conjuge
#   'V2007', # Sexo
#   'V2009', #Idade
# ))))
# 
# dados_srvyr_2018<-as_survey(pnadc_design(read_pnadc("./PNADC_042018.txt","./input/input_PNADC_trimestral.txt",vars=c(
#   'V1022', # Situação do Domicílio: 1- Urbano 2-Rural
#   'V2005', #Situação da pessoa no domicílio'01'-Pessoa responsavel pelo domicilio/02-Conjuge/03-Conjuge
#   'V2007', # Sexo
#   'V2009', #Idade
# ))))



# Functions to aggregate data from 100+ people -----------------------------


mais_de_100 <- function(pea_Data){
  idade_cent <-  pea_Data %>% mutate(idade_cent = if_else(V2009>=100,1,0)) %>% group_by(
    idade_cent) %>% summarise(V2009,PEA_Total,total_cent = sum(PEA_Total)) %>% select(
      V2009,PEA_Total,idade_cent,total_cent)
  
  PIA_Data <-  bind_cols(PIA_Data,idade_cent$total_cent)
  PIA_Data <- PIA_Data %>% filter(V2009<=100) %>% mutate(PIA_Total = if_else(V2009==100,`...3`,PIA_Total)) %>% select(V2009,PIA_Total)
  return(PIA_Data)
}


rename_PNAD <- function(PIA_Data,ano){
  names(PIA_Data) <- c('idade',paste0('x',ano))
  return(PIA_Data)
}


# 2021 --------------------------------------------------------------------

# Define PEA como sendo a população ocupada ou desocupada
dados_srvyr_2021<- dados_srvyr_2021 %>% filter(V2009>=16) %>% group_by(V2009,V2005,V2007) %>% summarise(PIA = survey_total())

# Writing in file routine -------------------------------------------------


wb <- createWorkbook()

addWorksheet(wb,sheetName = 'PEA_H_Urb')
addWorksheet(wb,sheetName = 'PEA_H_Rur')
addWorksheet(wb,sheetName = 'PEA_M_Urb')
addWorksheet(wb,sheetName = 'PEA_M_Rur')

writeData(wb,sheet = 1,PEA_H_Urb)
writeData(wb,sheet = 2,PEA_H_Rur)
writeData(wb,sheet = 3,PEA_M_Urb)
writeData(wb,sheet = 4,PEA_M_Rur)

saveWorkbook(wb,"PEA_2018_2021.xlsx",overwrite = TRUE)
  

