library(PNADcIBGE)
library(tidyr)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(openxlsx)

rm(list = ls())

# Data import --------------------------------------------------------------------



dados_srvyr_2021<-as_survey(pnadc_design(read_pnadc("./PNADC_042021.txt","./input/input_PNADC_trimestral.txt",vars=c(
  'V1022', # Situação do Domicílio: 1- Urbano 2-Rural
  'V2005', #Situação da pessoa no domicílio'01'-Pessoa responsavel pelo domicilio/02-Conjuge/03-Conjuge
  'V2007', # Sexo
  'V2009', #Idade
  'VD4002', # Condição de ocupação 1. Ocupadas/2. Desocupadas/NA
  'VD4010' #Grupramento de atividades
  ))))

dados_srvyr_2020<-as_survey(pnadc_design(read_pnadc("./PNADC_042020.txt","./input/input_PNADC_trimestral.txt",vars=c(
  'V1022', # Situação do Domicílio: 1- Urbano 2-Rural
  'V2005', #Situação da pessoa no domicílio'01'-Pessoa responsavel pelo domicilio/02-Conjuge/03-Conjuge
  'V2007', # Sexo
  'V2009', #Idade
  'VD4002', # Condição de ocupação 1. Ocupadas/2. Desocupadas/NA
  'VD4010' #Grupramento de atividades
))))


dados_srvyr_2019<-as_survey(pnadc_design(read_pnadc("./PNADC_042019.txt","./input/input_PNADC_trimestral.txt",vars=c(
  'V1022', # Situação do Domicílio: 1- Urbano 2-Rural
  'V2005', #Situação da pessoa no domicílio'01'-Pessoa responsavel pelo domicilio/02-Conjuge/03-Conjuge
  'V2007', # Sexo
  'V2009', #Idade
  'VD4002', # Condição de ocupação 1. Ocupadas/2. Desocupadas/NA
  'VD4010' #Grupramento de atividades
))))

dados_srvyr_2018<-as_survey(pnadc_design(read_pnadc("./PNADC_042018.txt","./input/input_PNADC_trimestral.txt",vars=c(
  'V1022', # Situação do Domicílio: 1- Urbano 2-Rural
  'V2005', #Situação da pessoa no domicílio'01'-Pessoa responsavel pelo domicilio/02-Conjuge/03-Conjuge
  'V2007', # Sexo
  'V2009', #Idade
  'VD4002', # Condição de ocupação 1. Ocupadas/2. Desocupadas/NA
  'VD4010' #Grupramento de atividades
))))



# Functions to aggregate data from 100+ people -----------------------------


mais_de_100 <- function(pea_Data){
  idade_cent <-  pea_Data %>% mutate(idade_cent = if_else(V2009>=100,1,0)) %>% group_by(
    idade_cent) %>% summarise(V2009,PEA_Total,total_cent = sum(PEA_Total)) %>% select(
      V2009,PEA_Total,idade_cent,total_cent)
  
  pea_Data <-  bind_cols(pea_Data,idade_cent$total_cent)
  pea_Data <- pea_Data %>% filter(V2009<=100) %>% mutate(PEA_Total = if_else(V2009==100,`...3`,PEA_Total)) %>% select(V2009,PEA_Total)
  return(pea_Data)
}


rename_PNAD <- function(pea_Data,ano){
  names(pea_Data) <- c('idade',paste0('x',ano))
  return(pea_Data)
}


# 2021 --------------------------------------------------------------------

# Define PEA como sendo a população ocupada ou desocupada
dados_srvyr_2021<- dados_srvyr_2021 %>% mutate(PEA = if_else(VD4002 %in% c('1','2'),1,0))

PEA_H_Urb_2021 <- PEA_H_Urb_2021 %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))
PEA_H_Urb_2021 <- dados_srvyr_2021 %>% filter(V2007=='1',V1022=='1') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))
PEA_H_Rur_2021 <- dados_srvyr_2021 %>% filter(V2007=='1',V1022=='2') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))
PEA_M_Urb_2021 <- dados_srvyr_2021 %>% filter(V2007=='2',V1022=='1') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))
PEA_M_Rur_2021 <- dados_srvyr_2021 %>% filter(V2007=='2',V1022=='2') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))

PEA_H_Urb_2021<- mais_de_100(PEA_H_Urb_2021)
PEA_H_Rur_2021<- mais_de_100(PEA_H_Rur_2021)
PEA_M_Urb_2021<- mais_de_100(PEA_M_Urb_2021)
PEA_M_Rur_2021<- mais_de_100(PEA_M_Rur_2021)

PEA_H_Urb_2021<- rename_PNAD(PEA_H_Urb_2021,2021)
PEA_H_Rur_2021<- rename_PNAD(PEA_H_Rur_2021,2021)
PEA_M_Urb_2021<- rename_PNAD(PEA_M_Urb_2021,2021)
PEA_M_Rur_2021<- rename_PNAD(PEA_M_Rur_2021,2021)

# 2020 --------------------------------------------------------------------

# Define PEA como sendo a população ocupada ou desocupada
dados_srvyr_2020<- dados_srvyr_2020 %>% mutate(PEA = if_else(VD4002 %in% c('1','2'),1,0))

PEA_H_Urb_2020 <- dados_srvyr_2020 %>% filter(V2007=='1',V1022=='1') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))
PEA_H_Rur_2020 <- dados_srvyr_2020 %>% filter(V2007=='1',V1022=='2') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))
PEA_M_Urb_2020 <- dados_srvyr_2020 %>% filter(V2007=='2',V1022=='1') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))
PEA_M_Rur_2020 <- dados_srvyr_2020 %>% filter(V2007=='2',V1022=='2') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))

PEA_H_Urb_2020<- mais_de_100(PEA_H_Urb_2020)
PEA_H_Rur_2020<- mais_de_100(PEA_H_Rur_2020)
PEA_M_Urb_2020<- mais_de_100(PEA_M_Urb_2020)
PEA_M_Rur_2020<- mais_de_100(PEA_M_Rur_2020)

PEA_H_Urb_2020<- rename_PNAD(PEA_H_Urb_2020,2020)
PEA_H_Rur_2020<- rename_PNAD(PEA_H_Rur_2020,2020)
PEA_M_Urb_2020<- rename_PNAD(PEA_M_Urb_2020,2020)
PEA_M_Rur_2020<- rename_PNAD(PEA_M_Rur_2020,2020)

# 2019 --------------------------------------------------------------------

# Define PEA como sendo a população ocupada ou desocupada
dados_srvyr_2019<- dados_srvyr_2019 %>% mutate(PEA = if_else(VD4002 %in% c('1','2'),1,0))

PEA_H_Urb_2019 <- dados_srvyr_2019 %>% filter(V2007=='1',V1022=='1') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))
PEA_H_Rur_2019 <- dados_srvyr_2019 %>% filter(V2007=='1',V1022=='2') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))
PEA_M_Urb_2019 <- dados_srvyr_2019 %>% filter(V2007=='2',V1022=='1') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))
PEA_M_Rur_2019 <- dados_srvyr_2019 %>% filter(V2007=='2',V1022=='2') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))

PEA_H_Urb_2019<- mais_de_100(PEA_H_Urb_2019)
PEA_H_Rur_2019<- mais_de_100(PEA_H_Rur_2019)
PEA_M_Urb_2019<- mais_de_100(PEA_M_Urb_2019)
PEA_M_Rur_2019<- mais_de_100(PEA_M_Rur_2019)

PEA_H_Urb_2019<- rename_PNAD(PEA_H_Urb_2019,2019)
PEA_H_Rur_2019<- rename_PNAD(PEA_H_Rur_2019,2019)
PEA_M_Urb_2019<- rename_PNAD(PEA_M_Urb_2019,2019)
PEA_M_Rur_2019<- rename_PNAD(PEA_M_Rur_2019,2019)

# 2018 --------------------------------------------------------------------

# Define PEA como sendo a população ocupada ou desocupada
dados_srvyr_2018<- dados_srvyr_2018 %>% mutate(PEA = if_else(VD4002 %in% c('1','2'),1,0))

PEA_H_Urb_2018 <- dados_srvyr_2018 %>% filter(V2007=='1',V1022=='1') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))
PEA_H_Rur_2018 <- dados_srvyr_2018 %>% filter(V2007=='1',V1022=='2') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))
PEA_M_Urb_2018 <- dados_srvyr_2018 %>% filter(V2007=='2',V1022=='1') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))
PEA_M_Rur_2018 <- dados_srvyr_2018 %>% filter(V2007=='2',V1022=='2') %>% group_by(V2009) %>% summarise(PEA_Total =survey_total(PEA)) %>% select(!PEA_Total_se) %>%  mutate(PEA_Total=round(PEA_Total,0))

PEA_H_Urb_2018<- mais_de_100(PEA_H_Urb_2018)
PEA_H_Rur_2018<- mais_de_100(PEA_H_Rur_2018)
PEA_M_Urb_2018<- mais_de_100(PEA_M_Urb_2018)
PEA_M_Rur_2018<- mais_de_100(PEA_M_Rur_2018)

PEA_H_Urb_2018<- rename_PNAD(PEA_H_Urb_2018,2018)
PEA_H_Rur_2018<- rename_PNAD(PEA_H_Rur_2018,2018)
PEA_M_Urb_2018<- rename_PNAD(PEA_M_Urb_2018,2018)
PEA_M_Rur_2018<- rename_PNAD(PEA_M_Rur_2018,2018)



# Join all the years ------------------------------------------------------

PEA_H_Urb <- cbind(PEA_H_Urb_2018,PEA_H_Urb_2019$x2019,PEA_H_Urb_2020$x2020,PEA_H_Urb_2021$x2021)
names(PEA_H_Urb) <- c('idade',2018,2019,2020,2021)
PEA_H_Rur <- cbind(PEA_H_Rur_2018,PEA_H_Rur_2019$x2019,PEA_H_Rur_2020$x2020,PEA_H_Rur_2021$x2021)
names(PEA_H_Rur) <- c('idade',2018,2019,2020,2021)
PEA_M_Urb <- cbind(PEA_M_Urb_2018,PEA_M_Urb_2019$x2019,PEA_M_Urb_2020$x2020,PEA_M_Urb_2021$x2021)
names(PEA_M_Urb) <- c('idade',2018,2019,2020,2021)
PEA_M_Rur <- cbind(PEA_M_Rur_2018,PEA_M_Rur_2019$x2019,PEA_M_Rur_2020$x2020,PEA_M_Rur_2021$x2021)
names(PEA_M_Rur) <- c('idade',2018,2019,2020,2021)








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
  

