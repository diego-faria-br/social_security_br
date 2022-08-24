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
  'V2007', # Sexo
  'V2009', #Idade
  'VD4012' #Contribuição para Previdencia 1-Contribuinte/2-Não Contribuinte
  ))))

dados_srvyr_2020<-as_survey(pnadc_design(read_pnadc("./PNADC_042020.txt","./input/input_PNADC_trimestral.txt",vars=c(
  'V1022', # Situação do Domicílio: 1- Urbano 2-Rural
  'V2007', # Sexo
  'V2009', #Idade
  'VD4012' #Contribuição para Previdencia 1-Contribuinte/2-Não Contribuinte
))))


dados_srvyr_2019<-as_survey(pnadc_design(read_pnadc("./PNADC_042019.txt","./input/input_PNADC_trimestral.txt",vars=c(
  'V1022', # Situação do Domicílio: 1- Urbano 2-Rural
  'V2007', # Sexo
  'V2009', #Idade
  'VD4012' #Contribuição para Previdencia 1-Contribuinte/2-Não Contribuinte
))))

dados_srvyr_2018<-as_survey(pnadc_design(read_pnadc("./PNADC_042018.txt","./input/input_PNADC_trimestral.txt",vars=c(
  'V1022', # Situação do Domicílio: 1- Urbano 2-Rural
  'V2007', # Sexo
  'V2009', #Idade
  'VD4012' #Contribuição para Previdencia 1-Contribuinte/2-Não Contribuinte
))))



# Functions to aggregate data from 100+ people -----------------------------


mais_de_100 <- function(pea_Data){
  idade_cent <-  pea_Data %>% mutate(idade_cent = if_else(V2009>=100,1,0)) %>% group_by(
    idade_cent) %>% summarise(V2009,Cont_Total,total_cent = sum(Cont_Total)) %>% select(
      V2009,Cont_Total,idade_cent,total_cent)
  
  pea_Data <-  bind_cols(pea_Data,idade_cent$total_cent)
  pea_Data <- pea_Data %>% filter(V2009<=100) %>% mutate(Cont_Total = if_else(V2009==100,`...3`,Cont_Total)) %>% select(V2009,Cont_Total)
  return(pea_Data)
}


rename_PNAD <- function(pea_Data,ano){
  names(pea_Data) <- c('idade',paste0('x',ano))
  return(pea_Data)
}


# 2021 --------------------------------------------------------------------

# Define PEA como sendo a população ocupada ou desocupada
dados_srvyr_2021<- dados_srvyr_2021 %>% mutate(Cont = if_else(VD4012 == '1',1,0))

Cont_H_Urb_2021 <- dados_srvyr_2021 %>% filter(V2007=='1',V1022=='1') %>% group_by(V2009) %>% summarise(Cont_Total =survey_total(Cont, na.rm = TRUE)) %>% select(!Cont_Total_se) %>%  mutate(Cont_Total=round(Cont_Total,0))
Cont_H_Rur_2021 <- dados_srvyr_2021 %>% filter(V2007=='1',V1022=='2') %>% group_by(V2009) %>% summarise(Cont_Total =survey_total(Cont, na.rm = TRUE)) %>% select(!Cont_Total_se) %>%  mutate(Cont_Total=round(Cont_Total,0))
Cont_M_Urb_2021 <- dados_srvyr_2021 %>% filter(V2007=='2',V1022=='1') %>% group_by(V2009) %>% summarise(Cont_Total =survey_total(Cont, na.rm = TRUE)) %>% select(!Cont_Total_se) %>%  mutate(Cont_Total=round(Cont_Total,0))
Cont_M_Rur_2021 <- dados_srvyr_2021 %>% filter(V2007=='2',V1022=='2') %>% group_by(V2009) %>% summarise(Cont_Total =survey_total(Cont, na.rm = TRUE)) %>% select(!Cont_Total_se) %>%  mutate(Cont_Total=round(Cont_Total,0))

Cont_H_Urb_2021<- mais_de_100(Cont_H_Urb_2021)
Cont_H_Rur_2021<- mais_de_100(Cont_H_Rur_2021)
Cont_M_Urb_2021<- mais_de_100(Cont_M_Urb_2021)
Cont_M_Rur_2021<- mais_de_100(Cont_M_Rur_2021)

Cont_H_Urb_2021<- rename_PNAD(Cont_H_Urb_2021,2021)
Cont_H_Rur_2021<- rename_PNAD(Cont_H_Rur_2021,2021)
Cont_M_Urb_2021<- rename_PNAD(Cont_M_Urb_2021,2021)
Cont_M_Rur_2021<- rename_PNAD(Cont_M_Rur_2021,2021)

# 2020 --------------------------------------------------------------------

# Define Cont como sendo a população ocupada ou desocupada
dados_srvyr_2020<- dados_srvyr_2020 %>% mutate(Cont = if_else(VD4012 == '1',1,0))

Cont_H_Urb_2020 <- dados_srvyr_2020 %>% filter(V2007=='1',V1022=='1') %>% group_by(V2009) %>% summarise(Cont_Total =survey_total(Cont, na.rm = TRUE)) %>% select(!Cont_Total_se) %>%  mutate(Cont_Total=round(Cont_Total,0))
Cont_H_Rur_2020 <- dados_srvyr_2020 %>% filter(V2007=='1',V1022=='2') %>% group_by(V2009) %>% summarise(Cont_Total =survey_total(Cont, na.rm = TRUE)) %>% select(!Cont_Total_se) %>%  mutate(Cont_Total=round(Cont_Total,0))
Cont_M_Urb_2020 <- dados_srvyr_2020 %>% filter(V2007=='2',V1022=='1') %>% group_by(V2009) %>% summarise(Cont_Total =survey_total(Cont, na.rm = TRUE)) %>% select(!Cont_Total_se) %>%  mutate(Cont_Total=round(Cont_Total,0))
Cont_M_Rur_2020 <- dados_srvyr_2020 %>% filter(V2007=='2',V1022=='2') %>% group_by(V2009) %>% summarise(Cont_Total =survey_total(Cont, na.rm = TRUE)) %>% select(!Cont_Total_se) %>%  mutate(Cont_Total=round(Cont_Total,0))

Cont_H_Urb_2020<- mais_de_100(Cont_H_Urb_2020)
Cont_H_Rur_2020<- mais_de_100(Cont_H_Rur_2020)
Cont_M_Urb_2020<- mais_de_100(Cont_M_Urb_2020)
Cont_M_Rur_2020<- mais_de_100(Cont_M_Rur_2020)

Cont_H_Urb_2020<- rename_PNAD(Cont_H_Urb_2020,2020)
Cont_H_Rur_2020<- rename_PNAD(Cont_H_Rur_2020,2020)
Cont_M_Urb_2020<- rename_PNAD(Cont_M_Urb_2020,2020)
Cont_M_Rur_2020<- rename_PNAD(Cont_M_Rur_2020,2020)

# 2019 --------------------------------------------------------------------

# Define Cont como sendo a população ocupada ou desocupada
dados_srvyr_2019<- dados_srvyr_2019 %>% mutate(Cont = if_else(VD4012 == '1',1,0))

Cont_H_Urb_2019 <- dados_srvyr_2019 %>% filter(V2007=='1',V1022=='1') %>% group_by(V2009) %>% summarise(Cont_Total =survey_total(Cont, na.rm = TRUE)) %>% select(!Cont_Total_se) %>%  mutate(Cont_Total=round(Cont_Total,0))
Cont_H_Rur_2019 <- dados_srvyr_2019 %>% filter(V2007=='1',V1022=='2') %>% group_by(V2009) %>% summarise(Cont_Total =survey_total(Cont, na.rm = TRUE)) %>% select(!Cont_Total_se) %>%  mutate(Cont_Total=round(Cont_Total,0))
Cont_M_Urb_2019 <- dados_srvyr_2019 %>% filter(V2007=='2',V1022=='1') %>% group_by(V2009) %>% summarise(Cont_Total =survey_total(Cont, na.rm = TRUE)) %>% select(!Cont_Total_se) %>%  mutate(Cont_Total=round(Cont_Total,0))
Cont_M_Rur_2019 <- dados_srvyr_2019 %>% filter(V2007=='2',V1022=='2') %>% group_by(V2009) %>% summarise(Cont_Total =survey_total(Cont, na.rm = TRUE)) %>% select(!Cont_Total_se) %>%  mutate(Cont_Total=round(Cont_Total,0))

Cont_H_Urb_2019<- mais_de_100(Cont_H_Urb_2019)
Cont_H_Rur_2019<- mais_de_100(Cont_H_Rur_2019)
Cont_M_Urb_2019<- mais_de_100(Cont_M_Urb_2019)
Cont_M_Rur_2019<- mais_de_100(Cont_M_Rur_2019)

Cont_H_Urb_2019<- rename_PNAD(Cont_H_Urb_2019,2019)
Cont_H_Rur_2019<- rename_PNAD(Cont_H_Rur_2019,2019)
Cont_M_Urb_2019<- rename_PNAD(Cont_M_Urb_2019,2019)
Cont_M_Rur_2019<- rename_PNAD(Cont_M_Rur_2019,2019)

# 2018 --------------------------------------------------------------------

# Define Cont como sendo a população ocupada ou desocupada
dados_srvyr_2018<- dados_srvyr_2018 %>% mutate(Cont = if_else(VD4012 == '1',1,0))

Cont_H_Urb_2018 <- dados_srvyr_2018 %>% filter(V2007=='1',V1022=='1') %>% group_by(V2009) %>% summarise(Cont_Total =survey_total(Cont, na.rm = TRUE)) %>% select(!Cont_Total_se) %>%  mutate(Cont_Total=round(Cont_Total,0))
Cont_H_Rur_2018 <- dados_srvyr_2018 %>% filter(V2007=='1',V1022=='2') %>% group_by(V2009) %>% summarise(Cont_Total =survey_total(Cont, na.rm = TRUE)) %>% select(!Cont_Total_se) %>%  mutate(Cont_Total=round(Cont_Total,0))
Cont_M_Urb_2018 <- dados_srvyr_2018 %>% filter(V2007=='2',V1022=='1') %>% group_by(V2009) %>% summarise(Cont_Total =survey_total(Cont, na.rm = TRUE)) %>% select(!Cont_Total_se) %>%  mutate(Cont_Total=round(Cont_Total,0))
Cont_M_Rur_2018 <- dados_srvyr_2018 %>% filter(V2007=='2',V1022=='2') %>% group_by(V2009) %>% summarise(Cont_Total =survey_total(Cont, na.rm = TRUE)) %>% select(!Cont_Total_se) %>%  mutate(Cont_Total=round(Cont_Total,0))

Cont_H_Urb_2018<- mais_de_100(Cont_H_Urb_2018)
Cont_H_Rur_2018<- mais_de_100(Cont_H_Rur_2018)
Cont_M_Urb_2018<- mais_de_100(Cont_M_Urb_2018)
Cont_M_Rur_2018<- mais_de_100(Cont_M_Rur_2018)

Cont_H_Urb_2018<- rename_PNAD(Cont_H_Urb_2018,2018)
Cont_H_Rur_2018<- rename_PNAD(Cont_H_Rur_2018,2018)
Cont_M_Urb_2018<- rename_PNAD(Cont_M_Urb_2018,2018)
Cont_M_Rur_2018<- rename_PNAD(Cont_M_Rur_2018,2018)



# Join all the years ------------------------------------------------------

Cont_H_Urb <- cbind(Cont_H_Urb_2018,Cont_H_Urb_2019$x2019,Cont_H_Urb_2020$x2020,Cont_H_Urb_2021$x2021)
names(Cont_H_Urb) <- c('idade',2018,2019,2020,2021)
Cont_H_Rur <- cbind(Cont_H_Rur_2018,Cont_H_Rur_2019$x2019,Cont_H_Rur_2020$x2020,Cont_H_Rur_2021$x2021)
names(Cont_H_Rur) <- c('idade',2018,2019,2020,2021)
Cont_M_Urb <- cbind(Cont_M_Urb_2018,Cont_M_Urb_2019$x2019,Cont_M_Urb_2020$x2020,Cont_M_Urb_2021$x2021)
names(Cont_M_Urb) <- c('idade',2018,2019,2020,2021)
Cont_M_Rur <- cbind(Cont_M_Rur_2018,Cont_M_Rur_2019$x2019,Cont_M_Rur_2020$x2020,Cont_M_Rur_2021$x2021)
names(Cont_M_Rur) <- c('idade',2018,2019,2020,2021)


# Writing in file routine -------------------------------------------------


wb <- createWorkbook()

addWorksheet(wb,sheetName = 'Cont_H_Urb')
addWorksheet(wb,sheetName = 'Cont_H_Rur')
addWorksheet(wb,sheetName = 'Cont_M_Urb')
addWorksheet(wb,sheetName = 'Cont_M_Rur')

writeData(wb,sheet = 1,Cont_H_Urb)
writeData(wb,sheet = 2,Cont_H_Rur)
writeData(wb,sheet = 3,Cont_M_Urb)
writeData(wb,sheet = 4,Cont_M_Rur)

saveWorkbook(wb,"Cont_2018_2021.xlsx",overwrite = TRUE)
  

