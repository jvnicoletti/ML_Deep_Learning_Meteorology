library(tidyverse)
library(stringr)
library(openxlsx)
library(lubridate)
library(SciViews)

setwd('G:/My Drive/ESALQ/8º semestre/Meteorologia Aplicada/YG/YG_final')

diretorio = 'G:/My Drive/ESALQ/8º semestre/Meteorologia Aplicada/YG/YG_final/';diretorio

lista_csv = list.files(diretorio,full.names = T,pattern = "PPf");lista_csv
lista_PR = list.files(diretorio,full.names = T,pattern = "PR");lista_PR

PR_csv = read.xlsx(lista_PR[1],check.names = FALSE);PR_csv
row.names(PR_csv) = PR_csv[,1];PR_csv
colnames(PR_csv) = gsub("\\."," ",colnames(PR_csv))
PR_csv[,1] = NULL

csv = read.csv(lista_csv[1],row.names = NULL);csv

cidades = unique(csv$CIDADE);cidades
j=1;j

while (j<= length(cidades)) {
  if(j==1){
  k = 1;k
  l = 1;l
  csv_cidade = subset(csv,CIDADE == cidades[j]);csv_cidade
  PR_cidades = PR_csv[colnames(PR_csv) == cidades[j]];PR_cidades
  PR_cidades[is.na(PR_cidades)] = 0;PR_cidades
  PR_cidades_media = mean(PR_cidades[PR_cidades[,1]>0,1]);PR_cidades_media
while(k<=29){
  if(k==1){
    
        ciclo = csv_cidade[seq(l,(l+129),by=1),];ciclo
    
        PAest = as.numeric(ciclo$PPf_ac[length(ciclo$PPf_ac)])*(1 - as.numeric(0.6)*(1-mean(as.numeric(ciclo$ETR_ETC[1:19]))));PAest
        PAveg = as.numeric(PAest)*(1 - as.numeric(0.6)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[20:51])))));PAveg
        PAflo = as.numeric(PAveg)*(1 - as.numeric(1.2)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[52:72])))));PAflo
        PAfru = as.numeric(PAflo)*(1 - as.numeric(1)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[73:108])))));PAfru
        PAmat = as.numeric(PAfru)*(1 - as.numeric(0.3)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[109:130])))));PAmat
        PAmat_sc = as.numeric(PAfru)*(1 - as.numeric(0.3)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[109:130])))))/60;PAmat_sc
        YGDH  =  (as.numeric(ciclo$PPf_ac[length(ciclo$PPf_ac)]) - PAmat);YGDH
        YGDH_sc  =  (as.numeric(ciclo$PPf_ac[length(ciclo$PPf_ac)]) - PAmat)/60;YGDH_sc
        PP_SC = as.numeric(ciclo$PPf_ac[length(ciclo$PPf_ac)])/60;PP_SC
        EC = (PAmat_sc/PP_SC)*100;EC
        PR = ifelse(PR_cidades[k,1] == 0,yes = PR_cidades_media,no = PR_cidades[k,1])
        PR_sc = PR/60
        YGDM = PAmat - PR;YGDM
        YGDM_sc = YGDM/60;YGDM_sc
        EA = (PR/PAmat)*100;EA
        Ex_08 = cbind(PAest,PAveg,PAflo,PAfru,PAmat,PAmat_sc,YGDH,YGDH_sc,PP_SC,EC,PR,PR_sc,YGDM,YGDM_sc,EA,cidades[j]);Ex_08  
        row.names(Ex_08) = paste0(ciclo$YYYYMMDD[1]," - ",ciclo$YYYYMMDD[130]);Ex_08
        k = k+1;k
        l = l+130;l
    }else{
    
    
    ciclo = csv_cidade[seq(l,(l+129),by=1),];ciclo
    
    PAest = as.numeric(ciclo$PPf_ac[length(ciclo$PPf_ac)])*(1 - as.numeric(0.6)*(1-mean(as.numeric(ciclo$ETR_ETC[1:19]))));PAest
    PAveg = as.numeric(PAest)*(1 - as.numeric(0.6)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[20:51])))));PAveg
    PAflo = as.numeric(PAveg)*(1 - as.numeric(1.2)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[52:72])))));PAflo
    PAfru = as.numeric(PAflo)*(1 - as.numeric(1)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[73:108])))));PAfru
    PAmat = as.numeric(PAfru)*(1 - as.numeric(0.3)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[109:130])))));PAmat
    PAmat_sc = as.numeric(PAfru)*(1 - as.numeric(0.3)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[109:130])))))/60;PAmat_sc
    YGDH  =  (as.numeric(ciclo$PPf_ac[length(ciclo$PPf_ac)]) - PAmat);YGDH
    YGDH_sc  =  (as.numeric(ciclo$PPf_ac[length(ciclo$PPf_ac)]) - PAmat)/60;YGDH_sc
    PP_SC = as.numeric(ciclo$PPf_ac[length(ciclo$PPf_ac)])/60;PP_SC
    EC = (PAmat_sc/PP_SC)*100;EC
    PR = ifelse(PR_cidades[k,1] == 0,yes = PR_cidades_media,no = PR_cidades[k,1])
    PR_sc = PR/60
    YGDM = PAmat - PR;YGDM
    YGDM_sc = YGDM/60;YGDM_sc
    EA = (PR/PAmat)*100;EA
    Ex_08_parcial = cbind(PAest,PAveg,PAflo,PAfru,PAmat,PAmat_sc,YGDH,YGDH_sc,PP_SC,EC,PR,PR_sc,YGDM,YGDM_sc,EA,cidades[j]);Ex_08_parcial
    row.names(Ex_08_parcial) = paste0(ciclo$YYYYMMDD[1]," - ",ciclo$YYYYMMDD[130]);Ex_08_parcial
    Ex_08 = rbind(Ex_08,Ex_08_parcial)
    k = k+1;k
    l = l+130;l
    }}
  EX_08_cidades_final = Ex_08;EX_08_cidades_final
  j=j+1;j
  }else{
    l = 1;l
    k=1;k
    csv_cidade = subset(csv,CIDADE == cidades[j]);csv_cidade
    PR_cidades = PR_csv[colnames(PR_csv) == cidades[j]];PR_cidades
    PR_cidades[is.na(PR_cidades)] = 0;PR_cidades
    PR_cidades_media = mean(PR_cidades[PR_cidades[,1]>0,1]);PR_cidades_media
    while(k<=29){
      if(k==1){
        
        ciclo = csv_cidade[seq(l,(l+129),by=1),];ciclo
        
        PAest = as.numeric(ciclo$PPf_ac[length(ciclo$PPf_ac)])*(1 - as.numeric(0.6)*(1-mean(as.numeric(ciclo$ETR_ETC[1:19]))));PAest
        PAveg = as.numeric(PAest)*(1 - as.numeric(0.6)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[20:51])))));PAveg
        PAflo = as.numeric(PAveg)*(1 - as.numeric(1.2)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[52:72])))));PAflo
        PAfru = as.numeric(PAflo)*(1 - as.numeric(1)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[73:108])))));PAfru
        PAmat = as.numeric(PAfru)*(1 - as.numeric(0.3)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[109:130])))));PAmat
        PAmat_sc = as.numeric(PAfru)*(1 - as.numeric(0.3)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[109:130])))))/60;PAmat_sc
        YGDH  =  (as.numeric(ciclo$PPf_ac[length(ciclo$PPf_ac)]) - PAmat);YGDH
        YGDH_sc  =  (as.numeric(ciclo$PPf_ac[length(ciclo$PPf_ac)]) - PAmat)/60;YGDH_sc
        PP_SC = as.numeric(ciclo$PPf_ac[length(ciclo$PPf_ac)])/60;PP_SC
        EC = (PAmat_sc/PP_SC)*100;EC
        PR = ifelse(PR_cidades[k,1] == 0,yes = PR_cidades_media,no = PR_cidades[k,1])
        PR_sc = PR/60
        YGDM = PAmat - PR;YGDM
        YGDM_sc = YGDM/60;YGDM_sc
        EA = (PR/PAmat)*100;EA
        Ex_08 = cbind(PAest,PAveg,PAflo,PAfru,PAmat,PAmat_sc,YGDH,YGDH_sc,PP_SC,EC,PR,PR_sc,YGDM,YGDM_sc,EA,cidades[j]);Ex_08  
        row.names(Ex_08) = paste0(ciclo$YYYYMMDD[1]," - ",ciclo$YYYYMMDD[130]);Ex_08
        k = k+1;k
        l = l+130;l
      }else{
        
        
        ciclo = csv_cidade[seq(l,(l+129),by=1),];ciclo
        
        PAest = as.numeric(ciclo$PPf_ac[length(ciclo$PPf_ac)])*(1 - as.numeric(0.6)*(1-mean(as.numeric(ciclo$ETR_ETC[1:19]))));PAest
        PAveg = as.numeric(PAest)*(1 - as.numeric(0.6)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[20:51])))));PAveg
        PAflo = as.numeric(PAveg)*(1 - as.numeric(1.2)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[52:72])))));PAflo
        PAfru = as.numeric(PAflo)*(1 - as.numeric(1)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[73:108])))));PAfru
        PAmat = as.numeric(PAfru)*(1 - as.numeric(0.3)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[109:130])))));PAmat
        PAmat_sc = as.numeric(PAfru)*(1 - as.numeric(0.3)*(1-as.numeric(mean(as.numeric(ciclo$ETR_ETC[109:130])))))/60;PAmat_sc
        YGDH  =  (as.numeric(ciclo$PPf_ac[length(ciclo$PPf_ac)]) - PAmat);YGDH
        YGDH_sc  =  (as.numeric(ciclo$PPf_ac[length(ciclo$PPf_ac)]) - PAmat)/60;YGDH_sc
        PP_SC = as.numeric(ciclo$PPf_ac[length(ciclo$PPf_ac)])/60;PP_SC
        EC = (PAmat_sc/PP_SC)*100;EC
        PR = ifelse(PR_cidades[k,1] == 0,yes = PR_cidades_media,no = PR_cidades[k,1])
        PR_sc = PR/60
        YGDM = PAmat - PR;YGDM
        YGDM_sc = YGDM/60;YGDM_sc
        EA = (PR/PAmat)*100;EA
        Ex_08_parcial = cbind(PAest,PAveg,PAflo,PAfru,PAmat,PAmat_sc,YGDH,YGDH_sc,PP_SC,EC,PR,PR_sc,YGDM,YGDM_sc,EA,cidades[j]);Ex_08_parcial
        row.names(Ex_08_parcial) = paste0(ciclo$YYYYMMDD[1]," - ",ciclo$YYYYMMDD[130]);Ex_08_parcial
        Ex_08 = rbind(Ex_08,Ex_08_parcial)
        k = k+1;k
        l = l+130;l
      }}
    EX_08_cidades_parcial = Ex_08;EX_08_cidades_parcial
    EX_08_cidades_final = rbind(EX_08_cidades_final,EX_08_cidades_parcial)
    j=j+1;j
  }}
write.csv(EX_08_cidades_final,paste0(diretorio,"Ex_08_final.csv"))  
