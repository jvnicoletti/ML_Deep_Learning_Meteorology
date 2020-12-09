library(tidyverse)
library(stringr)
library(openxlsx)
library(lubridate)
library(SciViews)

setwd('G:/My Drive/ESALQ/8º semestre/Meteorologia Aplicada/YG/')

diretorio = 'G:/My Drive/ESALQ/8º semestre/Meteorologia Aplicada/YG/'

lista_csv = list.files(diretorio,full.names = T,pattern = "final.csv");lista_csv
lista_kc = list.files(diretorio,full.names = T,pattern = "Kc");lista_kc
lista_IAF = list.files(diretorio,full.names = T,pattern = "IAF");lista_IAF
lista_CAD = list.files(diretorio,full.names = T,pattern = "CAD");lista_CAD
lista_plantio = list.files(diretorio,full.names = T,pattern = "plantio");lista_plantio

csv = read.csv(lista_csv[1],row.names = NULL);csv
kc_soja = read.xlsx(lista_kc[1],rowNames = F);kc_soja
kc_soja = kc_soja[3:nrow(kc_soja),2];kc_soja
IAF_soja = read.xlsx(lista_IAF[1],rowNames = F);IAF_soja
csv_CAD = read.xlsx(lista_CAD);csv_CAD
csv_plantio = read.csv(lista_plantio);csv_plantio


lista_cidades = unique(csv$CIDADE);lista_cidades
k=1;k
while(k<=29){
  if(k==1){
    j = 1;j
    amostra = subset(csv_plantio,CIDADE == lista_cidades[j])
    data_plantio = as.Date(amostra$inicio,format = "%Y-%m-%d");data_plantio
    data_final   = as.Date(data_plantio,format = "%Y-%m-%d")+ duration(129,"days");data_final
    while(j<=length(lista_cidades)){
      if(j==1){
        csv_cidade = subset(csv,CIDADE == lista_cidades[j]);csv_cidade
        
        CAD = subset(csv_CAD, NASAPOWER == lista_cidades[j]);CAD = CAD$`CAD-ESTIMADA`*100;CAD
        Armazenamento_Anterior = CAD;Armazenamento_Anterior
        Neg_Acumulado_anterior = 0;Neg_Acumulado_anterior
        
        
        csv_cidade$YYYYMMDD = openxlsx::convertToDate(csv_cidade$YYYYMMDD,format ="%Y-%m-%d");csv_cidade$YYYYMMDD
        #csv_cidade$YYYYMMDD = as.POSIXct(csv$YYYYMMDD,format = "%Y-%m-%d");csv_cidade$YYYYMMDD
        
        
        ciclo = csv_cidade[csv_cidade$YYYYMMDD >= data_plantio & csv_cidade$YYYYMMDD <= data_final,];
        
        if(is.na(ciclo$X[1]) & is.na(ciclo$X[2])){
          ciclo = ciclo[-c(1,2),];ciclo
        }
        
        ciclo$kc = kc_soja;ciclo
        
        ciclo$ETC = as.numeric(ciclo$kc)*as.numeric(ciclo$Eto)
        ciclo$"P-ETC" = as.numeric(ciclo$P)-as.numeric(ciclo$ETC)
        
        
        i=1;i
        while (i<=length(ciclo$`P-ETC`)) {
          if(i<=2){
            ARM_primeiro = 0
            NAC_primeiro = 0
            if(ciclo$`P-ETC`[1]<0){
              NAC_primeiro = Neg_Acumulado_anterior+ciclo$`P-ETC`[1]
              ARM_primeiro = CAD*exp((NAC_primeiro/CAD))
            }else{
              if((Armazenamento_Anterior+ciclo$`P-ETC`[1])>CAD){
                ARM_primeiro = CAD
                NAC_primeiro = CAD*SciViews::ln((ARM_primeiro/CAD))
              }else{
                ARM_primeiro = Armazenamento_Anterior+ciclo$`P-ETC`[1]
                NAC_primeiro = CAD*SciViews::ln((ARM_primeiro/CAD))
              }
            }
            i = 2;i
            if(ciclo$`P-ETC`[i]<0){
              NAC_demais = NAC_primeiro+ciclo$`P-ETC`[i]
              ARM_demais = CAD*exp((NAC_demais/CAD))
            }else{
              if((ARM_primeiro+ciclo$`P-ETC`[i])>CAD){
                ARM_demais = CAD
                NAC_demais = CAD*SciViews::ln((ARM_demais/CAD))
              }else{
                ARM_demais = ARM_primeiro+ciclo$`P-ETC`[i]
                NAC_demais = CAD*SciViews::ln((ARM_demais/CAD))
              }
            }
            NAC_ARM = cbind(ARM_primeiro,NAC_primeiro);NAC_ARM
            NAC_ARM_demais = cbind(ARM_demais,NAC_demais);NAC_ARM_demais
            NAC_ARM = rbind(NAC_ARM,NAC_ARM_demais);NAC_ARM
            i=i+1;i
          }else{
            if(ciclo$`P-ETC`[i]<0){
              NAC_demais = NAC_demais+ciclo$`P-ETC`[i]
              ARM_demais = CAD*exp((NAC_demais/CAD))
            }else{
              if((ARM_demais+ciclo$`P-ETC`[i])>CAD){
                ARM_demais = CAD
                NAC_demais = CAD*SciViews::ln((ARM_demais/CAD))
              }else{
                ARM_demais = ARM_demais+ciclo$`P-ETC`[i]
                NAC_demais = CAD*SciViews::ln((ARM_demais/CAD))
              }
            }
            i=i+1;i
            NAC_ARM_parcial = cbind(ARM_demais,NAC_demais);NAC_ARM_parcial
            NAC_ARM = rbind(NAC_ARM,NAC_ARM_parcial);NAC_ARM
          }}
        NAC_ARM = as.data.frame(NAC_ARM);NAC_ARM
        names(NAC_ARM) = c("Arm","Nac");NAC_ARM
        ciclo = cbind(ciclo,NAC_ARM);ciclo
        
        i = 1;i
        while (i <= nrow(ciclo)) {
          if(i==1){
            ALT = ciclo$Arm[1] - Armazenamento_Anterior;ALT
            i=i+1;  
          }else{
            ALT_parcial = ciclo$Arm[i] - ciclo$Arm[i-1];ALT_parcial
            ALT = rbind(ALT,ALT_parcial);ALT
            i=i+1;i
          } 
        }
        ciclo$ALT = ALT;ciclo$ALT
        
        i=1;i
        while (i <= nrow(ciclo)) {
          if(i==1){
            if(ciclo$`P-ETC`[i] >= 0){
              ETR = ciclo$ETC[i]
              i = i+1
            }else{
              ETR = ciclo$P[i]+abs(ciclo$ALT[1])
              i = i+1
            }
          }else{
            if(ciclo$`P-ETC`[i] >= 0){
              ETR_parcial = ciclo$ETC[i]
              ETR = rbind(ETR,ETR_parcial)
              i = i+1
            }else{
              ETR_parcial = ciclo$P[i]+abs(ciclo$ALT[i])
              ETR = rbind(ETR,ETR_parcial)
              i = i+1
            }
          }}
        
        ciclo$ETR = ETR;ciclo
        
        ciclo$ETR_ETC = ciclo$ETR / ciclo$ETC;ciclo
        ciclo$DEF = ciclo$ETC - ciclo$ETR;ciclo
        ciclo$DEF_1 = ciclo$DEF*-1;ciclo
        
        i=1;i
        while (i<=nrow(ciclo)) {
          if(i==1){
            if(ciclo$Arm[i] == CAD){
              EXC = ciclo$`P-ETC`[i]-ciclo$ALT[i];EXC
              i=i+1  
            }else{
              EXC = 0  
              i =i+1;i
            }}else{
              if(ciclo$Arm[i] == CAD){
                EXC_parcial = ciclo$`P-ETC`[i]-ciclo$ALT[i];EXC
                EXC = rbind(EXC,EXC_parcial)
                i=i+1  
              }else{
                EXC_parcial = 0;EXC
                EXC = rbind(EXC,EXC_parcial)
                i=i+1 
              } 
            }}
        ciclo$EXC = EXC;ciclo
        ciclo$ARM_CAD = ciclo$Arm / CAD;ciclo
        ciclo$CTN = ifelse(ciclo$Tmed<16.5,yes = -0.0425+0.035*ciclo$Tmed+0.00325*ciclo$Tmed^2-0.0000925*ciclo$Tmed^3,no = 0.583+0.014*ciclo$Tmed+0.0013*ciclo$Tmed^2-0.000037*ciclo$Tmed^3);ciclo
        ciclo$CTC = ifelse(ciclo$Tmed<16.5,yes = -1.085+0.07*ciclo$Tmed+0.0065*ciclo$Tmed^2-0.000185*ciclo$Tmed^3,no = -0.0425+0.035*ciclo$Tmed+0.00325*ciclo$Tmed^2-0.0000925*ciclo$Tmed^3);ciclo
        ciclo$PPBn = ((31.7+5.234*ciclo$Q0)*(1-ciclo$n/ciclo$fotóperiodo)*ciclo$CTN)
        ciclo$PPBc = ((107.2+8.604*ciclo$Q0)*(ciclo$n/ciclo$fotóperiodo)*ciclo$CTC)
        ciclo$PPBp = (ciclo$PPBc + ciclo$PPBn);ciclo
        ciclo$IAF = IAF_soja[,1];ciclo
        ciclo$CIAF = ifelse((0.0186+0.37*(ciclo$IAF)-0.035*(ciclo$IAF^2))>1,yes = 1,no = (0.0186+0.37*(ciclo$IAF)-0.035*(ciclo$IAF^2)));
        Cresp = ifelse(ciclo$Tmed>=20,yes = 0.5,no = 0.6);ciclo$Cresp = Cresp;ciclo
        ciclo$Cc = 0.4;ciclo
        ciclo$Cum = 100/(100-13);ciclo
        ciclo$CN = ciclo$CIAF*ciclo$Cresp*ciclo$Cc*ciclo$Cum;ciclo
        ciclo$PPf = ciclo$PPBp*ciclo$CN;ciclo
        
        i=1;i
        while(i<=nrow(ciclo)){
          if(i==1){
            PPf_ac = ciclo$PPf[i];PPf_ac
            i=i+1
          }else{
            PPf_parcial = ciclo$PPf[i]+PPf_ac[i-1];PPf_parcial
            PPf_ac = rbind(PPf_ac,PPf_parcial);PPf_ac
            i=i+1;i
          }
        }
        ciclo$PPf_ac = PPf_ac;ciclo
        ciclo_final = ciclo
        j=j+1
      }else{
        csv_cidade = subset(csv,CIDADE == lista_cidades[j]);csv_cidade
        
        CAD = subset(csv_CAD, NASAPOWER == lista_cidades[j]);CAD = CAD$`CAD-ESTIMADA`*100;CAD
        Armazenamento_Anterior = CAD;Armazenamento_Anterior
        Neg_Acumulado_anterior = 0;Neg_Acumulado_anterior
        
        csv_cidade$YYYYMMDD = openxlsx::convertToDate(csv_cidade$YYYYMMDD,format ="%Y-%m-%d");csv_cidade$YYYYMMDD
        #csv_cidade$YYYYMMDD = as.POSIXct(csv$YYYYMMDD,format = "%Y-%m-%d");csv_cidade$YYYYMMDD
        
        
        ciclo = csv_cidade[csv_cidade$YYYYMMDD >= data_plantio & csv_cidade$YYYYMMDD <= data_final,];ciclo
        if(is.na(ciclo$X[1]) & is.na(ciclo$X[2])){
          ciclo = ciclo[-c(1,2),];ciclo
        }
        ciclo$kc = kc_soja;ciclo
        
        ciclo$ETC = as.numeric(ciclo$kc)*as.numeric(ciclo$Eto)
        ciclo$"P-ETC" = as.numeric(ciclo$P)-as.numeric(ciclo$ETC)
        
        
        i=1;i
        while (i<=length(ciclo$`P-ETC`)) {
          if(i<=2){
            ARM_primeiro = 0
            NAC_primeiro = 0
            if(ciclo$`P-ETC`[1]<0){
              NAC_primeiro = Neg_Acumulado_anterior+ciclo$`P-ETC`[1]
              ARM_primeiro = CAD*exp((NAC_primeiro/CAD))
            }else{
              if((Armazenamento_Anterior+ciclo$`P-ETC`[1])>CAD){
                ARM_primeiro = CAD
                NAC_primeiro = CAD*SciViews::ln((ARM_primeiro/CAD))
              }else{
                ARM_primeiro = Armazenamento_Anterior+ciclo$`P-ETC`[1]
                NAC_primeiro = CAD*SciViews::ln((ARM_primeiro/CAD))
              }
            }
            i = 2;i
            if(ciclo$`P-ETC`[i]<0){
              NAC_demais = NAC_primeiro+ciclo$`P-ETC`[i]
              ARM_demais = CAD*exp((NAC_demais/CAD))
            }else{
              if((ARM_primeiro+ciclo$`P-ETC`[i])>CAD){
                ARM_demais = CAD
                NAC_demais = CAD*SciViews::ln((ARM_demais/CAD))
              }else{
                ARM_demais = ARM_primeiro+ciclo$`P-ETC`[i]
                NAC_demais = CAD*SciViews::ln((ARM_demais/CAD))
              }
            }
            NAC_ARM = cbind(ARM_primeiro,NAC_primeiro);NAC_ARM
            NAC_ARM_demais = cbind(ARM_demais,NAC_demais);NAC_ARM_demais
            NAC_ARM = rbind(NAC_ARM,NAC_ARM_demais);NAC_ARM
            i=i+1;i
          }else{
            if(ciclo$`P-ETC`[i]<0){
              NAC_demais = NAC_demais+ciclo$`P-ETC`[i]
              ARM_demais = CAD*exp((NAC_demais/CAD))
            }else{
              if((ARM_demais+ciclo$`P-ETC`[i])>CAD){
                ARM_demais = CAD
                NAC_demais = CAD*SciViews::ln((ARM_demais/CAD))
              }else{
                ARM_demais = ARM_demais+ciclo$`P-ETC`[i]
                NAC_demais = CAD*SciViews::ln((ARM_demais/CAD))
              }
            }
            i=i+1;i
            NAC_ARM_parcial = cbind(ARM_demais,NAC_demais);NAC_ARM_parcial
            NAC_ARM = rbind(NAC_ARM,NAC_ARM_parcial);NAC_ARM
          }}
        NAC_ARM = as.data.frame(NAC_ARM);NAC_ARM
        names(NAC_ARM) = c("Arm","Nac");NAC_ARM
        ciclo = cbind(ciclo,NAC_ARM);ciclo
        
        i = 1;i
        while (i <= nrow(ciclo)) {
          if(i==1){
            ALT = ciclo$Arm[1] - Armazenamento_Anterior;ALT
            i=i+1;  
          }else{
            ALT_parcial = ciclo$Arm[i] - ciclo$Arm[i-1];ALT_parcial
            ALT = rbind(ALT,ALT_parcial);ALT
            i=i+1;i
          } 
        }
        ciclo$ALT = ALT;ciclo$ALT
        
        i=1;i
        while (i <= nrow(ciclo)) {
          if(i==1){
            if(ciclo$`P-ETC`[i] >= 0){
              ETR = ciclo$ETC[i]
              i = i+1
            }else{
              ETR = ciclo$P[i]+abs(ciclo$ALT[1])
              i = i+1
            }
          }else{
            if(ciclo$`P-ETC`[i] >= 0){
              ETR_parcial = ciclo$ETC[i]
              ETR = rbind(ETR,ETR_parcial)
              i = i+1
            }else{
              ETR_parcial = ciclo$P[i]+abs(ciclo$ALT[i])
              ETR = rbind(ETR,ETR_parcial)
              i = i+1
            }
          }}
        
        ciclo$ETR = ETR;ciclo
        
        ciclo$ETR_ETC = ciclo$ETR / ciclo$ETC;ciclo
        ciclo$DEF = ciclo$ETC - ciclo$ETR;ciclo
        ciclo$DEF_1 = ciclo$DEF*-1;ciclo
        
        i=1;i
        while (i<=nrow(ciclo)) {
          if(i==1){
            if(ciclo$Arm[i] == CAD){
              EXC = ciclo$`P-ETC`[i]-ciclo$ALT[i];EXC
              i=i+1  
            }else{
              EXC = 0  
              i =i+1;i
            }}else{
              if(ciclo$Arm[i] == CAD){
                EXC_parcial = ciclo$`P-ETC`[i]-ciclo$ALT[i];EXC
                EXC = rbind(EXC,EXC_parcial)
                i=i+1  
              }else{
                EXC_parcial = 0;EXC
                EXC = rbind(EXC,EXC_parcial)
                i=i+1 
              } 
            }}
        ciclo$EXC = EXC;ciclo
        ciclo$ARM_CAD = ciclo$Arm / CAD;ciclo
        ciclo$CTN = ifelse(ciclo$Tmed<16.5,yes = -0.0425+0.035*ciclo$Tmed+0.00325*ciclo$Tmed^2-0.0000925*ciclo$Tmed^3,no = 0.583+0.014*ciclo$Tmed+0.0013*ciclo$Tmed^2-0.000037*ciclo$Tmed^3);ciclo
        ciclo$CTC = ifelse(ciclo$Tmed<16.5,yes = -1.085+0.07*ciclo$Tmed+0.0065*ciclo$Tmed^2-0.000185*ciclo$Tmed^3,no = -0.0425+0.035*ciclo$Tmed+0.00325*ciclo$Tmed^2-0.0000925*ciclo$Tmed^3);ciclo
        ciclo$PPBn = ((31.7+5.234*ciclo$Q0)*(1-ciclo$n/ciclo$fotóperiodo)*ciclo$CTN)
        ciclo$PPBc = ((107.2+8.604*ciclo$Q0)*(ciclo$n/ciclo$fotóperiodo)*ciclo$CTC)
        ciclo$PPBp = (ciclo$PPBc + ciclo$PPBn);ciclo
        ciclo$IAF = IAF_soja[,1];ciclo
        ciclo$CIAF = ifelse((0.0186+0.37*(ciclo$IAF)-0.035*(ciclo$IAF^2))>1,yes = 1,no = (0.0186+0.37*(ciclo$IAF)-0.035*(ciclo$IAF^2)));
        Cresp = ifelse(ciclo$Tmed>=20,yes = 0.5,no = 0.6);ciclo$Cresp = Cresp;ciclo
        ciclo$Cc = 0.4;ciclo
        ciclo$Cum = 100/(100-13);ciclo
        ciclo$CN = ciclo$CIAF*ciclo$Cresp*ciclo$Cc*ciclo$Cum;ciclo
        ciclo$PPf = ciclo$PPBp*ciclo$CN;ciclo
        
        i=1;i
        while(i<=nrow(ciclo)){
          if(i==1){
            PPf_ac = ciclo$PPf[i];PPf_ac
            i=i+1
          }else{
            PPf_parcial = ciclo$PPf[i]+PPf_ac[i-1];PPf_parcial
            PPf_ac = rbind(PPf_ac,PPf_parcial);PPf_ac
            i=i+1;i
          }
        }
        ciclo$PPf_ac = PPf_ac;ciclo
        ciclo_parcial = ciclo
        ciclo_final = rbind(ciclo_parcial,ciclo_final)
        j=j+1
      }}
    ciclo_final_anos = ciclo_final
    k=k+1
  }else{
      j = 1;j
    data_plantio = data_plantio+duration(365,"day");data_plantio
    data_final   = data_plantio+duration(129,"day");data_final
while(j<=length(lista_cidades)){
  if(j==1){
csv_cidade = subset(csv,CIDADE == lista_cidades[j]);csv_cidade

csv_cidade$YYYYMMDD = openxlsx::convertToDate(csv_cidade$YYYYMMDD,format ="%Y-%m-%d");csv_cidade$YYYYMMDD
#csv_cidade$YYYYMMDD = as.POSIXct(csv$YYYYMMDD,format = "%Y-%m-%d");csv_cidade$YYYYMMDD


ciclo = csv_cidade[csv_cidade$YYYYMMDD >= data_plantio & csv_cidade$YYYYMMDD <= data_final,];

if(is.na(ciclo$X[1]) & is.na(ciclo$X[2])){
  ciclo = ciclo[-c(1,2),];ciclo
}

ciclo$kc = kc_soja;ciclo

ciclo$ETC = as.numeric(ciclo$kc)*as.numeric(ciclo$Eto)
ciclo$"P-ETC" = as.numeric(ciclo$P)-as.numeric(ciclo$ETC)


i=1;i
while (i<=length(ciclo$`P-ETC`)) {
if(i<=2){
  ARM_primeiro = 0
  NAC_primeiro = 0
  if(ciclo$`P-ETC`[1]<0){
    NAC_primeiro = Neg_Acumulado_anterior+ciclo$`P-ETC`[1]
    ARM_primeiro = CAD*exp((NAC_primeiro/CAD))
  }else{
    if((Armazenamento_Anterior+ciclo$`P-ETC`[1])>CAD){
      ARM_primeiro = CAD
      NAC_primeiro = CAD*SciViews::ln((ARM_primeiro/CAD))
    }else{
      ARM_primeiro = Armazenamento_Anterior+ciclo$`P-ETC`[1]
      NAC_primeiro = CAD*SciViews::ln((ARM_primeiro/CAD))
    }
  }
  i = 2;i
  if(ciclo$`P-ETC`[i]<0){
    NAC_demais = NAC_primeiro+ciclo$`P-ETC`[i]
    ARM_demais = CAD*exp((NAC_demais/CAD))
  }else{
    if((ARM_primeiro+ciclo$`P-ETC`[i])>CAD){
      ARM_demais = CAD
      NAC_demais = CAD*SciViews::ln((ARM_demais/CAD))
    }else{
      ARM_demais = ARM_primeiro+ciclo$`P-ETC`[i]
      NAC_demais = CAD*SciViews::ln((ARM_demais/CAD))
    }
  }
  NAC_ARM = cbind(ARM_primeiro,NAC_primeiro);NAC_ARM
  NAC_ARM_demais = cbind(ARM_demais,NAC_demais);NAC_ARM_demais
  NAC_ARM = rbind(NAC_ARM,NAC_ARM_demais);NAC_ARM
  i=i+1;i
}else{
if(ciclo$`P-ETC`[i]<0){
  NAC_demais = NAC_demais+ciclo$`P-ETC`[i]
  ARM_demais = CAD*exp((NAC_demais/CAD))
}else{
  if((ARM_demais+ciclo$`P-ETC`[i])>CAD){
    ARM_demais = CAD
    NAC_demais = CAD*SciViews::ln((ARM_demais/CAD))
  }else{
    ARM_demais = ARM_demais+ciclo$`P-ETC`[i]
    NAC_demais = CAD*SciViews::ln((ARM_demais/CAD))
  }
}
i=i+1;i
NAC_ARM_parcial = cbind(ARM_demais,NAC_demais);NAC_ARM_parcial
NAC_ARM = rbind(NAC_ARM,NAC_ARM_parcial);NAC_ARM
}}
NAC_ARM = as.data.frame(NAC_ARM);NAC_ARM
names(NAC_ARM) = c("Arm","Nac");NAC_ARM
ciclo = cbind(ciclo,NAC_ARM);ciclo

i = 1;i
while (i <= nrow(ciclo)) {
if(i==1){
ALT = ciclo$Arm[1] - Armazenamento_Anterior;ALT
i=i+1;  
}else{
ALT_parcial = ciclo$Arm[i] - ciclo$Arm[i-1];ALT_parcial
ALT = rbind(ALT,ALT_parcial);ALT
i=i+1;i
} 
}
ciclo$ALT = ALT;ciclo$ALT

i=1;i
while (i <= nrow(ciclo)) {
if(i==1){
  if(ciclo$`P-ETC`[i] >= 0){
    ETR = ciclo$ETC[i]
    i = i+1
  }else{
    ETR = ciclo$P[i]+abs(ciclo$ALT[1])
    i = i+1
  }
}else{
  if(ciclo$`P-ETC`[i] >= 0){
  ETR_parcial = ciclo$ETC[i]
  ETR = rbind(ETR,ETR_parcial)
  i = i+1
}else{
  ETR_parcial = ciclo$P[i]+abs(ciclo$ALT[i])
  ETR = rbind(ETR,ETR_parcial)
  i = i+1
}
}}

ciclo$ETR = ETR;ciclo

ciclo$ETR_ETC = ciclo$ETR / ciclo$ETC;ciclo
ciclo$DEF = ciclo$ETC - ciclo$ETR;ciclo
ciclo$DEF_1 = ciclo$DEF*-1;ciclo

i=1;i
while (i<=nrow(ciclo)) {
if(i==1){
if(ciclo$Arm[i] == CAD){
EXC = ciclo$`P-ETC`[i]-ciclo$ALT[i];EXC
i=i+1  
}else{
  EXC = 0  
  i =i+1;i
}}else{
  if(ciclo$Arm[i] == CAD){
    EXC_parcial = ciclo$`P-ETC`[i]-ciclo$ALT[i];EXC
    EXC = rbind(EXC,EXC_parcial)
    i=i+1  
  }else{
    EXC_parcial = 0;EXC
    EXC = rbind(EXC,EXC_parcial)
    i=i+1 
  } 
}}
ciclo$EXC = EXC;ciclo
ciclo$ARM_CAD = ciclo$Arm / CAD;ciclo
ciclo$CTN = ifelse(ciclo$Tmed<16.5,yes = -0.0425+0.035*ciclo$Tmed+0.00325*ciclo$Tmed^2-0.0000925*ciclo$Tmed^3,no = 0.583+0.014*ciclo$Tmed+0.0013*ciclo$Tmed^2-0.000037*ciclo$Tmed^3);ciclo
ciclo$CTC = ifelse(ciclo$Tmed<16.5,yes = -1.085+0.07*ciclo$Tmed+0.0065*ciclo$Tmed^2-0.000185*ciclo$Tmed^3,no = -0.0425+0.035*ciclo$Tmed+0.00325*ciclo$Tmed^2-0.0000925*ciclo$Tmed^3);ciclo
ciclo$PPBn = ((31.7+5.234*ciclo$Q0)*(1-ciclo$n/ciclo$fotóperiodo)*ciclo$CTN)
ciclo$PPBc = ((107.2+8.604*ciclo$Q0)*(ciclo$n/ciclo$fotóperiodo)*ciclo$CTC)
ciclo$PPBp = (ciclo$PPBc + ciclo$PPBn);ciclo
ciclo$IAF = IAF_soja[,1];ciclo
ciclo$CIAF = ifelse((0.0186+0.37*(ciclo$IAF)-0.035*(ciclo$IAF^2))>1,yes = 1,no = (0.0186+0.37*(ciclo$IAF)-0.035*(ciclo$IAF^2)));
Cresp = ifelse(ciclo$Tmed>=20,yes = 0.5,no = 0.6);ciclo$Cresp = Cresp;ciclo
ciclo$Cc = 0.4;ciclo
ciclo$Cum = 100/(100-13);ciclo
ciclo$CN = ciclo$CIAF*ciclo$Cresp*ciclo$Cc*ciclo$Cum;ciclo
ciclo$PPf = ciclo$PPBp*ciclo$CN;ciclo

i=1;i
while(i<=nrow(ciclo)){
if(i==1){
  PPf_ac = ciclo$PPf[i];PPf_ac
  i=i+1
}else{
  PPf_parcial = ciclo$PPf[i]+PPf_ac[i-1];PPf_parcial
  PPf_ac = rbind(PPf_ac,PPf_parcial);PPf_ac
  i=i+1;i
}
}
ciclo$PPf_ac = PPf_ac;ciclo
ciclo_final = ciclo
j=j+1
}else{
    csv_cidade = subset(csv,CIDADE == lista_cidades[j]);csv_cidade
    
    csv_cidade$YYYYMMDD = openxlsx::convertToDate(csv_cidade$YYYYMMDD,format ="%Y-%m-%d");csv_cidade$YYYYMMDD
    #csv_cidade$YYYYMMDD = as.POSIXct(csv$YYYYMMDD,format = "%Y-%m-%d");csv_cidade$YYYYMMDD
    
    
    ciclo = csv_cidade[csv_cidade$YYYYMMDD >= data_plantio & csv_cidade$YYYYMMDD <= data_final,];ciclo
    if(is.na(ciclo$X[1]) & is.na(ciclo$X[2])){
      ciclo = ciclo[-c(1,2),];ciclo
    }
    ciclo$kc = kc_soja;ciclo
    
    ciclo$ETC = as.numeric(ciclo$kc)*as.numeric(ciclo$Eto)
    ciclo$"P-ETC" = as.numeric(ciclo$P)-as.numeric(ciclo$ETC)
    
    
    i=1;i
    while (i<=length(ciclo$`P-ETC`)) {
      if(i<=2){
        ARM_primeiro = 0
        NAC_primeiro = 0
        if(ciclo$`P-ETC`[1]<0){
          NAC_primeiro = Neg_Acumulado_anterior+ciclo$`P-ETC`[1]
          ARM_primeiro = CAD*exp((NAC_primeiro/CAD))
        }else{
          if((Armazenamento_Anterior+ciclo$`P-ETC`[1])>CAD){
            ARM_primeiro = CAD
            NAC_primeiro = CAD*SciViews::ln((ARM_primeiro/CAD))
          }else{
            ARM_primeiro = Armazenamento_Anterior+ciclo$`P-ETC`[1]
            NAC_primeiro = CAD*SciViews::ln((ARM_primeiro/CAD))
          }
        }
        i = 2;i
        if(ciclo$`P-ETC`[i]<0){
          NAC_demais = NAC_primeiro+ciclo$`P-ETC`[i]
          ARM_demais = CAD*exp((NAC_demais/CAD))
        }else{
          if((ARM_primeiro+ciclo$`P-ETC`[i])>CAD){
            ARM_demais = CAD
            NAC_demais = CAD*SciViews::ln((ARM_demais/CAD))
          }else{
            ARM_demais = ARM_primeiro+ciclo$`P-ETC`[i]
            NAC_demais = CAD*SciViews::ln((ARM_demais/CAD))
          }
        }
        NAC_ARM = cbind(ARM_primeiro,NAC_primeiro);NAC_ARM
        NAC_ARM_demais = cbind(ARM_demais,NAC_demais);NAC_ARM_demais
        NAC_ARM = rbind(NAC_ARM,NAC_ARM_demais);NAC_ARM
        i=i+1;i
      }else{
        if(ciclo$`P-ETC`[i]<0){
          NAC_demais = NAC_demais+ciclo$`P-ETC`[i]
          ARM_demais = CAD*exp((NAC_demais/CAD))
        }else{
          if((ARM_demais+ciclo$`P-ETC`[i])>CAD){
            ARM_demais = CAD
            NAC_demais = CAD*SciViews::ln((ARM_demais/CAD))
          }else{
            ARM_demais = ARM_demais+ciclo$`P-ETC`[i]
            NAC_demais = CAD*SciViews::ln((ARM_demais/CAD))
          }
        }
        i=i+1;i
        NAC_ARM_parcial = cbind(ARM_demais,NAC_demais);NAC_ARM_parcial
        NAC_ARM = rbind(NAC_ARM,NAC_ARM_parcial);NAC_ARM
      }}
    NAC_ARM = as.data.frame(NAC_ARM);NAC_ARM
    names(NAC_ARM) = c("Arm","Nac");NAC_ARM
    ciclo = cbind(ciclo,NAC_ARM);ciclo
    
    i = 1;i
    while (i <= nrow(ciclo)) {
      if(i==1){
        ALT = ciclo$Arm[1] - Armazenamento_Anterior;ALT
        i=i+1;  
      }else{
        ALT_parcial = ciclo$Arm[i] - ciclo$Arm[i-1];ALT_parcial
        ALT = rbind(ALT,ALT_parcial);ALT
        i=i+1;i
      } 
    }
    ciclo$ALT = ALT;ciclo$ALT
    
    i=1;i
    while (i <= nrow(ciclo)) {
      if(i==1){
        if(ciclo$`P-ETC`[i] >= 0){
          ETR = ciclo$ETC[i]
          i = i+1
        }else{
          ETR = ciclo$P[i]+abs(ciclo$ALT[1])
          i = i+1
        }
      }else{
        if(ciclo$`P-ETC`[i] >= 0){
          ETR_parcial = ciclo$ETC[i]
          ETR = rbind(ETR,ETR_parcial)
          i = i+1
        }else{
          ETR_parcial = ciclo$P[i]+abs(ciclo$ALT[i])
          ETR = rbind(ETR,ETR_parcial)
          i = i+1
        }
      }}
    
    ciclo$ETR = ETR;ciclo
    
    ciclo$ETR_ETC = ciclo$ETR / ciclo$ETC;ciclo
    ciclo$DEF = ciclo$ETC - ciclo$ETR;ciclo
    ciclo$DEF_1 = ciclo$DEF*-1;ciclo
    
    i=1;i
    while (i<=nrow(ciclo)) {
      if(i==1){
        if(ciclo$Arm[i] == CAD){
          EXC = ciclo$`P-ETC`[i]-ciclo$ALT[i];EXC
          i=i+1  
        }else{
          EXC = 0  
          i =i+1;i
        }}else{
          if(ciclo$Arm[i] == CAD){
            EXC_parcial = ciclo$`P-ETC`[i]-ciclo$ALT[i];EXC
            EXC = rbind(EXC,EXC_parcial)
            i=i+1  
          }else{
            EXC_parcial = 0;EXC
            EXC = rbind(EXC,EXC_parcial)
            i=i+1 
          } 
        }}
    ciclo$EXC = EXC;ciclo
    ciclo$ARM_CAD = ciclo$Arm / CAD;ciclo
    ciclo$CTN = ifelse(ciclo$Tmed<16.5,yes = -0.0425+0.035*ciclo$Tmed+0.00325*ciclo$Tmed^2-0.0000925*ciclo$Tmed^3,no = 0.583+0.014*ciclo$Tmed+0.0013*ciclo$Tmed^2-0.000037*ciclo$Tmed^3);ciclo
    ciclo$CTC = ifelse(ciclo$Tmed<16.5,yes = -1.085+0.07*ciclo$Tmed+0.0065*ciclo$Tmed^2-0.000185*ciclo$Tmed^3,no = -0.0425+0.035*ciclo$Tmed+0.00325*ciclo$Tmed^2-0.0000925*ciclo$Tmed^3);ciclo
    ciclo$PPBn = ((31.7+5.234*ciclo$Q0)*(1-ciclo$n/ciclo$fotóperiodo)*ciclo$CTN)
    ciclo$PPBc = ((107.2+8.604*ciclo$Q0)*(ciclo$n/ciclo$fotóperiodo)*ciclo$CTC)
    ciclo$PPBp = (ciclo$PPBc + ciclo$PPBn);ciclo
    ciclo$IAF = IAF_soja[,1];ciclo
    ciclo$CIAF = ifelse((0.0186+0.37*(ciclo$IAF)-0.035*(ciclo$IAF^2))>1,yes = 1,no = (0.0186+0.37*(ciclo$IAF)-0.035*(ciclo$IAF^2)));
    Cresp = ifelse(ciclo$Tmed>=20,yes = 0.5,no = 0.6);ciclo$Cresp = Cresp;ciclo
    ciclo$Cc = 0.4;ciclo
    ciclo$Cum = 100/(100-13);ciclo
    ciclo$CN = ciclo$CIAF*ciclo$Cresp*ciclo$Cc*ciclo$Cum;ciclo
    ciclo$PPf = ciclo$PPBp*ciclo$CN;ciclo
    
    i=1;i
    while(i<=nrow(ciclo)){
      if(i==1){
        PPf_ac = ciclo$PPf[i];PPf_ac
        i=i+1
      }else{
        PPf_parcial = ciclo$PPf[i]+PPf_ac[i-1];PPf_parcial
        PPf_ac = rbind(PPf_ac,PPf_parcial);PPf_ac
        i=i+1;i
      }
    }
    ciclo$PPf_ac = PPf_ac;ciclo
    ciclo_parcial = ciclo
    ciclo_final = rbind(ciclo_parcial,ciclo_final)
    j=j+1
}}
  ciclo_parcial_anos = ciclo_final
  ciclo_final_anos = rbind(ciclo_parcial_anos,ciclo_final_anos)
  k=k+1
  }}
path = paste0(diretorio,"PPf.csv")
write_csv(ciclo_final_anos,path)
