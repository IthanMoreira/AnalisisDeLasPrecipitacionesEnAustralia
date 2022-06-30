## Actividad - Muestreo
# Base de datos: Encuesta Nacional de Salud.xlsx  ("Poblaci?n")

###########################
# 1. Seleccionar una m.a.s. de n=500 y estimar proporcion de diab?ticos
###########################
# Cargamos "la poblaci?n"
# verifique si en el directorio de trabajo ya dispone de la base de datos
dir()
# recuerde, su directorio getdir() y para asignar otro diferente setdir()
Pobl <- rio::import("Encuesta nacional de salud.xlsx")
str(Pobl)
# son N=3251 casos

set.seed(2022)  # para que todos obtengamos la misma muestra == "semilla"

n<-500
N <- nrow(Pobl)

mas<- sample(1:N,size=n,replace=FALSE)
mas  # hemos seleccionado las filas (indices)
muestra1<- Pobl[mas, ]  # cargamos la base seleccionada
head(muestra1)  
mas
Pobl[228,]  #primer caso seleccionado
######################################################
## Alternativa - usando dplyr
library(dplyr)
set.seed(2022)  # para obtener la misma muestra
muestra1<-sample_n(tbl= Pobl, size=n, replace=FALSE)
head(muestra1)
#####################################################
# Proporcion de diab?ticos estimados en base a la muestra
mean(muestra1$Diabetes)
# si indica "NA" significa que hay "datos faltantes"
mean(muestra1$Diabetes, na.rm=T)  # que no considere los "NA"
round(100*mean(muestra1$Diabetes, na.rm=T),1)
# Proporcion de diab?ticos "Poblaci?n"
round(100*mean(Pobl$Diabetes, na.rm=T),1)
#?cu?l es la real?.....

###########################
# 2.- Si hombres y mujeres en Chile es de 45% y 55%, realice un 
# muestreo estratificado por sexo (Hombres = 225 y Mujeres = 275)
###########################
# obtener los "tama?os poblacionales por sexo"
table(Pobl$Sexo)
Nh=1471 ; nh = 225
Nm=1780 ; nm = 275
# - usando seleccion
mH = sample(Nh, nh, replace=F)
muestraH = Pobl[Pobl$Sexo=="Hombre",][mH,]
mM = sample(Nm, nm, replace=F)

muestraM = Pobl[Pobl$Sexo=="Mujer",][mM,]
muestra2 = rbind(muestraH, muestraM)
table(muestra2$Sexo)

round(100*mean(muestra2$Diabetes,na.rm=T),2)

###########################
# 3.- muestreo estratificado por fumador de tama?o 250 para cada
# estrato, ?Cu?l es la proporci?n de diab?ticos?
###########################
# obtener los "tama?os poblacionales por FUMADOR"
table(Pobl$Fumador)
NFum=1130 ; nFum = 250
NNFu=2121 ; nNFu = 250
# - usando seleccion
mFum = 
sample(NFum, nFum, replace=F)
muestraFum = Pobl[Pobl$Fumador==1,][mFum,]
mNFu = sample(NNFu, nNFu, replace=F)
muestraNFu = Pobl[Pobl$Fumador==0,][mNFu,]
muestra3 = rbind(muestraFum, muestraNFu)
round(100*mean(muestra3$Diabetes,na.rm=T),2)

## para muestreo sistem?tico debemos instalar y cargar:
install.packages("remotes")
remotes::install_github("DFJL/SamplingUtil")

# seleccionamos los "indices" en forma sistem?tica
N=nrow(Pobl) ; n=300
msys <- SamplingUtil::sys.sample(N,n)
muestra4 = Pobl[msys,]
round(100*mean(muestra4$Diabetes,na.rm=T),2)

## RESUMEN 
e0 = round(100*mean(Pobl$Diabetes,na.rm=T),2)
e1 = round(100*mean(muestra1$Diabetes,na.rm=T),2)
e2 = round(100*mean(muestra2$Diabetes,na.rm=T),2)
e3 = round(100*mean(muestra3$Diabetes,na.rm=T),2)
e4 = round(100*mean(muestra4$Diabetes,na.rm=T),2)
estimac <- cbind(e0,e1,e2,e3,e4)
colnames(estimac)=c("Poblaci?n","m.a.s.","Estraf-sexo","Estraf-Fum","Sistem?tico")
#########################################
estimac[1,]  # lista las estimaciones.
#########################################
