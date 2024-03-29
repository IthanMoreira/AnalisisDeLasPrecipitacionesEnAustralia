
# Library -----------------------------------------------------------------
#install.packages("tidyverse")
#install.packages("devtools")
#library(devtools)
#devtools::install_github("DFJL/SamplingUtil")
library(dplyr)
library(readr)
library(ggplot2)
library(SamplingUtil)

# Pregunta 1 --------------------------------------------------------------

# Carga de la información -------------------------------------------------

df = read.csv("Data/lluvia.csv")

dplyr::glimpse(df)

summary(df)

set.seed(200) #Semilla fijada en 200
df_muestra<-df %>% dplyr::sample_n(size=5000, remplace = FALSE)

# Pregunta 2 --------------------------------------------------------------
#A - Probabilidad de que llueva hoy 
df_muestra$LluviaHoy %>% mean()*100
#R//22.26 %
#Data Completa
df$LluviaHoy %>% mean()*100
#R//21.96 %

#------------------------------------------------------------------------------
#B - Probabilidad de que un día tenga menos de 8 horas de sol 

df_muestra %>% filter(Sol<8) %>% summarise(media = n()/nrow(df_muestra)*100 )
#R//42.9 %

#Data Completa
df %>% filter(Sol<8)%>% select(Sol) %>% summarise(media = n()/nrow(df)*100 )
#R//44.21 %

#------------------------------------------------------------------------------
#C - Probabilidad de que la temperatura a las 3pm sea sobre 20°C en invierno 

df_muestra %>% filter(Estacion == 'Invierno') %>% summarise(Prom_Tem3 = mean(if_else(Temp3pm > 20,1,0))*100)
#R// 26.8 %


#Data Completa
df %>% filter(Estacion == 'Invierno') %>% summarise(Prom_Tem3 = mean(if_else(Temp3pm > 20,1,0))*100)
#R// 25.92 %

#------------------------------------------------------------------------------
#D - Probabilidad de que lluevan entre 5mm y 10mm, en un clima Koppen Subtropical 

df_muestra %>% filter(Koppen == 'Subtropical') %>% 
  summarise(Prob_lluvia = mean(if_else(Lluvia > 5 & Lluvia < 10,1,0))*100)
#R// 5.32 %

#Data Completa
df %>% 
  filter(Koppen == 'Subtropical') %>% 
  summarise(Prob_lluvia = mean(if_else(Lluvia > 5 & Lluvia < 10,1,0))*100)
#R// 4.67 %

#------------------------------------------------------------------------------
# E- Repita las preguntas anteriores utilizando el conjunto completo de datos y comprare sus resultados.
#R//
#La muestra obtenida es representativa?,¿cómo mejoraría la muestra aleatoria?
#Si, pero se puede mejorar dando significancia a los estratos
#
#------------------------------------------------------------------------------
# Pregunta 3 --------------------------------------------------------------

#Muestra Estratos
df %>% select(Koppen) %>% table() %>% prop.table()
Estrato=SamplingUtil::nstrata(5000, wh = c(0.029850, 0.088250,0.356350,0.385875,0.139675))

#MuestraDesert
set.seed(200) #Semilla fijada en 200
MuestraDesert = df %>% filter(Koppen=="Desert") %>% dplyr::sample_n(size=Estrato[1], remplace = FALSE)
#MuestraDesert
set.seed(200) #Semilla fijada en 200
MuestraGrassland = df %>% filter(Koppen=="Grassland") %>% dplyr::sample_n(size=Estrato[2], remplace = FALSE)
#MuestraDesert
set.seed(200) #Semilla fijada en 200
MuestraSubtropical = df %>% filter(Koppen=="Subtropical") %>% dplyr::sample_n(size=Estrato[3], remplace = FALSE)
#MuestraDesert
set.seed(200) #Semilla fijada en 200
MuestraTemperate = df %>% filter(Koppen=="Temperate") %>% dplyr::sample_n(size=Estrato[4], remplace = FALSE)
#MuestraDesert
set.seed(200) #Semilla fijada en 200
MuestraTropical = df %>% filter(Koppen=="Tropical") %>% dplyr::sample_n(size=Estrato[5], remplace = FALSE)

df_muestraEstratificado = rbind(MuestraDesert, MuestraGrassland, MuestraSubtropical,
                                MuestraTemperate, MuestraTropical)
rm(MuestraDesert); rm(MuestraGrassland); rm(MuestraSubtropical)
rm(MuestraTemperate); rm(MuestraTropical);rm(Estrato)

#------------------------------------Duda--------------------------------------
# se puede hacer asi y que funcione el set.seed(200) ????
Estrato=SamplingUtil::nstrata(5000, wh = c(0.029850, 0.088250,0.356350,0.385875,0.139675))
set.seed(200)
  df_muestraEstratificado = df %>%   # Se selecciona la base de datos
  filter(Koppen=="Desert") %>% dplyr::sample_n(size=Estrato[1], remplace = FALSE) %>% 
  add_row(df %>% filter(Koppen=="Grassland") %>% dplyr::sample_n(size=Estrato[2], remplace = FALSE)) %>% 
  add_row(df %>% filter(Koppen=="Subtropical") %>% dplyr::sample_n(size=Estrato[3], remplace = FALSE)) %>% 
  add_row(df %>% filter(Koppen=="Temperate") %>% dplyr::sample_n(size=Estrato[4], remplace = FALSE)) %>% 
  add_row(df %>% filter(Koppen=="Tropical") %>% dplyr::sample_n(size=Estrato[5], remplace = FALSE))
  rm(Estrato)
#------------------------------------------------------------------------------

#equivalente a la proporción real de la población.

prop.table(table(df_muestraEstratificado$Koppen))
#Desert   Grassland Subtropical   Temperate    Tropical 
#0.02998201  0.08834699  0.35618629  0.38576854  0.13971617 
prop.table(table(df$Koppen))
#Desert   Grassland Subtropical   Temperate    Tropical 
#0.029850    0.088250    0.356350    0.385875    0.139675 

#------------------------------------------------------------------------------
#A - Probabilidad de que llueva hoy 

df_muestraEstratificado$LluviaHoy %>% mean()*100
#R//21.56 %

#------------------------------------------------------------------------------
#B - Probabilidad de que un día tenga menos de 8 horas de sol 

df_muestraEstratificado %>% filter(Sol<8) %>% 
  summarise(media = n()/nrow(df_muestraEstratificado)*100 )
#R//44.21 %

#------------------------------------------------------------------------------
#C - Probabilidad de que la temperatura a las 3pm sea sobre 20°C en invierno 

df_muestraEstratificado %>% filter(Estacion == 'Invierno') %>% 
  summarise(Prom_Tem3 = mean(if_else(Temp3pm > 20,1,0))*100)
#R// 26.78 %

#------------------------------------------------------------------------------
#D - Probabilidad de que lluevan entre 5mm y 10mm, en un clima Koppen Subtropical 

df_muestraEstratificado %>% filter(Koppen == 'Subtropical') %>% 
  summarise(Prob_lluvia = mean(if_else(Lluvia > 5 & Lluvia < 10,1,0))*100)
#R// 4.15 %

#------------------------------------------------------------------------------
#Han mejorado los resultados al utilizar un muestreo aleatorio estratificado en comparación al muestreo
#aleatorio simple?
#------------------------------------------------------------------------------
#R//
#Matriz de comparacion
tab = matrix ( c(21.96,22.26,21.56,44.21,42.9,44.21,6.5,6.54,6.95,1.66,1.92,1.47), ncol = 3 , byrow = TRUE )

#define los nombres de las columnas y los nombres de las filas de los nombres de las columnas de la matriz

colnames (tab) = c ('df', ' df_muestra', 'df_muestraEstratificado')
rownames (tab) = c ('Pregunta 1','Pregunta 2','Pregunta 3','Pregunta 4')
tab = as.data.frame(tab)
tab

#------------------------------------------------------------------------------

# Pregunta 4 --------------------------------------------------------------
# Utilizando la muestra estratificada, determine un intervalo de confianza al 95%:


#A -  Para el promedio de temperatura de Australia a las 9am

ConfPromedio9=df_muestraEstratificado%>% select(Temp9am)%>% t.test(conf.level=0.95)

ConfPromedio9$conf.int 
rm(ConfPromedio9)
#R// 17.93521 18.30185
#Datos completos
df$Temp9am %>% mean()
# 18.08529

#------------------------------------------------------------------------------
#B - Para el promedio de temperatura de Australia a las 9am en las distintas estaciones del año. 
#Compare sus intervalos

# Verano
df_muestraEstratificado%>% filter(Estacion=="Verano") %>% select(Temp9am)%>% t.test(conf.level=0.95) 
#R// 22.73771 23.34432
#Datos completos
df %>% filter(Estacion=="Verano") %>% summarise(media = mean(Temp9am) )
# 22.84

#------------------------------------------------------------------------------

# Otono
df_muestraEstratificado%>% 
  filter(Estacion=="Oto\xf1o") %>% 
  select(Temp9am)%>%
  t.test(conf.level=0.95) 
#R// 18.01701 18.63500

#Datos completos
df %>% filter(Estacion=="Oto\xf1o")%>% summarise(media = mean(Temp9am) )
# 18.25

#------------------------------------------------------------------------------
# Invierno
df_muestraEstratificado%>% 
  filter(Estacion=="Invierno") %>% 
  select(Temp9am)%>%
  t.test(conf.level=0.95) 
#R// 12.68185 13.27242
#Datos completos
df %>% filter(Estacion=="Invierno") %>% summarise(media = mean(Temp9am) )
# 12.75787

#------------------------------------------------------------------------------
# Primavera
df_muestraEstratificado%>% 
  filter(Estacion=="Primavera") %>% 
  select(Temp9am)%>%
  t.test(conf.level=0.95) 
#R// 18.40465 19.06540
#Datos completos
df %>% filter(Estacion=="Primavera") %>% summarise(media = mean(Temp9am) )
# 18.76364

#------------------------------------------------------------------------------
#C - Para la proporción de días con lluvias (variable lluviaHoy)

df_muestraEstratificado%>% select(LluviaHoy) %>% table() 
#   LluviaHoy
#     0    1 
#   3924 1079 
prop.test(x=1079,n=5003,conf.level=0.95)$conf.int
#R// 0.2043964 0.2273849
#Datos completos
df$LluviaHoy %>% mean()
# 0.219625

#------------------------------------------------------------------------------
#D - Obtenga los indicadores puntuales en cada caso para los datos completos, ¿estos están contenidos en el intervalo de confianza propuesto?

#R// Si, todos estan dentro del intervalo de confianza   
#------------------------------------------------------------------------------

# Pregunta 5 --------------------------------------------------------------

#A - La temperatura promedio a a las 9am en Australia es menor a los 18°C

df_muestraEstratificado %>% select(Temp9am) %>% t.test(mu=18,alternative= "l", conf.level=0.90)
#R//H0>=18 ; H1<18
#p-value = 0.8975
#no existe evidencia estadística suficiente para rechazar H0

#Datos completos
df$Temp9am %>% mean()
# 18.08

#------------------------------------------------------------------------------
#B - Las horas de sol en una zona Koppen desértica es de 9 horas

df_muestraEstratificado %>%
  filter(Koppen == "Desert") %>%
  select(Sol) %>% t.test(mu=9, alternative = "t", conf.level = 0.9)

#R//H0=9 ; H1!=9
#p-value = 0.477
#no existe evidencia estadística suficiente para rechazar H0

#Datos completos

df %>% filter(Koppen=="Desert") %>% summarise(media = mean(Sol) )

# 8.9

#------------------------------------------------------------------------------
#C - El 65% de las mediciones de humedad a las 3pm en invierno en una zona tropical tienen una humedad menor al 45% 
(df_muestraEstratificado$Hum3pm < 45) %>%
  table() %>%
  prop.test(p = 0.65 , alternative= "l", conf.level=0.9)
#R//H0>=65 ; H1<65
#p-value = 1.633e-08
#existe evidencia estadística suficiente para rechazar H0

#Datos completos

(df$Hum3pm < 45) %>% table() %>% prop.table()*100
#FALSE  TRUE 
#24698 15302 

#------------------------------------------------------------------------------
#D - En invierno, la humedad a las 3pm es igual en una zona tropical y una zona subtropical

Mtropical=df_muestraEstratificado %>% 
                        filter(Estacion=="Invierno" & Koppen=="Tropical" ) %>% 
                        select(Hum3pm)

Msubtropical=df_muestraEstratificado %>% 
                        filter(Estacion=="Invierno" & Koppen=="Subtropical") %>% 
                        select(Hum3pm)

t.test(x=Msubtropical$Hum3pm, y=Mtropical$Hum3pm,
       mu = 0, alternative = "t", var.equal = FALSE)
rm(Mtropical);rm(Msubtropical)
#R//H0:tropical==subtropical ; H1:tropical!=subtropical
#p-value = 3.232e-05
#existe evidencia estadística suficiente para rechazar H0

#Datos completos

df %>% filter(Estacion=="Invierno" & Koppen=="Tropical" ) %>% 
  select(Hum3pm) %>% summarise(media = mean(Hum3pm) )

df %>% filter(Estacion=="Invierno" & Koppen=="Subtropical") %>% 
  select(Hum3pm) %>% summarise(media = mean(Hum3pm) )

#46.13137
#52.44091

#------------------------------------------------------------------------------
#E - Obtenga los indicadores puntuales en cada caso para los datos completos y compare sus resultados.
#
#
#
#
#------------------------------------------------------------------------------
# Pregunta 6 --------------------------------------------------------------

#A -  Para el promedio de temperatura de Australia a las 9am


df_muestraEstratificado %>%
  select(Temp9am) %>%
  ggplot(., aes(x = Temp9am)) +
  geom_histogram(position = "identity", bins = 150, alpha = 0.8) +
  scale_fill_manual(values = c("#98ECAE", "#EC98A9"))+
  geom_vline(xintercept = 17.93521, 
             color = "#A61F3A", size=1)+
  geom_vline(xintercept = 18.30185, 
             color = "#5A9A16", size=1,show.legend = "Intervalo")+
  theme_minimal()



df_muestraEstratificado%>%
  select(Temp9am) %>% summarise(media = mean(Temp9am)) %>% mutate(TempMedia='TempMedia') %>% 
  
  ggplot(., aes(x = TempMedia, y=media)) +
  geom_bar( stat = "identity", alpha = 0.8) +
  geom_hline(yintercept = 17.93521, 
             color = "#A61F3A", size=1)+
  geom_hline(yintercept = 18.30185, 
             color = "#5A9A16", size=1)+
  theme_minimal()
#------------------------------------------------------------------------------
#B - Para el promedio de temperatura de Australia a las 9am en las distintas estaciones del año. Compare sus intervalos

# Verano
GfVerano=df_muestraEstratificado%>%  filter(Estacion=="Verano")%>%
  select(Temp9am,Estacion)%>% group_by(Estacion) %>% summarise(media = mean(Temp9am)) %>% 
ggplot(., aes(x = Estacion, y=media)) +
  geom_bar( stat = "identity", alpha = 0.8) +
  geom_hline(yintercept = 22.73771, 
             color = "#A61F3A", size=1)+
  geom_hline(yintercept = 23.34432, 
             color = "#5A9A16", size=1)

GfInvierno=df_muestraEstratificado%>%  filter(Estacion=="Invierno")%>%
  select(Temp9am,Estacion)%>% group_by(Estacion) %>% summarise(media = mean(Temp9am)) %>% 
  ggplot(., aes(x = Estacion, y=media)) +
  geom_bar( stat = "identity", alpha = 0.8) +
  geom_hline(yintercept = 12.68185, 
             color = "#A61F3A", size=1)+
  geom_hline(yintercept =13.27242, 
             color = "#5A9A16", size=1)

GfPrimavera=df_muestraEstratificado%>%  filter(Estacion=="Primavera")%>%
  select(Temp9am,Estacion)%>% group_by(Estacion) %>% summarise(media = mean(Temp9am)) %>% 
  ggplot(., aes(x = Estacion, y=media)) +
  geom_bar( stat = "identity", alpha = 0.8) +
  geom_hline(yintercept = 18.40465, 
             color = "#A61F3A", size=1)+
  geom_hline(yintercept =19.06540, 
             color = "#5A9A16", size=1)
 
GfOtono=
  df_muestraEstratificado%>%  filter(Estacion=="Oto\xf1o")%>%
  select(Temp9am)%>%mutate(Estacion="Otono") %>% group_by(Estacion) %>% summarise(media = mean(Temp9am)) %>% 
  ggplot(., aes(x = Estacion, y=media)) +
  geom_bar( stat = "identity", alpha = 0.8) +
  geom_hline(yintercept = 18.01701, 
             color = "#A61F3A", size=1)+
  geom_hline(yintercept =18.63500, 
             color = "#5A9A16", size=1)

df_muestraEstratificado%>%  filter(Estacion=="Verano") %>%
  select(Temp9am,Estacion) %>%
  ggplot(., aes(x = Temp9am)) +
  geom_histogram(position = "identity", bins = 150, alpha = 0.8) +
  scale_fill_manual(values = c("#98ECAE", "#EC98A9"))+
  geom_vline(xintercept = 22.73771, 
             color = "#A61F3A", size=1)+
  geom_vline(xintercept = 23.34432, 
             color = "#5A9A16", size=1,show.legend = "Intervalo")+
  theme_minimal()

GfOtono ;GfVerano ;GfInvierno;GfPrimavera
rm(GfOtono) ;rm(GfVerano) ;rm(GfInvierno);rm(GfPrimavera)
#------------------------------------------------------------------------------
# +
# facet_wrap(~Estacion, scales="free") + #scales free es para que cada histograma tenga su rango en el eje x y eje y
#   labs(x="Estacion", y="Temperatura Media",
#        title="Temperaturas Medias a las 9 AM") +
#   theme_classic()
#------------------------------------------------------------------------------

#C - Para la proporción de días con lluvias (variable lluviaHoy)

df_muestraEstratificado%>%
  select(LluviaHoy)%>% mutate(target=case_when(LluviaHoy==0~ "No",
                                               LluviaHoy==1~"Si")) %>% 
  group_by(target) %>%summarise(contador = n()) %>% mutate(prop=contador/sum(contador)*100) %>% 
  
  ggplot(., aes(x = target, y=prop)) +
  geom_bar( stat = "identity", alpha = 0.8) +
  geom_hline(yintercept = 20.43964, 
             color = "#A61F3A", size=1)+
  geom_hline(yintercept = 22.73849, 
             color = "#5A9A16", size=1)+
  theme_minimal()