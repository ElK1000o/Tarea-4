#Tarea 4 -----------------------------------------------------------------------

#Eugenio Ortega - Camilo Riquelme

#Carga de paquetes

pacman::p_load(haven, tidyverse, sjmisc, sjPlot)

#Carga de datos

data=read_sav("input/data/Ortega.sav")

#Exploracion de variables ------------------------------------------------------

frq(data$f7_3)
frq(data$f6)
frq(data$g1)
frq(data$edad)

names(data)
head(data)

sjPlot::view_df(data,
                encoding = "UTF-8")

#Procesamiento -----------------------------------------------------------------

data_proc = data%>%
  mutate(cuidarse = f7_3,
         riesgo = f6,
         trabaja =g1,
         dummy_cuidarse = case_when(cuidarse<=2~0,
                                    cuidarse>=3 & cuidarse<=5~1),
         edad_tr = case_when(edad<=29 ~"Jovenes",
                          edad>=30 & edad<=59 ~"Adultos",
                          edad>=60 ~"Adultos mayores"), #Tramos de edad según caracteristicas de la poblacion INE
         riesgo_cod = case_when(riesgo==1~"Nada peligroso",
                                riesgo==2~"Algo peligroso",
                                riesgo==3~"Bastante peligroso",
                                riesgo==4~"Muy peligroso",
                                riesgo==5~"Extremadamente peligroso",
                                TRUE~NA_character_),
         trabaja_cod = case_when(g1==1~"Si", g1==2~"No", TRUE~NA_character_))%>%
  mutate_at(vars(cuidarse, riesgo), ~(car::recode(., recodes = c("c(8, 9) = NA"))))%>%
  select(entrevistado, factor_expansion, edad, edad_tr, cuidarse, dummy_cuidarse, 
         riesgo, riesgo_cod, trabaja, trabaja_cod)

#Revision de procesamiento -----------------------------------------------------

frq(data_proc$cuidarse)
frq(data_proc$dummy_cuidarse)
frq(data_proc$edad)
frq(data_proc$edad_tr)
frq(data_proc$riesgo)
frq(data_proc$riesgo_cod)
frq(data_proc$trabaja)
frq(data_proc$trabaja_cod)

view_df(data_proc)

#Guardar datos -----------------------------------------------------------------

saveRDS(data_proc, file = "output/data/datos_proc.rds")
