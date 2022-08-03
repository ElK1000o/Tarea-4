#Tarea 4 -----------------------------------------------------------------------

#Eugenio Ortega - Camilo Riquelme

#Carga de paquetes

pacman::p_load(haven, tidyverse, sjmisc, sjPlot, srvyr, survey)
 
#Carga de datos

data_proc=readRDS("output/data/datos_proc.rds")

#Análisis de Correlación  ------------------------------------------------------

modelo0 <- lm(cuidarse ~ riesgo ,
              data = data_proc, 
              weights = factor_expansion)
summary(modelo0)

sjPlot::tab_model(modelo0, 
                  show.ci=FALSE,  
                  encoding = "UTF-8")

sjPlot::plot_model(modelo0, 
                   show.p = T,
                   show.values =  T,
                   ci.lvl = c(0.95), 
                   title = "Modelo0", 
                   vline.color = "cyan")

#Regresión Lineal Multiple -----------------------------------------------------

modelo1 = glm(cuidarse ~ riesgo + trabaja + edad_tr,
                    family = gaussian(link = "identity"),
                    data = data_proc, 
                    weights = factor_expansion)

summary(modelo1)

modelo2 = glm(cuidarse ~ riesgo_cod + trabaja + edad_tr,
                    family = gaussian(link = "identity"),
                    data = data_proc, 
                    weights = factor_expansion)

summary(modelo2)

#Tabla unica y grafico de modelos

sjPlot::tab_model(list(modelo1, modelo2),
                  show.ci=FALSE,
                  p.style = "stars",
                  dv.labels = c("Modelo 1", "Modelo 2"),
                  string.pred = "Predictores", string.est = "β",
                  encoding =  "UTF-8")

sjPlot::plot_model(modelo2, 
                   show.p = T,
                   digits = 3,
                   show.values =  T,
                   ci.lvl = c(0.95), 
                   title = "Modelo2", 
                   vline.color = "black")

#Modelos Nulos -----------------------------------------------------------------

#Sin ponderador

modelonulo_sin <- lm(dummy_cuidarse ~ 1,
                  data = data_proc)
summary(modelonulo_sin)

#Con ponderador
modelonulo_con <- lm(dummy_cuidarse ~ 1,
              data = data_proc, 
              weights = factor_expansion)
summary(modelonulo_con)

#Comparación modelos nulos

summary(modelonulo_con);summary(modelonulo_sin)

#Otros Modelos -----------------------------------------------------------------

modelo3 = glm(dummy_cuidarse ~ edad,
              family = gaussian(link = "identity"),
              data = data_proc, 
              weights = factor_expansion)
summary(modelo3)

modelo4 = glm(dummy_cuidarse ~ trabaja,
             family = gaussian(link = "identity"),
             data = data_proc, 
             weights = factor_expansion)
summary(modelo4)

modelo5 = glm(dummy_cuidarse ~ edad + trabaja,
              family = gaussian(link = "identity"),
              data = data_proc, 
              weights = factor_expansion)
summary(modelo5)

modelo6 = glm(dummy_cuidarse ~ riesgo_cod,
              family = gaussian(link = "identity"),
              data = data_proc, 
              weights = factor_expansion)
summary(modelo6)

#Tabla unica de modelos

sjPlot::tab_model(list(modelonulo_con, modelonulo_sin, modelo3, modelo4, modelo5, modelo6),
                  show.ci=FALSE,
                  digits = 3,
                  p.style = "stars",
                  dv.labels = c("Modelo Nulo_con", "Modelo Nulo_sin", "Modelo 3", "Modelo 4", "Modelo 5", "Modelo 6"),
                  string.pred = "Predictores", string.est = "β",
                  encoding =  "UTF-8")

#Grafico valores predichos -----------------------------------------------------

modelo6_pred = get_model_data(modelo6, 
               type = "pred")

sjPlot::tab_model(list(modelo6, modelo3),
                  show.ci=FALSE,
                  p.style = "stars",
                  dv.labels = c("Modelo 6", "Modelo 3"),
                  string.pred = "Predictores", string.est = "β",
                  encoding =  "UTF-8")

#Modelo survey

esi_design <- as_survey_design(data_proc, 
                               ids = 1,
                               weights = factor_expansion)

modelo6_survey <- svyglm(dummy_cuidarse ~ riesgo_cod,
                         family = gaussian(link = "identity"),
                         design = esi_design)
summary(modelo6_survey)

#Tabla y grafico para modelo6 y modelo6_survey

################################################################################

sjPlot::tab_model(list(modelo6, modelo6_survey),
                  show.ci=FALSE,
                  p.style = "stars",
                  dv.labels = c("Modelo 6", "Modelo 6 Survey"),
                  string.pred = "Predictores", string.est = "β",
                  encoding =  "UTF-8")

################################################################################

sjPlot::plot_models(modelo6, modelo6_survey, 
                   show.p = T,
                   show.values =  T,
                   ci.lvl = c(0.95), 
                   title = "Modelo6/survey", 
                   vline.color = "black")


#Guardar modelos como data.frames y pasar al output como .rds ------------------

print <- broom::augment(modelo0)
print1 <- broom::augment(modelo1)
print2 <- broom::augment(modelo2)
print3 <- broom::augment(modelo3)
print4 <- broom::augment(modelo4)
print5 <- broom::augment(modelo5)
print6 <- broom::augment(modelo6)
print7 <- broom::augment(modelo6_survey)
print8 <- broom::augment(modelonulo_con)
print9 <- broom::augment(modelonulo_sin)

save(print, print1, print2, print3, print4, print5, print6, print7, 
     print8, print9, file = "output/data/Modelos.RData")

