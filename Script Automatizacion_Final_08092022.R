######################AUTOMATIZACIÓN MODELO DETECCIÓN AR########################

#Este script contiene una rutina de automatización aplicada a la modelización de
#la detección de la Artritis Rematoidea partiendo de los sintomas reportados en
#los sistemas de información cargada en los registros electronicos de las subredes
#de salud del Distrito de Bogota.

##FIJAMOS DIRECTORIO DE TRABAJO##
setwd("C:/Users/jhatt/OneDrive/Documentos/Maestria_Big_Data/TFM/TFM2/data/Data_final")
if(!dir.exists("Outputs")){dir.create("Outputs")}

# install.packages("tidymodels")
# install.packages ("rpart.plot")
# install.packages("DataExplorer")
# install.packages("tidyverse")

##CARGUE DE LIBRERIAS##
library(tidyverse) #Herramientas para organizar y procesar datos
library(tidymodels) #Construye de modelos con pipeline de Dplyr
library(rpart.plot) #Herramientas Graficar Arboles
library(readxl) #Manejo de archivos Excel
library(DataExplorer) #Herramientas para el Analisis Exploratorio

##CARGUE SET DE DATOS##

#SET DE DATOS PACIENTES HISTORICO#
hist_pacientes <- read_excel("Inputs/ArchivoFinalVariableDummy_1.xlsx") #Set de Datos

hist_pacientes <- as.data.frame(hist_pacientes) # Ajuste set de datos

hist_pacientes$`Paciente id` <- as.character(hist_pacientes$`Paciente id`) #id a tipo character.

#SET DE DATOS PACIENTES NUEVOS#
new_pacientes <- read_excel("Inputs/ArchivoFinalVariableDummy_PRUEBA.xlsx") #Set de Datos

new_pacientes <- as.data.frame(new_pacientes) # Ajuste set de datos

new_pacientes$`Paciente id` <- as.character(new_pacientes$`Paciente id`) #id a tipo character.

##MODELADO PREDICTIVO##

#ENTRENAMIENTO DE MODELO#

#AJUSTE VARIABLES

#Codificación AD-HOC variable Genero Paciente#
hist_pacientes <- hist_pacientes%>%
  mutate(`Genero Paciente` = ifelse(`Genero Paciente` == "F", 1, 0))

#Codificación AD-HOC variable Etnia#
etnia <- hist_pacientes%>%
  select(`Paciente id`, Etnia)%>%
  mutate(Valor = rep(1))%>%
  spread(., Etnia, value = Valor)%>%
  replace(is.na(.), 0)%>%
  select(-Ninguno) #Removemos la Etnia Ninguno para que sea la indicadora

hist_pacientes <- hist_pacientes%>%
  left_join(., etnia, by = "Paciente id")%>%
  select(-Etnia)

hist_pacientes <- hist_pacientes[, c(1, 28, 2:3, 29:32, 5:27)]
hist_pacientes$Artritis <- as.factor(hist_pacientes$Artritis)
rm(list = setdiff(ls(), c("hist_pacientes","new_pacientes")))

#Convertimos ID en indicador de fila#
rownames(hist_pacientes) <- hist_pacientes[, 1]
hist_pacientes <- hist_pacientes[, -1]

#Separación set en datos para Entrenamiento y Testeo#
set.seed(1234)

pacientes_split <- hist_pacientes%>%
  initial_split(prop = 0.70)

pacientes_train <- training(pacientes_split)
pacientes_test <- testing(pacientes_split)

#Establecer parametros del modelo#
recp_rlg <- logistic_reg()%>%
  set_engine("glm")%>%
  set_mode("classification")

#Entrenar el modelo de Regresión Logistica#
relog_fit <- recp_rlg%>%
  fit(Artritis ~ ., data = pacientes_train)

#Hacer testeo#
testeo <- pacientes_test%>%
  select(Artritis)%>%
  bind_cols(relog_fit %>%
              predict(new_data = pacientes_test))

test<- summary(testeo)

capture.output(test, file = "Outputs/metricas_summary.txt")

#Establecer metricas de evaluación#
eval_metrics <- metric_set(ppv, recall, specificity, accuracy, f_meas)
relog_metrics <- eval_metrics(data = testeo, truth = Artritis, estimate = .pred_class)
colnames(relog_metrics)[3] <- "estimate_logis"

write.csv2(relog_metrics, file = "Outputs/Metricas de Evalucion.csv", row.names = F)

#Matrix de Confuncion#
mat_conf <- conf_mat(data = testeo, truth = Artritis, estimate = .pred_class)

capture.output(mat_conf, file = "Outputs/Matrix de Confunsion.txt")

#Validación Cruzada#


testeo$Artritis <- as.numeric(testeo$Artritis)
testeo$.pred_class <- as.numeric(testeo$.pred_class)

summary(testeo)

error_test <- rmse(data = testeo, truth = Artritis, estimate = .pred_class,
                   na_rm = T)%>%
  mutate(modelo = "GLM")

error_test1 <- mae(data = testeo, truth = Artritis, estimate = .pred_class,
                   na_rm = T)%>%
mutate(modelo = "GLM")


capture.output(error_test, file = "Outputs/Validacion Cruzada_rsme.txt")
capture.output(error_test1, file = "Outputs/Validacion Cruzada_mae.txt")

#Hacer Prediccion#

#Codificación AD-HOC variable Genero Paciente#
new_pacientes <- new_pacientes%>%
  mutate(`Genero Paciente` = ifelse(`Genero Paciente` == "F", 1, 0))

#Codificación AD-HOC variable Etnia#
etnia <- new_pacientes%>%
  select(`Paciente id`, Etnia)%>%
  mutate(Valor = rep(1))%>%
  spread(., Etnia, value = Valor)%>%
  replace(is.na(.), 0)%>%
  select(-Ninguno) #Removemos la Etnia Ninguno para que sea la indicadora

new_pacientes <- new_pacientes%>%
  left_join(., etnia, by = "Paciente id")%>%
  select(-Etnia)

new_pacientes <- new_pacientes[, c(1, 28, 2:3, 29:32, 5:27)]
new_pacientes$Artritis <- as.factor(new_pacientes$Artritis)


#Convertimos ID en indicador de fila#
rownames(new_pacientes) <- new_pacientes[, 1]
new_pacientes <- new_pacientes[, -1]

predicts <- new_pacientes%>%
  bind_cols(relog_fit %>%
              predict(new_data = new_pacientes))

#Ajuste Salida#
predicts <- predicts[, c(2:30, 1, 31)]
colnames(predicts)[31] <- "Prediccion"
predicts <- rownames_to_column(predicts, "Paciente")

# predicts$.pred_0 <- format(predicts$.pred_0, scientific = F)
# predicts$.pred_1 <- format(predicts$.pred_1, scientific = F)
write.csv2(predicts, file = "Outputs/Pacientes Prediccion.csv", row.names = F)


#Matrix de Confuncion nuevos datos#
mat_conf <- conf_mat(data = predicts, truth = Artritis, estimate = .pred_class)

capture.output(mat_conf, file = "Outputs/Matrix de Confunsion.txt")


rm(list = ls())
