
#################################################################################
#################################################################################
#                           Modelos Predictivos de PM2.5
#################################################################################
#################################################################################
# biblioteca para vif

#  --        01. Preparaci?n de los datos: selecci?n de variables
estacion <- "MD"
data_com <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/merge_tot/",estacion,"_merge_comp.csv",sep=""))
names(data_com)
#Poner en formato la fecha
data_com$date <- as.POSIXct(as.character(data_com$date), format = "%Y-%m-%d")
# Agregamos numero de dia
data_com$dayWeek <- wday(data_com$date, week_start = 1)
unique(year(data_com$date))
# Nos quedamos solo con los datos 2015-2023
data_com<- data_com[year(data_com$date) != 2024,]
# Verificamos
unique(year(data_com$date))

#Generamos modelo lineal multiple con todas las variables  (17 Vars)
modelo <- lm(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + OCSMASS_dia+
               SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
               d2m_mean + t2m_mean + v10_mean + u10_mean + tp_mean + DEM + dayWeek,
             data = data_com)
modelo <- lm(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
               SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
               d2m_mean  + v10_mean + u10_mean + tp_mean +  dayWeek,
             data = data_com)

vif(modelo)
# Esto te devuelve una tabla con el VIF para cada variable. Como regla general:

#   VIF ??? 1: no hay colinealidad
# 
# VIF entre 5 y 10: hay cierta colinealidad, ojo
# 
# VIF > 10: colinealidad severa ??? deber?as eliminar o transformar alguna variable
sort(vif(modelo), decreasing = TRUE)
a<- data.frame(vif(modelo))
a <- 

car::vif(modelo)
summary(modelo)
#Vamos a guardar en el mismo archivo pero con el dayokweek
write.csv(data_com ,paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/merge_tot/",estacion,"_merge_comp.csv",sep=""))

data_com
######################################################
#########################################################
## Complejidad de los modelos
#########################################################
#funcion para evaluar modelos

evaluar_modelo <- function(modelo, datos_test, variable_real = "PM25",tipoModelo,y_test) {
    predicciones <- predict(modelo, newdata = datos_test)

  
  if(tipoModelo=="XGB"){
    valores_reales <- y_test
  }
    else{
      valores_reales <- datos_test[[variable_real]]
    }
  # Extraer los valores reales de la variable objetivo
  
  
  # Calcular m?tricas
  r2 <- cor(predicciones, valores_reales)^2
  pearson <- cor(valores_reales, predicciones, method = "pearson")
  rmse <- sqrt(mean((predicciones - valores_reales)^2))
  bias <- mean(predicciones - valores_reales)
  
  # Resultados
  resultados <- data.frame(
    R2 = round(r2, 5),
    Pearson = round(pearson, 3),
    RMSE = round(rmse, 3),
    Bias = round(bias, 3),
    Min_Pred = round(min(predicciones), 3),
    Max_Pred = round(max(predicciones), 3)
  )
  
  return(resultados)
}

# Modelo
### ----- RLS  -----
#########################################################
estacion <-"MD"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
# Entrenar el modelo de regresi?n lineal m?ltiple
lm_model <- lm(PM25 ~ AOD_055,
               data = train_data)

resultados_RLS <- evaluar_modelo(modelo=lm_model, datos_test=test_data, variable_real = "PM25",tipoModelo="RLS",y_test=NA)

print(resultados_RLS)

#########################################################################################
#########################################################################################
### ----- RLM  -----
estacion <-"MD"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
names(train_data)
# Entrenar el modelo de regresi?n lineal m?ltiple
lm_model <- lm(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
                 SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
                 d2m_mean  +t2m_mean +v10_mean + u10_mean + tp_mean + DEM+ dayWeek,
               data = train_data)


resultados_RLM <- evaluar_modelo(modelo=lm_model, datos_test=test_data, variable_real = "PM25",tipoModelo="RLS",y_test=NA)

print(resultados_RLM)
#########################################################################################
#########################################################################################
### ----- SVR  -----
library(e1071)  # para usar svm
estacion <-"MD"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))


svr_model <- svm(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
                   SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
                   d2m_mean  +t2m_mean +v10_mean + u10_mean + tp_mean + DEM+ dayWeek,
                 data = train_data,
                 type = "eps-regression",  # regresi?n epsilon-SVR
                 kernel = "radial",        # pod?s probar tambi?n "linear" o "polynomial"
                 cost = 10,                # par?metro de penalizaci?n (ajustable)
                 epsilon = 0.1)            # margen de tolerancia (ajustable)


resultados_SVR <- evaluar_modelo(modelo=svr_model, datos_test=test_data, variable_real = "PM25",tipoModelo="SVR",y_test=NA)

print(resultados_SVR)




#########################################################################################
#########################################################################################
### ----- ET   -----
library(caret)
library(ranger)
estacion <-"MD"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))

# Entrenamiento simple sin b?squeda de hiperpar?metros ni CV
modelo_ranger <- train(
  PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
    SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
    d2m_mean  +t2m_mean +v10_mean + u10_mean + tp_mean + DEM+ dayWeek,
  data = train_data,
  method = "ranger",
  trControl = trainControl(method = "none"),  # sin validaci?n cruzada
  tuneGrid = data.frame(
    mtry = 5,                # eleg? un valor fijo, por ejemplo 5
    splitrule = "extratrees", # Extra Trees
    min.node.size = 5        # tambi?n pod?s cambiar este valor si quer?s
  ),
  importance = 'impurity'
)

resultados_ET <- evaluar_modelo(modelo=modelo_ranger, datos_test=test_data, variable_real = "PM25",tipoModelo="ET",y_test=NA)

print(resultados_ET)
#########################################################################################
#########################################################################################
### ----- RF   -----
estacion <-"MD"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
control <- trainControl(method = "none")
# Entrenar el modelo una sola vez
modelo_RF <- train(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
                     SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
                     d2m_mean  +t2m_mean +v10_mean + u10_mean + tp_mean + DEM+ dayWeek,
                   data = train_data,
                   method = "rf",
                   trControl = control,
                   importance = TRUE)



resultados_RF <- evaluar_modelo(modelo=modelo_RF, datos_test=test_data, variable_real = "PM25",tipoModelo="RF",y_test=NA)

print(resultados_RF)
#########################################################################################
#########################################################################################

# Cargar librer?as necesarias
library(xgboost)
library(Matrix)
### ----- XGB   -----
estacion <-"MD"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))


X <- train_data[ , c( "AOD_055",
                      "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia"
                      "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "blh_mean", 
                      "sp_mean", "d2m_mean", "t2m_mean","v10_mean",
                        "u10_mean", "tp_mean","DEM",
                      "dayWeek")]#


y <- train_data$PM25

X_test <- test_data[ ,c("AOD_055",
                        "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia"
                        "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "blh_mean", 
                        "sp_mean", "d2m_mean", "t2m_mean","v10_mean",
                        "u10_mean", "tp_mean","DEM",
                        "dayWeek")]#
y_test<- test_data$PM25
# Convertir a matrices xgboost
dtrain <- xgb.DMatrix(data = as.matrix(X), label = y)

# Especificar los par?metros del modelo
params <- list(
  booster = "gbtree", 
  objective = "reg:squarederror",  # Tarea de regresi?n
  eval_metric = "rmse",             # M?trica para evaluaci?n
  eta = 0.3,      #0.1chat                  # Tasa de aprendizaje
  max_depth = 6,                    # Profundidad m?xima de los ?rboles
  gamma = 0,                        # Regularizaci?n L2
  subsample = 0.8,                  # Proporci?n de datos para entrenamiento
  colsample_bytree = 1, #0.8 chat           # Proporci?n de caracter?sticas para entrenamiento
  min_child_weight = 1 
)

# Ajustar el modelo
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 2000#,      #2000    # N?mero de rondas de boosting
  #early_stopping_rounds = 10  # Detener el entrenamiento si no mejora
)


dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
resultados_XGB <- evaluar_modelo(modelo=xgb_model, datos_test=dtest, variable_real = "PM25",tipoModelo="XGB",y_test=y_test)

print(resultados_XGB)

#########################################################################################
#########################################################################################

