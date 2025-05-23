#################################################################################
#################################################################################
#                           Modelos Predictivos de PM2.5 con CV
#################################################################################
#################################################################################

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
  
  
  # Calcular métricas
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

##############################################################################
##############################################################################
##############################################################################
### ----- SVR   -----
estacion <-"SP"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))

# 1. Definimos el control de entrenamiento con CV = 10
ctrl <- trainControl(method = "cv", number = 10,
                     savePredictions = "final",
                     verboseIter = TRUE)

# 2. Entrenamiento del modelo SVR con validación cruzada en train_data
set.seed(123)
modelo_cv_svr <- train(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
                         SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + sp_mean +
                         d2m_mean + v10_mean + u10_mean + tp_mean + dayWeek,
                       data = train_data,
                       method = "svmRadial",
                       trControl = ctrl,
                       preProcess = c("center", "scale"),
                       tuneLength = 5) 
resultados_SVR_cv <- evaluar_modelo(modelo_cv_svr, test_data)
print(resultados_SVR_cv)


setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(modelo_cv_svr, file=paste("01-SVR-CV-M",modelo,"-200525-",estacion,".RData",sep=""))

##############################################################################
##############################################################################
##############################################################################
### ----- ET   -----
estacion <-"SP"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))

# Entrenamiento con validación cruzada de 10 pliegues
modelo_ranger <- train(
  PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
    SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + sp_mean +
    d2m_mean + v10_mean + u10_mean + tp_mean + dayWeek,
  data = train_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 10),  # Validación cruzada 10-fold
  tuneGrid = data.frame(
    mtry = 5,                   # valor fijo
    splitrule = "extratrees",  # Extra Trees
    min.node.size = 5          # valor fijo
  ),
  importance = 'impurity'
)

modelo_ET_cv <- modelo_ranger

resultados_ET_cv <- evaluar_modelo(modelo_ET_cv, test_data)
print(resultados_ET_cv)


setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(modelo_ET_cv, file=paste("01-ET-CV-M",modelo,"-200525-",estacion,".RData",sep=""))


##############################################################################
##############################################################################
##############################################################################
### ----- RF   -----
estacion <-"SP"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))

train_control <- trainControl(
  method = "cv",          # Método de validación cruzada
  number = 10,            # Número de pliegues para la validación cruzada
  verboseIter = TRUE,     # Mostrar progreso de entrenamiento
  allowParallel = TRUE    # Permitir procesamiento paralelo
)


modelo_RF_cv <- train(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
                       SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + sp_mean +
                       d2m_mean + v10_mean + u10_mean + tp_mean + dayWeek,
                     data = train_data, method = "rf",
                     trControl = train_control,importance = TRUE)

resultados_RF_cv <- evaluar_modelo(modelo_RF_cv, test_data)
print(resultados_RF_cv)


setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(modelo_RF_cv, file=paste("01-RF-CV-",modelo,"-200525-",estacion,".RData",sep=""))


##############################################################################
##############################################################################
##############################################################################
### ----- XGB   -----

estacion <-"SP"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))


X <- train_data[ , c( "AOD_055",
                      "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia"
                      "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "blh_mean", 
                      "sp_mean", "d2m_mean", "v10_mean",
                      "u10_mean", "tp_mean",
                      "dayWeek")]#


y <- train_data$PM25

X_test <- test_data[ ,c("AOD_055",
                        "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia"
                        "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "blh_mean", 
                        "sp_mean", "d2m_mean", "v10_mean",
                        "u10_mean", "tp_mean",
                        "dayWeek")]#
y_test<- test_data$PM25
# Convertir a matrices xgboost
dtrain <- xgb.DMatrix(data = as.matrix(X), label = y)

# Especificar los parámetros del modelo
# Configurar los parámetros del modelo
params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",  # Tarea de regresión
  eval_metric = "rmse",             # Métrica para evaluación
  eta = 0.3,                       # Tasa de aprendizaje
  max_depth = 6,                   # Profundidad máxima de los árboles
  gamma = 0,                       # Regularización L2
  subsample = 0.8,                 # Proporción de datos para entrenamiento
  colsample_bytree = 1,            # Proporción de características para entrenamiento
  min_child_weight = 1
)

# Realizar validación cruzada
#Esto es lo que mas tarda!! igual en comparacion con rf tarda mucho menos
# porque?
cv_results  <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 2000,                   # Número de rondas de boosting
  nfold = 10,                        # Número de pliegues para la validación cruzada
  early_stopping_rounds = 20,       # Detener si no mejora
  verbose = TRUE                    # Mostrar progreso
)


# Obtener el número óptimo de rondas
best_nrounds <- cv_results$best_iteration

# Ajustar el modelo con el número óptimo de rondas
xgb_cv_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds
)



dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
resultados_XGB <- evaluar_modelo(modelo=xgb_cv_model, datos_test=dtest, variable_real = "PM25",tipoModelo="XGB",y_test=y_test)
print(resultados_XGB)

setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(xgb_cv_model, file=paste("01-XGB-CV-",modelo,"-200525-",estacion,".RData",sep=""))

