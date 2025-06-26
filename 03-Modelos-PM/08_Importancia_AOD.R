#################################################################################
#            Modelos Predictivos de PM2.5 importancia del uso/NO uso de AOD
# similar al codido de imporancia de variables!
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
  
  df <- data.frame(predicciones=predicciones, valores_reales=valores_reales)
  df <- df[df$predicciones>0,]
  # Calcular m?tricas
  r2 <- cor(df$predicciones, df$valores_reales)^2
  pearson <- cor(df$valores_reales, df$predicciones, method = "pearson")
  rmse <- sqrt(mean((df$predicciones - df$valores_reales)^2))
  bias <- mean(df$predicciones - df$valores_reales)
  
  # Resultados
  resultados <- data.frame(
    R2 = round(r2, 5),
    Pearson = round(pearson, 3),
    RMSE = round(rmse, 3),
    Bias = round(bias, 3),
    Min_Pred = round(min(df$predicciones), 3),
    Max_Pred = round(max(df$predicciones), 3)
  )
  
  return(resultados)
}


#############
### ----- SVR   -----
estacion <-"MX"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))

# 1. Definimos el control de entrenamiento con CV = 10
ctrl <- trainControl(method = "cv", number = 10,
                     savePredictions = "final",
                     verboseIter = TRUE)

start_time <- Sys.time()
# 2. Entrenamiento del modelo SVR con validación cruzada en train_data
set.seed(123)
modelo_cv_svr <- train(PM25 ~ AOD_055+ ndvi + BCSMASS_dia + 
                         DUSMASS_dia + SO2SMASS_dia + SO4SMASS_dia + 
                         SSSMASS_dia + blh_mean + sp_mean +
                         d2m_mean + t2m_mean +v10_mean +
                         u10_mean + tp_mean + DEM +
                         dayWeek,
                       data = train_data,
                       method = "svmRadial",
                       trControl = ctrl,
                       preProcess = c("center", "scale"),
                       tuneLength = 5) 


end_time <- Sys.time()
print(end_time - start_time)

resultados_SVR_cv <- evaluar_modelo(modelo=modelo_cv_svr, datos_test=test_data,
                                    variable_real = "PM25",tipoModelo="SVR",y_test=NA)
print(resultados_SVR_cv)


setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(modelo_cv_svr, file=paste("02-SVR-CV-M",modelo,"-210525-sAOD-",estacion,".RData",sep=""))


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
modelo_ranger <- train( ####AOD_055
  PM25 ~  ndvi + BCSMASS_dia + DUSMASS_dia + 
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


resultados_ET_cv <- evaluar_modelo(modelo=modelo_ET_cv, datos_test=test_data,
                                    variable_real = "PM25",tipoModelo="ET",y_test=NA)
print(resultados_ET_cv)


setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(modelo_ET_cv, file=paste("02-ET-CV-M",modelo,"-210525-sAOD-",estacion,".RData",sep=""))

##############################################################################
##############################################################################
##############################################################################
### ----- RF   -----
estacion <-"MX"
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
start_time <- Sys.time()
##########AOD_055 
modelo_RF_cv <- train(PM25  ~ AOD_055+ ndvi + BCSMASS_dia + #DUSMASS_dia +
                         SO2SMASS_dia + SO4SMASS_dia + 
                        SSSMASS_dia + blh_mean + sp_mean +
                        d2m_mean + t2m_mean +v10_mean, #+
                         #tp_mean,# + DEM + 
                        #dayWeek, #u10_mean +
                      data = train_data, method = "rf",
                      trControl = train_control,importance = TRUE)

end_time <- Sys.time()
print(end_time - start_time)
resultados_RF_cv <- evaluar_modelo(modelo=modelo_RF_cv, datos_test=test_data,
                                   variable_real = "PM25",tipoModelo="RF",y_test=NA)
print(resultados_RF_cv)


setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(modelo_RF_cv, file=paste("02-RF-CV-M",modelo,"-210525-sAOD-",estacion,".RData",sep=""))


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


X <- train_data[ , c( #"AOD_055",
                      "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia"
                      "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "blh_mean", 
                      "sp_mean", "d2m_mean", "v10_mean",
                      "u10_mean", "tp_mean",
                      "dayWeek")]#


y <- train_data$PM25

X_test <- test_data[ ,c(#"AOD_055",
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
resultados_XGB <- evaluar_modelo(modelo=xgb_cv_model, datos_test=dtest, 
                                 variable_real = "PM25",tipoModelo="XGB",y_test=y_test)
print(resultados_XGB)

setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(xgb_cv_model, file=paste("02-XGB-CV-",modelo,"-210525-sAOD",estacion,".RData",sep=""))



##############################################################################
##########################################################################
# scale_fill_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256","#6a51a3")) +

# Plot regresion lineal +metricas
estacion <-"SP"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
dir <- paste("D:/Josefina/Proyectos/tesis/",estacion,"/modelos/",sep="")
setwd(dir)
# para seleccionar el modelo
list.files(pattern = "RF")
#Oscuro SAOD - Claro con AOD
# SP "#00441b","#238b45"

#SP
load("01-RF-CV-M1-200525-SP.RData" )# Sin AOD
modelo_rf_AOD <- modelo_RF_cv 
load("02-RF-CV-M1-210525-sAOD-SP.RData")# Con AOD
modelo_rf_sinAOD <- modelo_RF_cv  # Guardás con un nuevo nombre

predicciones <- predict(xgb_cv_model, newdata = test_data)

# Solo para XGB

X_test <- test_data[ ,c( #"AOD_055",
  "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia"
  "SO2SMASS_dia",  "SO4SMASS_dia", "SSSMASS_dia","blh_mean", 
  "sp_mean","d2m_mean", "v10_mean", "u10_mean", "tp_mean","dayWeek"
 )]#
y_test<- test_data$PM25
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
predicciones <- predict(xgb_cv_model, newdata = dtest)



df_combinado <- data.frame(pred=predicciones, real=test_data$PM25)


resultados_XGB <- evaluar_modelo(modelo=xgb_cv_model, datos_test=dtest, 
                                 variable_real = "PM25",tipoModelo="XGB",y_test=y_test)
print(resultados_XGB)

resultados_RF_cv_AOD <- evaluar_modelo(modelo=modelo_rf_AOD, datos_test=test_data,
                                   variable_real = "PM25",tipoModelo="RF",y_test=NA)

resultados_RF_cv_sAOD <- evaluar_modelo(modelo=modelo_rf_sinAOD, datos_test=test_data,
                                   variable_real = "PM25",tipoModelo="RF",y_test=NA)

df_combinado_sAOD <- data.frame(pred=predicciones, real=test_data$PM25)

resultados_RF_cv
model_rf <- resultados_RF_cv
R2_model_rf <- resultados_RF_cv$R2
RMSE_model_rf <- resultados_RF_cv$RMSE
Bias_model_rf <- resultados_RF_cv$Bias
n_model_rf <- nrow(test_data)


resultados_RF_cv
model_rf <- resultados_XGB
R2_model_rf <- resultados_XGB$R2
RMSE_model_rf <- resultados_XGB$RMSE
Bias_model_rf <- resultados_XGB$Bias
n_model_rf <- nrow(test_data)


# Crear el gráfico con ggplot2
plot_regresion_rf <- ggplot(df_combinado, aes(y = real, x= pred)) +
  
  geom_point(color = "#00441b", size = 1.5, alpha = 0.6) +  # Puntos de datos
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE,size = 0.6, linetype = "dashed") +  # Línea de regresión
  
  labs(
    x = "Observado",
    y = "Prediccion",
    #subtitle = "XGB Sin AOD",
    #title = "BSQ"
  ) +
  #ggplot2::annotate("text",x = 130, y = 90,label = paste("Modelo RF Sin AOD"), size = 3, color = "black")+ 
  #ggplot2::annotate("text",x = 130, y = 70,label = paste("XGB-sAOD"), size = 3, color = "black")+ 
  
  ggplot2::annotate("text",x = 130, y = 60,label = paste("R² =", round(R2_model_rf, 2)), size = 3, color = "black")+ 
  
  ggplot2::annotate("text",x = 130, y = 50,label = paste("RMSE =", round(RMSE_model_rf, 2)), size = 3, color = "black")+ 
  
  ggplot2::annotate("text",x = 130, y = 40,label = paste("Bias =", round(Bias_model_rf, 2)), size = 3, color = "black")+ 
  
  ggplot2::annotate("text",x = 130, y = 30,label = paste("n =", round(n_model_rf, 2)), size = 3, color = "black")+ 
 theme_classic() #+
plot_regresion_rf

############################################################
#########################################################
# Crear el gráfico con ggplot2
plot_regresion_rf <- ggplot() +
  
  geom_point(df_combinado, aes(y = real, x= pred),color = "#00441b", size = 1.5, alpha = 0.6) +  # Puntos de datos
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE,size = 0.6, linetype = "dashed") +  # Línea de regresión
  
  labs(
    x = "Observado",
    y = "Prediccion",
    #subtitle = "XGB Sin AOD",
    #title = "BSQ"
  ) +
  #ggplot2::annotate("text",x = 130, y = 90,label = paste("Modelo RF Sin AOD"), size = 3, color = "black")+ 
  #ggplot2::annotate("text",x = 130, y = 70,label = paste("XGB-sAOD"), size = 3, color = "black")+ 
  
  ggplot2::annotate("text",x = 130, y = 60,label = paste("R² =", round(R2_model_rf, 2)), size = 3, color = "black")+ 
  
  ggplot2::annotate("text",x = 130, y = 50,label = paste("RMSE =", round(RMSE_model_rf, 2)), size = 3, color = "black")+ 
  
  ggplot2::annotate("text",x = 130, y = 40,label = paste("Bias =", round(Bias_model_rf, 2)), size = 3, color = "black")+ 
  
  ggplot2::annotate("text",x = 130, y = 30,label = paste("n =", round(n_model_rf, 2)), size = 3, color = "black")+ 
  theme_classic() #+
plot_regresion_rf




###################################################
#####################################################
#################################################
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
  
  df <- data.frame(predicciones=predicciones, valores_reales=valores_reales)
  df <- df[df$predicciones>0,]
  # Calcular m?tricas
  r2 <- cor(df$predicciones, df$valores_reales)^2
  pearson <- cor(df$valores_reales, df$predicciones, method = "pearson")
  rmse <- sqrt(mean((df$predicciones - df$valores_reales)^2))
  bias <- mean(df$predicciones - df$valores_reales)
  
  # Resultados
  resultados <- data.frame(
    R2 = round(r2, 5),
    Pearson = round(pearson, 3),
    RMSE = round(rmse, 3),
    Bias = round(bias, 3),
    Min_Pred = round(min(df$predicciones), 3),
    Max_Pred = round(max(df$predicciones), 3)
  )
  
  return(resultados)
}



# scale_fill_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256","#6a51a3")) +

# Plot regresion lineal +metricas
estacion <-"MX"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
dir <- paste("D:/Josefina/Proyectos/tesis/",estacion,"/modelos/",sep="")
setwd(dir)

# para seleccionar el modelo
list.files(pattern = "RF")
#Oscuro SAOD - Claro con AOD
# SP "#00441b","#238b45"
#ST "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dba",
load("01-SVR-CV-M1-200625-sAOD-MX.RData" )
modelo_rf_AOD <- modelo_RF_cv
load("01-SVR-CV-M1-210625-sAODMX.RData")
modelo_rf_sinAOD <- modelo_RF_cv  # Guardás con un nuevo nombre
## predicciones
predicciones_AOD <- predict(modelo_rf_AOD, newdata = test_data)
predicciones_sAOD <- predict(modelo_rf_sinAOD, newdata = test_data)

df_combinado <- data.frame(pred_AOD=predicciones_AOD, pred_sAOD=predicciones_sAOD,real=test_data$PM25)

## Metricas
resultados_RF_cv_AOD <- evaluar_modelo(modelo=modelo_rf_AOD, datos_test=test_data,
                                       variable_real = "PM25",tipoModelo="RF",y_test=NA)

resultados_RF_cv_sAOD <- evaluar_modelo(modelo=modelo_rf_sinAOD, datos_test=test_data,
                                        variable_real = "PM25",tipoModelo="RF",y_test=NA)

#resultados_RF_cv_AOD
R2_model_rf_AOD <- resultados_RF_cv_AOD$R2
RMSE_model_rf_AOD <- resultados_RF_cv_AOD$RMSE
Bias_model_rf_AOD <- resultados_RF_cv_AOD$Bias
n_model_rf_AOD <- nrow(test_data)

resultados_RF_cv_sAOD
R2_model_rf_sAOD <- resultados_RF_cv_sAOD$R2
RMSE_model_rf_sAOD <- resultados_RF_cv_sAOD$RMSE
Bias_model_rf_sAOD <- resultados_RF_cv_sAOD$Bias
n_model_rf_sAOD <- nrow(test_data)


# Crear el gráfico con ggplot2
plot_regresion_rf <- ggplot(df_combinado) +
  geom_point(aes(y = real, x= pred_sAOD),color =   "#41b6c4", alpha=0.9,size = 1.5,shape=20, ) +  # Puntos de datos
  
  geom_point(aes(y = real, x= pred_AOD),color = "#023858" ,   size = 1.5, shape=8) +  # Puntos de datos
  
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  geom_smooth(aes(y = real, x= pred_sAOD),method = "lm", color = "red", se = FALSE,size = 0.6, linetype = "dashed") +  # Línea de regresión
  geom_smooth(aes(y = real, x= pred_AOD),method = "lm", color = "red", se = FALSE,size = 0.6, linetype = "solid") +  # Línea de regresión
  
  labs(
    x = "Observado",
    y = "Prediccion",
    #subtitle = "XGB Sin AOD",
    #title = "BSQ"
  ) +
  theme(
    #legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )+
  ggplot2::annotate("text",x = 130, y = 90,label = paste("Modelo RF Sin AOD"), size = 3, color = "black")+
  ggplot2::annotate("text",x = 100, y = 70,label = paste("sAOD"), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 60,label = paste("R² =", round(R2_model_rf_sAOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 50,label = paste("RMSE =", round(RMSE_model_rf_sAOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 40,label = paste("Bias =", round(Bias_model_rf_sAOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n_model_rf_sAOD, 2)), size = 3, color = "black")+
  
  
  ggplot2::annotate("text",x = 130, y = 70,label = paste("AOD"), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 130, y = 60,label = paste("R² =", round(R2_model_rf_AOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 130, y = 50,label = paste("RMSE =", round(RMSE_model_rf_AOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 130, y = 40,label = paste("Bias =", round(Bias_model_rf_AOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n_model_rf_AOD, 2)), size = 3, color = "black")+
  
  
  theme_classic() #+
plot_regresion_rf

###############################################################
###############################################################

estacion <-"MD"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
dir <- paste("D:/Josefina/Proyectos/tesis/",estacion,"/modelos/",sep="")
setwd(dir)

# para seleccionar el modelo
list.files(pattern = "ET")
#Oscuro SAOD - Claro con AOD
# SP "#00441b","#238b45"
#ST "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dba",
load("01-ET-CV-M1-260525-MD.RData")
modelo_ET_AOD <- modelo_ET_cv
load("01-ET-CV-M1-270525-sAOD-MD.RData" )
modelo_ET_sinAOD <- modelo_ET_cv  # Guardás con un nuevo nombre
## predicciones
predicciones_AOD <- predict(modelo_ET_AOD, newdata = test_data)
predicciones_sAOD <- predict(modelo_ET_sinAOD, newdata = test_data)

df_combinado <- data.frame(pred_AOD=predicciones_AOD, pred_sAOD=predicciones_sAOD,real=test_data$PM25)

## Metricas
resultados_ET_cv_AOD <- evaluar_modelo(modelo=modelo_ET_AOD, datos_test=test_data,
                                       variable_real = "PM25",tipoModelo="ET",y_test=NA)

resultados_ET_cv_sAOD <- evaluar_modelo(modelo=modelo_ET_sinAOD, datos_test=test_data,
                                        variable_real = "PM25",tipoModelo="ET",y_test=NA)

#resultados_ET_cv_AOD
R2_model_ET_AOD <- resultados_ET_cv_AOD$R2
RMSE_model_ET_AOD <- resultados_ET_cv_AOD$RMSE
Bias_model_ET_AOD <- resultados_ET_cv_AOD$Bias
n_model_ET_AOD <- nrow(test_data)

resultados_ET_cv_sAOD
R2_model_ET_sAOD <- resultados_ET_cv_sAOD$R2
RMSE_model_ET_sAOD <- resultados_ET_cv_sAOD$RMSE
Bias_model_ET_sAOD <- resultados_ET_cv_sAOD$Bias
n_model_ET_sAOD <- nrow(test_data)


# Crear el gráfico con ggplot2
plot_regresion_ET <- ggplot(df_combinado) +
  geom_point(aes(y = real, x= pred_sAOD),color =   "#41b6c4", alpha=0.9,size = 1.5,shape=20, ) +  # Puntos de datos
  
  geom_point(aes(y = real, x= pred_AOD),color = "#023858" ,   size = 1.5, shape=8) +  # Puntos de datos
  
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  geom_smooth(aes(y = real, x= pred_sAOD),method = "lm", color = "red", se = FALSE,size = 0.6, linetype = "dashed") +  # Línea de regresión
  geom_smooth(aes(y = real, x= pred_AOD),method = "lm", color = "red", se = FALSE,size = 0.6, linetype = "solid") +  # Línea de regresión
  
  labs(
    x = "Observado",
    y = "Prediccion",
    #subtitle = "XGB Sin AOD",
    #title = "BSQ"
  ) +
  theme(
    #legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )+
  # ggplot2::annotate("text",x = 130, y = 90,label = paste("Modelo RF Sin AOD"), size = 3, color = "black")+
  # ggplot2::annotate("text",x = 100, y = 70,label = paste("sAOD"), size = 3, color = "black")+
  # 
  # ggplot2::annotate("text",x = 100, y = 60,label = paste("R² =", round(R2_model_ET_sAOD, 2)), size = 3, color = "black")+
  # 
  # ggplot2::annotate("text",x = 100, y = 50,label = paste("RMSE =", round(RMSE_model_ET_sAOD, 2)), size = 3, color = "black")+
  # 
  # ggplot2::annotate("text",x = 100, y = 40,label = paste("Bias =", round(Bias_model_ET_sAOD, 2)), size = 3, color = "black")+
  # 
  # ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n_model_ET_sAOD, 2)), size = 3, color = "black")+
  # 
# 
# ggplot2::annotate("text",x = 130, y = 70,label = paste("AOD"), size = 3, color = "black")+
# 
# ggplot2::annotate("text",x = 130, y = 60,label = paste("R² =", round(R2_model_ET_AOD, 2)), size = 3, color = "black")+
# 
# ggplot2::annotate("text",x = 130, y = 50,label = paste("RMSE =", round(RMSE_model_ET_AOD, 2)), size = 3, color = "black")+
# 
# ggplot2::annotate("text",x = 130, y = 40,label = paste("Bias =", round(Bias_model_ET_AOD, 2)), size = 3, color = "black")+
# 
# ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n_model_ET_AOD, 2)), size = 3, color = "black")+
# 
# 
theme_classic() #+
plot_regresion_ET

###########################################################
########################################################
estacion <-"BA"
modelo <- "1"
#Oscuro SAOD - Claro con AOD
# SP "#00441b","#238b45"
#ST "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dba",

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
dir <- paste("D:/Josefina/Proyectos/tesis/",estacion,"/modelos/",sep="")
setwd(dir)

# para seleccionar el modelo
list.files(pattern = "ET")
#Oscuro SAOD - Claro con AOD
# SP "#00441b","#238b45"
#ST "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dba",
load("01-ET-CV-M1-170625-BA.RData")
modelo_ET_AOD <- modelo_ET_cv
load("02-ET-CV-M1-230625-sAOSBA.RData" )
modelo_ET_sinAOD <- modelo_ET_cv  # Guardás con un nuevo nombre
## predicciones
predicciones_AOD <- predict(modelo_ET_AOD, newdata = test_data)
predicciones_sAOD <- predict(modelo_ET_sinAOD, newdata = test_data)

df_combinado <- data.frame(pred_AOD=predicciones_AOD, pred_sAOD=predicciones_sAOD,real=test_data$PM25)

## Metricas
resultados_ET_cv_AOD <- evaluar_modelo(modelo=modelo_ET_AOD, datos_test=test_data,
                                       variable_real = "PM25",tipoModelo="ET",y_test=NA)

resultados_ET_cv_sAOD <- evaluar_modelo(modelo=modelo_ET_sinAOD, datos_test=test_data,
                                        variable_real = "PM25",tipoModelo="ET",y_test=NA)

#resultados_ET_cv_AOD
R2_model_ET_AOD <- resultados_ET_cv_AOD$R2
RMSE_model_ET_AOD <- resultados_ET_cv_AOD$RMSE
Bias_model_ET_AOD <- resultados_ET_cv_AOD$Bias
n_model_ET_AOD <- nrow(test_data)

resultados_ET_cv_sAOD
R2_model_ET_sAOD <- resultados_ET_cv_sAOD$R2
RMSE_model_ET_sAOD <- resultados_ET_cv_sAOD$RMSE
Bias_model_ET_sAOD <- resultados_ET_cv_sAOD$Bias
n_model_ET_sAOD <- nrow(test_data)

# BA  "#99000d"  "#fb6a4a",
# Crear el gráfico con ggplot2
plot_regresion_ET <- ggplot(df_combinado) +
  geom_point(aes(y = real, x= pred_sAOD),color =    "#fb6a4a", alpha=0.9,size = 1.5,shape=20, ) +  # Puntos de datos
  
  geom_point(aes(y = real, x= pred_AOD),color = "#99000d" ,   size = 1.5, shape=8) +  # Puntos de datos
  
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  geom_smooth(aes(y = real, x= pred_sAOD),method = "lm", color = "red", se = FALSE,size = 0.6, linetype = "dashed") +  # Línea de regresión
  geom_smooth(aes(y = real, x= pred_AOD),method = "lm", color = "red", se = FALSE,size = 0.6, linetype = "solid") +  # Línea de regresión
  
  labs(
    x = "Observado",
    y = "Prediccion",
    #subtitle = "XGB Sin AOD",
    #title = "BSQ"
  ) +
  theme(
    #legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )+
  # ggplot2::annotate("text",x = 130, y = 90,label = paste("Modelo RF Sin AOD"), size = 3, color = "black")+
  # ggplot2::annotate("text",x = 100, y = 70,label = paste("sAOD"), size = 3, color = "black")+
  # 
  # ggplot2::annotate("text",x = 100, y = 60,label = paste("R² =", round(R2_model_ET_sAOD, 2)), size = 3, color = "black")+
  # 
  # ggplot2::annotate("text",x = 100, y = 50,label = paste("RMSE =", round(RMSE_model_ET_sAOD, 2)), size = 3, color = "black")+
  # 
  # ggplot2::annotate("text",x = 100, y = 40,label = paste("Bias =", round(Bias_model_ET_sAOD, 2)), size = 3, color = "black")+
  # 
  # ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n_model_ET_sAOD, 2)), size = 3, color = "black")+
  # 
# 
# ggplot2::annotate("text",x = 130, y = 70,label = paste("AOD"), size = 3, color = "black")+
# 
# ggplot2::annotate("text",x = 130, y = 60,label = paste("R² =", round(R2_model_ET_AOD, 2)), size = 3, color = "black")+
# 
# ggplot2::annotate("text",x = 130, y = 50,label = paste("RMSE =", round(RMSE_model_ET_AOD, 2)), size = 3, color = "black")+
# 
# ggplot2::annotate("text",x = 130, y = 40,label = paste("Bias =", round(Bias_model_ET_AOD, 2)), size = 3, color = "black")+
# 
# ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n_model_ET_AOD, 2)), size = 3, color = "black")+
# 

theme_classic() #+
plot_regresion_ET

##############################################
############################################

estacion <-"SP"
modelo <- "1"
#Oscuro SAOD - Claro con AOD
# SP "#00441b","#238b45"
#ST "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dba",

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
dir <- paste("D:/Josefina/Proyectos/tesis/",estacion,"/modelos/",sep="")
setwd(dir)

# para seleccionar el modelo
list.files(pattern = "XGB")
#Oscuro SAOD - Claro con AOD
# SP "#00441b","#238b45"
#ST "#fc4e2a",  "#feb24c",
# BA  "#99000d"  "#fb6a4a",
#MD "#023858", "#4292c6"
# MX "#3f007d", "#807dba",
load("01-XGB-CV-M1-200525-SP.RData")
modelo_XGB_AOD <- xgb_cv_model
load("02-XGB-CV-1-210525-sAODSP.RData" )
modelo_XGB_sinAOD <- xgb_cv_model  # Guardás con un nuevo nombre
## predicciones

# Solo para XGB

X_test_sAOD <- test_data[ ,c( #"AOD_055",
  "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia" "d2m_mean", "DEM"
  "SO2SMASS_dia",  "SO4SMASS_dia", "SSSMASS_dia","blh_mean", 
  "sp_mean", "d2m_mean","v10_mean", "u10_mean", "tp_mean","dayWeek"
)]#
X_test_AOD <- test_data[ ,c( "AOD_055",
                             "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia" "d2m_mean", "DEM"
                             "SO2SMASS_dia",  "SO4SMASS_dia", "SSSMASS_dia","blh_mean", 
                             "sp_mean","d2m_mean", "v10_mean", "u10_mean", "tp_mean","dayWeek"
)]#


y_test<- test_data$PM25
dtest_AOD <- xgb.DMatrix(data = as.matrix(X_test_AOD), label = y_test)
dtest_sAOD <- xgb.DMatrix(data = as.matrix(X_test_sAOD), label = y_test)
predicciones_AOD <- predict(modelo_XGB_AOD, newdata = dtest_AOD)
predicciones_sAOD <- predict(modelo_XGB_sinAOD, newdata = dtest_sAOD)


df_combinado <- data.frame(pred_AOD=predicciones_AOD, pred_sAOD=predicciones_sAOD,real=test_data$PM25)

## Metricas
resultados_XGB_cv_AOD <- evaluar_modelo(modelo=modelo_XGB_AOD, datos_test=dtest_AOD,
                                        variable_real = "PM25",tipoModelo="XGB",y_test=y_test)

resultados_XGB_cv_sAOD <- evaluar_modelo(modelo=modelo_XGB_sinAOD, datos_test=dtest_sAOD,
                                         variable_real = "PM25",tipoModelo="XGB",y_test=y_test)

#resultados_XGB_cv_AOD
R2_model_XGB_AOD <- resultados_XGB_cv_AOD$R2
RMSE_model_XGB_AOD <- resultados_XGB_cv_AOD$RMSE
Bias_model_XGB_AOD <- resultados_XGB_cv_AOD$Bias
n_model_XGB_AOD <- nrow(test_data)

resultados_XGB_cv_sAOD
R2_model_XGB_sAOD <- resultados_XGB_cv_sAOD$R2
RMSE_model_XGB_sAOD <- resultados_XGB_cv_sAOD$RMSE
Bias_model_XGB_sAOD <- resultados_XGB_cv_sAOD$Bias
n_model_XGB_sAOD <- nrow(test_data)

# BA  "#99000d"  "#fb6a4a",
# Crear el gráfico con ggplot2
plot_regresion_XGB <- ggplot(df_combinado) +
  geom_point(aes(y = real, x= pred_sAOD),color =  "#238b45"  , alpha=0.9,size = 1.5,shape=20, ) +  # Puntos de datos
  
  geom_point(aes(y = real, x= pred_AOD),color = "#00441b",  size = 1.5, shape=8) +  # Puntos de datos
  
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  geom_smooth(aes(y = real, x= pred_sAOD),method = "lm", color = "red", se = FALSE,size = 0.6, linetype = "dashed") +  # Línea de regresión
  geom_smooth(aes(y = real, x= pred_AOD),method = "lm", color = "red", se = FALSE,size = 0.6, linetype = "solid") +  # Línea de regresión
  
  labs(
    x = "Observado",
    y = "Prediccion",
    #subtitle = "XGB Sin AOD",
    #title = "BSQ"
  ) +
  theme(
    #legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )+
  # ggplot2::annotate("text",x = 130, y = 90,label = paste("Modelo RF Sin AOD"), size = 3, color = "black")+
  # ggplot2::annotate("text",x = 100, y = 70,label = paste("sAOD"), size = 3, color = "black")+
  # 
  # ggplot2::annotate("text",x = 100, y = 60,label = paste("R² =", round(R2_model_XGB_sAOD, 2)), size = 3, color = "black")+
  # 
  # ggplot2::annotate("text",x = 100, y = 50,label = paste("RMSE =", round(RMSE_model_XGB_sAOD, 2)), size = 3, color = "black")+
  # 
  # ggplot2::annotate("text",x = 100, y = 40,label = paste("Bias =", round(Bias_model_XGB_sAOD, 2)), size = 3, color = "black")+
  # 
  # ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n_model_XGB_sAOD, 2)), size = 3, color = "black")+
  # 
# 
# ggplot2::annotate("text",x = 130, y = 70,label = paste("AOD"), size = 3, color = "black")+
# 
# ggplot2::annotate("text",x = 130, y = 60,label = paste("R² =", round(R2_model_XGB_AOD, 2)), size = 3, color = "black")+
# 
# ggplot2::annotate("text",x = 130, y = 50,label = paste("RMSE =", round(RMSE_model_XGB_AOD, 2)), size = 3, color = "black")+
# 
# ggplot2::annotate("text",x = 130, y = 40,label = paste("Bias =", round(Bias_model_XGB_AOD, 2)), size = 3, color = "black")+
# 
# ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n_model_XGB_AOD, 2)), size = 3, color = "black")+


theme_classic() #+
plot_regresion_XGB
