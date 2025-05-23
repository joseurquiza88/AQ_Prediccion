#################################################################################
#################################################################################
#            Modelos Predictivos de PM2.5 importancia de variables
#################################################################################
#################################################################################
# RF
#funcion para evaluar modelos
evaluar_modelo <- function(modelo, datos_test, variable_real = "PM25",tipoModelo,y_test=NA) {
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
#############
# cargar el modelo y aplicalo a otro set de datos
estacion<- "SP"
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos",sep = ""))

# Paso 1: Cargar el modelo
load("01-RF-CV-M1-200525-SP.RData")


## Importancia de las variables
importancia <- varImp(modelo_RF_cv, scale = TRUE)
print(importancia)
plot(importancia, main = "Importancia de Variables M1")

# Gráfico personalizado con ggplot2
importancia_df <- as.data.frame(importancia$importance)
importancia_df$Variable <- rownames(importancia_df)
importancia_df$Variable2 <- recode(importancia_df$Variable,
                                   "tp_mean" = "tp",
                                   "blh_mean" = "blh",
                                   "d2m_mean" = "d2m",
                                   #"t2m_mean" = "t2m",
                                   "v10_mean" = "v10",
                                   "u10_mean" = "u10",
                                   "BCSMASS_dia" = "BCSMASS",
                                   "DUSMASS_dia" = "DUSMASS",
                                   "SO2SMASS_dia" = "SO2SMASS",
                                   "SO4SMASS_dia" = "SO4SMASS",
                                   "SSSMASS_dia" = "SSSMASS",
                                   "AOD_055" = "AOD",
                                   "ndvi" = "NDVI",
                                   "sp_mean" = "SP",
                                   "dayWeek" =  "dayWeek",
                                   # Dejá las que no cambian fuera o ponelas igual a sí mismas
                                   .default = importancia_df$Variable
)

ggplot(importancia_df, aes(x = reorder(Variable2, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  #geom_hline(yintercept = 50, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_classic() +
  labs(#title = "Importancia de Variables 02-RF_cv_M1-090125_MX", 
    x = "Variable", 
    y = "Importancia Modelo RF") +
  theme(axis.text.y = element_text(size = 8))  # Ajusta

#######################################################
# Corremos el modelo eliminando variables de a 1
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

#Dayweek, sp, ssmass, aod, so4mass

modelo_RF_cv <- train(PM25 ~ ndvi + BCSMASS_dia + DUSMASS_dia + 
                        SO2SMASS_dia +    blh_mean + #SO4SMASS_dia +
                        d2m_mean + v10_mean + u10_mean + tp_mean,# +  AOD_055 + dayWeek,sp_mean + SSSMASS_dia
                      data = train_data, method = "rf",
                      trControl = train_control,importance = TRUE)

resultados_RF_cv <- evaluar_modelo(modelo=modelo_RF_cv, datos_test=test_data,
                                   variable_real = "PM25",tipoModelo="RF",y_test=NA)
print(resultados_RF_cv)



##############################################################################
##############################################################################
#XGB Importancia

# cargar el modelo y aplicalo a otro set de datos
estacion<- "SP"
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos",sep = ""))

# Paso 1: Cargar el modelo
load("01-XGB-CV-M1-200525-SP.RData")

# Importancia XGB
importance_matrix <- xgb.importance(model = xgb_cv_model)
xgb.plot.importance(importance_matrix = importance_matrix)
importance_matrix


# Asumimos que 'importance_matrix' es el resultado de xgb.importance
importancia_df <- as.data.frame(importance_matrix)
importancia_df$Variable <- rownames(importancia_df)

importancia_df$Variable2 <- recode(importancia_df$Feature,
                                   "tp_mean" = "tp",
                                   "blh_mean" = "blh",
                                   "d2m_mean" = "d2m",
                                   #"t2m_mean" = "t2m",
                                   "v10_mean" = "v10",
                                   "u10_mean" = "u10",
                                   "BCSMASS_dia" = "BCSMASS",
                                   "DUSMASS_dia" = "DUSMASS",
                                   "SO2SMASS_dia" = "SO2SMASS",
                                   "SO4SMASS_dia" = "SO4SMASS",
                                   "SSSMASS_dia" = "SSSMASS",
                                   "AOD_055" = "AOD",
                                   "ndvi" = "NDVI",
                                   "sp_mean" = "SP",
                                   "dayWeek" =  "dayWeek",
                                   # Dejá las que no cambian fuera o ponelas igual a sí mismas
                                   .default = importancia_df$Feature
)



nameModel<- "02-XGB-cv-TunGrid_M1-280224_MX"
# Crear el gráfico con ggplot
ggplot(importancia_df, aes(x = reorder(Variable2,Gain) , y = (Gain*100))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  #geom_hline(yintercept = 50, linetype = "dashed", color = "red") +  # Línea horizontal de referencia
  coord_flip() +  # Cambia el eje x e y
  theme_classic() +  # Estilo limpio
  labs(#title = "Importancia de Variables 02-XGB_M1-260225_MX", 
       x = "Variables", 
       y = "Importancia Modelo XGB") +
  theme(axis.text.y = element_text(size = 8))  # Ajustar el tamaño de la fuente en el eje Y

#######################################################
# Corremos el modelo eliminando variables de a 1

### ----- XGB   -----

estacion <-"SP"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))


X <- train_data[ , c( #"AOD_055",
                      "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia"
                      "SO2SMASS_dia",   "blh_mean", #"SO4SMASS_dia",
                      "d2m_mean", "v10_mean", #,"dayWeek"+"sp_mean","SSSMASS_dia",
                      
                      "u10_mean", "tp_mean")]#


y <- train_data$PM25

X_test <- test_data[ ,c( #"AOD_055",
                         "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia"
                         "SO2SMASS_dia",   "blh_mean", #"SO4SMASS_dia",
                          "d2m_mean", "v10_mean", #,"dayWeek"+"sp_mean","SSSMASS_dia",
                         
                         "u10_mean", "tp_mean")]#
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


############################################################################
###########################################################################
## ploots
##########################################
data_metricas <- read.csv("D:/Josefina/Proyectos/ProyectoChile/CH/modelos/Salidas/metricas/metricas.csv")
data_metricas <- data_metricas[data_metricas$tipo == "RF",]
data_metricas <- data_metricas[data_metricas$prueba == "2",]
data_metricas <- data_metricas[2:6,]
# SP - RF
library(ggplot2)

# Datos
data_metricas <- data.frame(
  numModelo = c(1, 2, 3, 4, 5),
  #rmse = c(5.96, 5.93, 5.99, 5.894, 5.979),
  #r2 = c(0.7, 0.72, 0.70, 0.72088, 0.70966)
  
  rmse = c(5.975,5.852,5.59,5.985,5.856),
  r2 = c(0.69481,0.70737,0.70361,0.69389,0.70669)
)

# Escalar RMSE para que esté en la misma escala que R2 (0 a 1)
max_rmse <- max(data_metricas$rmse)
min_rmse <- min(data_metricas$rmse)

data_metricas$rmse_escalado <- (data_metricas$rmse - min_rmse) / (max_rmse - min_rmse)

# RMSE y R2 para el modelo con todas las variables (RF por ejemplo)
rmse_all <- 5.74
r_all <- 0.71822

# Escalar también rmse_all para que se muestre correctamente
rmse_all_escalado <- (rmse_all - min_rmse) / (max_rmse - min_rmse)

# Gráfico
ggplot(data_metricas, aes(x = numModelo)) +
  geom_line(aes(y = r2, color = "R²"), size = 1) +
  geom_point(aes(y = r2, color = "R²"), size = 2) +
  geom_line(aes(y = rmse_escalado, color = "RMSE"), size = 0.5) +
  geom_point(aes(y = rmse_escalado, color = "RMSE"), size = 2) +
  scale_y_continuous(
    name = "R²",
    sec.axis = sec_axis(~ . * (max_rmse - min_rmse) + min_rmse, name = "RMSE")
  ) +
  geom_line(aes(y = r_all, color = "R² Todas las variables"), size = 0.5, linetype = "dashed") +
  geom_line(aes(y = rmse_all_escalado, color = "RMSE Todas las variables"), size = 0.5, linetype = "dashed") +
  scale_x_continuous(breaks = 1:5) + 
  scale_color_manual(values = c(
    "R²" = "#2c7fb8",  
    "R² Todas las variables" = "black",
    "RMSE" = "#cb181d", 
    "RMSE Todas las variables" = "#fb6a4a"
  )) +
  labs(
    x = "Modelo XGB", 
    color = ""
  ) +
  theme_classic()

# theme_minimal(base_size = 16)
