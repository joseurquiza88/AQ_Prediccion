
#################################################################################
#################################################################################
#                           Modelos Predictivos de PM2.5 con CV espacial
#################################################################################
#################################################################################

#funcion para evaluar modelos
evaluar_modelo <- function(modelo, datos_test, variable_real = "PM25") {
  # Hacer predicciones
  predicciones <- predict(modelo, newdata = datos_test)
  
  # Extraer los valores reales de la variable objetivo
  valores_reales <- datos_test[[variable_real]]
  
  # Calcular métricas
  r2 <- cor(predicciones, valores_reales)^2
  pearson <- cor(valores_reales, predicciones, method = "pearson")
  rmse <- sqrt(mean((predicciones - valores_reales)^2))
  bias <- mean(predicciones - valores_reales)
  
  # Resultados
  resultados <- data.frame(
    R2 = round(r2, 5),
    Pearson = round(pearson, 2),
    RMSE = round(rmse, 2),
    Bias = round(bias, 2),
    Min_Pred = round(min(predicciones), 2),
    Max_Pred = round(max(predicciones), 2)
  )
  
  return(resultados)
}
##############################################################################
##############################################################################
##############################################################################

################# SVR ESPACIAL
estacion <- "MD"
modelo <- "1"

test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))


# 1. Obtener las estaciones únicas
stations <- unique(train_data$estacion)
#stations <- unique(train_data$ID)
# 2. Crear listas de índices personalizados
index_list <- list()
indexOut_list <- list()

for (i in seq_along(stations)) {
  test_station <- stations[i]
  train_index <- which(train_data$estacion != test_station)
  test_index <- which(train_data$estacion == test_station)
  
  index_list[[i]] <- train_index
  indexOut_list[[i]] <- test_index
}

# 3. Definir el control de entrenamiento con CV por estación
train_control_spatial <- trainControl(
  method = "cv",
  number = length(stations),
  index = index_list,
  indexOut = indexOut_list,
  savePredictions = "final",
  verboseIter = TRUE,
  allowParallel = TRUE
)

# 4. Entrenar el modelo SVR con validación cruzada espacial
modelo_svr_spatial <- train(
  PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
    SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
    d2m_mean  +t2m_mean +v10_mean + u10_mean + tp_mean + DEM+ dayWeek,
  data = train_data,
  method = "svmRadial",
  trControl = train_control_spatial,
  preProcess = c("center", "scale"),
  tuneLength = 5
)

# Metricas globales
resultados_SVR_cv_Espacial <- evaluar_modelo(modelo_svr_spatial, test_data)
print(resultados_SVR_cv_Espacial)

#Metricas por estacion
df_metricas<- data.frame(modelo_svr_spatial[["resample"]])
max_rmse <- max(df_metricas$RMSE)
min_rmse <- min(df_metricas$RMSE)

df_metricas$rmse_escalado <- (df_metricas$RMSE - min_rmse) / (max_rmse - min_rmse)

# df_metricas$estacion <- recode(df_metricas$Resample,
#                                    "Resample01" = "BSQ",
#                                    "Resample02" = "IND",
#                                    "Resample03" = "CERR-I",
#                                    "Resample04" = "CER-II",
#                                    "Resample05" = "CNA",
#                                    "Resample06" = "FLD",
#                                    "Resample07" = "CDE",
#                                    "Resample08" = "PDH",
#                                    "Resample09" = "PTA",
#                                    "Resample10" = "QUII",
#                                    "Resample11" = "OHG",
#                                    "Resample12" = "QUI",
#                                    # Dejá las que no cambian fuera o ponelas igual a sí mismas
#                                    .default = df_metricas$Resample
# )



# df_metricas$estacion <- recode(df_metricas$Resample,
#                                "Resample01" = "Carapicuiba",
#                                "Resample02" = "Marg.Tiete-Pte Remedios",
#                                "Resample03" = "Maua",
#                                "Resample04" = "Pico do Jaragua",
#                                "Resample05" = "Pinheiros",
#                                "Resample06" = "Santana",
#                                "Resample07" = "Santo Amaro",
#                                "Resample08" = "Tabao da Serra",
#                                "Resample09" = "Parque D.Pedro II",
#                                "Resample10" = "Guarulhos-Palco Municipal",
#                                "Resample11" = "Osasco",
#                                "Resample12" = "Ciudad Universitaria - USP",
#                                "Resample13" = "Guarulhos - Pimentas",
#                                "Resample14" = "Ibirapuera",
#                                "Resample15" = "Interlagos",
#                                "Resample16" = "Itaim Paulista",
#                                # Dejá las que no cambian fuera o ponelas igual a sí mismas
#                                .default = df_metricas$Resample
# )

df_metricas$estacion <- recode(df_metricas$Resample,
                               "Resample01" = "Estacion Trafico Centro",
                               "Resample02" = "Casa de Justicia Itagui",
                               "Resample03" = "Concejo Municipal de Itagui",
                               "Resample04" = "Caldas Joaquin Aristizabal",
                               "Resample05" = "La Estrella",
                               "Resample06" = "Medellin Altavista",
                               "Resample07" = "Medellin Villahermosa",
                               "Resample08" = "Barbosa - Torre Social",
                               "Resample09" = "Copacabana",
                               "Resample10" = "Medellin - Belen",
                               "Resample11" = "Medellin - El Poblado",
                               "Resample12" = "Medellin - San Cristobal",
                               "Resample13" = "Medellin - Aranjuez",
                               "Resample14" = "Bello - Fernando Velez",
                               "Resample15" = "Envigado - Santa Gertrudis",
                               "Resample16" ="Sabaneta - Rafael J. Mejia",
                               "Resample17" ="Medellin - Santa Elena",
                               # Dejá las que no cambian fuera o ponelas igual a sí mismas
                               .default = df_metricas$Resample
)

# Ordenar las estaciones por R² de mayor a menor
df_metricas <- df_metricas %>%
  arrange(desc(Rsquared)) %>%
  mutate(estacion = factor(estacion, levels = estacion))  # fija el orden en el gráfico
#c("R²" = "blue", "RMSE" = "red")) +
# Crear el histograma
r2<-ggplot(df_metricas, aes(x = estacion, y = Rsquared)) +
  geom_bar(stat = "identity", color = "#0570b0", fill = "#74a9cf") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "SVR Spatial", y = "R² ") +
  theme_classic() +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),  # Agrandar los números de los ticks
    legend.title = element_text(family = "Roboto", size = 6, face = 2),
    legend.position = "top",  # Elimina la leyenda
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 


r2

rmse<-ggplot(df_metricas, aes(x = estacion, y = RMSE)) +
  geom_bar(stat = "identity", color = "#b30000", fill = "#e34a33") +
  scale_y_continuous(limits = c(0, 11.5)) +
  labs(x = "SVR Spatial", y = "RMSE ") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),  # Agrandar los números de los ticks
    legend.title = element_text(family = "Roboto", size = 6, face = 2),
    legend.position = "top",  # Elimina la leyenda
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 


r2
rmse
##############################################################################
##############################################################################
##############################################################################
library(caret)
library(ranger)
################# ET ESPACIAL
estacion <- "MD"
modelo <- "1"

test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
set.seed(123)

# 1. Estaciones únicas
stations <- unique(train_data$estacion)

# 2. Índices personalizados
index_list <- list()
indexOut_list <- list()

for (i in seq_along(stations)) {
  test_station <- stations[i]
  train_index <- which(train_data$estacion != test_station)
  test_index <- which(train_data$estacion == test_station)
  
  index_list[[i]] <- train_index
  indexOut_list[[i]] <- test_index
}

# 3. Control de entrenamiento con CV por estación
train_control_spatial <- trainControl(
  method = "cv",
  number = length(stations),
  index = index_list,
  indexOut = indexOut_list,
  savePredictions = "final",
  verboseIter = TRUE,
  allowParallel = TRUE
)

# 4. Entrenar el modelo Extra Trees (ranger con splitrule = "extratrees")
modelo_et_spatial <- train(
  PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
    SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
    d2m_mean  +t2m_mean +v10_mean + u10_mean + tp_mean + DEM+ dayWeek,
  data = train_data,
  method = "ranger",
  trControl = train_control_spatial,
  tuneGrid = data.frame(
    mtry = 5,                   # valor fijo
    splitrule = "extratrees",  # Extra Trees
    min.node.size = 5          # valor fijo
  ),
  importance = 'impurity'
)
getwd()
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()


#Guardar modelo
save(modelo_et_spatial, file=paste("01-ET-CV-Esp_M",modelo,"-260525-",estacion,".RData",sep=""))
# Metricas globales
resultados_ET_cv_Espacial <- evaluar_modelo(modelo_et_spatial, test_data)
print(resultados_ET_cv_Espacial)

df_metricas<- data.frame(modelo_et_spatial[["resample"]])
max_rmse <- max(df_metricas$RMSE)
min_rmse <- min(df_metricas$RMSE)

df_metricas$rmse_escalado <- (df_metricas$RMSE - min_rmse) / (max_rmse - min_rmse)
#--CH
# df_metricas$estacion <- recode(df_metricas$Resample,
#                                    "Resample01" = "BSQ",
#                                    "Resample02" = "IND",
#                                    "Resample03" = "CERR-I",
#                                    "Resample04" = "CER-II",
#                                    "Resample05" = "CNA",
#                                    "Resample06" = "FLD",
#                                    "Resample07" = "CDE",
#                                    "Resample08" = "PDH",
#                                    "Resample09" = "PTA",
#                                    "Resample10" = "QUII",
#                                    "Resample11" = "OHG",
#                                    "Resample12" = "QUI",
#                                    # Dejá las que no cambian fuera o ponelas igual a sí mismas
#                                    .default = df_metricas$Resample
# )


#--SP
# df_metricas$estacion <- recode(df_metricas$Resample,
#                                "Resample01" = "Carapicuiba",
#                                "Resample02" = "Marg.Tiete-Pte Remedios",
#                                "Resample03" = "Maua",
#                                "Resample04" = "Pico do Jaragua",
#                                "Resample05" = "Pinheiros",
#                                "Resample06" = "Santana",
#                                "Resample07" = "Santo Amaro",
#                                "Resample08" = "Tabao da Serra",
#                                "Resample09" = "Parque D.Pedro II",
#                                "Resample10" = "Guarulhos-Palco Municipal",
#                                "Resample11" = "Osasco",
#                                "Resample12" = "Ciudad Universitaria - USP",
#                                "Resample13" = "Guarulhos - Pimentas",
#                                "Resample14" = "Ibirapuera",
#                                "Resample15" = "Interlagos",
#                                "Resample16" = "Itaim Paulista",
#                                # Dejá las que no cambian fuera o ponelas igual a sí mismas
#                                .default = df_metricas$Resample
# )

# Ordenar las estaciones por R² de mayor a menor
df_metricas <- df_metricas %>%
  arrange(desc(Rsquared)) %>%
  mutate(estacion = factor(estacion, levels = estacion))  # fija el orden en el gráfico
#c("R²" = "blue", "RMSE" = "red")) +
# Crear el histograma
r2<-ggplot(df_metricas, aes(x = estacion, y = Rsquared)) +
  geom_bar(stat = "identity", color = "#0570b0", fill = "#74a9cf") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "ET Spatial", y = "R² ") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),  # Agrandar los números de los ticks
    legend.title = element_text(family = "Roboto", size = 6, face = 2),
    legend.position = "top",  # Elimina la leyenda
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 



rmse<-ggplot(df_metricas, aes(x = estacion, y = RMSE)) +
  geom_bar(stat = "identity", color = "#b30000", fill = "#e34a33") +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, 2)) +
  labs(x = "ET Spatial", y = "RMSE ") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),  # Agrandar los números de los ticks
    legend.title = element_text(family = "Roboto", size = 6, face = 2),
    legend.position = "top",  # Elimina la leyenda
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 
r2
rmse
##############################################################################
##############################################################################
##############################################################################

################# RF  ESPACIAL
estacion <- "SP"
modelo <- "1"

test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))

# Estaciones
stations <- unique(train_data$estacion)

# Índices personalizados
index_list <- list()
indexOut_list <- list()

for (i in seq_along(stations)) {
  test_station <- stations[i]
  train_index <- which(train_data$estacion != test_station)
  test_index <- which(train_data$estacion == test_station)
  
  index_list[[i]] <- train_index
  indexOut_list[[i]] <- test_index
}

# Control de entrenamiento
train_control_spatial <- trainControl(
  method = "cv",
  number = length(stations),
  index = index_list,
  indexOut = indexOut_list,
  savePredictions = "final",
  verboseIter = TRUE,
  allowParallel = TRUE
)

# Modelo
rf_spatial_model <- train(
  PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
    SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + sp_mean +
    d2m_mean + v10_mean + u10_mean + tp_mean + dayWeek,
  data = train_data,
  method = "rf",
  trControl = train_control_spatial,
  tuneGrid = data.frame(mtry = 5),  # <<---- fijás mtry aquí
  importance = TRUE
)

save(rf_spatial_model, file=paste("01-RF-CV-Esp_M",modelo,"-200525-",estacion,".RData",sep=""))



load(paste("11-RF-CV-Esp_M",modelo,"-100425-",estacion,".RData",sep=""))


df_metricas<- data.frame(rf_spatial_model[["resample"]])
max_rmse <- max(df_metricas$RMSE)
min_rmse <- min(df_metricas$RMSE)

df_metricas$rmse_escalado <- (df_metricas$RMSE - min_rmse) / (max_rmse - min_rmse)

# df_metricas$estacion <- recode(df_metricas$Resample,
#                                    "Resample01" = "BSQ",
#                                    "Resample02" = "IND",
#                                    "Resample03" = "CERR-I",
#                                    "Resample04" = "CER-II",
#                                    "Resample05" = "CNA",
#                                    "Resample06" = "FLD",
#                                    "Resample07" = "CDE",
#                                    "Resample08" = "PDH",
#                                    "Resample09" = "PTA",
#                                    "Resample10" = "QUII",
#                                    "Resample11" = "OHG",
#                                    "Resample12" = "QUI",
#                                    # Dejá las que no cambian fuera o ponelas igual a sí mismas
#                                    .default = df_metricas$Resample
# )



df_metricas$estacion <- recode(df_metricas$Resample,
                               "Resample01" = "Carapicuiba",
                               "Resample02" = "Marg.Tiete-Pte Remedios",
                               "Resample03" = "Maua",
                               "Resample04" = "Pico do Jaragua",
                               "Resample05" = "Pinheiros",
                               "Resample06" = "Santana",
                               "Resample07" = "Santo Amaro",
                               "Resample08" = "Tabao da Serra",
                               "Resample09" = "Parque D.Pedro II",
                               "Resample10" = "Guarulhos-Palco Municipal",
                               "Resample11" = "Osasco",
                               "Resample12" = "Ciudad Universitaria - USP",
                               "Resample13" = "Guarulhos - Pimentas",
                               "Resample14" = "Ibirapuera",
                               "Resample15" = "Interlagos",
                               "Resample16" = "Itaim Paulista",
                               # Dejá las que no cambian fuera o ponelas igual a sí mismas
                               .default = df_metricas$Resample
)


# Crear el gráfico
library(ggplot2)
library(dplyr)

# Ordenar las estaciones por R² de mayor a menor
df_metricas <- df_metricas %>%
  arrange(desc(Rsquared)) %>%
  mutate(estacion = factor(estacion, levels = estacion))  # fija el orden en el gráfico
#c("R²" = "blue", "RMSE" = "red")) +
# Crear el histograma
r2<-ggplot(df_metricas, aes(x = estacion, y = Rsquared)) +
  geom_bar(stat = "identity", color = "#0570b0", fill = "#74a9cf") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "RF Spatial", y = "R² ") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



rmse<-ggplot(df_metricas, aes(x = estacion, y = RMSE)) +
  geom_bar(stat = "identity", color = "#b30000", fill = "#e34a33") +
  scale_y_continuous(limits = c(0, 11.5)) +
  labs(x = "RF Spatial", y = "RMSE ") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/plots/R2-RF-espacial.png",sep=""),r2,
       width = 10,
       height = 8,
       units = "cm",
       dpi = 500)



##############################################################################
##############################################################################
##############################################################################
### ----- XGB   -----
# Este es distinto en comparacio con el resto de los modelos
library(xgboost)
library(Matrix)
library(Metrics)       # rmse, mae, etc.
library(dplyr)


estacion <- "SP"
modelo <- "1"
#Data modelo 1
test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))

# Estaciones
stations <- unique(train_data$estacion)

# Fórmula y selección de columnas
vars <- c("AOD_055",
          "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia"
          "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "blh_mean", 
          "sp_mean", "d2m_mean", "v10_mean",
          "u10_mean", "tp_mean",
          "dayWeek")
target <- "PM25"

predicciones <- data.frame()
pred_entrenamiento <- data.frame()

for (test_station in stations) {
  # Separar train/test según estación
  train_fold <- train_data %>% filter(estacion != test_station)
  test_fold <- train_data %>% filter(estacion == test_station)
  
  # Matrices para xgboost
  dtrain <- xgb.DMatrix(data = as.matrix(train_fold[, vars]), label = train_fold[[target]])
  dtest <- xgb.DMatrix(data = as.matrix(test_fold[, vars]), label = test_fold[[target]])
  
  # Entrenar modelo XGBoost
  xgb_model <- xgboost(
    data = dtrain,
    objective = "reg:squarederror",
    nrounds = 100,
    eta = 0.1,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8,
    verbose = 0
  )
  
  # Predicciones test
  preds_test <- predict(xgb_model, dtest)
  predicciones <- rbind(predicciones, data.frame(
    obs = test_fold[[target]],
    pred = preds_test
  ))
  
  # Predicciones train
  preds_train <- predict(xgb_model, dtrain)
  pred_entrenamiento <- rbind(pred_entrenamiento, data.frame(
    obs = train_fold[[target]],
    pred = preds_train
  ))
}


# TEST
r2_test <- cor(predicciones$obs, predicciones$pred)^2
pearson_test <- cor(predicciones$obs, predicciones$pred, method = "pearson")
rmse_test <- rmse(predicciones$obs, predicciones$pred)
bias_test <- mean(predicciones$pred - predicciones$obs)



cat("### TEST\n")
cat("R2 test:", round(r2_test, 3), "\n")
cat("R pearson test:", round(pearson_test, 3), "\n")
cat("RMSE test:", round(rmse_test, 3), "\n")
cat("Bias test:", round(bias_test, 3), "\n")
cat("min pred test:", round(min(predicciones$pred), 3), "\n")
cat("max pred test:", round(max(predicciones$pred), 3), "\n")



###############################
###############################
###############################
# DataFrames para guardar
predicciones <- data.frame()
resultados_por_estacion <- data.frame()

for (test_station in stations) {
  print(test_station)
  train_fold <- train_data %>% filter(estacion != test_station)
  test_fold <- train_data %>% filter(estacion == test_station)
  
  dtrain <- xgb.DMatrix(data = as.matrix(train_fold[, vars]), label = train_fold[[target]])
  dtest <- xgb.DMatrix(data = as.matrix(test_fold[, vars]), label = test_fold[[target]])
  
  xgb_model <- xgboost(
    data = dtrain,
    objective = "reg:squarederror",
    nrounds = 100,
    eta = 0.1,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8,
    verbose = 0
  )
  
  preds_test <- predict(xgb_model, dtest)
  
  # Guardar predicciones individuales
  predicciones <- rbind(predicciones, data.frame(
    estacion = test_station,
    obs = test_fold[[target]],
    pred = preds_test
  ))
  
  # Calcular métricas para esta estación
  r2 <- cor(test_fold[[target]], preds_test)^2
  pearson <- cor(test_fold[[target]], preds_test, method = "pearson")
  rmse_val <- rmse(test_fold[[target]], preds_test)
  mae_val <- mae(test_fold[[target]], preds_test)
  mape_val <- mape(test_fold[[target]], preds_test) * 100
  mse_val <- mse(test_fold[[target]], preds_test)
  medae_val <- median(abs(test_fold[[target]] - preds_test))
  
  resultados_por_estacion <- rbind(resultados_por_estacion, data.frame(
    estacion = test_station,
    R2 = r2,
    Pearson = pearson,
    RMSE = rmse_val,
    MAE = mae_val,
    MAPE = mape_val,
    MSE = mse_val,
    MedAE = medae_val
  ))
}

# Orden personalizado de estaciones
orden_estaciones <- c("CNA", "QUII", "PDH", "BSQ", "CER-II", "CERR-I", "OHG", "QUI", "IND", "FLD", "PTA", "CDE")
orden_estaciones <- c("Carapicuiba", "Marg.Tiete-Pte Remedios", "Maua", "Pico do Jaragua",
                      "Pinheiros", "Santana", "Santo Amaro", "Tabao da Serra","Parque D.Pedro II",
                      "Parque D.Pedro II","Guarulhos-Palco Municipal","Osasco","Ciudad Universitaria - USP",
                      "Guarulhos - Pimentas","Ibirapuera","Interlagos","Itaim Paulista")

resultados_por_estacion <- resultados_por_estacion2
# Aplicás ese orden al factor
resultados_por_estacion <- resultados_por_estacion %>%
  mutate(estacion = factor(estacion, levels = orden_estaciones))
# fija el orden en el gráfico

r2<-ggplot(resultados_por_estacion, aes(x = estacion, y = R2)) +
  geom_bar(stat = "identity", color = "#0570b0", fill = "#74a9cf") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "XGB Spatial", y = "R² ") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



rmse<-ggplot(resultados_por_estacion, aes(x = estacion, y = RMSE)) +
  geom_bar(stat = "identity", color = "#b30000", fill = "#e34a33") +
  scale_y_continuous(limits = c(0, 11.5)) +
  labs(x = "XGB Spatial", y = "RMSE ") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/plots/RMSE-XGB-espacial.png",sep=""),rmse,
       width = 10,
       height = 8,
       units = "cm",
       dpi = 500)