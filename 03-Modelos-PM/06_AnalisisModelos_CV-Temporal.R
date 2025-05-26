
#################################################################################
#################################################################################
#                           Modelos Predictivos de PM2.5 con CV TEMPORAL
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

################# SVR Temporal
estacion <- "SP"
modelo <- "1"
#Data modelo 1
test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
#nrow(test_data)+nrow(train_data)

# 1. Asegurarse de que tenés la columna 'year'
train_data$year <- as.numeric(format(as.Date(train_data$date), "%Y"))

# 2. Extraer los años únicos
years <- sort(unique(train_data$year))

# 3. Crear listas de índices de entrenamiento y prueba
index_list <- list()
indexOut_list <- list()

for (i in seq_along(years)) {
  test_year <- years[i]
  train_index <- which(train_data$year != test_year)
  test_index <- which(train_data$year == test_year)
  
  index_list[[i]] <- train_index
  indexOut_list[[i]] <- test_index
}

# 4. Crear control de entrenamiento para CV temporal
train_control_temporal <- trainControl(
  method = "cv",
  number = length(years),
  index = index_list,
  indexOut = indexOut_list,
  savePredictions = "final",
  verboseIter = TRUE,
  allowParallel = TRUE
)

# 5. Entrenar el modelo SVR con validación cruzada por año
set.seed(123)
modelo_cv_svr_temporal <- train(
  PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
    SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + sp_mean +
    d2m_mean + v10_mean + u10_mean + tp_mean + dayWeek,
  data = train_data,
  method = "svmRadial",
  trControl = train_control_temporal,
  preProcess = c("center", "scale"),
  tuneLength = 5
)

#Guardar
getwd()
save(modelo_cv_svr_temporal, file=paste("01-SVR-CV-Temp_M",modelo,"-200525-",estacion,".RData",sep=""))


### metricas por año
df_metricas<- data.frame(modelo_cv_svr_temporal[["resample"]])

max_rmse <- max(df_metricas$RMSE)
min_rmse <- min(df_metricas$RMSE)

df_metricas$rmse_escalado <- (df_metricas$RMSE - min_rmse) / (max_rmse - min_rmse)

df_metricas$year <- recode(df_metricas$Resample,
                           "Resample1" = "2015",
                           "Resample2" = "2016",
                           "Resample3" = "2017",
                           "Resample4" = "2018",
                           "Resample5" = "2019",
                           "Resample6" = "2020",
                           "Resample7" = "2021",
                           "Resample8" = "2022",
                           "Resample9" = "2023",
                           # Dejá las que no cambian fuera o ponelas igual a sí mismas
                           .default = df_metricas$Resample
)


# Crear el gráfico
library(ggplot2)
library(dplyr)

df_metricas$year <- as.numeric(as.character(df_metricas$year))
# Crear el gráfico
SVR_temporal<-ggplot(df_metricas, aes(x = year)) +
  geom_line(aes(y = Rsquared, color = "R²"), size = 1.2) +
  geom_point(aes(y = Rsquared, color = "R²"), size = 1.8) +
  geom_line(aes(y = rmse_escalado, color = "RMSE"), size = 1.2)+#, linetype = "dashed") +
  geom_point(aes(y = rmse_escalado, color = "RMSE"), size = 1.8)+#, shape = 1) +
  scale_y_continuous(
    name = "R²",
    sec.axis = sec_axis(~ . * (max_rmse - min_rmse) + min_rmse, name = "RMSE")
  ) +
  scale_x_continuous(breaks = 2015:2023) + 
  #scale_x_continuous(breaks = 2015:2023) + 
  scale_color_manual(values = c("R²" = "#2c7fb8",  "RMSE" = "#cb181d")) +
  labs(#title = "Evaluación del modelo",
    x = "SVR-Model", color = "") +
  theme_classic()+
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.y.right = element_text(size = 16),  # Para RMSE
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(size = 14),  # Ticks del eje derecho
    legend.text = element_text(size = 14),
    legend.position = "none"  # opcional: ubica la leyenda arriba
  )





##############################################################################
##############################################################################
##############################################################################

################# ET Temporal
library(caret)
library(ranger)

estacion <- "SP"
modelo <- "1"

# Leer datos
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/", estacion,
                             "/modelos/ParticionDataSet/Modelo_", modelo,
                             "/M", modelo, "_train_", estacion, ".csv", sep=""))

test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/", estacion,
                            "/modelos/ParticionDataSet/Modelo_", modelo,
                            "/M", modelo, "_test_", estacion, ".csv", sep=""))

# Crear columna 'year' desde la columna 'date'
train_data$year <- as.numeric(format(as.Date(train_data$date), "%Y"))

# Años únicos
years <- sort(unique(train_data$year))

# Índices Leave-One-Year-Out
index_list <- list()
indexOut_list <- list()

for (i in seq_along(years)) {
  test_year <- years[i]
  train_index <- which(train_data$year != test_year)
  test_index <- which(train_data$year == test_year)
  
  index_list[[i]] <- train_index
  indexOut_list[[i]] <- test_index
}

# Control de entrenamiento para validación cruzada temporal
train_control_temporal <- trainControl(
  method = "cv",
  number = length(years),
  index = index_list,
  indexOut = indexOut_list,
  savePredictions = "final",
  verboseIter = TRUE,
  allowParallel = TRUE
)

# Modelo Extra Trees con CV temporal
modelo_et_temporal <- train(
  PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
    SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + sp_mean +
    d2m_mean + v10_mean + u10_mean + tp_mean + dayWeek,
  data = train_data,
  method = "ranger",
  trControl = train_control_temporal,
  tuneGrid = data.frame(
    mtry = 5,
    splitrule = "extratrees",
    min.node.size = 5
  ),
  importance = "impurity"
)


#Guardar
getwd()
save(modelo_et_temporal, file=paste("01-ET-CV-Temp_M",modelo,"-200525-",estacion,".RData",sep=""))

# Metricas globales
resultados_ET_cv_Temporal <- evaluar_modelo(modelo_et_temporal, test_data)
print(resultados_ET_cv_Temporal)


### metricas por año
df_metricas<- data.frame(modelo_et_temporal[["resample"]])

max_rmse <- max(df_metricas$RMSE)
min_rmse <- min(df_metricas$RMSE)

df_metricas$rmse_escalado <- (df_metricas$RMSE - min_rmse) / (max_rmse - min_rmse)

df_metricas$year <- recode(df_metricas$Resample,
                           "Resample1" = "2015",
                           "Resample2" = "2016",
                           "Resample3" = "2017",
                           "Resample4" = "2018",
                           "Resample5" = "2019",
                           "Resample6" = "2020",
                           "Resample7" = "2021",
                           "Resample8" = "2022",
                           "Resample9" = "2023",
                           # Dejá las que no cambian fuera o ponelas igual a sí mismas
                           .default = df_metricas$Resample
)

df_metricas$year <- as.numeric(as.character(df_metricas$year))
# Crear el gráfico
ET_temporal<-ggplot(df_metricas, aes(x = year)) +
  geom_line(aes(y = Rsquared, color = "R²"), size = 1.2) +
  geom_point(aes(y = Rsquared, color = "R²"), size = 1.8) +
  geom_line(aes(y = rmse_escalado, color = "RMSE"), size = 1.2)+#, linetype = "dashed") +
  geom_point(aes(y = rmse_escalado, color = "RMSE"), size = 1.8)+#, shape = 1) +
  scale_y_continuous(
    name = "R²",
    sec.axis = sec_axis(~ . * (max_rmse - min_rmse) + min_rmse, name = "RMSE")
  ) +
  scale_x_continuous(breaks = 2015:2023) + 
  #scale_x_continuous(breaks = 2015:2023) + 
  scale_color_manual(values = c("R²" = "#2c7fb8",  "RMSE" = "#cb181d")) +
  labs(#title = "Evaluación del modelo",
    x = "ET-Model", color = "") +
  theme_classic()+
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.y.right = element_text(size = 16),  # Para RMSE
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(size = 14),  # Ticks del eje derecho
    legend.text = element_text(size = 14),
    legend.position = "none"  # opcional: ubica la leyenda arriba
  )
ET_temporal


##############################################################################
##############################################################################
##############################################################################

################# RF  Temporal
estacion <- "SP"
modelo <- "1"
#Data modelo 1
test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
#nrow(test_data)+nrow(train_data)

train_data$year <- as.numeric(format(as.Date(train_data$date), "%Y"))

# Extraer los años únicos
years <- sort(unique(train_data$year))

# Crear índices para Leave-One-Year-Out
index_list <- list()
indexOut_list <- list()

for (i in seq_along(years)) {
  test_year <- years[i]
  train_index <- which(train_data$year != test_year)
  test_index <- which(train_data$year == test_year)
  
  index_list[[i]] <- train_index
  indexOut_list[[i]] <- test_index
}

# Control de entrenamiento para CV temporal
train_control_temporal <- trainControl(
  method = "cv",
  number = length(years),
  index = index_list,
  indexOut = indexOut_list,
  savePredictions = "final",
  verboseIter = TRUE,
  allowParallel = TRUE
)


# Entrenar modelo con CV temporal
rf_temporal_model <- train(
  PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
    SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + sp_mean +
    d2m_mean + v10_mean + u10_mean + tp_mean + dayWeek,
  data = train_data,
  method = "rf",
  trControl = train_control_temporal,
  tuneGrid = data.frame(mtry = 5),
  importance = TRUE
)



#Guardar
getwd()
save(rf_temporal_model, file=paste("01-RF-CV-Temp_M",modelo,"-200525-",estacion,".RData",sep=""))

# Metricas globales
resultados_RF_cv_Temporal <- evaluar_modelo(modelo_RF_temporal, test_data)
print(resultados_RF_cv_Temporal)


### metricas por año
df_metricas<- data.frame(modelo_RF_temporal[["resample"]])

max_rmse <- max(df_metricas$RMSE)
min_rmse <- min(df_metricas$RMSE)

df_metricas$rmse_escalado <- (df_metricas$RMSE - min_rmse) / (max_rmse - min_rmse)

df_metricas$year <- recode(df_metricas$Resample,
                           "Resample1" = "2015",
                           "Resample2" = "2016",
                           "Resample3" = "2017",
                           "Resample4" = "2018",
                           "Resample5" = "2019",
                           "Resample6" = "2020",
                           "Resample7" = "2021",
                           "Resample8" = "2022",
                           "Resample9" = "2023",
                           # Dejá las que no cambian fuera o ponelas igual a sí mismas
                           .default = df_metricas$Resample
)

df_metricas$year <- as.numeric(as.character(df_metricas$year))
# Crear el gráfico
ET_temporal<-ggplot(df_metricas, aes(x = year)) +
  geom_line(aes(y = Rsquared, color = "R²"), size = 1.2) +
  geom_point(aes(y = Rsquared, color = "R²"), size = 1.8) +
  geom_line(aes(y = rmse_escalado, color = "RMSE"), size = 1.2)+#, linetype = "dashed") +
  geom_point(aes(y = rmse_escalado, color = "RMSE"), size = 1.8)+#, shape = 1) +
  scale_y_continuous(
    name = "R²",
    sec.axis = sec_axis(~ . * (max_rmse - min_rmse) + min_rmse, name = "RMSE")
  ) +
  scale_x_continuous(breaks = 2015:2023) + 
  #scale_x_continuous(breaks = 2015:2023) + 
  scale_color_manual(values = c("R²" = "#2c7fb8",  "RMSE" = "#cb181d")) +
  labs(#title = "Evaluación del modelo",
    x = "RF-Model", color = "") +
  theme_classic()+
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.y.right = element_text(size = 16),  # Para RMSE
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(size = 14),  # Ticks del eje derecho
    legend.text = element_text(size = 14),
    legend.position = "none"  # opcional: ubica la leyenda arriba
  )
ET_temporal


##############################################################################
##############################################################################
##############################################################################

################# XGB Temporal
estacion <- "SP"
modelo <- "1"
#Data modelo 1
test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
#nrow(test_data)+nrow(train_data)

# Asegurarse que haya columna 'año'
train_data$year <- as.numeric(format(as.Date(train_data$date), "%Y"))


# Variables predictoras y variable target
# Fórmula y selección de columnas
vars <- c("AOD_055",
          "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia"
          "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "blh_mean", 
          "sp_mean", "d2m_mean", "v10_mean",
          "u10_mean", "tp_mean",
          "dayWeek")
target <- "PM25"

# Dataframes para guardar predicciones
predicciones <- data.frame()
pred_entrenamiento <- data.frame()

# Validación cruzada por año
anios <- sort(unique(train_data$year))

for (test_year in anios) {
  cat("Procesando año:", test_year, "\n")
  
  train_fold <- train_data %>% filter(year != test_year)
  test_fold <- train_data %>% filter(year == test_year)
  
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
  
  # Predicciones
  preds_test <- predict(xgb_model, dtest)
  predicciones <- rbind(predicciones, data.frame(obs = test_fold[[target]], pred = preds_test, year = test_fold$year))
  
  preds_train <- predict(xgb_model, dtrain)
  pred_entrenamiento <- rbind(pred_entrenamiento, data.frame(obs = train_fold[[target]], pred = preds_train,year=test_year))
}

# Métricas de evaluación
eval_metrics <- function(obs, pred) {
  data.frame(
    # year = year,
    R2 = cor(obs, pred)^2,
    Pearson = cor(obs, pred, method = "pearson"),
    RMSE = rmse(obs, pred),
    MAE = mae(obs, pred),
    MAPE = mape(obs, pred) * 100,
    MSE = mse(obs, pred),
    MedAE = median(abs(obs - pred)),
    Min_Pred = min(pred),
    Max_Pred = max(pred),
    bias = mean(pred - obs)
  )
}
# Métricas por año
metricas_por_anio <- predicciones %>%
  group_by(year) %>%
  summarise(
    R2 = round(cor(obs, pred)^2,2),
    Pearson = round(cor(obs, pred, method = "pearson"),2),
    RMSE =round( rmse(obs, pred),2),
    MAE = round(mae(obs, pred),2),
    MAPE = round(mape(obs, pred) * 100,2),
    MSE = round(mse(obs, pred),2),
    MedAE = round(median(abs(obs - pred)),2),
    bias = mean(pred - obs),
    .groups = "drop"
  )

# Mostrar métricas por año
cat("\n### MÉTRICAS POR AÑO EN VALIDACIÓN CRUZADA (Leave-One-Year-Out)\n")
print(round(metricas_por_anio, 2))
d <- data.frame(metricas_por_anio)

View(d)


metrics_train <- eval_metrics(pred_entrenamiento$obs, pred_entrenamiento$pred)
metrics_test <- eval_metrics(predicciones$obs, predicciones$pred)

cat("### TRAIN\n")
print(round(metrics_train, 2))

cat("\n### TEST\n")
print(round(metrics_test, 2))
e<- rbind(round(metrics_train,2),round(metrics_test,2))

# Guardar modelo final entrenado con todo el train_data
dtrain_final <- xgb.DMatrix(data = as.matrix(train_data[, vars]), label = train_data[[target]])

modelo_xgb_final <- xgboost(
  data = dtrain_final,
  objective = "reg:squarederror",
  nrounds = 100,
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbose = 0
)

getwd()
save(modelo_xgb_final, file = paste0("01-XGB-CV-Temp_M", modelo, "-110425-", estacion, ".RData"))


# Crear el dataframe
resultados <- data.frame(
  anio = 2015:2023,
  R2 = c(0.52,0.63,0.61,0.60,0.52,0.57,0.66,0.65,0.49),
  RMSE = c(7.88,6.02,6.33,7.91,7.03,6.61,7.08,6.04,7.01)
)

# Pasar a formato largo para ggplot
resultados_largo <- resultados %>%
  pivot_longer(cols = c(R2, RMSE), names_to = "Metrica", values_to = "Valor")


ggplot(resultados, aes(x = anio, y = R2)) +
  geom_line(stat = "identity", color = "#0570b0", fill = "#74a9cf") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = 2015:2023) +
  labs(x = "XGB Spatial", y = "R² ") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Esto es solo para el gráfico, no modifica tus datos originales
max_rmse <- max(resultados$RMSE)
min_rmse <- min(resultados$RMSE)

resultados$rmse_escalado <- (resultados$RMSE - min_rmse) / (max_rmse - min_rmse)
#XGB
# rmse_all <- (6.41 - min_rmse) / (max_rmse - min_rmse)
#RF
rmse_all <- (13 - min_rmse) / (max_rmse - min_rmse)
# Crear el gráfico
xgb_temporal<-ggplot(resultados, aes(x = anio)) +
  geom_line(aes(y = R2, color = "R²"), size = 1.2) +
  geom_point(aes(y = R2, color = "R²"), size = 1.8) +
  geom_line(aes(y = rmse_escalado, color = "RMSE"), size = 1.2)+#, linetype = "dashed") +
  geom_point(aes(y = rmse_escalado, color = "RMSE"), size = 1.8)+#, shape = 1) +
  scale_y_continuous(
    name = "R²",
    sec.axis = sec_axis(~ . * (max_rmse - min_rmse) + min_rmse, name = "RMSE")
  ) +
  #XGB
  
  scale_x_continuous(breaks = 2015:2023) + 
  scale_color_manual(values = c("R²" = "#2c7fb8",  "RMSE" = "#cb181d")) +
  labs(#title = "Evaluación del modelo",
    x = "XGB-Model", color = "") +
  theme_classic()+
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.y.right = element_text(size = 16),  # Para RMSE
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(size = 14),  # Ticks del eje derecho
    legend.text = element_text(size = 14),
    legend.position = "none"  # opcional: ubica la leyenda arriba
  )
# theme_minimal(base_size = 16)
# 
# 
# ggsave(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/plots/XGB_temporal.png",sep=""),xgb_temporal,
#        width = 16,
#        height = 8,
#        units = "cm",
#        dpi = 500)
