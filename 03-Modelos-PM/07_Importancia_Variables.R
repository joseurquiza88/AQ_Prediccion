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
estacion<- "MX"
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos",sep = ""))

# Paso 1: Cargar el modelo
load("01-RF-CV-M1-200525-SP.RData") # SP
load("01-RF-CV-M1-260525-MD.RData") 
load ("01-RF-CV-M1-170625-CH.RData")
load ("01-RF-CV-M1-170625-BA.RData")
load ("01-RF-CV-M1-290525-MX.RData")
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
                                   "t2m_mean" = "t2m",
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
                                   "DEM" = "DEM",
                                   "dayWeek" =  "dayWeek",
                                   # Dejá las que no cambian fuera o ponelas igual a sí mismas
                                   .default = importancia_df$Variable
)
# scale_fill_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256","#6a51a3")) +
  
ggplot(importancia_df, aes(x = reorder(Variable2, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "#6a51a3") +
  #geom_hline(yintercept = 50, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_classic() +
  labs(#title = "Importancia de Variables 02-RF_cv_M1-090125_MX", 
    x = " ", 
    y = " ") +#"Importancia Modelo RF") +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12))  # Ajusta

#######################################################
# Corremos el modelo eliminando variables de a 1
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

#Dayweek, sp, ssmass, aod, so4mass

modelo_RF_cv <- train(  PM25 ~  AOD_055 +ndvi + BCSMASS_dia +
                          DUSMASS_dia +
                          SO4SMASS_dia + v10_mean +
                           SSSMASS_dia + blh_mean + sp_mean + 
                          SO2SMASS_dia + d2m_mean +# u10_mean + 
                          tp_mean+#dayWeek,
                          t2m_mean,#+
                          #DEM, # 
                        data = train_data, method = "rf",
                        trControl = train_control,importance = TRUE)

resultados_RF_cv <- evaluar_modelo(modelo=modelo_RF_cv, datos_test=test_data,
                                   variable_real = "PM25",tipoModelo="RF",y_test=NA)
print(resultados_RF_cv)
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()

save(xgb_model, file=paste("01-XGB-CV-Esp_M",modelo,"-180625-",estacion,".RData",sep=""))
save(modelo_RF_cv, file=paste("02-RF-CV-M",modelo,"-240625-sAOD",estacion,".RData",sep=""))


##############################################################################
##############################################################################
#XGB Importancia
# scale_fill_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256","#6a51a3")) +

# cargar el modelo y aplicalo a otro set de datos
estacion<- "MD"
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos",sep = ""))

# Paso 1: Cargar el modelo
load("01-XGB-CV-M1-200525-SP.RData") # SP
load("01-XGB-CV-M1-260525-MD.RData") #MD
load("01-XGB-CV-M1-190625-CH.RData") #MD
load("01-XGB-CV-M1-190625-BA.RData")
load("01-XGB-CV-M1-290525-MX.RData")
# Importancia XGB
importance_matrix <- xgb.importance(model = xgb_cv_model)
xgb.plot.importance(importance_matrix = importance_matrix)
#importance_matrix


# Asumimos que 'importance_matrix' es el resultado de xgb.importance
importancia_df <- as.data.frame(importance_matrix)
importancia_df$Variable <- rownames(importancia_df)

importancia_df$Variable2 <- recode(importancia_df$Feature,
                                   "tp_mean" = "tp",
                                   "blh_mean" = "blh",
                                   "d2m_mean" = "d2m",
                                   "t2m_mean" = "t2m",
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
                                   "DEM" = "DEM",
                                   "dayWeek" =  "dayWeek",
                                   # Dejá las que no cambian fuera o ponelas igual a sí mismas
                                   .default = importancia_df$Feature
)



#nameModel<- "02-XGB-cv-TunGrid_M1-280224_MX"
# Crear el gráfico con ggplot
# scale_fill_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256","#6a51a3")) +

ggplot(importancia_df, aes(x = reorder(Variable2,Gain) , y = (Gain*100))) +
  geom_bar(stat = "identity", fill = "#023858") +
  #geom_hline(yintercept = 50, linetype = "dashed", color = "red") +  # Línea horizontal de referencia
  coord_flip() +  # Cambia el eje x e y
  ylim(0, 50) +
  theme_classic() +  # Estilo limpio
  labs(#title = "Importancia de Variables 02-RF_cv_M1-090125_MX", 
    x = " ", 
    y = " ") +#"Importancia Modelo RF") +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12))  # Ajusta

#######################################################
# Corremos el modelo eliminando variables de a 1

### ----- XGB   -----

estacion <-"MD"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))

# PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + OCSSMASS_dia
#   SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
#   d2m_mean  +t2m_mean +v10_mean + u10_mean + tp_mean + DEM+ dayWeek,

X <- train_data[ , c("AOD_055",
                     "ndvi", "BCSMASS_dia",#"DUSMASS_dia", #"DUSMASS25_dia"
                      "SO4SMASS_dia", "SSSMASS_dia",# "blh_mean", "SO2SMASS_dia",
                     "sp_mean", "d2m_mean", #"t2m_mean",#"v10_mean",
                     "u10_mean", "tp_mean", "DEM"
                     )]#"dayWeek"


y <- train_data$PM25

X_test <- test_data[ ,c("AOD_055",
                        "ndvi", "BCSMASS_dia",#"DUSMASS_dia", #"DUSMASS25_dia"
                         "SO4SMASS_dia", "SSSMASS_dia", #"blh_mean", "SO2SMASS_dia",
                        "sp_mean", "d2m_mean",# "t2m_mean",#"v10_mean",
                        "u10_mean", "tp_mean", "DEM")]#
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

data_metricas<- read.csv("D:/Josefina/Proyectos/Tesis/TOT/resultados/modelos_variables_metricas.csv")
data_metricas <- data_metricas[data_metricas$sitio == "MX",]
data_metricas <- data_metricas[data_metricas$modelo == "XGB",]
data_metricas <- data_metricas[data_metricas$numModelo != "Naod",]
data_metricas_tot <- data_metricas[data_metricas$numModelo != 0,]
data_metricas_comp <- data_metricas[data_metricas$numModelo == 0,]
data_metricas_tot$numModelo <- as.numeric(data_metricas_tot$numModelo)
# Escalar RMSE para que esté en la misma escala que R2 (0 a 1)
max_rmse <- max(data_metricas_tot$rmse)
min_rmse <- min(data_metricas_tot$rmse)

data_metricas_tot$rmse_escalado <- (data_metricas_tot$rmse - min_rmse) / (max_rmse - min_rmse)

# RMSE y R2 para el modelo con todas las variables (RF por ejemplo)
# SP
# rmse_all <- 5.74
# r_all <- 0.71822
#MD
rmse_all <-  data_metricas_comp$rmse 
r_all <- data_metricas_comp$r2


# Escalar también rmse_all para que se muestre correctamente
rmse_all_escalado <- (rmse_all - min_rmse) / (max_rmse - min_rmse)

# Gráfico
ggplot(data_metricas_tot, aes(x = numModelo)) +
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
    x = "Modelo XGB", #"Modelo RF",
    color = ""
  ) +
  theme_classic() + 
  theme(legend.position = "none")


##################################################################
############################################################
data_metricas<- read.csv("D:/Josefina/Proyectos/Tesis/TOT/resultados/modelos_variables_metricas.csv")
data_metricas <- data_metricas[data_metricas$sitio == "SP",]
data_metricas <- data_metricas[data_metricas$numModelo != "Naod",]

data_metricas_xgb <- data_metricas[data_metricas$modelo == "XGB",]
data_metricas_xgb_tot <- data_metricas_xgb[data_metricas_xgb$numModelo != 0,]
data_metricas_xgb_comp <- data_metricas_xgb[data_metricas_xgb$numModelo == 0,]
rmse_all_xgb <-  data_metricas_xgb_comp$rmse 
r_all_xgb <- data_metricas_xgb_comp$r2
data_metricas_xgb_tot$numModelo <- as.numeric(data_metricas_xgb_tot$numModelo)

#### RF
data_metricas_rf <- data_metricas[data_metricas$modelo == "RF",]
data_metricas_rf_tot <- data_metricas_rf[data_metricas_rf$numModelo != 0,]
data_metricas_rf_comp <- data_metricas_rf[data_metricas_rf$numModelo == 0,]
rmse_all_rf <-  data_metricas_rf_comp$rmse 
r_all_rf <- data_metricas_rf_comp$r2
data_metricas_rf_tot$numModelo <- as.numeric(data_metricas_rf_tot$numModelo)

# scale_fill_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256","#6a51a3")) +
ggplot() +
  geom_line(data = data_metricas_xgb_tot, 
            aes(x = numModelo, y = rmse, color = "XGB"), size = 1) +
  geom_point(data = data_metricas_xgb_tot, 
            aes(x = numModelo, y = rmse, color = "XGB"), size = 2) +
  
  geom_line(data = data_metricas_xgb_tot, 
            aes(x = numModelo, y = rmse_all_xgb, color = "XGB Todas las variables"), 
            size = 0.5, linetype = "dashed") +
  
  geom_line(data = data_metricas_rf_tot, 
            aes(x = numModelo, y = rmse, color = "RF"), size = 1) +
  geom_point(data = data_metricas_rf_tot, 
            aes(x = numModelo, y = rmse, color = "RF"), size = 2) +
  
  geom_line(data = data_metricas_rf_tot, 
            aes(x = numModelo, y = rmse_all_rf, color = "RF Todas las variables"), 
            size = 0.5, linetype = "dashed") +
  
  scale_color_manual(values = c(
    "XGB" = "#00441b",
    "XGB Todas las variables" = "#00441b",
    "RF" = "#238b45",
    "RF Todas las variables" = "#238b45"
  )) +
  
  scale_x_continuous(breaks = 1:5) +
  #scale_y_continuous(limits = c(0.4, 0.9)) +
  scale_y_continuous(limits = c(4, 8)) +
  labs(
    x = "SP",
    #y = "R²",
    y = "RMSE",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
##################################################################
############################################################
##################################################################
############################################################
data_metricas<- read.csv("D:/Josefina/Proyectos/Tesis/TOT/resultados/modelos_variables_metricas.csv")
data_metricas <- data_metricas[data_metricas$sitio == "ST",]
data_metricas <- data_metricas[data_metricas$numModelo != "Naod",]

data_metricas_xgb <- data_metricas[data_metricas$modelo == "XGB",]
data_metricas_xgb_tot <- data_metricas_xgb[data_metricas_xgb$numModelo != 0,]
data_metricas_xgb_comp <- data_metricas_xgb[data_metricas_xgb$numModelo == 0,]
rmse_all_xgb <-  data_metricas_xgb_comp$rmse 
r_all_xgb <- data_metricas_xgb_comp$r2
data_metricas_xgb_tot$numModelo <- as.numeric(data_metricas_xgb_tot$numModelo)

#### RF
data_metricas_rf <- data_metricas[data_metricas$modelo == "RF",]
data_metricas_rf_tot <- data_metricas_rf[data_metricas_rf$numModelo != 0,]
data_metricas_rf_comp <- data_metricas_rf[data_metricas_rf$numModelo == 0,]
rmse_all_rf <-  data_metricas_rf_comp$rmse 
r_all_rf <- data_metricas_rf_comp$r2
data_metricas_rf_tot$numModelo <- as.numeric(data_metricas_rf_tot$numModelo)

# scale_fill_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256","#6a51a3")) +
ggplot() +
  geom_line(data = data_metricas_xgb_tot, 
            aes(x = numModelo, y = rmse, color = "XGB"), size = 1) +
  geom_point(data = data_metricas_xgb_tot, 
            aes(x = numModelo, y = rmse, color = "XGB"), size = 2) +
  
   geom_line(data = data_metricas_xgb_tot, 
            aes(x = numModelo, y = rmse_all_xgb, color = "XGB Todas las variables"), 
            size = 0.5, linetype = "dashed") +
  
  geom_line(data = data_metricas_rf_tot, 
            aes(x = numModelo, y = rmse, color = "RF"), size = 1) +
  geom_point(data = data_metricas_rf_tot, 
            aes(x = numModelo, y = rmse, color = "RF"), size = 2) +
  
  geom_line(data = data_metricas_rf_tot, 
            aes(x = numModelo, y = rmse_all_rf, color = "RF Todas las variables"), 
            size = 0.5, linetype = "dashed") +
  
  scale_color_manual(values = c(
    "XGB" = "#fc4e2a",
    "XGB Todas las variables" = "#fc4e2a",
    "RF" = "#feb24c",
    "RF Todas las variables" = "#feb24c"
  )) +
  
  scale_x_continuous(breaks = 1:5) +
  scale_y_continuous(limits = c(4, 8)) +
  #scale_y_continuous(limits = c(0.4, 0.9)) +
  labs(
    x = "ST",
     #y = "R²",
    y = "RMSE",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )


##################################################################
############################################################
##################################################################
############################################################
data_metricas<- read.csv("D:/Josefina/Proyectos/Tesis/TOT/resultados/modelos_variables_metricas.csv")
data_metricas <- data_metricas[data_metricas$sitio == "BA",]
data_metricas <- data_metricas[data_metricas$numModelo != "Naod",]

data_metricas_xgb <- data_metricas[data_metricas$modelo == "XGB",]
data_metricas_xgb_tot <- data_metricas_xgb[data_metricas_xgb$numModelo != 0,]
data_metricas_xgb_comp <- data_metricas_xgb[data_metricas_xgb$numModelo == 0,]
rmse_all_xgb <-  data_metricas_xgb_comp$rmse 
r_all_xgb <- data_metricas_xgb_comp$r2
data_metricas_xgb_tot$numModelo <- as.numeric(data_metricas_xgb_tot$numModelo)

#### RF
data_metricas_rf <- data_metricas[data_metricas$modelo == "RF",]
data_metricas_rf_tot <- data_metricas_rf[data_metricas_rf$numModelo != 0,]
data_metricas_rf_comp <- data_metricas_rf[data_metricas_rf$numModelo == 0,]
rmse_all_rf <-  data_metricas_rf_comp$rmse 
r_all_rf <- data_metricas_rf_comp$r2
data_metricas_rf_tot$numModelo <- as.numeric(data_metricas_rf_tot$numModelo)

# scale_fill_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256","#6a51a3")) +
ggplot() +
  geom_line(data = data_metricas_xgb_tot, 
            aes(x = numModelo, y = r2, color = "XGB"), size = 1) +
  geom_point(data = data_metricas_xgb_tot, 
             aes(x = numModelo, y = r2, color = "XGB"), size = 2) +
  
  geom_line(data = data_metricas_xgb_tot, 
            aes(x = numModelo, y = r_all_xgb, color = "XGB Todas las variables"), 
            size = 0.5, linetype = "dashed") +
  
  geom_line(data = data_metricas_rf_tot, 
            aes(x = numModelo, y = r2, color = "RF"), size = 1) +
  geom_point(data = data_metricas_rf_tot, 
             aes(x = numModelo, y = r2, color = "RF"), size = 2) +
  
  geom_line(data = data_metricas_rf_tot, 
            aes(x = numModelo, y = r_all_rf, color = "RF Todas las variables"), 
            size = 0.5, linetype = "dashed") +
  
  scale_color_manual(values = c(
    "XGB" = "#99000d",
    "XGB Todas las variables" = "#99000d",
    "RF" = "#fb6a4a",
    "RF Todas las variables" = "#fb6a4a"
  )) +
  
  scale_x_continuous(breaks = 1:5) +
  #scale_y_continuous(limits = c(4, 8)) +
  scale_y_continuous(limits = c(0.4, 0.9)) +
  labs(
    x = "BA",
    y = "R²",
    #y = "RMSE",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )



##################################################################
############################################################
##################################################################
############################################################
data_metricas<- read.csv("D:/Josefina/Proyectos/Tesis/TOT/resultados/modelos_variables_metricas.csv")
data_metricas <- data_metricas[data_metricas$sitio == "MD",]
data_metricas <- data_metricas[data_metricas$numModelo != "Naod",]

data_metricas_xgb <- data_metricas[data_metricas$modelo == "XGB",]
data_metricas_xgb_tot <- data_metricas_xgb[data_metricas_xgb$numModelo != 0,]
data_metricas_xgb_comp <- data_metricas_xgb[data_metricas_xgb$numModelo == 0,]
rmse_all_xgb <-  data_metricas_xgb_comp$rmse 
r_all_xgb <- data_metricas_xgb_comp$r2
data_metricas_xgb_tot$numModelo <- as.numeric(data_metricas_xgb_tot$numModelo)

#### RF
data_metricas_rf <- data_metricas[data_metricas$modelo == "RF",]
data_metricas_rf_tot <- data_metricas_rf[data_metricas_rf$numModelo != 0,]
data_metricas_rf_comp <- data_metricas_rf[data_metricas_rf$numModelo == 0,]
rmse_all_rf <-  data_metricas_rf_comp$rmse 
r_all_rf <- data_metricas_rf_comp$r2
data_metricas_rf_tot$numModelo <- as.numeric(data_metricas_rf_tot$numModelo)

# scale_fill_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256","#6a51a3")) +
ggplot() +
  geom_line(data = data_metricas_xgb_tot, 
            aes(x = numModelo, y = rmse, color = "XGB"), size = 1) +
  geom_point(data = data_metricas_xgb_tot, 
             aes(x = numModelo, y = rmse, color = "XGB"), size = 2) +
  
  geom_line(data = data_metricas_xgb_tot, 
            aes(x = numModelo, y = rmse_all_xgb, color = "XGB Todas las variables"), 
            size = 0.5, linetype = "dashed") +
  
  geom_line(data = data_metricas_rf_tot, 
            aes(x = numModelo, y = rmse, color = "RF"), size = 1) +
  geom_point(data = data_metricas_rf_tot, 
             aes(x = numModelo, y = rmse, color = "RF"), size = 2) +
  
  geom_line(data = data_metricas_rf_tot, 
            aes(x = numModelo, y = rmse_all_rf, color = "RF Todas las variables"), 
            size = 0.5, linetype = "dashed") +
  
  scale_color_manual(values = c(
    "XGB" = "#023858",
    "XGB Todas las variables" = "#023858",
    "RF" = "#4292c6",
    "RF Todas las variables" = "#4292c6"
  )) +
  
  scale_x_continuous(breaks = 1:5) +
  scale_y_continuous(limits = c(4, 8)) +
  #scale_y_continuous(limits = c(0.4, 0.9)) +
  labs(
    x = "MD",
    #y = "R²",
    y = "RMSE",
    color = NULL
  ) +
  theme_classic() +
  theme(
    #legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

##################################################################
############################################################
##################################################################
############################################################
data_metricas<- read.csv("D:/Josefina/Proyectos/Tesis/TOT/resultados/modelos_variables_metricas.csv")
data_metricas <- data_metricas[data_metricas$sitio == "MX",]
data_metricas <- data_metricas[data_metricas$numModelo != "Naod",]

data_metricas_xgb <- data_metricas[data_metricas$modelo == "XGB",]
data_metricas_xgb_tot <- data_metricas_xgb[data_metricas_xgb$numModelo != 0,]
data_metricas_xgb_comp <- data_metricas_xgb[data_metricas_xgb$numModelo == 0,]
rmse_all_xgb <-  data_metricas_xgb_comp$rmse 
r_all_xgb <- data_metricas_xgb_comp$r2
data_metricas_xgb_tot$numModelo <- as.numeric(data_metricas_xgb_tot$numModelo)

#### RF
data_metricas_rf <- data_metricas[data_metricas$modelo == "RF",]
data_metricas_rf_tot <- data_metricas_rf[data_metricas_rf$numModelo != 0,]
data_metricas_rf_comp <- data_metricas_rf[data_metricas_rf$numModelo == 0,]
rmse_all_rf <-  data_metricas_rf_comp$rmse 
r_all_rf <- data_metricas_rf_comp$r2
data_metricas_rf_tot$numModelo <- as.numeric(data_metricas_rf_tot$numModelo)

# scale_fill_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256","#6a51a3")) +
ggplot() +
  geom_line(data = data_metricas_xgb_tot, 
            aes(x = numModelo, y = r2, color = "XGB"), size = 1) +
  geom_point(data = data_metricas_xgb_tot, 
             aes(x = numModelo, y = r2, color = "XGB"), size = 2) +
  
  geom_line(data = data_metricas_xgb_tot, 
            aes(x = numModelo, y = r_all_xgb, color = "XGB Todas las variables"), 
            size = 0.5, linetype = "dashed") +
  
  geom_line(data = data_metricas_rf_tot, 
            aes(x = numModelo, y = r2, color = "RF"), size = 1) +
  geom_point(data = data_metricas_rf_tot, 
             aes(x = numModelo, y = r2, color = "RF"), size = 2) +
  
  geom_line(data = data_metricas_rf_tot, 
            aes(x = numModelo, y = r_all_rf, color = "RF Todas las variables"), 
            size = 0.5, linetype = "dashed") +
  
  scale_color_manual(values = c(
    "XGB" = "#3f007d",
    "XGB Todas las variables" = "#3f007d",
    "RF" = "#807dba",
    "RF Todas las variables" = "#807dba"
  )) +
  
  scale_x_continuous(breaks = 1:5) +
  #scale_y_continuous(limits = c(4, 8)) +
  scale_y_continuous(limits = c(0.4, 0.9)) +
  labs(
    x = "MX",
    y = "R²",
    #y = "RMSE",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

###############################################################
##########################################################


estacion <-"CH"
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
load("01-XGB-CV-M1-190625-CH.RData")
modelo_XGB_AOD <- xgb_cv_model
load("02-XGB-CV-M1-230625-sAODCH.RData" )
modelo_XGB_sinAOD <- xgb_cv_model  # Guardás con un nuevo nombre
## predicciones

# Solo para XGB

X_test_sAOD <- test_data[ ,c( #"AOD_055", "sp_mean",
  "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia" "d2m_mean", "DEM"
  "SO2SMASS_dia",  "SO4SMASS_dia", "SSSMASS_dia",
  "blh_mean", "d2m_mean","v10_mean",
  "t2m_mean", "u10_mean", "tp_mean",
  "DEM","dayWeek"
)]#
X_test_AOD <- test_data[ ,c( "AOD_055",
                             "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia" "d2m_mean", "DEM"
                             "SO2SMASS_dia",  "SO4SMASS_dia", "SSSMASS_dia",
                             "blh_mean", "d2m_mean","t2m_mean",
                             "v10_mean",  "u10_mean", "tp_mean",
                             "DEM","dayWeek"
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
  geom_point(aes(y = real, x= pred_sAOD),color =   "#feb24c",  alpha=0.9,size = 1.5,shape=20, ) +  # Puntos de datos
  
  geom_point(aes(y = real, x= pred_AOD),color = "#fc4e2a",   size = 1.5, shape=8) +  # Puntos de datos
  
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
  
  ggplot2::annotate("text",x = 100, y = 60,label = paste("R² =", round(R2_model_XGB_sAOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 50,label = paste("RMSE =", round(RMSE_model_XGB_sAOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 40,label = paste("Bias =", round(Bias_model_XGB_sAOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n_model_XGB_sAOD, 2)), size = 3, color = "black")+
  
  
  ggplot2::annotate("text",x = 130, y = 70,label = paste("AOD"), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 130, y = 60,label = paste("R² =", round(R2_model_XGB_AOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 130, y = 50,label = paste("RMSE =", round(RMSE_model_XGB_AOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 130, y = 40,label = paste("Bias =", round(Bias_model_XGB_AOD, 2)), size = 3, color = "black")+
  
  ggplot2::annotate("text",x = 100, y = 30,label = paste("n =", round(n_model_XGB_AOD, 2)), size = 3, color = "black")+
  
  
  theme_classic() #+
plot_regresion_XGB