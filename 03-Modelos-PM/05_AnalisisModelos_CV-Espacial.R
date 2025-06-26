
#################################################################################
#################################################################################
#                           Modelos Predictivos de PM2.5 con CV espacial
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

##############################################################################
##############################################################################
##############################################################################

################# SVR ESPACIAL
estacion <- "BA"
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
  PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + #+t2m_mean
    SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
    d2m_mean  +v10_mean + u10_mean + tp_mean +  DEM+dayWeek,  
  data = train_data,
  method = "svmRadial",
  trControl = train_control_spatial,
  preProcess = c("center", "scale"),
  tuneLength = 5
)

09:07
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()


#Guardar modelo
save(modelo_svr_spatial, file=paste("01-SVR-CV-Esp_M",modelo,"-180625-",estacion,".RData",sep=""))


# Metricas globales
resultados_SVR_cv_Espacial <- evaluar_modelo(modelo=modelo_svr_spatial, datos_test=test_data, variable_real = "PM25",tipoModelo="ET",y_test=NA)

print(resultados_SVR_cv_Espacial)

#Metricas por estacion
df_metricas<- data.frame(modelo_svr_spatial[["resample"]])
max_rmse <- max(df_metricas$RMSE)
min_rmse <- min(df_metricas$RMSE)

df_metricas$rmse_escalado <- (df_metricas$RMSE - min_rmse) / (max_rmse - min_rmse)

df_metricas$estacion <- recode(df_metricas$Resample,
                               "Resample1" = "EMC I" ,
                               "Resample2" = "EMCII-LE",
                               "Resample3" = "EMC II LM-MB",
                               "Resample4" = "EMC II LM-AER",
                               "Resample5" = "EMB",
                               
                               # Dejá las que no cambian fuera o ponelas igual a sí mismas
                               .default = df_metricas$Resample
)

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



df_metricas$estacion <- recode(df_metricas$Resample,
                               "Resample01" = "MER",
                               "Resample02" = "SFE",
                               "Resample03" =  "UAX" ,
                               "Resample04" = "CCA",
                               "Resample05" = "AJU",
                               "Resample06" = "AJM" ,
                               "Resample07" = "BJU" ,
                               "Resample08" = "GAM",
                               "Resample09" =  "INN",
                               "Resample10" =  "MGH" ,
                               "Resample11" = "MON" ,
                               "Resample12" = "PED" ,
                               "Resample13" = "MPA" ,
                               "Resample14" = "FAR" ,
                               "Resample15" = "SAC",
                               "Resample16" ="CAM" ,
                               "Resample17" ="SAG",
                               "Resample18" =  "TLA",
                               "Resample19" =  "XAL" ,
                               "Resample20" ="NEZ",
                               "Resample21" = "UIZ" ,
                               "Resample22" ="HGM",
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
estacion <- "BA"
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
  PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + # +t2m_mean 
    SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
    d2m_mean  +v10_mean + u10_mean + tp_mean +  DEM+dayWeek,
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

08:50-
getwd()
setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()


#Guardar modelo
save(modelo_et_spatial, file=paste("01-ET-CV-Esp_M",modelo,"-180625-",estacion,".RData",sep=""))

# Metricas globales
resultados_ET_cv_Espacial <- evaluar_modelo(modelo=modelo_et_spatial, datos_test=test_data, variable_real = "PM25",tipoModelo="ET",y_test=NA)

print(resultados_ET_cv_Espacial)

df_metricas<- data.frame(modelo_et_spatial[["resample"]])
max_rmse <- max(df_metricas$RMSE)
min_rmse <- min(df_metricas$RMSE)

df_metricas$rmse_escalado <- (df_metricas$RMSE - min_rmse) / (max_rmse - min_rmse)
df_metricas$estacion <- recode(df_metricas$Resample,
                               "Resample1" = "EMC I" ,
                               "Resample2" = "EMCII-LE",
                               "Resample3" = "EMC II LM-MB",
                               "Resample4" = "EMC II LM-AER",
                               "Resample5" = "EMB",
                               
                               # Dejá las que no cambian fuera o ponelas igual a sí mismas
                               .default = df_metricas$Resample
)
#--CH
df_metricas$estacion <- recode(df_metricas$Resample,
                                   "Resample01" = "BSQ",
                                   "Resample02" = "IND",
                                   "Resample03" = "CERR-I",
                                   "Resample04" = "CER-II",
                                   "Resample05" = "CNA",
                                   "Resample06" = "FLD",
                                   "Resample07" = "CDE",
                                   "Resample08" = "PDH",
                                   "Resample09" = "PTA",
                                   "Resample10" = "QUII",
                                   "Resample11" = "OHG",
                                   "Resample12" = "QUI",
                                   # Dejá las que no cambian fuera o ponelas igual a sí mismas
                                   .default = df_metricas$Resample
)


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
estacion <- "BA"
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
  PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + #+t2m_mean 
    SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
    d2m_mean   +v10_mean + u10_mean + tp_mean +  DEM+dayWeek,
  data = train_data,
  method = "rf",
  trControl = train_control_spatial,
  tuneGrid = data.frame(mtry = 5),  # <<---- fijás mtry aquí
  importance = TRUE
)
10:03-11:35
11:38

setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(rf_spatial_model, file=paste("01-RF-CV-Esp_M",modelo,"-180625-",estacion,".RData",sep=""))


# Metricas globales
resultados_RF_cv_Espacial <- evaluar_modelo(modelo=rf_spatial_model, datos_test=test_data, variable_real = "PM25",tipoModelo="RF",y_test=NA)
print(resultados_RF_cv_Espacial)

# load(paste("11-RF-CV-Esp_M",modelo,"-100425-",estacion,".RData",sep=""))


df_metricas<- data.frame(rf_spatial_model[["resample"]])
max_rmse <- max(df_metricas$RMSE)
min_rmse <- min(df_metricas$RMSE)

df_metricas$rmse_escalado <- (df_metricas$RMSE - min_rmse) / (max_rmse - min_rmse)

df_metricas$estacion <- recode(df_metricas$Resample,
                               "Resample1" = "EMC I" ,
                               "Resample2" = "EMCII-LE",
                               "Resample3" = "EMC II LM-MB",
                               "Resample4" = "EMC II LM-AER",
                               "Resample5" = "EMB",
                               
                               # Dejá las que no cambian fuera o ponelas igual a sí mismas
                               .default = df_metricas$Resample
)

df_metricas$estacion <- recode(df_metricas$Resample,
                                   "Resample01" = "BSQ",
                                   "Resample02" = "IND",
                                   "Resample03" = "CERR-I",
                                   "Resample04" = "CER-II",
                                   "Resample05" = "CNA",
                                   "Resample06" = "FLD",
                                   "Resample07" = "CDE",
                                   "Resample08" = "PDH",
                                   "Resample09" = "PTA",
                                   "Resample10" = "QUII",
                                   "Resample11" = "OHG",
                                   "Resample12" = "QUI",
                                   # Dejá las que no cambian fuera o ponelas igual a sí mismas
                                   .default = df_metricas$Resample
)



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

View(df_metricas)
#c("R²" = "blue", "RMSE" = "red")) +
# Crear el histograma
r2<-ggplot(df_metricas, aes(x = estacion, y = Rsquared)) +
  geom_bar(stat = "identity", color = "#0570b0", fill = "#74a9cf") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "RF Spatial", y = "R² ") +
  theme_classic() +
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
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, 2)) +
  labs(x = "RF Spatial", y = "RMSE ") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),  # Agrandar los números de los ticks
    legend.title = element_text(family = "Roboto", size = 6, face = 2),
    legend.position = "top",  # Elimina la leyenda
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 

rmse
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


estacion <- "BA"
modelo <- "1"
#Data modelo 1
test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))

# Estaciones
stations <- unique(train_data$estacion)

# Fórmula y selección de columnas
vars <- c("AOD_055",
          "ndvi", "BCSMASS_dia","DUSMASS_dia", #"DUSMASS25_dia" "t2m_mean",
          "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "blh_mean", 
          "d2m_mean","v10_mean", "sp_mean", 
          "u10_mean", "tp_mean", "DEM",
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
    #nfold = 10,
    #verbose = 0
    verbose = TRUE 
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


setwd(paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep=""))
getwd()
save(xgb_model, file=paste("01-XGB-CV-Esp_M",modelo,"-180625-",estacion,".RData",sep=""))

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
    verbose = TRUE 
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
nombres_cortos <- c("EMC I" ,"EMCII-LE","EMC II LM-MB",
                 "EMC II LM-AER","EMB")
orden_estaciones <- c("EMC I" ,"EMCII-LE","EMC II LM-MB",
                    "EMC II LM-AER","EMB")
orden_estaciones <- c("CNA", "QUII", "PDH", "BSQ", "CER-II", "CERR-I", "OHG", "QUI", "IND", "FLD", "PTA", "CDE")
nombres_cortos <- c("CNA", "QUII", "PDH", "BSQ", "CER-II", "CERR-I", "OHG", "QUI", "IND", "FLD", "PTA", "CDE")

orden_estaciones <- c("Carapicuiba", "Marg.Tiete-Pte Remedios", "Maua", "Pico do Jaragua",
                      "Pinheiros", "Santana", "Santo Amaro", "Tabao da Serra","Parque D.Pedro II",
                      "Parque D.Pedro II","Guarulhos-Palco Municipal","Osasco","Ciudad Universitaria - USP",
                      "Guarulhos - Pimentas","Ibirapuera","Interlagos","Itaim Paulista")
orden_estaciones <- c("Estacion Trafico Centro","Itagui - Casa de Justicia Itagui","Itagui - I.E. Concejo Municipal de Itagui",
                      "Caldas - E U Joaquin Aristizabal","La Estrella - Hospital","Medellin Altavista - I.E. Pedro Octavio Amado",
                      "Medellin Villahermosa - Planta de produccion de agua potable EPM","Barbosa - Torre Social",
                      "Copacabana - Ciudadela Educativa La Vida","Medellin, Belen - I.E Pedro Justo Berrio","Medellin, El Poblado - I.E INEM sede Santa Catalina",
                      "Medellin, San Cristobal - Parque Biblioteca Fernando Botero","Medellin, Aranjuez - I.E Ciro Mendia",
                      "Bello - I.E. Fernando Velez","Envigado - E.S.E. Santa Gertrudis","Sabaneta - I.E. Rafael J. Mejia","Medellin - Santa Elena" )
# Paso 1: vector con nombres cortos
nombres_cortos <- c("Estacion Trafico Centro", "Casa de Justicia Itagui", "Concejo Municipal de Itagui", 
                    "Caldas Joaquin Aristizabal", "La Estrella", "Medellin Altavista",
                    "Medellin Villahermosa",  "Barbosa - Torre Social", "Copacabana", 
                    "Medellin - Belen", "Medellin - El Poblado", "Medellin - San Cristobal", 
                    "Medellin - Aranjuez", "Bello - Fernando Velez", "Envigado - Santa Gertrudis",
                    "Sabaneta - Rafael J. Mejia", "Medellin - Santa Elena")

# Paso 2: tabla de equivalencias
tabla_nombres <- data.frame(
  estacion = orden_estaciones,
  estacion_corta = nombres_cortos
)

# Paso 3: unir nombres cortos
resultados_por_estacion <- resultados_por_estacion %>%
  left_join(tabla_nombres, by = "estacion")
# Aplicás ese orden al factor
resultados_por_estacion <- resultados_por_estacion %>%
  mutate(estacion = factor(estacion, levels = orden_estaciones))
# fija el orden en el gráfico

#r2<-ggplot(resultados_por_estacion, aes(x = estacion_corta, y = R2)) +
r2<-ggplot(resultados_por_estacion, aes(x = estacion, y = R2)) +
  geom_bar(stat = "identity", color = "#0570b0", fill = "#74a9cf") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "XGB Spatial", y = "R² ") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),  # Agrandar los números de los ticks
    legend.title = element_text(family = "Roboto", size = 6, face = 2),
    legend.position = "top",  # Elimina la leyenda
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 


r2
#rmse<-ggplot(resultados_por_estacion, aes(x = estacion_corta, y = RMSE)) +
rmse<-ggplot(resultados_por_estacion, aes(x = estacion, y = RMSE)) + 
 geom_bar(stat = "identity", color = "#b30000", fill = "#e34a33") +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, 2)) +
  labs(x = "XGB Spatial", y = "RMSE ") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),  # Agrandar los números de los ticks
    legend.title = element_text(family = "Roboto", size = 6, face = 2),
    legend.position = "top",  # Elimina la leyenda
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 


rmse

ggsave(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/plots/RMSE-XGB-espacial.png",sep=""),rmse,
       width = 10,
       height = 8,
       units = "cm",
       dpi = 500)