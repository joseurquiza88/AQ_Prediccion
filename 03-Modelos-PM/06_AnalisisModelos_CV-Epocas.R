#################################################################################
#################################################################################
#                           Modelos Predictivos de PM2.5 con CV por epoca
#################################################################################
#################################################################################
#  Objetivo: Evaluar cómo varía el rendimiento del modelo SVR por estación del año

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

##############################################################################
##############################################################################
##############################################################################
### ----- SVR   -----
estacion <-"MD"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))



# Asegurate que 'fecha' esté en formato Date
train_data$date <- as.Date(train_data$date)
test_data$date  <- as.Date(test_data$date)

# Función para clasificar estaciones
get_season <- function(date) {
  m <- month(date)
  ifelse(m %in% c(12, 1, 2), "Verano",
         ifelse(m %in% c(3, 4, 5), "Otoño",
                ifelse(m %in% c(6, 7, 8), "Invierno", "Primavera")))
}




# Agregar columna season
train_data$season <- get_season(train_data$date)
test_data$season <- get_season(test_data$date)

# Crear lista para guardar modelos y resultados
modelos_svr_por_estacion <- list()
resultados_svr_por_estacion <- list()

estaciones <- c("Primavera", "Verano", "Otoño", "Invierno")

for (s in estaciones) {
  cat("/n--- Entrenando SVR para:", s, "---/n")
  
  # Filtrar datos por estación
  train_s <- subset(train_data, season == s)
  test_s <- subset(test_data, season == s)
  
  if (nrow(train_s) >= 30) {  # Asegura tamaño mínimo para CV
    # Entrenamiento con CV=10
    ctrl <- trainControl(method = "cv", number = 10, 
                         savePredictions = "final", verboseIter = TRUE)
    
    set.seed(123)
    modelo_svr <- train(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + #
                          SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
                          d2m_mean  +t2m_mean +v10_mean + u10_mean + tp_mean + DEM+ dayWeek,
                        data = train_s,
                        method = "svmRadial",
                        trControl = ctrl,
                        preProcess = c("center", "scale"),
                        tuneLength = 5)
    
    modelos_svr_por_estacion[[s]] <- modelo_svr
    
    # Evaluación
    resultado <- evaluar_modelo(modelo_svr, test_s,variable_real = "PM25",tipoModelo="SVR",y_test=NA)
    
    # resultado <- evaluar_modelo(modelo_svr, test_s)
    resultados_svr_por_estacion[[s]] <- resultado
    print(resultado)
    
  } else {
    cat("No hay suficientes datos para entrenar en", s, "/n")
  }
}


df_resultados <- bind_rows(
  lapply(names(resultados_svr_por_estacion), function(s) {
    df <- as.data.frame(resultados_svr_por_estacion[[s]])
    df$season <- s
    return(df)
  })
)

print(df_resultados)

#Plot
df_resultados$season <- factor(df_resultados$season,
                               levels = c("Otoño", "Invierno", "Primavera", "Verano"))


ggplot(df_resultados, aes(x = season, y = RMSE)) +
  geom_bar(stat = "identity", fill = "#e34a33") +
  labs(title = "Modelo SVR",
       x = "Estación del año", y = "RMSE") +
  scale_y_continuous(limits = c(0, 10)) +
  theme_classic()

ggplot(df_resultados, aes(x = season, y = R2)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(subtitle =  "Modelo SVR",
       x = "Estación del año", y = "R²") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_classic()



##############################################################################
##############################################################################
##############################################################################
### ----- ET   -----
# --- Parámetros iniciales
estacion <- "MD"
modelo <- "1"

# --- Cargar datos
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/", sep = "")
setwd(dir)

train_data <- read.csv(paste(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv", sep = ""))
test_data <- read.csv(paste(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv", sep = ""))

# Asegurar formato de fecha
train_data$date <- as.Date(train_data$date)
test_data$date <- as.Date(test_data$date)

# Clasificación estacional
get_season <- function(date) {
  m <- month(date)
  ifelse(m %in% c(12, 1, 2), "Verano",
         ifelse(m %in% c(3, 4, 5), "Otoño",
                ifelse(m %in% c(6, 7, 8), "Invierno", "Primavera")))
}

train_data$season <- get_season(train_data$date)
test_data$season <- get_season(test_data$date)

# Crear listas
modelos_et_por_estacion <- list()
resultados_et_por_estacion <- list()

estaciones <- c("Primavera", "Verano", "Otoño", "Invierno")

for (s in estaciones) {
  cat("/n--- Entrenando ET para:", s, "---/n")
  
  # Filtrar por estación
  train_s <- subset(train_data, season == s)
  test_s  <- subset(test_data,  season == s)
  
  if (nrow(train_s) >= 30) {
    ctrl <- trainControl(method = "cv", number = 10,
                         savePredictions = "final", verboseIter = TRUE)
    
    set.seed(123)
    modelo_et <- train(
      PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + #
        SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
        d2m_mean  +t2m_mean +v10_mean + u10_mean + tp_mean + DEM+ dayWeek,
      data = train_s,
      method = "ranger",
      trControl = ctrl,
      tuneGrid = data.frame(
        mtry = 5,
        splitrule = "extratrees",
        min.node.size = 5
      ),
      importance = "impurity"
    )
    
    modelos_et_por_estacion[[s]] <- modelo_et
    
    # Evaluar modelo
    resultado <- evaluar_modelo(modelo_et, test_s, variable_real = "PM25", tipoModelo = "ET")
    resultados_et_por_estacion[[s]] <- resultado
    print(resultado)
    
  } else {
    cat("No hay suficientes datos para entrenar en", s, "/n")
  }
}

# Consolidar resultados
df_resultados_et <- bind_rows(
  lapply(names(resultados_et_por_estacion), function(s) {
    df <- as.data.frame(resultados_et_por_estacion[[s]])
    df$season <- s
    return(df)
  })
)

print(df_resultados_et)
# Ordenar estaciones
df_resultados_et$season <- factor(df_resultados_et$season,
                                  levels = c("Otoño", "Invierno", "Primavera", "Verano"))

# --- Plot RMSE
ggplot(df_resultados_et, aes(x = season, y = RMSE)) +
  geom_bar(stat = "identity", fill = "#e34a33") +
  labs(title = "Modelo ET",
       x = "Estación del año", y = "RMSE") +
  scale_y_continuous(limits = c(0, 10)) +
  theme_classic()

# --- Plot R²
ggplot(df_resultados_et, aes(x = season, y = R2)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(subtitle = "Modelo ET",
       x = "Estación del año", y = "R²") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_classic()

##############################################################################
##############################################################################
##############################################################################
### ----- RF   -----
# -------------------------------------------------------------------
# Configuración inicial
estacion <- "MD"
modelo <- "1"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/", sep = "")
setwd(dir)
train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

train_data$date <- as.Date(train_data$date)
test_data$date  <- as.Date(test_data$date)

# Clasificación estacional
get_season <- function(date) {
  m <- month(date)
  ifelse(m %in% c(12, 1, 2), "Verano",
         ifelse(m %in% c(3, 4, 5), "Otoño",
                ifelse(m %in% c(6, 7, 8), "Invierno", "Primavera")))
}

train_data$season <- get_season(train_data$date)
test_data$season  <- get_season(test_data$date)

# Listas para almacenar modelos y resultados
modelos_rf_por_estacion <- list()
resultados_rf_por_estacion <- list()

estaciones <- c("Primavera", "Verano", "Otoño", "Invierno")

for (s in estaciones) {
  cat("/n--- Entrenando RF para:", s, "---/n")
  
  train_s <- subset(train_data, season == s)
  test_s  <- subset(test_data, season == s)
  
  if (nrow(train_s) >= 30) {
    ctrl <- trainControl(method = "cv", number = 5,
                         savePredictions = "final", verboseIter = TRUE,
                         allowParallel = TRUE)
    
    set.seed(123)
    modelo_rf <- train(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + #
                         SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
                         d2m_mean  +t2m_mean +v10_mean + u10_mean + tp_mean + DEM+ dayWeek,
                       data = train_s,
                       method = "rf",
                       trControl = ctrl,
                       importance = TRUE)
    
    modelos_rf_por_estacion[[s]] <- modelo_rf
    
    resultado <- evaluar_modelo(modelo_rf, test_s, variable_real = "PM25", tipoModelo = "RF")
    resultados_rf_por_estacion[[s]] <- resultado
    print(resultado)
    
  } else {
    cat("No hay suficientes datos para entrenar en", s, "/n")
  }
}

# Combinar resultados en un dataframe
df_resultados_rf <- bind_rows(
  lapply(names(resultados_rf_por_estacion), function(s) {
    df <- as.data.frame(resultados_rf_por_estacion[[s]])
    df$season <- s
    return(df)
  })
)

print(df_resultados_rf)

# Ordenar y graficar
df_resultados_rf$season <- factor(df_resultados_rf$season,
                                  levels = c("Otoño", "Invierno", "Primavera", "Verano"))

ggplot(df_resultados_rf, aes(x = season, y = RMSE)) +
  geom_bar(stat = "identity", fill = "#e34a33") +
  labs(title = "Modelo RF",
       x = "Estación del año", y = "RMSE") +
  scale_y_continuous(limits = c(0, 10)) +
  theme_classic()

ggplot(df_resultados_rf, aes(x = season, y = R2)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(subtitle = "Modelo RF",
       x = "Estación del año", y = "R²") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_classic()
                     

##############################################################################
##############################################################################
##############################################################################
### ----- XGB   -----
library(xgboost)
library(Matrix)
# ----------------- Configuración inicial -----------------
estacion <- "MD"
modelo <- "1"
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/", sep = "")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

# Convertir a fecha
train_data$date <- as.Date(train_data$date)
test_data$date <- as.Date(test_data$date)

# ----------------- Función estación -----------------
get_season <- function(date) {
  m <- month(date)
  ifelse(m %in% c(12, 1, 2), "Verano",
         ifelse(m %in% c(3, 4, 5), "Otoño",
                ifelse(m %in% c(6, 7, 8), "Invierno", "Primavera")))
}

train_data$season <- get_season(train_data$date)
test_data$season <- get_season(test_data$date)

# ----------------- Evaluación por estación -----------------
estaciones <- c("Primavera", "Verano", "Otoño", "Invierno")
resultados_xgb_por_estacion <- list()
modelos_xgb_por_estacion <- list()

# Variables predictoras
vars <- c("AOD_055", "ndvi", "BCSMASS_dia", "DUSMASS_dia",
          "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "blh_mean",
          "sp_mean", "d2m_mean", "t2m_mean","v10_mean", "u10_mean", "tp_mean", "DEM","dayWeek")

for (s in estaciones) {
  cat("/n--- Entrenando XGB para:", s, "---/n")
  
  train_s <- subset(train_data, season == s)
  test_s  <- subset(test_data, season == s)
  
  if (nrow(train_s) >= 30) {
    X <- train_s[, vars]
    y <- train_s$PM25
    dtrain <- xgb.DMatrix(data = as.matrix(X), label = y)
    
    # Parámetros del modelo
    params <- list(
      booster = "gbtree",
      objective = "reg:squarederror",
      eval_metric = "rmse",
      eta = 0.3,
      max_depth = 6,
      gamma = 0,
      subsample = 0.8,
      colsample_bytree = 1,
      min_child_weight = 1
    )
    
    set.seed(123)
    cv_results <- xgb.cv(
      params = params,
      data = dtrain,
      nrounds = 2000,
      nfold = 10,
      early_stopping_rounds = 20,
      verbose = FALSE
    )
    
    best_nrounds <- cv_results$best_iteration
    
    xgb_model <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = best_nrounds
    )
    
    modelos_xgb_por_estacion[[s]] <- xgb_model
    
    # Test
    X_test <- test_s[, vars]
    y_test <- test_s$PM25
    dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
    
    resultado <- evaluar_modelo(modelo = xgb_model,
                                datos_test = dtest,
                                variable_real = "PM25",
                                tipoModelo = "XGB",
                                y_test = y_test)
    
    resultados_xgb_por_estacion[[s]] <- resultado
    print(resultado)
  } else {
    cat("No hay suficientes datos para entrenar en", s, "/n")
  }
}

# ----------------- Consolidar resultados -----------------
df_resultados_xgb <- bind_rows(
  lapply(names(resultados_xgb_por_estacion), function(s) {
    df <- as.data.frame(resultados_xgb_por_estacion[[s]])
    df$season <- s
    return(df)
  })
)

df_resultados_xgb$season <- factor(df_resultados_xgb$season,
                                   levels = c("Otoño", "Invierno", "Primavera", "Verano"))

print(df_resultados_xgb)

# ----------------- Gráficos -----------------

ggplot(df_resultados_xgb, aes(x = season, y = RMSE)) +
  geom_bar(stat = "identity", fill = "#e34a33") +
  labs(title = "Modelo XGB", x = "Estación del año", y = "RMSE") +
  scale_y_continuous(limits = c(0, 10)) +
  theme_classic()

ggplot(df_resultados_xgb, aes(x = season, y = R2)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(subtitle = "Modelo XGB", x = "Estación del año", y = "R²") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_classic()

#####################################
data <- read.csv("D:/Josefina/Proyectos/Tesis/TOT/resultados/modelos_variables_metricas.csv")

df <- data[data$numModelo == "primavera" | data$numModelo == "verano"
             | data$numModelo == "otonio" | data$numModelo == "invierno",]

names(data)

library(ggplot2)

# Asegurarte de que las estaciones estén en orden lógico
df$r2 <- as.numeric(df$r2)  # Por si r2 está como carácter
df$numModelo <- factor(df$numModelo, levels = c("otonio", "invierno","primavera", "verano"))

ggplot(df, aes(x = numModelo, y = r2, color = modelo, group = modelo)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    x = "",
    y = expression(R^2),
    color = "Modelo"
  ) +
  scale_y_continuous(limits = c(0.5, 0.9)) +
  scale_x_discrete(labels = c(
    "otonio" = "Otoño",
    "invierno" = "Invierno",
    "primavera" = "Primavera",
    "verano" = "Verano"
  )) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

