#################################################################################
#################################################################################
#                           Modelos Predictivos de PM2.5 simples
#################################################################################
#################################################################################
#  Objetivo: Evaluar cómo varía el rendimiento del modelo SVR por estación del año
#funcion para evaluar modelos
evaluar_modelo <- function(modelo, datos_test, variable_real = "PM25",tipoModelo,y_test=NA) {
  #predicciones <- predict(modelo, newdata = datos_test)
  
  if(tipoModelo == "LME") {
    predicciones <- predict(modelo, newdata = datos_test, allow.new.levels = TRUE)
  } else if(tipoModelo == "XGB") {
    predicciones <- predict(modelo, newdata = datos_test)
  } else {
    predicciones <- predict(modelo, newdata = datos_test)
  }
  
  if(tipoModelo == "XGB"){
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
##01. --- RLS
# Cargar los datos
estacion <- "SP"
modelo <- "1"

dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

# Ajustar el modelo de regresión lineal simple
modelo_lm <- lm(PM25 ~ AOD_055, data = train_data)

# Evaluar el desempeño con tu función personalizada
resultados_lm <- evaluar_modelo(modelo = modelo_lm,
                                datos_test = test_data,
                                variable_real = "PM25",
                                tipoModelo = "LM",  # o cualquier texto distinto de "XGB"
                                y_test = NULL)

print(resultados_lm)


##############################################################################
##############################################################################
##############################################################################
## --- Corregir AOD por la PBL
# Cargar los datos
estacion <- "SP"
modelo <- "1"
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))


train_data$AOD_055_correct <- train_data$AOD_055 /train_data$blh_mean
test_data$AOD_055_correct <- test_data$AOD_055 /test_data$blh_mean

# Ajustar el modelo de regresión lineal simple
modelo_lm <- lm(PM25 ~ AOD_055_correct, data = train_data)

# Evaluar el desempeño con tu función personalizada
resultados_lm <- evaluar_modelo(modelo = modelo_lm,
                                datos_test = test_data,
                                variable_real = "PM25",
                                tipoModelo = "LM",  # o cualquier texto distinto de "XGB"
                                y_test = NULL)

print(resultados_lm)


#############################################################################
#############################################################################
#############################################################################
## --- Corregir AOD por la humedad
estacion <- "SP"
data<- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/merge_tot/",estacion,"_merge_comp_RH.csv",sep=""))

data_completo <- data[complete.cases(data),]
## Agreego la variable numero de dias
data_completo$date <- strptime(data_completo$date, format = "%Y-%m-%d")
data_completo$dayWeek <- wday(data_completo$date, week_start = 1)
# Sin 2024 que lo usamos para verificar despues con los mapas
data_completo <- data_completo[year(data_completo$date) != 2024,]
# verifficamos que solo esten los años 2015-2023
unique(year(data_completo$date))
###### ------  Modelo 1 - Aleatorio  ------  ##### 
# Dividir el dataframe en 70% entrenamiento y 30% testeo
train_index <- createDataPartition(data_completo$PM25, p = 0.7, list = FALSE)
train_data <- data_completo[train_index, ]
test_data <- data_completo[-train_index, ]

train_data$funcRH <- 1/(1-(train_data$RH/100))
test_data$funcRH <- 1/(1-(test_data$RH/100))

train_data$AOD_055_correct <- train_data$AOD_055 /train_data$funcRH
test_data$AOD_055_correct <- test_data$AOD_055 /test_data$funcRH

# Ajustar el modelo de regresión lineal simple
modelo_lm <- lm(PM25 ~ AOD_055_correct, data = train_data)

# Evaluar el desempeño con tu función personalizada
resultados_lm <- evaluar_modelo(modelo = modelo_lm,
                                datos_test = test_data,
                                variable_real = "PM25",
                                tipoModelo = "LM",  # o cualquier texto distinto de "XGB"
                                y_test = NULL)

print(resultados_lm)

##############################################################################
##############################################################################
##############################################################################
## --- Corregir AOD por la PBL + humedad
estacion <- "SP"
data<- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/merge_tot/",estacion,"_merge_comp_RH.csv",sep=""))

data_completo <- data[complete.cases(data),]
## Agreego la variable numero de dias
data_completo$date <- strptime(data_completo$date, format = "%Y-%m-%d")
data_completo$dayWeek <- wday(data_completo$date, week_start = 1)
# Sin 2024 que lo usamos para verificar despues con los mapas
data_completo <- data_completo[year(data_completo$date) != 2024,]
# verifficamos que solo esten los años 2015-2023
unique(year(data_completo$date))
###### ------  Modelo 1 - Aleatorio  ------  ##### 
# Dividir el dataframe en 70% entrenamiento y 30% testeo
train_index <- createDataPartition(data_completo$PM25, p = 0.7, list = FALSE)
train_data <- data_completo[train_index, ]
test_data <- data_completo[-train_index, ]

# Primero calculamos la corrección por humedad relativa
train_data$funcRH <- 1 / (1 - (train_data$RH / 100))
test_data$funcRH <- 1 / (1 - (test_data$RH / 100))

# Corregimos el AOD dividiéndolo por la humedad y la BLH al mismo tiempo
train_data$AOD_055_correct <- train_data$AOD_055 / (train_data$funcRH * train_data$blh_mean)
test_data$AOD_055_correct <- test_data$AOD_055 / (test_data$funcRH * test_data$blh_mean)

# Ahora entrenamos el modelo con el AOD corregido
modelo_lm <- lm(PM25 ~ AOD_055_correct, data = train_data)


# Evaluar el desempeño con tu función personalizada
resultados_lm <- evaluar_modelo(modelo = modelo_lm,
                                datos_test = test_data,
                                variable_real = "PM25",
                                tipoModelo = "LM",  # o cualquier texto distinto de "XGB"
                                y_test = NULL)

print(resultados_lm)

##############################################################################
##############################################################################
##############################################################################
## --- RLM

# Cargar los datos
estacion <- "SP"
modelo <- "1"

dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

# Ajustar el modelo de regresión lineal múltiple
modelo_lm_multiple <- lm(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia +
                           SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + sp_mean +
                           d2m_mean + v10_mean + u10_mean + tp_mean + dayWeek,
                         data = train_data)
# Ajustar el modelo de regresión lineal múltiple CON AOD Corregido
modelo_lm_multiple <- lm(PM25 ~ AOD_055_correct + ndvi + BCSMASS_dia + DUSMASS_dia +
                           SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + sp_mean +
                           d2m_mean + v10_mean + u10_mean + tp_mean + dayWeek,
                         data = train_data)
# Evaluar el modelo usando tu función
resultados_lm_multiple <- evaluar_modelo(modelo = modelo_lm_multiple,
                                         datos_test = test_data,
                                         variable_real = "PM25",
                                         tipoModelo = "LM",  # cualquier valor excepto "XGB"
                                         y_test = NULL)

# Mostrar los resultados
print(resultados_lm_multiple)

##############################################################################
##############################################################################
##############################################################################
#-- RLM Agregando de auna las variables
# Cargar datos

estacion <- "SP"
modelo <- "1"

dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

# Orden de variables a agregar
variables <- c("blh_mean", "d2m_mean", "sp_mean", "v10_mean", "u10_mean", "tp_mean",
               "ndvi", "BCSMASS_dia", "DUSMASS_dia", "SO2SMASS_dia", "SO4SMASS_dia",
               "SSSMASS_dia", "dayWeek")

# Lista para guardar resultados
resultados_modelos <- list()

# Incluir primer modelo solo con AOD_055
vars_actuales <- "AOD_055_correct" #"AOD_055"
formula_lm <- as.formula(paste("PM25 ~", vars_actuales))
modelo_lm <- lm(formula_lm, data = train_data)
resultados <- evaluar_modelo(modelo = modelo_lm,
                             datos_test = test_data,
                             variable_real = "PM25",
                             tipoModelo = "LM",
                             y_test = NULL)
resultados$Modelo <- "Modelo_1"
resultados$Variables <- vars_actuales
resultados_modelos[[1]] <- resultados

# Iterar para agregar progresivamente más variables
for (i in seq_along(variables)) {
  vars_actuales <-  c("AOD_055_correct", variables[1:i])#c("AOD_055", variables[1:i])
  formula_lm <- as.formula(paste("PM25 ~", paste(vars_actuales, collapse = " + ")))
  modelo_lm <- lm(formula_lm, data = train_data)
  
  resultados <- evaluar_modelo(modelo = modelo_lm,
                               datos_test = test_data,
                               variable_real = "PM25",
                               tipoModelo = "LM",
                               y_test = NULL)
  
  resultados$Modelo <- paste0("Modelo_", i + 1)  # +1 porque ya hicimos el primero
  resultados$Variables <- paste(vars_actuales, collapse = ", ")
  
  resultados_modelos[[i + 1]] <- resultados
}

# Combinar resultados
tabla_resultados <- do.call(rbind, resultados_modelos)
tabla_resultados$Num_Variables <- sapply(strsplit(tabla_resultados$Variables, ", "), length)

# Convertir a formato largo para ggplot
data_melt <- melt(tabla_resultados[, c("Num_Variables", "R2", "RMSE")], id.vars = "Num_Variables")

# Establecer límites por variable para el gráfico
limites <- data.frame(
  variable = c("R2", "RMSE"),
  ymin = c(0, 6),
  ymax = c(1, 12)
)

data_melt_limited <- left_join(data_melt, limites, by = "variable")

# Renombrar variable con notación matemática
data_melt_limited$variable <- recode(data_melt_limited$variable,
                                     "R2" = "R^2",
                                     "RMSE" = "RMSE")

# Graficar con títulos de facet parseados
ggplot(data_melt_limited, aes(x = Num_Variables, y = value)) +
  geom_blank(aes(y = ymin)) +
  geom_blank(aes(y = ymax)) +
  geom_line(aes(color = variable), size = 1.2) +
  geom_point(aes(color = variable), size = 2) +
  facet_wrap(~variable, scales = "free_y", labeller = label_parsed) +
  scale_x_continuous(breaks = 1:14) +
  labs(title = "Evolución del desempeño del modelo",
       x = "N° de Variables",
       y = "Valor de Métrica",
       color = "Métrica") +
  theme_classic()+
  theme(
    # axis.title.x = element_text(size = 16),
    # axis.title.y = element_text(size = 16),
    # axis.title.y.right = element_text(size = 16),  # Para RMSE
    # axis.text.x = element_text(size = 14),
    # axis.text.y = element_text(size = 14),
    # axis.text.y.right = element_text(size = 14),  # Ticks del eje derecho
    # legend.text = element_text(size = 14),
    legend.position = "none"  # opcional: ubica la leyenda arriba
  )

#############################################################################
#############################################################################
#############################################################################
#Lasso
# The model was trained using alpha = 0.1 with max_iter = 1500, ensuring convergence.
# Ganesh Machhindra Kunjir 2025
# regularization parameter: 0.1 Bagheri 2022
# Instalar si no tenés glmnet
# install.packages("glmnet")

library(glmnet)

# Función para evaluar modelos glmnet (Lasso o Ridge)
evaluar_glmnet <- function(modelo, x_test, y_test, lambda_usar) {
  predicciones <- predict(modelo, newx = x_test, s = lambda_usar)
  predicciones <- as.numeric(predicciones)
  
  r2 <- cor(predicciones, y_test)^2
  pearson <- cor(y_test, predicciones, method = "pearson")
  rmse <- sqrt(mean((predicciones - y_test)^2))
  bias <- mean(predicciones - y_test)
  
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

# =====================
# Cargar datos
# =====================
estacion <- "SP"
modelo <- "1"
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

# =====================
# Seleccionar variables predictoras
# =====================
variables <- c("AOD_055", "ndvi", "BCSMASS_dia", "DUSMASS_dia", 
               "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", 
               "blh_mean", "sp_mean", "d2m_mean", "v10_mean", 
               "u10_mean", "tp_mean", "dayWeek")

# =====================
# Estandarizar predictores (media 0, sd 1) usando los parámetros de entrenamiento
# =====================
train_scaled <- scale(train_data[, variables])
test_scaled <- scale(test_data[, variables], center = attr(train_scaled, "scaled:center"), 
                     scale = attr(train_scaled, "scaled:scale"))

# =====================
# Preparar matrices modelo
# =====================
x_train <- as.matrix(train_scaled)
y_train <- train_data$PM25

x_test <- as.matrix(test_scaled)
y_test <- test_data$PM25

# =====================
# Ajustar Lasso con validación cruzada
# =====================
set.seed(123)  # reproducibilidad
#cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 0.1, maxit = 1500)

# Mejor lambda
best_lambda <- cv_lasso$lambda.min
cat("Mejor lambda:", best_lambda, "\n")

# =====================
# Evaluar modelo
# =====================
resultados_lasso <- evaluar_glmnet(cv_lasso, x_test, y_test, lambda_usar = best_lambda)
print(resultados_lasso)

# =====================
# Ver coeficientes del modelo Lasso
# =====================
coef_lasso <- coef(cv_lasso, s = best_lambda)
print(coef_lasso)



# Obtener coeficientes del modelo
coeficientes <- coef(cv_lasso, s = best_lambda)

# Convertir a data frame
coef_df <- as.data.frame(as.matrix(coeficientes))
coef_df$variable <- rownames(coef_df)
colnames(coef_df)[1] <- "coeficiente"

# Filtrar variables con coeficiente distinto de cero (descarta intercepto)
coef_filtrado <- coef_df[coef_df$coeficiente != 0 & coef_df$variable != "(Intercept)", ]

# Ordenar por valor absoluto del coeficiente
coef_filtrado <- coef_filtrado[order(abs(coef_filtrado$coeficiente), decreasing = TRUE), ]

# Graficar
ggplot(coef_filtrado, aes(x = reorder(variable, abs(coeficiente)), y = coeficiente)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Importancia de las variables según regresión Lasso",
       x = "Variables",
       y = "Coeficiente") +
  theme_classic()


##############################################################################
##############################################################################
##############################################################################
# Librerías necesarias RIDGE

library(glmnet)
library(ggplot2)
evaluar_modelo_ridge <- function(modelo, x_test, y_test) {
  predicciones <- predict(modelo, newx = x_test)
  predicciones <- as.numeric(predicciones)
  
  # Calcular métricas
  r2 <- cor(predicciones, y_test)^2
  pearson <- cor(y_test, predicciones, method = "pearson")
  rmse <- sqrt(mean((predicciones - y_test)^2))
  bias <- mean(predicciones - y_test)
  
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

# Parámetros
estacion <- "SP"
modelo <- "1"
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")

# Cargar datos
train_data <- read.csv(paste0(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv"))
test_data  <- read.csv(paste0(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv"))

# Preparar matrices para glmnet (quitar intercepto con [,-1])
x_train <- model.matrix(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
                          SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + sp_mean +
                          d2m_mean  + v10_mean + u10_mean + tp_mean + dayWeek, data = train_data)[,-1]
y_train <- train_data$PM25

x_test <- model.matrix(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
                         SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + sp_mean +
                         d2m_mean  + v10_mean + u10_mean + tp_mean + dayWeek, data = test_data)[,-1]
y_test <- test_data$PM25

# Ajustar Ridge con validación cruzada para obtener mejor lambda
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 1, standardize = TRUE)
best_lambda <- cv_ridge$lambda.min
cat("Mejor lambda Ridge:", best_lambda, "\n")

# Ajustar modelo final Ridge con mejor lambda
ridge_model <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda, standardize = TRUE,nfods=10)

# Predicciones
predicciones <- predict(ridge_model, s = best_lambda, newx = x_test)
predicciones_num <- as.numeric(predicciones)

# Data frame para graficar
df <- data.frame(
  valores_reales = y_test,
  predicciones = predicciones_num
)

# Límites iguales para x e y
min_val <- min(c(df$valores_reales, df$predicciones), na.rm = TRUE)
max_val <- max(c(df$valores_reales, df$predicciones), na.rm = TRUE)

# Gráfico de dispersión: valores reales vs predichos
ggplot(df, aes(x = valores_reales, y = predicciones)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  coord_fixed(ratio = 1) +
  xlim(min_val, max_val) +
  ylim(min_val, max_val) +
  labs(
    title = "Predicciones Ridge vs Valores Reales de PM2.5",
    x = "Valores Reales de PM2.5",
    y = "Predicciones del Modelo Ridge"
  ) +
  theme_minimal()

# Extraer coeficientes del modelo Ridge
coef_ridge <- coef(ridge_model)
coef_df <- data.frame(
  variable = rownames(coef_ridge),
  coeficiente = as.numeric(coef_ridge)
)

# Quitar el intercepto
coef_df <- coef_df[coef_df$variable != "(Intercept)", ]

# Gráfico de importancia de variables (coeficientes)
ggplot(coef_df, aes(x = reorder(variable, abs(coeficiente)), y = coeficiente)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = paste("Importancia de variables - Modelo Ridge (λ =", round(best_lambda, 4), ")"),
    x = "Variables",
    y = "Coeficientes"
  ) +
  theme_minimal()
evaluar_modelo_ridge(ridge_model, x_test, y_test)


##############################################################################
##############################################################################
##############################################################################

# Cargar librerías necesarias
library(glmnet)

# Supongamos que ya tenés tus datos
# x_train: matriz de predictores (sin intercepto)
# y_train: vector de la variable respuesta

# Crear una secuencia de lambda en escala logarítmica
lambda_grid <- 10^seq(4, -4, length = 100)

# Validación cruzada (k=10) para modelo Ridge (alpha = 0)
cv_ridge <- cv.glmnet(
  x = x_train,
  y = y_train,
  alpha = 0,                  # alpha = 0 para Ridge
  lambda = lambda_grid,       # secuencia de lambda
  nfolds = 10,                # 10-fold cross-validation
  standardize = TRUE          # estandariza variables automáticamente
)

# Mostrar el mejor lambda
best_lambda <- cv_ridge$lambda.min
cat("Mejor lambda:", best_lambda, "\n")

# Graficar el error medio de validación cruzada según lambda
plot(cv_ridge)
abline(v = log(best_lambda), col = "red", lty = 2)

# Ajustar el modelo final usando el mejor lambda
ridge_model_final <- glmnet(
  x = x_train,
  y = y_train,
  alpha = 0,
  lambda = best_lambda,
  standardize = TRUE
)

# Coeficientes del modelo
coef(ridge_model_final)
evaluar_modelo_ridge(ridge_model_final, x_test, y_test)


##############################################################################
##############################################################################
##############################################################################
## --- GAM

# Cargar librerías necesarias
library(mgcv)

# Definir estación y modelo
estacion <- "SP"
modelo <- "1"

# Definir directorio y cargar datasets
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

# Ajustar el modelo GAM (con suavizadores para variables continuas)
modelo_gam <- gam(PM25 ~ s(AOD_055) + s(ndvi) + s(BCSMASS_dia) + s(DUSMASS_dia) +
                    s(SO2SMASS_dia) + s(SO4SMASS_dia) + s(SSSMASS_dia) + 
                    s(blh_mean) + s(sp_mean) + s(d2m_mean) + 
                    s(v10_mean) + s(u10_mean) + s(tp_mean) + dayWeek,
                  data = train_data)

modelo_gam <- gam(PM25 ~ s(AOD_055, k=10) + s(ndvi, k=20) + s(BCSMASS_dia, k=10) + 
                    s(DUSMASS_dia, k=10) + s(SO2SMASS_dia, k=10) + s(SO4SMASS_dia, k=10) + 
                    s(SSSMASS_dia, k=12) + s(blh_mean, k=10) + s(sp_mean, k=10) +
                    s(d2m_mean, k=10) + s(v10_mean, k=10) + s(u10_mean, k=10) + 
                    s(tp_mean, k=12) + dayWeek,
                  data = train_data, select = TRUE)

# Evaluar el modelo usando tu función
resultados_gam <- evaluar_modelo(modelo = modelo_gam,
                                 datos_test = test_data,
                                 variable_real = "PM25",
                                 tipoModelo = "GAM",  # puede ser cualquier string excepto "XGB"
                                 y_test = NULL)

plot(modelo_gam)
# Mostrar resultados
print(resultados_gam)

##$ segun MA, 2022
GAM_model <- gam (PM ~ s(AOD) + s   (WS), family, data =    modeling_dataset)
gam.check(modelo_gam)

##########################################
#Lista de variables en orden acumulativo
# Variables numéricas (para aplicar s())
vars_numericas <- c("AOD_055","blh_mean", "d2m_mean", "sp_mean", "v10_mean", "u10_mean", "tp_mean",
                    "ndvi", "BCSMASS_dia", "DUSMASS_dia", "SO2SMASS_dia", "SO4SMASS_dia",
                    "SSSMASS_dia")

# Variables categóricas (no usar s())
vars_categoricas <- c("dayWeek")

# Lista completa en orden
var_orden <- c(vars_numericas, vars_categoricas)

resultados <- data.frame()
vars_utilizadas <- c()

for (i in 1:length(var_orden)) {
  vars_utilizadas <- c(vars_utilizadas, var_orden[i])
  
  partes_formula <- sapply(vars_utilizadas, function(v) {
    if (v %in% vars_numericas) {
      paste0("s(", v, ")")
    } else {
      paste0("factor(", v, ")")
    }
  })
  
  formula_text <- paste("PM25 ~", paste(partes_formula, collapse = " + "))
  formula_gam <- as.formula(formula_text)
  
  modelo <- gam(formula_gam, data = train_data)
  
  metrica <- evaluar_modelo(modelo, datos_test = test_data, tipoModelo = "GAM")
  metrica$Variables_incluidas <- paste(vars_utilizadas, collapse = ", ")
  metrica$Paso <- i
  
  resultados <- bind_rows(resultados, metrica)
}


# Mostrar resultados
View(resultados)
write.csv(resultados,"lalal.csv")
##############################################################################
##############################################################################
##############################################################################
## --- LME

library(lme4)
library(caret)  # Para crear folds

# Cargar datos
estacion <- "SP"
modelo <- "1"
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

train_data$fecha <- as.factor(train_data$date)
test_data$fecha <- as.factor(test_data$date)
train_data$estacion <- as.factor(train_data$estacion)
test_data$estacion <- as.factor(test_data$estacion)

# Crear folds para CV (con stratification si fuera necesario, acá simple)
set.seed(123)
folds <- createFolds(train_data$PM25, k = 10, list = TRUE, returnTrain = FALSE)

# Función para entrenar y evaluar un fold
evaluar_fold <- function(indices_test) {
  train_fold <- train_data[-indices_test, ]
  val_fold <- train_data[indices_test, ]
  
  # Entrenar modelo LME en train_fold
  #modelo_fold <- lmer(PM25 ~ 1+ (1|AOD_055) , data = train_fold)
  modelo_fold <- lmer(PM25 ~ AOD_055 + (1  | fecha), data = train_fold)
  
  # Predecir en val_fold
  pred <- predict(modelo_fold, newdata = val_fold, allow.new.levels = TRUE)
  
  # Calcular métricas manualmente
  r2 <- cor(pred, val_fold$PM25)^2
  rmse <- sqrt(mean((pred - val_fold$PM25)^2))
  bias <- mean(pred - val_fold$PM25)
  
  return(data.frame(R2 = r2, RMSE = rmse, Bias = bias))
}

# Evaluar todos los folds y sacar promedio
resultados_cv <- lapply(folds, evaluar_fold)
resultados_cv <- do.call(rbind, resultados_cv)

# Promedio y desviación estándar de métricas CV
resumen_cv <- data.frame(
  R2_mean = mean(resultados_cv$R2),
  R2_sd = sd(resultados_cv$R2),
  RMSE_mean = mean(resultados_cv$RMSE),
  RMSE_sd = sd(resultados_cv$RMSE),
  Bias_mean = mean(resultados_cv$Bias),
  Bias_sd = sd(resultados_cv$Bias)
)

print(resumen_cv)

# --- Entrenar modelo final con todo el training set ---
modelo_final <- lmer(PM25 ~ AOD_055 + (1 + AOD_055|dayWeek ), data = train_data)
modelo_final <- lmer(PM25 ~ 1 + ( + 1 | AOD_055), data = train_data)
modelo_final <- lmer(PM25 ~ AOD_055 + (1 + AOD_055 | dayWeek) + (1 | estacion), data = train_data)
# Variables a escalar (excluyendo la variable dependiente PM25 y la variable de agrupación fecha)
vars_a_escalar <- c("AOD_055", "t2m_mean", "d2m_mean", "v10_mean", "u10_mean",
                    "blh_mean", "sp_mean", "tp_mean", "ndvi")

# Crear copia del dataset y escalar las variables
train_data_scaled <- train_data
train_data_scaled[vars_a_escalar] <- scale(train_data[vars_a_escalar])

# Ajustar modelo con variables escaladas
modelo_final <- lmer(PM25 ~ AOD_055 + t2m_mean + d2m_mean + v10_mean + u10_mean +
                      blh_mean + sp_mean + tp_mean + #ndvi +
                      (1 + AOD_055 | fecha), data = train_data_scaled)




# Predecir en test_data
predicciones_test <- predict(modelo_final, newdata = test_data, allow.new.levels = TRUE)

# Evaluar con tu función evaluar_modelo
resultados_test <- evaluar_modelo(
  modelo = modelo_final,
  datos_test = test_data,
  variable_real = "PM25",
  tipoModelo = "LME"
)

print(resultados_test)
################################################################################
################################################################################
################################################################################
# cORRECCION DLE PM en base al RH

estacion <- "SP"
data <- read.csv(paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/proceed/merge_tot/", estacion, "_merge_comp_RH.csv"))

data_completo <- data[complete.cases(data),]
data_completo$date <- as.Date(data_completo$date, format = "%Y-%m-%d")
data_completo$dayWeek <- wday(data_completo$date, week_start = 1)
data_completo <- data_completo[year(data_completo$date) != 2024,]
unique(year(data_completo$date))

# Dividir en 70% entrenamiento y 30% testeo
train_index <- createDataPartition(data_completo$PM25, p = 0.7, list = FALSE)
train_data <- data_completo[train_index, ]
test_data <- data_completo[-train_index, ]

# CORRECCIÓN DE PM2.5 según Bagheri (2022)
train_data$PM25_corrected <- train_data$PM25 * (1 - train_data$RH / 100)^(-1)
test_data$PM25_corrected <- test_data$PM25 * (1 - test_data$RH / 100)^(-1)

# Ajustar el modelo usando AOD sin corregir y PM2.5 corregido como variable respuesta
modelo_lm <- lm(PM25_corrected ~ AOD_055, data = train_data)

# Función para evaluar modelo (asumiendo que tienes una función "evaluar_modelo")
resultados_lm <- evaluar_modelo(modelo = modelo_lm,
                                datos_test = test_data,
                                variable_real = "PM25_corrected",
                                tipoModelo = "LM",
                                y_test = NULL)

print(resultados_lm)


