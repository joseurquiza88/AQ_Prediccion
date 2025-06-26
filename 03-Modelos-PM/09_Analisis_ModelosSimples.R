#################################################################################
#################################################################################
#                           Modelos Predictivos de PM2.5 simples
#################################################################################
#################################################################################
#  Objetivo: Evaluar cómo varía el rendimiento del modelo SVR por estación del año
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
##01. --- RLS
# Cargar los datos
estacion <- "MX"
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

# Para hacer el plot
library(ggplot2)

# Predicciones sobre los datos de entrenamiento
test_data$pred <- predict(modelo_lm, newdata = test_data)
plot_RLS<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +     # puntos reales vs predicción
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  # línea ideal
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") +  # ajuste de regresión
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y

  theme_classic()
plot_RLS


library(ggplot2)
library(ggpointdensity)  # si no lo tenés: install.packages("ggpointdensity")

plot_RLS <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  theme_classic()+ theme(legend.position="none")

plot_RLS

##############################################################################
##############################################################################
##############################################################################
## --- Corregir AOD por la PBL
# Cargar los datos
estacion <- "MX"
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

# Predicciones sobre los datos de entrenamiento
test_data$pred <- predict(modelo_lm, newdata = test_data)
plot_RLS_aod<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +     # puntos reales vs predicción
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  # línea ideal
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") +  # ajuste de regresión
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  
  theme_classic()
plot_RLS_aod


library(ggplot2)
library(ggpointdensity)  # si no lo tenés: install.packages("ggpointdensity")

plot_RLS <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  theme_classic()+ theme(legend.position="none")

plot_RLS

#############################################################################
#############################################################################
#############################################################################
## --- Corregir AOD por la humedad
estacion <- "MX"
modelo <- "RH"

dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))


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

# Predicciones sobre los datos de entrenamiento
test_data$pred <- predict(modelo_lm, newdata = test_data)
plot_RLS_aod<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +     # puntos reales vs predicción
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  # línea ideal
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") +  # ajuste de regresión
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  
  theme_classic()
plot_RLS_aod


library(ggplot2)
library(ggpointdensity)  # si no lo tenés: install.packages("ggpointdensity")

plot_RLS <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  theme_classic()+ theme(legend.position="none")

plot_RLS
##############################################################################
##############################################################################
##############################################################################
## --- Corregir AOD por la PBL + humedad
estacion <- "MX"
modelo <- "RH"

dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))



train_data$AOD_corrected <- (train_data$AOD_055 * (1 - train_data$RH / 100)) / train_data$blh_mean
test_data$AOD_corrected <- (test_data$AOD_055 * (1 - test_data$RH / 100)) / test_data$blh_mean



# Ahora entrenamos el modelo con el AOD corregido
modelo_lm <- lm(PM25 ~ AOD_corrected, data = train_data)


# Evaluar el desempeño con tu función personalizada
resultados_lm <- evaluar_modelo(modelo = modelo_lm,
                                datos_test = test_data,
                                variable_real = "PM25",
                                tipoModelo = "LM",  # o cualquier texto distinto de "XGB"
                                y_test = NULL)

print(resultados_lm)
# Predicciones sobre los datos de entrenamiento
test_data$pred <- predict(modelo_lm, newdata = test_data)
plot_RLS_aod<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +     # puntos reales vs predicción
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  # línea ideal
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") +  # ajuste de regresión
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  
  theme_classic()
plot_RLS_aod


library(ggplot2)
library(ggpointdensity)  # si no lo tenés: install.packages("ggpointdensity")

plot_RLS <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  theme_classic()+ theme(legend.position="none")

plot_RLS
##############################################################################
##############################################################################
##############################################################################
## --- RLM

# Cargar los datos
estacion <- "MX"
modelo <- "1"

dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

# Ajustar el modelo de regresión lineal múltiple
modelo_lm_multiple <- lm(PM25 ~  AOD_055+ndvi + BCSMASS_dia + DUSMASS_dia + #OCSMASS
                           SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean+
                           t2m_mean + 
                           DEM+  d2m_mean   +v10_mean + u10_mean + tp_mean+ dayWeek,
                         data = train_data)
# Ajustar el modelo de regresión lineal múltiple CON AOD Corregido
# modelo_lm_multiple <- lm(PM25 ~ AOD_055_correct + ndvi + BCSMASS_dia + DUSMASS_dia +
#                            SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + sp_mean +
#                            d2m_mean + v10_mean + u10_mean + tp_mean + dayWeek,
#                          data = train_data)
# Evaluar el modelo usando tu función
resultados_lm_multiple <- evaluar_modelo(modelo = modelo_lm_multiple,
                                         datos_test = test_data,
                                         variable_real = "PM25",
                                         tipoModelo = "LM",  # cualquier valor excepto "XGB"
                                         y_test = NULL)

# Mostrar los resultados
print(resultados_lm_multiple)

test_data <- data.frame(pred=predict(modelo_lm_multiple, newdata = test_data) , PM25=test_data$PM25)
test_data <- test_data[test_data$pred>0,]
# Predicciones sobre los datos de entrenamiento
#test_data$pred <- predict(modelo_lm_multiple, newdata = test_data)
plot_RLM<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +     # puntos reales vs predicción
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  # línea ideal
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") +  # ajuste de regresión
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  
  theme_classic()
plot_RLM


library(ggplot2)
library(ggpointdensity)  # si no lo tenés: install.packages("ggpointdensity")

plot_RLM <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  theme_classic()+ theme(legend.position="none")

plot_RLM


##############################################################################
##############################################################################
##############################################################################
#-- RLM Agregando de auna las variables
# Cargar datos
library(reshape2)
estacion <- "MX"

modelo <- "1"

dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

# Orden de variables a agregar
variables <- c("blh_mean", "d2m_mean", "sp_mean","t2m_mean","v10_mean", "u10_mean", "tp_mean", # 
               "ndvi", "BCSMASS_dia", "DUSMASS_dia", "SO2SMASS_dia", "SO4SMASS_dia",
               "SSSMASS_dia", "DEM","dayWeek")#

# Lista para guardar resultados
resultados_modelos <- list()

# Incluir primer modelo solo con AOD_055
vars_actuales <- "AOD_055"#"AOD_055_correct" #
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
  vars_actuales <-  c("AOD_055", variables[1:i])#c("AOD_055_correct", variables[1:i])
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
  scale_x_continuous(breaks = 1:16) +
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
write.csv(tabla_resultados,"tabla_resultados_MX.csv")
#############################################################################
#############################################################################
#############################################################################
#Lasso
# The model was trained using alpha = 0.1 with max_iter = 1500, ensuring convergence.
# Ganesh Machhindra Kunjir 2025
# regularization parameter: 0.1 Bagheri 2022
# Instalar si no tenés glmnet
# install.packages("glmnet")
# alpha = 1 → puro Lasso (penalización L1).
# 
# alpha = 0 → puro Ridge (penalización L2).



library(glmnet)

# Función para evaluar modelos glmnet (Lasso o Ridge)
evaluar_glmnet <- function(modelo, x_test, y_test, lambda_usar) {
  predicciones <- predict(modelo, newx = x_test, s = lambda_usar)
  df <- data.frame(predicciones=as.numeric(predicciones),y_test=y_test)
  
  df <- df[df$predicciones>0,]
  r2 <- cor(df$predicciones, df$y_test)^2
  pearson <- cor(df$y_test, df$predicciones, method = "pearson")
  rmse <- sqrt(mean((df$predicciones - df$y_test)^2))
  bias <- mean(df$predicciones - df$y_test)
  
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

# =====================
# Cargar datos
# =====================
estacion <- "MX"
modelo <- "1"
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))

# =====================
# Seleccionar variables predictoras
# =====================
variables <- c("AOD_055", "ndvi", "BCSMASS_dia", "DUSMASS_dia", 
               "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "sp_mean",
               "blh_mean",  "d2m_mean","v10_mean", "DEM","t2m_mean" ,
               "u10_mean", "tp_mean", "dayWeek") #,,,,

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
#cv_lasso <- cv.glmnet(x_train, y_train, alpha = 0.1)#, maxit = 1500)
#cv_lasso <- cv.glmnet(x_train, y_train, alpha = 0.1, maxit = 1500)
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)#, maxit = 1500)
# Mejor lambda
best_lambda <- cv_lasso$lambda.min
cat("Mejor lambda:", best_lambda, "/n")

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
coef_df$variable2 <- c("intercept","AOD", 
                       "NDVI", "BCSMASS", "DUSMASS", 
                       "SO2SMASS", "SO4SMASS", "SSSMASS", 
                       "sp", "blh",  "d2m","v10", 
                       "t2m" ,"DEM","u10", "tp", "dayWeek")
colnames(coef_df)[1] <- "coeficiente"

# Filtrar variables con coeficiente distinto de cero (descarta intercepto)
coef_filtrado <- coef_df[coef_df$coeficiente != 0 & coef_df$variable != "(Intercept)", ]

# Ordenar por valor absoluto del coeficiente
coef_filtrado <- coef_filtrado[order(abs(coef_filtrado$coeficiente), decreasing = TRUE), ]

# Graficar
ggplot(coef_filtrado, aes(x = reorder(variable2, abs(coeficiente)), y = coeficiente)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(#title = "Importancia de las variables según regresión Lasso",
       x = "Variables",
       y = "Coeficiente") +
  theme_classic()

# Predicciones sobre los datos de entrenamiento

pred <- predict(cv_lasso, newx = x_test, s=best_lambda)
df <- data.frame(pred=pred,y_test=y_test)
names (df) <- c("pred","PM25")
test_data <- df[df$pred>0,]
plot_RLM_Lasso<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +     # puntos reales vs predicción
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  # línea ideal
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") +  # ajuste de regresión
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  
  theme_classic()
plot_RLM_Lasso


library(ggplot2)
library(ggpointdensity)  # si no lo tenés: install.packages("ggpointdensity")

plot_RLM_Lasso <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  theme_classic()+ theme(legend.position="none")

plot_RLM_Lasso


##############################################################################
##############################################################################
##############################################################################
# Librerías necesarias RIDGE

library(glmnet)
library(ggplot2)
evaluar_modelo_ridge <- function(modelo, x_test, y_test) {
  predicciones <- predict(modelo, newx = x_test)
  predicciones <- as.numeric(predicciones)
  df <- data.frame(predicciones=as.numeric(predicciones),y_test=y_test)
  
  df <- df[df$predicciones>0,]
  # Calcular métricas
  r2 <- cor(df$predicciones, df$y_test)^2
  pearson <- cor(df$y_test, df$predicciones, method = "pearson")
  rmse <- sqrt(mean((df$predicciones - df$y_test)^2))
  bias <- mean(df$predicciones - df$y_test)
  
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

# Parámetros
estacion <- "MX"
modelo <- "1"
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")

# Cargar datos
train_data <- read.csv(paste0(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv"))
test_data  <- read.csv(paste0(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv"))

# Preparar matrices para glmnet (quitar intercepto con [,-1])
x_train <- model.matrix(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
                          DEM + 
                          t2m_mean +
                          SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + sp_mean +
                          d2m_mean  + v10_mean + u10_mean + tp_mean + dayWeek, data = train_data)[,-1]
y_train <- train_data$PM25

x_test <- model.matrix(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + 
                         DEM + 
                         t2m_mean +
                         SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + sp_mean +
                         d2m_mean  + v10_mean + u10_mean + tp_mean + dayWeek, data = test_data)[,-1]
y_test <- test_data$PM25

# Ajustar Ridge con validación cruzada para obtener mejor lambda
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0, standardize = TRUE)
best_lambda <- cv_ridge$lambda.min
cat("Mejor lambda Ridge:", best_lambda, "/n")

# Ajustar modelo final Ridge con mejor lambda
ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda, standardize = TRUE,nfods=10)

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
evaluar_modelo_ridge(cv_ridge, x_test, y_test)



# Predicciones sobre los datos de entrenamiento
pred <- predict(ridge_model, newx = x_test, s=best_lambda)
df <- data.frame(pred=pred,PM25=y_test)
names(df) <- c("pred","PM25")
test_data <- df[df$pred>0,]

# test_data$pred <- predict(ridge_model, newx = x_test, s=best_lambda)

plot_RLM_ridge<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +     # puntos reales vs predicción
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  # línea ideal
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") +  # ajuste de regresión
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  
  theme_classic()
plot_RLM_ridge


library(ggplot2)
library(ggpointdensity)  # si no lo tenés: install.packages("ggpointdensity")

plot_RLM_ridge<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  theme_classic()+ theme(legend.position="none")

plot_RLM_ridge
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
cat("Mejor lambda:", best_lambda, "/n")

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
estacion <- "MX"
modelo <- "1"

# Definir directorio y cargar datasets
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))


model.gam1 <-gam(PM25~s(AOD_055,        #GAM formula (Y~x1+x2...) with a smooth function on AOD_550 
                        k=15),          #the dimension of the basis used to represent the smooth term
                 data=train_data,              #A data frame or list containing the model response variable and covariates required by the formula
                 method="ML")             #Maximum Likelihood smoothin parameter estimation method


# Ajustar el modelo GAM (con suavizadores para variables continuas)  
modelo_gam <- gam(PM25 ~ s(AOD_055) + s(ndvi) + s(BCSMASS_dia) + s(DUSMASS_dia) +
                    s(SO2SMASS_dia) + s(SO4SMASS_dia) + s(SSSMASS_dia) +s(sp_mean)+
                    s(blh_mean) + s(d2m_mean) + s(DEM, k=5) + s(t2m_mean)+ 
                    s(v10_mean) + s(u10_mean) + s(tp_mean)+ dayWeek,
                  data = train_data,method="ML")

# modelo_gam <- gam(PM25 ~ s(AOD_055, k=10) + s(ndvi, k=20) + s(BCSMASS_dia, k=10) + 
#                     s(DUSMASS_dia, k=10) + s(SO2SMASS_dia, k=10) + s(SO4SMASS_dia, k=10) + 
#                     s(SSSMASS_dia, k=12) + s(blh_mean, k=10) + s(sp_mean, k=10) +
#                     s(d2m_mean, k=10) + s(v10_mean, k=10) + s(u10_mean, k=10) + 
#                     s(tp_mean, k=12) + dayWeek,
#                   data = train_data, select = TRUE)

# Evaluar el modelo usando tu función
resultados_gam <- evaluar_modelo(modelo = modelo_gam,#model.gam1,#
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


#### Plot de cada una de las variables
# No gusta
windows(width = 14, height = 10)
# Graficar todos los términos suaves del modelo en subplots
plot(modelo_gam, 
     pages = 1,       # todos los gráficos en una sola página
     se = TRUE,       # muestra bandas de confianza
     rug = TRUE,      # muestra ticks en la base indicando la densidad de datos
     shade = TRUE,    # sombrea los intervalos de confianza
     scale = 0)       # permite comparar la escala entre gráficos (usa la misma)




##########################################
#Lista de variables en orden acumulativo
# Variables numéricas (para aplicar s())
vars_numericas <- c("AOD_055","blh_mean", "d2m_mean", "t2m_mean","sp_mean", "v10_mean", "u10_mean", "tp_mean",
                    "ndvi", "BCSMASS_dia", "DUSMASS_dia", "SO2SMASS_dia", "SO4SMASS_dia",
                    "SSSMASS_dia","DEM")

# Variables categóricas (no usar s())
vars_categoricas <- c("dayWeek")

# Lista completa en orden
var_orden <- c(vars_numericas, vars_categoricas)

resultados <- data.frame()
vars_utilizadas <- c()

for (i in 1:length(var_orden)) {
  print(i)
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
getwd()
write.csv(resultados,"lalal.csv")


######PLOT
# Predicciones sobre los datos de entrenamiento
# test_data$pred <- predict(modelo_gam, newdata = test_data)
pred <- predict(modelo_gam, newdata = test_data)
pred <- predict(model.gam1 , newdata = test_data)

df <- data.frame(pred=pred, PM25=test_data$PM25)
test_data <- df[df$pred>0,]
plot_GAM<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +     # puntos reales vs predicción
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  # línea ideal
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") +  # ajuste de regresión
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  
  theme_classic()
plot_GAM


library(ggplot2)
library(ggpointdensity)  # si no lo tenés: install.packages("ggpointdensity")

plot_GAM<- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  theme_classic()+ theme(legend.position="none")

plot_GAM


##############################################################################
##############################################################################
##############################################################################
## --- LME

library(lme4)
library(caret)  # Para crear folds

# Cargar datos
estacion <- "MX"
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
  min <- min(pred )
  max <- max(pred )
  return(data.frame(R2 = r2, RMSE = rmse, Bias = bias,min=min,max=max))
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
  Bias_sd = sd(resultados_cv$Bias),
  min = mean(resultados_cv$min),
  max = mean(resultados_cv$max)
)

print(resumen_cv)



######PLOT
library(lme4)
library(ggplot2)
library(ggpointdensity)

# Entrenar modelo final con todos los datos de entrenamiento
modelo_lme <- lmer(PM25 ~ AOD_055 + (1 | fecha), data = train_data)

# Hacer predicciones sobre test_data
test_data$pred <- predict(modelo_lme, newdata = test_data, allow.new.levels = TRUE)

# Graficar predicciones vs valores reales
plot_LME <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  scale_x_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  theme_classic() + theme(legend.position = "none")

# Mostrar el gráfico
plot_LME



plot_LME <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +     # puntos reales vs predicción
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  # línea ideal
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") +  # ajuste de regresión
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  scale_x_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  theme_classic() + theme(legend.position = "none")
plot_LME


# --- Entrenar modelo final con todo el training set ---
modelo_final <- lmer(PM25 ~ AOD_055 + (1 + AOD_055|dayWeek ), data = train_data)
modelo_final <- lmer(PM25 ~ 1 + ( + 1 | AOD_055), data = train_data)
modelo_final <- lmer(PM25 ~ AOD_055 + (1 + AOD_055 | dayWeek) + (1 | estacion), data = train_data)

modelo_final <- lmer(PM25 ~ AOD_055, data = train_data) #  Usar lm() si no necesitás efectos aleatorios
modelo_final <- lmer(PM25 ~ 1 + (1|AOD_055), data = train_data)
modelo_final <- lmer(PM25 ~ AOD_055+(1+ AOD_055|dayWeek), data = train_data)



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

estacion <- "MD"
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


################################################################################
################################################################################
################################################################################
# GLM sEGUN MA et al.,
# Cargar datos
estacion <- "MX"
modelo <- "1"
dir <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/modelos/ParticionDataSet/")
setwd(dir)

train_data <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_train_", estacion, ".csv"))
test_data  <- read.csv(paste0(dir, "Modelo_", modelo, "/M", modelo, "_test_", estacion, ".csv"))
train_data <- train_data[train_data$PM25>0,]
test_data <- test_data[test_data$PM25>0,]
familia <- "gaussian"
familia <- binomial #no!
familia <-"poisson"
familia <-"Gamma"
familia <- "inverse.gaussian"
familia <- "quasi"
familia <-"quasibinomial"
familia <- "quasipoisson"
glm_model <- glm(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + DEM+t2m_mean+
                   SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + blh_mean + sp_mean +
                   d2m_mean    +v10_mean + u10_mean + tp_mean   +dayWeek,#family = inverse.gaussian(),
                 data = train_data)

glm_model <- glm(PM25 ~ AOD_055,
                 data = train_data)
# Predecir en test_data
predicciones_test <- predict(glm_model, newdata = test_data, allow.new.levels = TRUE)

# Evaluar con tu función evaluar_modelo
resultados_test3 <- evaluar_modelo(
  modelo = glm_model3,
  datos_test = test_data,
  variable_real = "PM25",
  tipoModelo = "GLM"
)
resultados_test3


# Hacer predicciones sobre test_data
test_data$pred <- predict(glm_model, newdata = test_data, allow.new.levels = TRUE)
test_data<- test_data[test_data$pred>0,]

# Graficar predicciones vs valores reales
plot_glm <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_pointdensity(adjust = 1.5) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  scale_x_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  theme_classic() + theme(legend.position = "none")

# Mostrar el gráfico
plot_glm



plot_glm <- ggplot(test_data, aes(x = PM25, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +     # puntos reales vs predicción
  geom_abline(slope = 1, intercept = 0, color = "black", ) +  # línea ideal
  geom_smooth(method = "lm", se = FALSE, color = "red",linetype = "dashed") +  # ajuste de regresión
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  scale_x_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  theme_classic() + theme(legend.position = "none")

plot_glm


################################################################################
################################################################################
################################################################################
# Crear dataframe con valores de VIF antes y después
vif_data <- data.frame(
  Centro = c("SP", "ST", "BA", "MD", "MX"),
  VIF_Antes = c(9.78, 14.74, 11.44, 22.46, 7.83),
  VIF_Despues = c(3.25, 7.34, 3.12, 9.92, 3.70)
)

# Convertir a formato largo para ggplot
vif_long <- vif_data %>%
  pivot_longer(cols = c("VIF_Antes", "VIF_Despues"),
               names_to = "Estado",
               values_to = "VIF")

# Definir el orden deseado de los centros
vif_long$Centro <- factor(vif_long$Centro, levels = c("SP", "ST", "BA", "MD", "MX"))

# Renombrar para presentación
vif_long$Estado <- recode(vif_long$Estado,
                          "VIF_Antes" = "Antes de depuración",
                          "VIF_Despues" = "Después de depuración")

ggplot() +
  geom_bar(data = vif_long, aes(x = Centro, y = VIF, fill = Estado), stat = "identity", position = "dodge") +
  geom_hline(yintercept = 10, color = "black", linetype = "dashed") +
  labs(
    #title = "Reducción del VIF por centro urbano",
    x = "Sitio",
    y = "Valor máximo de VIF",
    fill = "Estado"
  ) +
  scale_y_continuous(limits = c(0, 25)) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("Antes de depuración" = "#225ea8",
                               "Después de depuración" = "#e7298a"))




################################################################################
################################################################################
################################################################################
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)

# Datos
desempeño_data <- data.frame(
  Centro = c("SP", "ST", "BA", "MD", "MX"),
  r2_simple = c(2.00E-05, 0.01223, 0.00012, 0.12, 0.16922),
  r2_multiples = c(0.3611, 0.56837, 0.45914, 0.52, 0.48979),
  rmse_simple = c(10.82, 16.368, 10.636, 8.20, 9.068),
  rmse_multiples = c(8.121, 10.823, 7.847, 6.06, 7.104)
)

# Definir el orden deseado de los centros
desempeño_data$Centro <- factor(desempeño_data$Centro, levels = c("SP", "ST", "BA", "MD", "MX"))

# Reorganizar datos en formato largo
# Usar funciones explícitas para evitar conflictos
r2_data <- desempeño_data %>%
  dplyr::select(Centro, r2_simple, r2_multiples) %>%
  #dplyr::select(Centro, rmse_simple, rmse_multiples) %>%
  tidyr::pivot_longer(
    cols = c(r2_simple, r2_multiples),
    #cols = c(rmse_simple, rmse_multiples),
    names_to = "Tipo",
    # values_to = "RMSE"
    values_to = "r2"
  )


# Etiquetas más legibles
r2_data$Tipo <- factor(r2_data$Tipo, 
                       levels = c("rmse_simple", "rmse_multiples"),
                       labels = c("RMSE simple", "RMSE múltiple"))
r2_data$Tipo <- factor(r2_data$Tipo, 
                       levels = c("r2_simple", "r2_multiples"),
                       labels = c("R² simple", "R² múltiple"))

# Gráfico
ggplot(r2_data, aes(x = Centro, y = r2, fill = Tipo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(#title = "Comparación de R² por ciudad",
       x = "Sitio", y = "R²",
       
       fill = "Tipo de modelo") +
  scale_y_continuous(limits = c(0, 1)) +
  theme(legend.position = "none") +
  theme_classic()+scale_fill_manual(values = c( "R² simple"= "#225ea8",
                                               "R² múltiple" = "#41b6c4"))
  # theme_classic()+scale_fill_manual(values = c( "RMSE simple"= "#cb181d",
  #                                               "RMSE múltiple" = "#fb6a4a"))

################################################################################
################################################################################
################################################################################
#Plot con todas las metricas de RLM de todas las ciudaes al mismo tiemp
df<- read.csv("D:/Josefina/Proyectos/ProyectoChile/Tot/dataset/tabla_resultados_RLM.csv")

# Filtrar SP y ST
df_sp <- df[df$sitio == "SP", ]
df_ST <- df[df$sitio == "ST", ]

# Combinar los dos sitios
df_comb <- df
df_comb$sitio <- factor(df_comb$sitio , levels = c("SP", "CH", "BA", "MD", "MX"))

# Convertir a formato largo
data_melt <- df_comb %>%
  pivot_longer(cols = c(R2, RMSE), names_to = "Metrica", values_to = "Valor")

# Establecer límites por métrica
limites <- data.frame(
  Metrica = c("R2", "RMSE"),
  ymin = c(0, 1),
  ymax = c(1, 16)
)

# Unir límites
data_plot <- left_join(data_melt, limites, by = "Metrica")

# Renombrar métricas para notación matemática
data_plot$Metrica <- recode(data_plot$Metrica,
                            "R2" = "R^2",
                            "RMSE" = "RMSE")

# Graficar
ggplot(data_plot, aes(x = Num_Variables, y = Valor, color = sitio)) +
  geom_blank(aes(y = ymin)) +
  geom_blank(aes(y = ymax)) +
  geom_point(size = 2) +
  geom_line(size = 1.2) +theme(legend.position = "none") +
  
  facet_wrap(~Metrica, scales = "free_y", labeller = label_parsed) +
  scale_x_continuous(breaks = sort(unique(data_plot$Num_Variables))) +
  scale_color_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256")) +
  
  labs(#title = "Desempeño del modelo según cantidad de variables (SP vs ST)",
       x = "Número de Variables",
       #y = "Valor de la Métrica",
       color = "Sitio") +
  # theme(
  #        axis.text.x = element_text(size = 1)  # Cambia 8 por el tamaño que prefieras
  #      )
theme(legend.position = "none") +

  theme_classic()



################################################################################
################################################################################
################################################################################
# RLS, Correccion RH, Correcciuon BLH, Ambas
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)

# Datos
desempeño_data <- data.frame(
  Centro = c("SP", "ST", "BA", "MD", "MX"),
  r2_blh = c(0.01062,0.33424,0.03947,0.03289,0.106),
  rmse_blh = c(10.76,13.44,10.417,8.57,9.412),
  r2_rh = c(0.10,0.05,0.002,0.15,0.27),
  rmse_rh = c(10.288,15.643,10.15,8.18,8.816),
  

  
  r2_ambas = c(0.15691,0.31548,0.065,0.11149,0.31876),
  rmse_ambas= c(9.94,13.239,9.796,8.377,8.329)
)

# Definir el orden deseado de los centros
desempeño_data$Centro <- factor(desempeño_data$Centro, levels = c("SP", "ST", "BA", "MD", "MX"))

# Reorganizar datos en formato largo
# Usar funciones explícitas para evitar conflictos
r2_data <- desempeño_data %>%
  #dplyr::select(Centro,  r2_blh,r2_rh,r2_ambas) %>%
  dplyr::select(Centro, rmse_blh, rmse_rh, rmse_ambas) %>%
  tidyr::pivot_longer(
    #cols = c( r2_blh,r2_rh ,r2_ambas),
    cols = c(rmse_blh, rmse_rh, rmse_ambas),
    names_to = "Tipo",
    values_to = "RMSE"
    #values_to = "r2"
  )


# Etiquetas más legibles
r2_data$Tipo <- factor(r2_data$Tipo, 
                       levels = c("r2_blh", "r2_rh", "r2_ambas"),
                       labels = c("R² BLH","R² RH",  "R² RH+BLH"))
r2_data$Tipo <- factor(r2_data$Tipo, 
                       levels = c("rmse_blh", "rmse_rh", "rmse_ambas"),
                       labels = c("RMSE BLH","RMSE RH",  "RMSE RH+BLH"))


# Gráfico
ggplot(r2_data, aes(x = Centro, y = RMSE, fill = Tipo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(#title = "Comparación de R² por ciudad",
    x = "Sitio", y = "RMSE",
    
    fill = "Tipo de modelo") +
  #scale_y_continuous(limits = c(0, 1)) +
  theme(legend.position = "none") +
  # theme_classic()+scale_fill_manual(values = c( "RMSE BLH"= "#7fcdbb"  ,#"#41b6c4" ,
  #                                               "RMSE RH" = "#02818a",#"#225ea8",
  #                                               "RM RH+BLH"= "#081d58"))
 theme_classic()+scale_fill_manual(values = c( "RMSE BLH"= "#feb24c",
                                               "RMSE RH" ="#fb6a4a", 
                                               "RMSE RH+BLH" = "#cb181d"))
