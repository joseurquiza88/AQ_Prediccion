##############################################################################
##otra forma de extraer los datos
estacion <- "MD"
modelo <- "01-ET-CV-M1-260525-MD"
modelo <- "01-ET-CV-M1-270525-sAOD-MD"
modelo <- "01-ET-CV-M1-260525-Combinado-MD"
#year<-2015

#dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/Salidas/SalidasDiarias/",modelo,"/",year,"/",sep="")
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/Salidas/SalidasDiarias/",modelo,"/",sep="")

setwd(dir)
id <- list.files(path = dir,
                 pattern = "*.tif",
                 full.names = FALSE)

data_estacciones <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/estaciones/sitios_",estacion,".csv",sep=""))
data_estacciones <- data_estacciones[data_estacciones$Considerado=="SI",]
#data_estacciones <- data_estacciones[data_estacciones$tipo=="referencia",]

puntos <- data_estacciones


crs_project <- "+proj=longlat +datum=WGS84"
df_rbind <- data.frame()

i<-1
for (i in 1:length(id)){
  print(i)
  pred_raster <- raster(id[i])
  
  #plot(pred_raster)
  # Extraer los valores del raster en las coordenadas especificadas
  valores_raster <- extract(pred_raster, puntos[, c("long", "lat")])
  
  # Unir los valores del raster al dataframe original
  puntos_con_valores <- puntos %>%
    mutate(valor_raster = valores_raster)
  
  fechaInteres <- as.Date(substr(id[i],4,13), format = "%Y-%m-%d")# Mostrar el dataframe resultante

  puntos_con_valores$date <- fechaInteres
  
  df_rbind <- rbind(df_rbind,puntos_con_valores)
}

data_sensores <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/06_estaciones/",estacion,"_estaciones.csv",sep=""))
data_sensores <- data_sensores[complete.cases(data_sensores$date),]
data_sensores$date <- as.Date(as.POSIXct(data_sensores$date, format = "%d/%m/%Y"))#"%Y-%m-%d")#
df_rbind$date <- as.Date(as.POSIXct(df_rbind$date, format = "%Y-%m-%d"))#
# vemos las variabñes
names(data_sensores)
names(df_rbind)
unique(df_rbind$ID)
unique(data_sensores$ID)
# merge
merged_df <- merge(df_rbind,data_sensores, by = c("ID", "date"), all.x = TRUE)
merged_df_subt <- merged_df[complete.cases(merged_df$mean),]
merged_df_subt <- merged_df_subt[complete.cases(merged_df_subt$valor_raster),]

merged_df_subt2 <- merged_df_subt[year(merged_df_subt$date) !=2024,]


model <- lm( mean~valor_raster , data = merged_df_subt2)
# Calculo de métricas de desempeño
R2 <- summary(model)$r.squared
RMSE <- sqrt(mean(residuals(model)^2))
Bias <- mean(merged_df_subt2$mean - merged_df_subt2$valor_raster)
n <- nrow(merged_df_subt2)
df_metrica <- data.frame(R2,RMSE,Bias,n)
df_metrica
#write.csv(merged_df_subt,paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_01-ET-CV-M1-260525-MD.csv",sep=""))
write.csv(merged_df_subt,paste("D:/Josefina/Proyectos/Tesis/",estacion,"/resultados/merge_Prediccion_Real/",estacion,"_merge_",modelo,".csv",sep=""))

merged_df_subt2$date <- as.Date(merged_df_subt2$date)
df_diario <- merged_df_subt2 %>%
  group_by(date) %>%
  summarise(
    mean_prediccion = mean(valor_raster, na.rm = TRUE),
    mean_medicion= mean(mean, na.rm = TRUE)
  )


model_v2 <- lm( mean_medicion~mean_prediccion , data = df_diario)
# Calculo de métricas de desempeño
R2_v2 <- summary(model_v2)$r.squared
RMSE_v2 <- sqrt(mean(residuals(model_v2)^2))
Bias_v2 <- mean(df_diario$mean_medicion - df_diario$mean_prediccion)
n_v2 <- nrow(df_diario)
df_metrica_v2 <- data.frame(R2_v2,RMSE_v2,Bias_v2,n_v2)
df_metrica_v2
####PLOT
ggplot(df_diario, aes(x = date)) +
  #ggplot(df_date_rbind, aes(x = date)) +
  # Línea para Registros.validados
  # Línea para valor_Raster
  geom_line(aes(y = mean_medicion, color = "Monitoreo"), size = 0.3,na.rm = TRUE) +
  
  geom_line(aes(y = mean_prediccion, color = "Modelo"), size = 0.3, na.rm = TRUE)+#, linetype = "dashed") +
  #geom_line(aes(y = valor_raster.y, color = "MERRA-2"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  
  #geom_line(aes(y = Registros.preliminares, color = "Registros.no.validados"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  
  # Separar en subplots por estación
  #facet_wrap(~ ID , scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0, 120),breaks = seq(0, 120, by = 40)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(title = modelo,
       x = "",
       y = "PM2.5",
       color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(values = c("Monitoreo" = "#2ca25f", "Modelo" = "#feb24c"),#,"Monitoreo"="blue"),
                     labels = c("Monitoreo" = "Monitoreo", "Modelo" = "Modelo"))+#, "mean"="Monitoreo")) +
  
  # Personalización del tema
  
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  theme_classic() +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),  # Tamaño y alineación del título
    axis.title.x = element_text(size = 8),              # Tamaño del título del eje X
    axis.title.y = element_text(size = 8),              # Tamaño del título del eje Y
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1), # Tamaño y rotación de los ticks del eje X
    axis.text.y = element_text(size = 6),               # Tamaño de los ticks del eje Y
    strip.text = element_text(size = 5),                # Tamaño del texto de los subplots
    legend.title = element_text(size = 8),              # Tamaño del título de la leyenda
    legend.text = element_text(size = 5)                # Tamaño del texto de la leyenda
  )

