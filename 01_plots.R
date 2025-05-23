setwd("D:/Josefina/Proyectos/Tesis/data/merge_tot/")


# Supongamos que AOD es de menor magnitud que PM25, entonces escalamos AOD para graficarlo
# El factor de escala se elige para que las líneas no se sobrepongan visualmente
color_maiac = ["#005a32","#fd8d3c","#99000d","#023858","#3f007d"] # CHILE "#fd8d3c"
color_maiac_61 = ["#74c476","#fed976","#fb6a4a", "#74a9cf","#df65b0","#807dba"]
color_maiac_60 = ["#005a32","#fd8d3c","#99000d","#023858","#ce1256","#3f007d"] # CHILE "#fd8d3c"
edge_color_maiac_60 = ["#004529", "#feb24c","#67000d","#081d58","#67001f","#4d004b"]
edge_color_maiac_61 = ["#41ab5d","#feb24c","#cb181d","#045a8d","#ce1256","#810f7c"]

############ ,
######################################
df_sp <- read.csv("01_SP_merge_comp.csv")
scale_factor_sp <- max(df_sp$PM25, na.rm = TRUE) / max(df_sp$AOD, na.rm = TRUE)

df_sp$date <- as.Date(df_sp$date, format = "%d/%m/%Y")
# Definí los nuevos límites para PM25 (eje izquierdo) basados en el límite deseado para AOD (eje derecho)

sp_plot<-ggplot(df_sp, aes(x = date)) +
  geom_line(aes(y = PM25, color = "PM2.5")) +
  geom_line(aes(y = AOD_055 * scale_factor_sp, color = "AOD")) +
  scale_y_continuous(
    name =  expression(PM[2.5]),
    limits = c(0, 160),
    sec.axis = sec_axis(~ . / scale_factor_sp, name = "AOD")  # Se deshace el escalado para mobarar valores reales
  ) +
  scale_color_manual(values = c("PM2.5" = "#005a32", "AOD" = "#74c476")) +
  labs(x = "Fecha", color = "Variable") +
  theme_classic() + theme(
    axis.title.x = element_text(size = 14),
    axis.title.y.left = element_text(color = "#3f007d", size = 14),
    axis.title.y.right = element_text(color = "#807dba", size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )+
  theme(
    axis.title.y.left = element_text(color = "#005a32"),
    axis.title.y.right = element_text(color = "#74c476")
  )
sp_plot


############
######################################
df_st <- read.csv("02_st_merge_comp.csv")
scale_factor_st <- max(df_st$PM25, na.rm = TRUE) / max(df_st$AOD, na.rm = TRUE)

df_st$date <- as.Date(df_st$date, format = "%d/%m/%Y")

st_plot<-ggplot(df_st, aes(x = date)) +
  geom_line(aes(y = PM25, color = "PM2.5")) +
  geom_line(aes(y = AOD_055 * scale_factor_st, color = "AOD")) +
  scale_y_continuous(
    name =  expression(PM[2.5]),
    limits = c(0, 160),
    sec.axis = sec_axis(~ . / scale_factor_st, name = "AOD")  # Se deshace el escalado para mobarar valores reales
  ) +
  scale_color_manual(values = c("PM2.5" = "#d95f0e", "AOD" = "#fec44f" )) +
  labs(x = "Fecha", color = "Variable") +
  theme_classic() +
 theme(
    axis.title.x = element_text(size = 14),
    axis.title.y.left = element_text(color = "#3f007d", size = 14),
    axis.title.y.right = element_text(color = "#807dba", size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )  +theme(
    axis.title.y.left = element_text(color =  "#d95f0e"),
    axis.title.y.right = element_text(color = "#fec44f")
  )

st_plot



############
######################################
df_ba <- read.csv("03_ba_merge_comp.csv")
scale_factor_ba <- max(df_ba$PM25, na.rm = TRUE) / max(df_ba$AOD, na.rm = TRUE)

df_ba$date <- as.Date(df_ba$date, format = "%d/%m/%Y")
ba_plot<-ggplot(df_ba, aes(x = date)) +
  geom_line(aes(y = PM25, color = "PM2.5")) +
  geom_line(aes(y = AOD_055 * scale_factor_ba, color = "AOD")) +
  scale_y_continuous(
    name =  expression(PM[2.5]),
    limits = c(0, 160),
    sec.axis = sec_axis(~ . / scale_factor_ba, name = "AOD")  # Se deshace el escalado para mobarar valores reales
  ) +
  scale_color_manual(values = c("PM2.5" = "#67000d", "AOD" = "#cb181d"  )) +
  labs(x = "Fecha", color = "Variable") +
  theme_classic() +
 theme(
    axis.title.x = element_text(size = 14),
    axis.title.y.left = element_text(color = "#3f007d", size = 14),
    axis.title.y.right = element_text(color = "#807dba", size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )  +theme(
    axis.title.y.left = element_text(color =  "#67000d"),
    axis.title.y.right = element_text(color = "#cb181d" )
  )

ba_plot


############
######################################
df_md <- read.csv("04_md_merge_comp.csv")
scale_factor_md <- max(df_md$PM25, na.rm = TRUE) / max(df_md$AOD, na.rm = TRUE)

df_md$date <- as.Date(df_md$date, format = "%d/%m/%Y")
md_plot<-ggplot(df_md, aes(x = date)) +
  geom_line(aes(y = PM25, color = "PM2.5")) +
  geom_line(aes(y = AOD_055 * scale_factor_md, color = "AOD")) +
  scale_y_continuous(
    name =  expression(PM[2.5]),
    limits = c(0, 160),
    sec.axis = sec_axis(~ . / scale_factor_md, name = "AOD")  # Se deshace el escalado para momdrar valores reales
  ) +
  scale_color_manual(values = c("PM2.5" = "#023858", "AOD" = "#74a9cf" )) +
  labs(x = "Fecha", color = "Variable") +
  theme_classic() +
  theme(

  )+theme(
    axis.title.x = element_text(size = 14),
    axis.title.y.left = element_text(color = "#3f007d", size = 14),
    axis.title.y.right = element_text(color = "#807dba", size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )+theme(
  axis.title.y.left = element_text(color =  "#023858"),
axis.title.y.right = element_text(color = "#74a9cf"  ))


md_plot




############
######################################
df_mx <- read.csv("05_mx_merge_comp.csv")
scale_factor_mx <- max(df_mx$PM25, na.rm = TRUE) / max(df_mx$AOD, na.rm = TRUE)

df_mx$date <- as.Date(df_mx$date, format = "%d/%m/%Y")
mx_plot<-ggplot(df_mx, aes(x = date)) +
  geom_line(aes(y = PM25, color = "PM2.5")) +
  geom_line(aes(y = AOD_055 * scale_factor_mx, color = "AOD")) +
  scale_y_continuous(
    name =  expression(PM[2.5]),
    limits = c(0, 160),
    sec.axis = sec_axis(~ . / scale_factor_mx, name = "AOD")
  ) +
  scale_color_manual(values = c("PM2.5" = "#3f007d", "AOD" = "#807dba")) +
  labs(x = "Fecha", color = "Variable") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y.left = element_text(color = "#3f007d", size = 14),
    axis.title.y.right = element_text(color = "#807dba", size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )




dir <- "D:/Josefina/Proyectos/Tesis/plot/"
ggsave(
  filename = paste(dir,"01_Serie-Temporal_AOD-PM25_mx.png",sep=""),
  plot = mx_plot,       # Usa el último gráfico generado
  width = 6,               # Ancho en pulgadas
  height = 4,               # Alto en pulgadas
  dpi = 300                 # Resolución en puntos por pulgada (alta calidad)
)  

###############################################################
############################################################
#pREDICCION DE pm2.5 a partir del AOD
estacion <-"MD"
modelo <- "1"
num_estacion <- "04"

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
# Entrenar el modelo de regresión lineal múltiple
lm_model <- lm(PM25 ~   AOD_055, data = train_data)

# predicciones <- predict(lm_model, newdata = test_data)
predictions <- predict(lm_model, newdata = test_data)
predictions_train <- predict(lm_model, newdata = train_data)
predicciones_hora<- predictions

df <- data.frame (predicciones=predictions,observaciones=test_data$PM25)
dir <- "D:/Josefina/Proyectos/Tesis/data/prediccion_RLS/"
nombre <- paste(dir,num_estacion,"-",estacion,"-prediccion_RLS.csv",sep="")
write.csv(df,nombre)




