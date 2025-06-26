

############################################################
#              Plots estadisticas MAIAC-AERONET
#              segun ventanas espacio-temporales
############################################################


metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V03/metricas_V03.csv")
metricas <- metricas[metricas$collection == "C61",]
metricas <- metricas[metricas$region == "latam",]


############################################################
##                     R2
############################################################
metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
unique(metricas$metrica)
metrica_interes <- "r2"   

metricas_subset <- metricas[metricas$metrica == metrica_interes,]
metricas_subset$buffer <- factor(metricas_subset$espacial, levels = c(1, 3, 5, 15, 25))
metricas_subset$buffer <- factor(metricas_subset$temporal, levels = c(30, 60, 90, 120))

metricas_subset$ciudad <- factor(metricas_subset$estacion)
metricas_subset$ciudad <- factor(metricas_subset$ciudad, 
                                 levels = c("SP", "ST", "BA", "MD", "LP", "MX"))
COLOR VIOLETA #756bb1
# Gráfico de barras agrupadas
r2_agrupadas<-ggplot(metricas_subset, aes(x = ciudad, y = valor, fill = buffer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(
    x = "Estacion",
    y = expression(R^2),
    fill = "Ventana espacial (km)"
  ) +
  scale_fill_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256")) +
  
  scale_y_continuous(limits=c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  theme_classic() +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),  # Agrandar los números de los ticks
    legend.title = element_text(family = "Roboto", size = 12, face = 2),
    legend.position = "none"  # Elimina la leyenda
  ) 
  #theme( 
    #axis.text.x = element_text(angle = 0, hjust = 1),
    #legend.position = "top"
  #)
r2_agrupadas



ggsave("D:/Josefina/Proyectos/Tesis/plot/01-MAIAC_Performance/MAIAC-C61-AER-Latam-R2.png",r2_agrupadas,
       width = 10,
       height = 8,
       units = "cm",
       dpi = 500)
############################################################
##                     RMSE
############################################################

metrica_interes <-"rmse" 

metricas_subset <- metricas[metricas$metrica == metrica_interes,]
metricas_subset$buffer <- factor(metricas_subset$espacial, levels = c(1, 3, 5, 15, 25))
metricas_subset$buffer <- factor(metricas_subset$temporal, levels = c(30,60,90,120))

metricas_subset$ciudad <- factor(metricas_subset$estacion)
metricas_subset$ciudad <- factor(metricas_subset$ciudad, 
                                 levels = c("SP", "ST", "BA", "MD", "LP", "MX"))

# Gráfico de barras agrupadas
rmse_agrupadas<- ggplot(metricas_subset, aes(x = ciudad, y = valor, fill = buffer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(
    x = "Estacion",
    y = "RMSE",
    fill = "Ventana espacial (km)"
  ) +
  scale_fill_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256")) +
  
  scale_y_continuous(limits=c(0, 0.12),breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12))+#,0.16,0.18,0.2,
  #       0.22,0.24,0.26,0.28))+
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),  # Agrandar los números de los ticks
    legend.title = element_text(family = "Roboto", size = 12, face = 2),
    legend.position = "none"  # Elimina la leyenda
  ) 
#theme( 
#axis.text.x = element_text(angle = 0, hjust = 1),
#legend.position = "top"
#)
rmse_agrupadas
ggsave("D:/Josefina/Proyectos/Tesis/plot/01-MAIAC_Performance/MAIAC-C61-AER-Latam-RMSE.png",rmse_agrupadas,
       width = 10,
       height = 8,
       units = "cm",
       dpi = 500)




############################################################
##                     BIAS
############################################################


metrica_interes <-"bias" 

metricas_subset <- metricas[metricas$metrica == metrica_interes,]
metricas_subset$buffer <- factor(metricas_subset$espacial, levels = c(1, 3, 5, 15, 25))
metricas_subset$buffer <- factor(metricas_subset$temporal, levels = c(30,60,90,120))

metricas_subset$ciudad <- factor(metricas_subset$estacion)
metricas_subset$ciudad <- factor(metricas_subset$ciudad, 
                                 levels = c("SP", "ST", "BA", "MD", "LP", "MX"))

# Gráfico de barras agrupadas
bias_agrupado<- ggplot(metricas_subset, aes(x = ciudad, y = valor, fill = buffer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(
    x = "Estacion",
    y = "Bias",
    fill = "Ventana espacial (km)"
  ) +
  scale_fill_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256")) +
  
  # Personalización de los ejes
  scale_y_continuous(limits=c(-0.06, 0.06))+#,breaks = c(-0.7,-0.06,-0.04,-0.02,0,0.02,0.04,0.06,0.08,0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28))+
  #       0.22,0.24,0.26,0.28))+
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),  # Agrandar los números de los ticks
    legend.title = element_text(family = "Roboto", size = 12, face = 2),
    legend.position = "none"  # Elimina la leyenda
  ) 
#theme( 
#axis.text.x = element_text(angle = 0, hjust = 1),
#legend.position = "top"
#)
bias_agrupado
ggsave("D:/Josefina/Proyectos/Tesis/plot/01-MAIAC_Performance/MAIAC-C61-AER-Latam-Bias.png",bias_agrupado,
       width = 10,
       height = 8,
       units = "cm",
       dpi = 500)



############################################################
##                     REU
############################################################


metrica_interes <-"reuMeanAOD" 

metricas_subset <- metricas[metricas$metrica == metrica_interes,]
metricas_subset$buffer <- factor(metricas_subset$espacial, levels = c(1, 3, 5, 15, 25))
metricas_subset$buffer <- factor(metricas_subset$temporal, levels = c(30,60,90,120))

metricas_subset$ciudad <- factor(metricas_subset$estacion)
metricas_subset$ciudad <- factor(metricas_subset$ciudad, 
                                 levels = c("SP", "ST", "BA", "MD", "LP", "MX"))



# Gráfico de barras agrupadas
reu_agrupado<- ggplot(metricas_subset, aes(x = ciudad, y = valor, fill = buffer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(
    x = "Estacion",
    y = "REU",
    fill = "Ventana espacial (km)"
  ) +
  scale_fill_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256")) +
  
  # Personalización de los ejes
  scale_y_continuous(limits=c(0, 300),breaks = c(0,40,80,120,160,200,240,280))+
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),  # Agrandar los números de los ticks
    legend.title = element_text(family = "Roboto", size = 12, face = 2),
    legend.position = "top"  # Elimina la leyenda
  ) 

reu_agrupado
ggsave("D:/Josefina/Proyectos/Tesis/plot/01-MAIAC_Performance/MAIAC-C61-AER-Latam-REU2.png",reu_agrupado,
       width = 14,
       height = 8,
       units = "cm",
       dpi = 500)

