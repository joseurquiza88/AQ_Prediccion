estacion <- "SP"
data<- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/06_estaciones/",estacion,"_estaciones.csv",sep=""))
data$date <- as.POSIXct(as.character(data$date), format = "%d/%m/%Y")
data <- data[complete.cases(data$mean),]
data <- data[data$mean !=0,]

data <- data[(year(data$date)) >= 2015,]


names(data)

######################################################
#             Estadisticas basicas generales
######################################################
summary(data$mean)

######################################################
#             Estadisticas basicas por estacion
######################################################
resumen_por_estacion <- data %>%
  group_by(estacion) %>%
  summarise(
    cantidad = n(),
    promedio = mean(mean, na.rm = TRUE),
    minimo = min(mean, na.rm = TRUE),
    maximo = max(mean, na.rm = TRUE),
    sd = sd(mean, na.rm = TRUE),
    .groups = "drop"
  )
View(resumen_por_estacion)
# Estacion con los valores promedios mas bajos
estacion_min <- resumen_por_estacion[resumen_por_estacion$promedio == min(resumen_por_estacion$promedio),]
estacion_min
# Estacion con los valores promedios mas altos
estacion_max <- resumen_por_estacion[resumen_por_estacion$promedio == max(resumen_por_estacion$promedio),]
estacion_max

# Estacion con los valores promedios mas altos
estacion_picos_max <- resumen_por_estacion[resumen_por_estacion$maximo == max(resumen_por_estacion$maximo),]
estacion_picos_max

######################################################
#     Estadisticas basicas por estacion por mes
######################################################
data_2024 <- data[year(data$date) == 2024,]
unique(year(data_2024$date))
resumen_por_mes <- data %>%
  #resumen_por_mes <- data_2024 %>%
  mutate(mes = month(date, label = TRUE, abbr = FALSE, locale = "es_ES")) %>%
  group_by(mes) %>%
  summarise(
    minimo = round(min(mean, na.rm = TRUE),2),
    maximo = round(max(mean, na.rm = TRUE),2),
    promedio = round(mean(mean, na.rm = TRUE),2),
    sd = round(sd(mean, na.rm = TRUE),2),
    .groups = "drop"
  ) %>%
  arrange(match(mes, month.name))  # ordena los meses correctamente

View(resumen_por_mes)

# Estacion con los valores PICOS mas altos
# Obtener los 3 valores máximos únicos (orden descendente)
top3_vals_max <- sort(unique(resumen_por_mes$maximo), decreasing = TRUE)[1:3]
# Filtrar filas que tienen esos valores
estacion_picos_max <- resumen_por_mes[resumen_por_mes$maximo %in% top3_vals_max, ]


# Estacion con los valores  promedios mas bajos
# Obtener los 3 valores máximos únicos (orden descendente)
top3_vals_promedioMin <- sort(unique(resumen_por_mes$promedio), decreasing = FALSE)[1:3]
# Filtrar filas que tienen esos valores
estacion_picos_min <- resumen_por_mes[resumen_por_mes$promedio %in% top3_vals_promedioMin, ]
estacion_picos_min

top3_vals_promedioMax <- sort(unique(resumen_por_mes$promedio), decreasing = TRUE)[1:3]
# Filtrar filas que tienen esos valores
estacion_picos_maxProm <- resumen_por_mes[resumen_por_mes$promedio %in% top3_vals_promedioMax, ]
estacion_picos_maxProm

######################################################
#       Serie temporal diaria por estacion para todo 
######################################################

media_por_estacion <- data %>%
  group_by(estacion) %>%
  summarise(media_estacion = mean(mean, na.rm = TRUE))
data$date <- as.Date(data$date)
# Unir la media por estación al dataframe original
data_plot <- left_join(data, media_por_estacion, by = "estacion")

stats_por_estacion <- data_plot %>%
  group_by(estacion) %>%
  summarise(
    media = round(mean(mean, na.rm = TRUE), 2),
    max = round(max(mean, na.rm = TRUE), 2),
    sd = round(sd(mean, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0("Mean: ", media, "\nStd dev: ", sd, "\nMax: ", max),
    x = as.Date("2015-01-01"),  # izquierda del gráfico
    # y = 350  #CH                  # altura deseada del texto
    y = 150
  )

# Crear un data frame para la línea horizontal (AMS mean)
lineas_extra <- data.frame(
  estacion = unique(data_plot$estacion),
  total_mean = 16.42899 #SP
)

# Graficar
serie_temporal <- ggplot() +
  geom_line(data = data_plot, aes(x = date, y = mean, color = "Media estacion")) +
  # Línea horizontal de AMS mean en cada faceta
  geom_hline(data = lineas_extra, aes(yintercept = total_mean, color = "Media SP"), size = 0.9) +
  geom_label(data = stats_por_estacion,
             aes(x = x, y = y, label = label),
             hjust = 0, vjust = 1,
             fill = "white", alpha = 0.8, size = 4) +
  # Facetas por estación
  facet_wrap(~ estacion, scales = "fixed") +
  # Ejes
  scale_x_date(limits = as.Date(c("2015-01-01", "2024-12-31"))) +
  # scale_y_continuous(limits = c(0, 350)) +
  scale_y_continuous(limits = c(0, 150)) +
  # Definir colores y etiquetas de leyenda
  scale_color_manual(
    name = NULL,
    values = c("Media estacion" = "#2ca25f", "Media SP" = "red")
  ) +
  #labs(y = expression(PM[2.5]), x= "Date") +
  labs(x = NULL, y = NULL)+
  theme_classic() +
  theme(
    legend.position = "none",  # Eliminar la leyenda
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotar las etiquetas del eje x a 45 grados y tamaño de texto más pequeño
    axis.text.y = element_text(size =13),  # Reducir el tamaño de las etiquetas del eje y
    strip.text = element_text(size = 12)  # Cambiar el tamaño de los títulos de las facetas (subplots)
  )
serie_temporal
dir <- "D:/Josefina/Proyectos/ProyectoChile/CH/plots/Paper Mapas santiago/"
ggsave(
  filename = paste(dir,"03_Serie-Temporal.png",sep=""),
  plot = serie_temporal,       # Usa el último gráfico generado
  width = 10,               # Ancho en pulgadas
  height = 6,               # Alto en pulgadas
  dpi = 500                 # Resolución en puntos por pulgada (alta calidad)
) 
######################################################
#             Promedios por año
######################################################

promedio_anuales <- data %>%
  group_by(year) %>%
  summarise(avg_pm25 = mean(mean, na.rm = TRUE))
View(promedio_anuales)
######################################################
#            % de cambio 2015-2024 total
######################################################
### Las concentraciones diminuyeron/aumentaron entre 2015-2024?
prom_2015 <- promedio_anuales[promedio_anuales$year==2015,]
prom_2024 <-promedio_anuales[promedio_anuales$year==2024,]
porcentajeCambio <- round(((prom_2015$avg_pm25 - prom_2024$avg_pm25)/prom_2015$avg_pm25 )*100,2)
#Si es negativo significa que aumentaron, si es positivo disminuyeron
porcentajeCambio

######################################################
#            % de cambio 2015-2024 por estaciones
######################################################
# Supongamos que tu dataframe se llama df y tiene columnas: year, estacion, mean

# 1) Calcular promedio anual por estación
promedio_anuales <- data %>%
  group_by(estacion, year) %>%
  summarise(avg_pm25 = mean(mean, na.rm = TRUE)) %>%
  ungroup()

# 2) Filtrar para años 2015 y 2024
datos_2015 <- promedio_anuales %>% filter(year == 2015)
datos_2024 <- promedio_anuales %>% filter(year == 2024)

# 3) Unir por estación y calcular porcentaje de cambio
cambios_por_estacion <- datos_2015 %>%
  dplyr::select(estacion, avg_pm25_2015 = avg_pm25) %>%
  dplyr::left_join(
    datos_2024 %>% dplyr::select(estacion, avg_pm25_2024 = avg_pm25),
    by = "estacion"
  ) %>%
  dplyr::mutate(
    porcentaje_cambio = round(((avg_pm25_2024 - avg_pm25_2015) / avg_pm25_2015) * 100, 2)
  )

# Si es positivo significa que aumento, si el valor es negativo es porque disminuyeron los
# valores entre 2015 y 2024
View(cambios_por_estacion)




######################################################
#       Serie temporal por año por estacion
######################################################
# Asegurar que la columna fecha esté en formato Date
data$date <- as.Date(data$date)

# Crear columna de año
data$year <- year(data$date)


# Promedio anual por estación
promedios_estacion <- data %>%
  group_by(estacion, year) %>%
  summarise(avg_pm25 = mean(mean, na.rm = TRUE), .groups = "drop")

# Promedio anual general
promedio_general <- data %>%
  group_by(year) %>%
  summarise(avg_pm25 = mean(mean, na.rm = TRUE)) %>%
  mutate(estacion = "Media SP")



# Unir ambos conjuntos
serie_completa <- bind_rows(promedios_estacion, promedio_general)

# Graficar
serie_temporal_anual <- ggplot() +
  # Línea del promedio general (AMS Mean) en negro, más gruesa y punteada

            aes(x = year, y = avg_pm25, color = estacion, group = estacion),
            size = 0.7) +

  geom_line(data = filter(serie_completa, estacion == "AMS Mean"),
            aes(x = year, y = avg_pm25, group = estacion),
            color = "black", size = 1, linetype = "solid") +
  labs(
    x = NULL,
    y = NULL,
    color = "Station"
  ) +
  scale_x_continuous(breaks = 2015:2024) +
  theme_classic() +
  theme(
    #legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 13),
    strip.text = element_text(size = 12)
  )


serie_temporal_anual
dir <- "D:/Josefina/Proyectos/ProyectoChile/CH/plots/Paper Mapas santiago/"
ggsave(
  filename = paste(dir,"S1_serie_temporal_anual.png",sep=""),
  plot = serie_temporal_anual,       # Usa el último gráfico generado
  width = 8,               # Ancho en pulgadas
  height = 4,               # Alto en pulgadas
  dpi = 500                 # Resolución en puntos por pulgada (alta calidad)
)  

####
# Crear columna con nombre del mes en inglés y completo
datos_boxplot <- data %>%
  mutate(
    mes = month(date, label = TRUE, abbr = FALSE, locale = "C"),
    mes = factor(mes, levels = month.name)  # ordenar de enero a diciembre
  )

 # Boxplot con todos los valores diarios por mes
ggplot(datos_boxplot, aes(x = mes, y = mean)) +
  #geom_boxplot(fill = "lightblue", color = "black") +
  geom_boxplot(
    fill = "lightblue",
    color = "black",
    outlier.shape = 21,         # círculo con borde
    outlier.size = 1.5,         # más pequeños
    outlier.stroke = 0.3,       # grosor del borde
    outlier.fill = NA,          # sin relleno
    outlier.colour = "black"    # color del borde
  )+
  labs(
    #title = "Distribución diaria de PM2.5 por mes (2015-2024)",
    x = "Month",
    y = "Daily PM2.5"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#################################################################################
#################################################################################
#                           Modelos Predictivos de PM2.5
#################################################################################
#################################################################################
# biblioteca para vif
library(car)
#  --        01. Preparación de los datos: selección de variables
estacion <- "SP"
data_com <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/merge_tot/",estacion,"_merge_comp.csv",sep=""))
names(data_com)
#Poner en formato la fecha
data_com$date <- as.POSIXct(as.character(data_com$date), format = "%Y-%m-%d")
# Agregamos numero de dia
data_com$dayWeek <- wday(data_com$date, week_start = 1)
unique(year(data_com$date))
# Nos quedamos solo con los datos 2015-2023
data_com<- data_com[year(data_com$date) != 2024,]
# Verificamos
unique(year(data_com$date))

#Generamos modelo lineal multiple con todas las variables  (17 Vars)
modelo <- lm(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + OCSMASS_dia+
               SO2SMASS_dia + SO4SMASS_dia +SSSMASS_dia + blh_mean + sp_mean +
               d2m_mean + t2m_mean + v10_mean + u10_mean + tp_mean + DEM + dayWeek,
             data = data_com)


vif(modelo)
# Esto te devuelve una tabla con el VIF para cada variable. Como regla general:

#   VIF ??? 1: no hay colinealidad
# 
# VIF entre 5 y 10: hay cierta colinealidad, ojo
# 
# VIF > 10: colinealidad severa ??? deberías eliminar o transformar alguna variable
sort(vif(modelo), decreasing = TRUE)
a<- data.frame(vif(modelo))


car::vif(modelo)
summary(modelo)


######################################################
#########################################################

