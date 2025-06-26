
# Definir el directorio donde están tus archivos raster
year<-2020
#year <- c(2016,2017)#,2018,2019,2020,2021,2022,2023)
i<-1
#modelo <- "01-ET-CV-M1-270525-sAOD-MD"
#modelo <- "01-ET-CV-M1-260525-MD"
modelo <- "01-ET-CV-M1-260525-Combinado-MD"
estacion <- "MD"
for (i in 1:length(year)){
  #dir_salida <- paste("D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasDiarias/Salida_03-XGB_cv_M1-041024/",year[i],"/",sep="")
  
  #dir_salida <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion"/modelos/salidas/SalidasDiarias/",modelo,"/",year[i],"/",sep="")
  dir_salida <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/salidas/SalidasDiarias/",modelo,"/",year,"/",sep="")
  
  setwd(dir_salida)
  
  # Lista de archivos raster (ajusta la extensión según sea necesario)
  lista_raster <- list.files(pattern = "*.tif") # Cambia la extensión si es necesario
  lista_raster_recorte <- lista_raster
  #
  len <- length(lista_raster_recorte)
  print(c(year[i],len))
  # Cargar los rasters en un RasterStack
  raster_stack <- stack(lista_raster_recorte)
  
  # Calcular el promedio anual
  promedio_anual <- calc(raster_stack, fun = mean, na.rm = TRUE)
  #sd_anual <- calc(raster_stack, fun = sd, na.rm = TRUE)
  # Calcular el coeficiente de variacion
  #coef_Var <- (sd_anual / promedio_anual) * 100
  #plot(promedio_anual)
  modelo2 <- substr(lista_raster[1],15,35)
  # Guardar el resultado en un nuevo archivo raster
  # dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasAnuales/Salida_03-XGB_cv_M1-041024/"
  dir_salida <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/Salidas/SalidasAnuales/",modelo,"/",sep="")
  writeRaster(promedio_anual, filename = paste(dir_salida,"Promedio_anual_",year[i],"-",modelo,".tif",sep=""), format = "GTiff", overwrite = TRUE)
}


