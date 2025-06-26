
# Este codigo es el mismo que 22_codigo_MES
###########################################################################
###########################################################################
#Usar version de r 2.3.4
###########################################################################
# -----------------------   Todas las variables ---------------------------
###########################################################################
#rm(list=ls())
df_rbind <- data.frame()
estacion <- "MD"
dir <- paste("D:/Josefina/Proyectos/tesis/",estacion,"/modelos/",sep="")
setwd(dir)
# para seleccionar el modelo
list.files(pattern = "ET")
modelo_ET_cv[["coefnames"]]

for (l in 1:1){
  rm(list = setdiff(ls(), "df_rbind"))
  estacion <- "MD"
  year<- 2024
  numRaster <- "05"
  modelo <- "01-ET-CV-M1-270525-sAOD-MD.RData"
  nombre_salida <- "01-ET-CV-M1-270525-sAOD-MD"
  setwd(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/dataset_ejemplo/Prediccion_",year,"/tiff/",sep=""))
  dir_salida <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/salidas/SalidasDiarias/",nombre_salida,"/",sep="")
  # Fechas de inter?1
  fechaInicio <- as.Date("01-05-2024", format = "%d-%m-%Y")
  fechaFin <- as.Date("31-05-2024", format = "%d-%m-%Y")
  ###
  fechaNDVI<- as.Date("01-05-2024", format = "%d-%m-%Y")
  lista_fecha <- data.frame(date=seq.Date(fechaInicio, fechaFin, by = "day"))
  dir_modelos <- paste("D:/Josefina/Proyectos/Tesis/",estacion,"/modelos/",sep="")
  load(paste(dir_modelos,modelo,sep=""))
}

# lista_fecha$date[j]+2
# j<-1
for (j in 1:nrow(lista_fecha)) {
  
  print(j)
  fechaInteres <- as.Date(lista_fecha$date[j], format = "%d-%m-%Y")
  # fechaInteres <- as.Date("08-03-2024", format = "%d-%m-%Y")
  ################# -----     00 MAIAC     -----
  # 2024001-MAIAC_raster.tif
  # Convertir a d?a juliano respecto al 1 de enero del mismo a?o
  dayJulian <- as.numeric(fechaInteres - as.Date(paste0(format(fechaInteres, "%Y"), "-01-01"))) + 1
  yearInteres <- year(fechaInteres)
  if(nchar(dayJulian)==1){
    sep = "00"
  }
  if (nchar(dayJulian)==2) {
    sep = "0"
  }
  
  if (nchar(dayJulian)==3) {
    sep = ""
  }
  #maiacDate <- paste(yearInteres,sep,dayJulian,sep = "")
  #MAIAC_raster <- raster(paste("00_MAIAC/00_MAIAC_IDW/IDW-",maiacDate,"-MAIAC_raster_",numRaster,".tif",sep=""))
  #MAIAC_raster <- raster(paste("00_MAIAC/00_MAIAC_IDW/IDW-",maiacDate,"-MAIAC_raster.tif",sep=""))
  #MAIAC_raster <- raster(paste("00_MAIAC/",maiacDate,"-MAIAC_raster.tif",sep=""))
  #num_na_MAIAC <- sum(is.na(MAIAC_raster[]))
  #print(c("MAIAC",num_na_MAIAC))
  
  ################### -----     NDVI     -----
  # Como es modis tambien la fecha es juliana pero ojo es un dato mensual
  # fechaNDVI<- as.Date("01-01-2024", format = "%d-%m-%Y")
  
  #fechaNDVI<- as.Date("01-04-2024", format = "%d-%m-%Y")
  
  yearNdvi <- year(fechaNDVI)
  dayJulianNDVI <- as.numeric(fechaNDVI - as.Date(paste0(format(fechaNDVI, "%Y"), "-01-01"))) + 1
  
  if(nchar(dayJulianNDVI)==1){
    sep = "00"
  }
  if (nchar(dayJulianNDVI)==2) {
    sep = "0"
  }
  
  if (nchar(dayJulianNDVI)==3) {
    sep = ""
  }
  NDVIDate <- paste(yearInteres,sep,dayJulianNDVI,sep = "")
  
  NDVI_raster <- raster(paste("01_NDVI/",NDVIDate,"-NDVI_raster.tif",sep=""))
  #plot(NDVI_raster)
  #num_na_NDVI <- sum(is.na(NDVI_raster[]))
  #print(c("NDVI",num_na_NDVI))
  ################# -----     LandCover     -----
  #Este dato es anual por lo que tambien tenemos que setear la fecha dierente
  # Pero es MODIS = Dia juliano
  # MCD12Q1.A2024001.h12v12.061.2022169161028.hdf
  
  
  # yearLandCover <- year(fechaNDVI)
  # LandCoverDate <- paste(yearLandCover,"001",sep = "")
  #LandCover_raster <- raster(paste("02_LandCover/2015001-LandCover_raster.tif",sep=""))
  # LandCover_NA <- sum(is.na(LandCover_raster[]))
  # print(c("landCover",LandCover_NA))
  #plot(LandCover_raster)
  
  ################# -----     DEM     -----
  
  DEM_raster <- raster("03_DEM/DEM_raster.tif")
  #plot(DEM_raster)
  #DEM_raster_NA <- sum(is.na(DEM_raster[]))
  #print(c("DEM",DEM_raster_NA))
  ################# -----     MERRA-2     -----
  fechaInteres_MERRA <- gsub("-", "", fechaInteres)
  # MERRA-2 AOD
  #MAIAC_AOD_MERRA <- raster(paste("07_MERRA-2_Dia_AOD/",fechaInteres_MERRA,"-AOD-MERRA_raster.tif",sep=""))
  
  ## BCSMASS
  #BCSMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-BCSMASS_raster.tif",sep=""))
  BCSMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-BCSMASS_raster_",numRaster,".tif",sep=""))
  #BCSMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-BCSMASS_raster.tif",sep=""))
  
  #BCSMASS_raster_NA <- sum(is.na(BCSMASS_raster[]))
  #print(c("BCSMASS",BCSMASS_raster_NA))
  #plot(BCSMASS_raster)

  ## DUSMASS
  #DUSMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-DUSMASS_raster.tif",sep=""))
  DUSMASS_raster <- raster(paste("04_MERRA-2_DIA/",fechaInteres_MERRA,"-DUSMASS_raster_",numRaster,".tif",sep=""))
  
  #DUSMASS_raster <- raster(paste("04_MERRA-2_DIA/",fechaInteres_MERRA,"-DUSMASS_raster.tif",sep=""))
  #DUSMASS_raster_NA <- sum(is.na(DUSMASS_raster[]))
  #print(c("DUSMASS",DUSMASS_raster_NA))
  #plot(DUSMASS_raster)
  
  ## DUSMASS25
  #DUSMASS25_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-DUSMASS25_raster.tif",sep=""))
  #DUSMASS25_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-DUSMASS25_raster_",numRaster,".tif",sep=""))
  #DUSMASS25_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-DUSMASS25_raster.tif",sep=""))
  
  #DUSMASS25_raster_NA <- sum(is.na(DUSMASS25_raster[]))
  #print(c("DUSMASS25",DUSMASS25_raster_NA))
  #plot(DUSMASS25_raster)
  
  ## OCSMASS
  #OCSMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-OCSMASS_raster.tif",sep=""))
  #OCSMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-OCSMASS_raster_",numRaster,".tif",sep=""))
  #OCSMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-OCSMASS_raster.tif",sep=""))
  
  #OCSMASS_raster_NA <- sum(is.na(OCSMASS_raster[]))
  #print(c("OCSMASS",OCSMASS_raster_NA))
  #plot(OCSMASS_raster)
  
  ## SO2SMASS
  # SO2SMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-SO2SMASS_raster.tif",sep=""))
  SO2SMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-SO2SMASS_raster_",numRaster,".tif",sep=""))
  #SO2SMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-SO2SMASS_raster.tif",sep=""))
  
  #SO2SMASS_raster_NA <- sum(is.na(SO2SMASS_raster[]))
  #print(c("SO2SMASS",SO2SMASS_raster_NA))
  #plot(SO2SMASS_raster)
  
  ## SO4SMASS
  #SO4SMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-SO4SMASS_raster.tif",sep=""))
  SO4SMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-SO4SMASS_raster_",numRaster,".tif",sep=""))
  #SO4SMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-SO4SMASS_raster.tif",sep=""))
  
  #SO4SMASS_raster_NA <- sum(is.na(SO4SMASS_raster[]))
  #print(c("SO4SMASS",SO4SMASS_raster_NA))
  #plot(SO4SMASS_raster)
  
  ## SSSMASS
  #SSSMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-SSSMASS_raster.tif",sep=""))
  SSSMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-SSSMASS_raster_",numRaster,".tif",sep=""))
  #SSSMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-SSSMASS_raster.tif",sep=""))
  #SSSMASS_raster_NA <- sum(is.na(SSSMASS_raster[]))
  #print(c("SSSMASS",SSSMASS_raster_NA))
  #plot(SSSMASS_raster)
  
  ## SSSMASS25
  #SSSMASS25_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-SSSMASS25_raster.tif",sep=""))
  #SSSMASS25_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-SSSMASS25_raster_",numRaster,".tif",sep=""))
  #SSSMASS25_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-SSSMASS25_raster.tif",sep=""))
  
  #SSSMASS25_raster_NA <- sum(is.na(SSSMASS25_raster[]))
  #print(c("SSSMASS25",SSSMASS25_raster_NA))
  #plot(SSSMASS25_raster)
  
  ################# -----     ERA5     -----
  ## BLH     -----
  BLH_raster <- raster(paste("05_ERA5/",fechaInteres,"-BLH_raster_",numRaster,".tif",sep=""))
  #BLH_raster <- raster(paste("05_ERA5/",fechaInteres,"-BLH_raster.tif",sep=""))
  
  
  #plot(BLH_raster)
  #BLH_raster_raster_NA <- sum(is.na(BLH_raster[]))
  #print(c("BLH",BLH_raster_raster_NA))
  ## D2M
  D2M_raster <- raster(paste("05_ERA5/",fechaInteres,"-D2M_raster_",numRaster,".tif",sep=""))
  #D2M_raster <- raster(paste("05_ERA5/",fechaInteres,"-D2M_raster.tif",sep=""))
  #plot(D2M_raster)
  #D2M_raster_NA <- sum(is.na(D2M_raster[]))
  #print(c("D2M",D2M_raster_NA))
  ## T2M
  T2M_raster <- raster(paste("05_ERA5/",fechaInteres,"-T2M_raster_",numRaster,".tif",sep=""))
  
  #T2M_raster <- raster(paste("05_ERA5/",fechaInteres,"-T2M_raster.tif",sep=""))
  #plot(T2M_raster)
  #T2M_raster_NA <- sum(is.na(T2M_raster[]))
  #print(c("T2M",T2M_raster_NA))
  
  ## TP
  TP_raster <- raster(paste("05_ERA5/",fechaInteres,"-TP_raster_",numRaster,".tif",sep=""))
  #TP_raster <- raster(paste("05_ERA5/",fechaInteres,"-TP_raster.tif",sep=""))
  
  #plot(TP_raster)
  #TP_raster_NA <- sum(is.na(TP_raster[]))
  #print(c("TP",TP_raster_NA))
  ## SP
  #SP_raster <- raster(paste("05_ERA5/",fechaInteres,"-SP_raster.tif",sep=""))
  SP_raster <- raster(paste("05_ERA5/",fechaInteres,"-SP_raster_",numRaster,".tif",sep=""))
  #plot(SP_raster)
  #SP_raster_NA <- sum(is.na(SP_raster[]))
  #print(c("SP",SP_raster_NA))
  ## V10
  #V10_raster <- raster(paste("05_ERA5/",fechaInteres,"-V10_raster.tif",sep=""))
  
  V10_raster <- raster(paste("05_ERA5/",fechaInteres,"-V10_raster_",numRaster,".tif",sep=""))
  #plot(V10_raster)
  #V10_raster_NA <- sum(is.na(V10_raster[]))
  #print(c("V10",V10_raster_NA))
  ## U10
  U10_raster <- raster(paste("05_ERA5/",fechaInteres,"-U10_raster_",numRaster,".tif",sep=""))
  #U10_raster <- raster(paste("05_ERA5/",fechaInteres,"-U10_raster.tif",sep=""))
  
  #plot(U10_raster)
  #U10_raster_NA <- sum(is.na(U10_raster[]))
  #print(c("U10",U10_raster_NA))
  ################# -----     DayWeek     -----
  ################# -----     ERA5     -----
  ## BLH     -----
  dayWeek_raster <- raster(paste("06_weekDay/",fechaInteres,"-weekDay_raster.tif",sep=""))
  #plot(dayWeek_raster)
  #dayWeek_raster_NA <- sum(is.na(dayWeek_raster[]))
  #print(c("dayWeek",dayWeek_raster_NA))
  ##### STACK
  
  r_stack <- stack(NDVI_raster, #MAIAC_raster
                   BCSMASS_raster ,DUSMASS_raster,
                   SO2SMASS_raster, SO4SMASS_raster,
                   SSSMASS_raster ,BLH_raster,
                   SP_raster, D2M_raster,
                   T2M_raster,V10_raster,
                   U10_raster,TP_raster,
                   DEM_raster,dayWeek_raster)  
  # r_stack <- stack(MAIAC_raster,NDVI_raster,#LandCover_raster,
  #                  BCSMASS_raster ,DUSMASS_raster,DUSMASS25_raster,
  #                  OCSMASS_raster,SO2SMASS_raster, SO4SMASS_raster,
  #                  SSSMASS_raster ,SSSMASS25_raster,BLH_raster,
  #                  SP_raster, D2M_raster, T2M_raster,V10_raster,
  #                  U10_raster,TP_raster,DEM_raster,dayWeek_raster)
  # 
  # r_stack <- stack(MAIAC_AOD_MERRA,NDVI_raster,#,#LandCover_raster
  #                  BCSMASS_raster ,DUSMASS_raster,DUSMASS25_raster,
  #                  OCSMASS_raster,SO2SMASS_raster, SO4SMASS_raster,
  #                  SSSMASS_raster ,SSSMASS25_raster,BLH_raster,
  #                  SP_raster, D2M_raster, T2M_raster,V10_raster,
  #                  U10_raster,TP_raster,DEM_raster,dayWeek_raster)
  # 
  #plot(r_stack)
  
  r_stack_df <- as.data.frame(r_stack, na.rm = TRUE)
  #names(r_stack_df)
  
  # names(r_stack_df) <- c( "AOD_055" ,"ndvi","LandCover","BCSMASS",
  #                         "DUSMASS","DUSMASS25",
  #                         "OCSMASS","SO2SMASS",
  #                         "SO4SMASS","SSSMASS",
  #                         "SSSMASS25","blh_mean" ,
  #                         "sp_mean","d2m_mean",
  #                         "t2m_mean","v10_mean",
  #                         "u10_mean" ,"tp_mean" , 
  #                         "DEM","dayWeek")
  
  names(r_stack_df) <- c( "ndvi", #"AOD_055" ,
                          "BCSMASS_dia","DUSMASS_dia",
                          "SO2SMASS_dia","SO4SMASS_dia",
                          "SSSMASS_dia","blh_mean" ,
                          "sp_mean","d2m_mean",
                          "t2m_mean","v10_mean",
                          "u10_mean" ,"tp_mean" , 
                          "DEM","dayWeek")
  ###############################################################
  ##################################################################
  ##############################################################
  ####
  
  # Aplicar el modelo
  predictions <- predict(modelo_ET_cv, newdata = r_stack_df)
  
  # Para XGB
  # X_test <- r_stack_df[ , c("AOD_055", "ndvi" ,
  #                           "BCSMASS_dia", "DUSMASS_dia", #"DUSMASS25_dia",
  #                           "SO2SMASS_dia", "OCSMASS_dia",
  #                           "SO4SMASS_dia", #"SSSMASS_dia", #"SSSMASS25_dia",
  #                           "blh_mean",  "d2m_mean", #"sp_mean",
  #                           "t2m_mean",  "u10_mean", #"v10_mean",
  #                           "tp_mean", "DEM","dayWeek")]
  # # 
  # # 
  # dtest <- as.matrix(X_test)
  # # dtest <- xgb.DMatrix(data = as.matrix(X_test))
  #predictions <- predict(xgb_tuned, newdata = dtest)
  # predictions <- predict(xgb_model, newdata = dtest)
  # predictions <- predict(xgb_cv_model, newdata = dtest)
  # dtest <- as.matrix(X_test)
  # dtrain <- as.matrix(X)
  
  
  # Crear un raster vac?o con la misma extensi?n y resoluci?n que el stack
  pred_raster <- raster(r_stack)
  
  # Asignar las predicciones al raster
  pred_raster[] <- NA  # Inicia con valores NA
  
  # Reinsertar las predicciones en las celdas correspondientes
  pred_raster[!is.na(values(r_stack[[1]]))] <- predictions
  #getwd()
  
 
  name_salida <- paste(dir_salida,"PM-",fechaInteres,"_",nombre_salida,".tif",sep="")
  writeRaster(pred_raster, filename = name_salida, format = "GTiff", overwrite = TRUE)

}

