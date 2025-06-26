
# MERGE MAIAC-MODIS AERONET
######     -------  EXAMPLE for one station     -------  ######
rm(list = setdiff(ls(), "time_correlation"))

buffer_time <- 120 #minutes
buffer_spatial <- "3KM"
city <- "SP"
num_estacion <- 1
for(i in 1:1){ 

 
  # BA
  MODIS <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V03/processed/merge_AER-MODIS/Latam/dia/",buffer_spatial,"/",buffer_time,"mins/",num_estacion,"_",city,"-",buffer_spatial,"-MODIS-",buffer_time,"-AER_MEAN.csv",sep=""))
  MAIAC <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V03/processed/merge_AER-MAIAC/Latam_C61/dia/",buffer_spatial,"/",buffer_time,"mins/",num_estacion,"_",city,"-",buffer_spatial,"-MAIAC-",buffer_time,"-AER_MEAN_c61.csv",sep=""))
  
  MODIS$date <-  as.Date(as.POSIXct(strptime(MODIS$date, format = "%Y-%m-%d %H:%M:%S", "GMT")))
  MAIAC$date <-   as.Date(as.POSIXct(strptime(MAIAC$date, format = "%Y-%m-%d", "GMT")))
  
  #Merge
  merge_sat <- merge(x = MAIAC, y = MODIS, by = "date") # Equivalente
  
  merge_sat <- data.frame(date = merge_sat$date,
                          AOD_modis = merge_sat$AOD_550_modis,
                          
                          AOD_maiac_61 = merge_sat$AOD_550_maiac_mean,
                          AOD_550_AER_mean = merge_sat$AOD_550_AER_mean.y)
  
  print(nrow(merge_sat))
  #merge_sat$igual <- merge_sat$AOD_550_AER_mean_modis==merge_sat$AOD_550_AER_mean_maiac
  write.csv(merge_sat,paste("D:/Josefina/paper_git/paper_maiac/datasets/V03/processed/merge_AER-MAIAC-MODIS/Latam/dia/",buffer_spatial,"/",buffer_time,"mins/",num_estacion,"_",city,"-",buffer_spatial,"-MAIAC-MODIS-",buffer_time,"-AER_MEAN_c61.csv",sep=""))
}
