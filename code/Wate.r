## Script to derive the NDWI & NDSI per season


install.packages("raster")
library(raster)
install.packages("sp")
library(sp)
install.packages("rgdal")
library(rgdal)

setwd("K:/04-Phil/GEO_L/04-Geo-SoSe-04GeoAPP2/data/crop")
#setwd("C:/Users/s372221/Desktop/cropped")
outputdir <- "K:/04-Phil/GEO_L/04-Geo-SoSe-04GeoAPP2/WorkinProgress/Water_group/Indices_results/"

#import Landsat aquisitions
f <- list.files(getwd(),pattern = "*.tif$")

aqu_dates <- c()
for (i in 1:length(f)) {
  date <- substr(f[i], 18, 23)
  aqu_dates <- append(aqu_dates, date)
}
aqu_dates <- sort(aqu_dates)

# import and create full, sorted stack
r <- list()
for (i in 1:length(aqu_dates)) {
  img_path <- grep(aqu_dates[i], f, value = TRUE)
  r <- append(r, raster::stack(img_path))
}
######DEFINING FUNCTIONS#########
#################################

#function to get the month of the aquisition
getDate <- function(file) {
  date <- as.character(c(195004:202007))
  for (i in 1:length(date)){
    if (grepl(date[i], file)) {
      return(date[i])
    }
  }
}

#function to differentiate between different landsat sensors
image_path <- January_Stack[[i]]@layers[[1]]@file@name
get_sensor_type <- function(image_path) {
  if (grepl("LC08", image_path)){
    return(c(3,5,6,7,4)) #green,nir,swir1,swir2,red band positions
  } else if (grepl("LT05", image_path) | (grepl("LT04", image_path))) {
    return(c(2,4,5,6,3)) #green,nir,swir1,swir2,red band positions
  } else {
    message("Sensor not found.")
  }
}

#NDWI function 
NDWI <- function(RasterStack,bandnr){
  return((RasterStack[[bandnr[1]]] - RasterStack[[bandnr[2]]])/(RasterStack[[bandnr[1]]] + RasterStack[[bandnr[2]]]))
}
# NDSI function 
NDSI <- function(RasterStack,bandnr){
  return((RasterStack[[bandnr[3]]] - RasterStack[[bandnr[4]]])/(RasterStack[[bandnr[3]]] + RasterStack[[bandnr[4]]]))
}
# NDVI
NDVI <- function(RasterStack,bandnr){
  return((RasterStack[[bandnr[2]]] - RasterStack[[bandnr[5]]])/(RasterStack[[bandnr[2]]] + RasterStack[[bandnr[5]]]))
}

#####DATA_PROCESSING#####
#########################

#group landsat aquisitions by month
January_Stack <- list()
August_Stack <- list()


for (i in 1:length(f)) {
  date <- getDate(f[i])
  if (substr(date, 5, 6)=="01") {
    January_Stack <- append(January_Stack,r[[i]])
  } else if (substr(date, 5, 6)=="08") {
    August_Stack <- append(August_Stack,r[[i]])
  }
}

# sorting
#sort.list()


# Indices for january
NDWI_January <- raster::stack()
NDSI_January <- raster::stack()
NDVI_January <- raster::stack()
for (i in 1:length(January_Stack)) {
  bandnr <- get_sensor_type(January_Stack[[i]]@layers[[1]]@file@name)
  # NDWI
  NDWI_aquisition <- NDWI(January_Stack[[i]],bandnr)
  NDWI_aquisition@title <- paste("NDWI for:", January_Stack[[i]]@layers[[1]]@file@name)
  NDWI_January <- raster::stack(NDWI_January,NDWI_aquisition)
  # NDSI
  NDSI_aquisition <- NDSI(January_Stack[[i]],bandnr)
  NDSI_aquisition@title <- paste("NDSI for:", January_Stack[[i]]@layers[[1]]@file@name)
  NDSI_January <- raster::stack(NDSI_January,NDSI_aquisition)
  # NDVI
  NDVI_aquisition <- NDVI(January_Stack[[i]],bandnr)
  NDVI_aquisition@title <- paste("NDVI for:", January_Stack[[i]]@layers[[1]]@file@name)
  NDVI_January <- raster::stack(NDVI_January,NDVI_aquisition)
}

# Indices for August
NDWI_August <- raster::stack()
NDSI_August <- raster::stack()
NDVI_August <- raster::stack()
for (i in 1:length(August_Stack)) {
  bandnr <- get_sensor_type(August_Stack[[i]]@layers[[1]]@file@name)
  # NDWI
  NDWI_aquisition <-   NDWI_aquisition <- NDWI(August_Stack[[i]],bandnr)
  NDWI_aquisition@title <- paste("NDWI for:", August_Stack[[i]]@layers[[1]]@file@name)
  NDWI_August <- raster::stack(NDWI_August,NDWI_aquisition)
  # NDSI
  NDSI_aquisition <- NDSI(August_Stack[[i]],bandnr)
  NDSI_aquisition@title <- paste("NDSI for:", August_Stack[[i]]@layers[[1]]@file@name)
  NDSI_August <- raster::stack(NDSI_August,NDSI_aquisition)
  # NDVI
  NDVI_aquisition <- NDVI(August_Stack[[i]],bandnr)
  NDVI_aquisition@title <- paste("NDVI for:", August_Stack[[i]]@layers[[1]]@file@name)
  NDVI_August <- raster::stack(NDVI_August,NDVI_aquisition)
}

rm(NDWI_aquisition)
rm(NDSI_aquisition)


############SAVE INDICES###########
###################################

#writeRaster(NDWI_January,paste0(outputdir,"NDWI_January"),"GTiff",overwrite=TRUE)
#writeRaster(NDWI_August,paste0(outputdir,"NDWI_August"),"GTiff",overwrite=TRUE)
#writeRaster(NDSI_January,paste0(outputdir,"NDSI_January"),"GTiff",overwrite=TRUE)
#writeRaster(NDSI_August,paste0(outputdir,"NDSI_August"),"GTiff",overwrite=TRUE)


############ Apply Threshold  ###################
NDWI_Jan_Thresh <- NDWI_January
NDWI_Aug_Thresh <- NDWI_August

NDWI_threshold <- 0.05
for (i in 1:length(NDWI_Jan_Thresh@layers)) {
  NDWI_Jan_Thresh@layers[[i]]@data@values <- NDWI_Jan_Thresh@layers[[i]]@data@values>NDWI_threshold
}

for (i in 1:length(NDWI_Aug_Thresh@layers)) {
  NDWI_Aug_Thresh@layers[[i]]@data@values <- NDWI_Aug_Thresh@layers[[i]]@data@values>NDWI_threshold
}

#writeRaster(NDWI_Jan_Thresh,paste0(outputdir,"NDWI_Jan_Thresh005"),"GTiff",overwrite=TRUE)
#writeRaster(NDWI_Aug_Thresh,paste0(outputdir,"NDWI_Aug_Thresh005"),"GTiff",overwrite=TRUE)

########### SUM UP WATER COVERAGE ###############
All_Stack <- stack(NDWI_Jan_Thresh, NDWI_Aug_Thresh)

water_cov <- sum(All_Stack)
water_cov_Jan <- sum(NDWI_Jan_Thresh)
water_cov_Aug <- sum(NDWI_Aug_Thresh)

plot(water_cov)
plot(water_cov_Jan)
plot(water_cov_Aug)

writeRaster(water_cov,paste0(outputdir,"Water_Coverage_All"),"GTiff",overwrite=TRUE)
writeRaster(water_cov_Jan,paste0(outputdir,"Water_Coverage_Jan"),"GTiff",overwrite=TRUE)
writeRaster(water_cov_Aug,paste0(outputdir,"Water_Coverage_Aug"),"GTiff",overwrite=TRUE)

############ CALC WATER AREA ##############
calc_water_area <- function(raster, res_x, res_y){
  #define default value for res_x and res_y
  if (missing(res_x) & missing(res_y)){
    res_x <- 0.03
    res_y <- 0.03
  }
  vals <- getValues(x)
  water_cells <- sum(vals ==1, na.rm = T)
  water_area <-  sum(vals ==1, na.rm = T) * res_x * res_y
  return(water_area)
}



#########

#-find threshold
#-permantent water bodies? sum of binary rasters with certain number??
#-convert to shp

# Create wetland extent layer (wetland = pixels that were covered min. once with water)??