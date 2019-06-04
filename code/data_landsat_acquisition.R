####### Data acquisition for the Azraq mapping project ######

# Project details  
### Time period: 1985-2018 
### Targeted time stamps: 2 records/ year (Jan, Aug)
### Sensor: Landsat-5 and 8
### Study-area: Azraq protected wetland with 25 km buffer

# Script content
### 1. Data selection 
### 2. Data download
### 3. Data cropping to AOI

library(devtools)
# devtools::install_github("https://github.com/hfisser/getSpatialData/tree/dev",force=TRUE)
library(getSpatialData)
library(rgdal)
library(raster)
library(sf)
library(sp)
library(RStoolbox)

################################# 0. Setup (needs to be adjusted by the user) ########################

# create local environment for directories 
dirs <- new.env()
dirs$main_dir <- "/Users/Ronjamac/Documents/Studium_Geographie/Master_EAGLE/APP2/azraq" # path to main directory 
dirs$hard_dir <- "/Volumes/Ronjas_1TB/Data/azraq/landsat"                               # path to external harddrive 
dirs$aoi_dir <- file.path(dirs$main_dir,"Shapefile")                                    # path to AOI-shapefile folder  
dirs$out_dir <- "/Volumes/Ronjas_1TB/Data/azraq/crop"                                   # path where cropped output is saved

# USGS User-login
username <-  "ronjalappe"
password <-  "Walddorf146"

# define time periods for data search 
time_params <- list("start_year" = 1985,
                    "end_year"= 2018)
yearly_months <- list(c("-01-01","-01-31"),c("-08-01","-08-31"))
overall_period <- c(time_params$start_year:time_params$end_year)

# define sensors
sensors <- c("LANDSAT_TM_C1", "LANDSAT_8_C1")

# define WRSPath (solution to solve the problem of receiving records which are only partly within AOI)
wrs_path <- 173

#################################### 1. Data selection #################################### 

# read AOI-shapefile
aoi <- readOGR(dsn = dirs$aoi_dir,layer = "Azraq_25km_Buffer") 

# create a list with all required time periods 
search_periods <- do.call(cbind,lapply(overall_period,function(year) {
  yearly <- lapply(yearly_months,function(yearly) {
    monthly_periods <- sapply(yearly,function(monthly) {
      dates <- paste0(year,monthly)
    })
  })
}))

# query Landsat data from USGS for the selected time periods
records_selected <- lapply(search_periods,function(time_range) { 
  cat("\nGetting records for year:",substr(time_range[1],1,4),"\n") 
  records <- getSpatialData::getLandsat_query(time_range = time_range, 
                                              aoi = aoi,
                                              username = username,
                                              password = password)                  # query data for time range
  records_cc <- getSpatialData::calcLandsat_aoi_cloudcov(records,aoi)               # calculate cloudcover within AOI
  records_filtered <- records_cc[records_cc$product %in% sensors,]                  # filter query by sensir
  records_path <- records_filtered[which(records_filtered$WRSPath == wrs_path),]    # filter query by WRSPath
  records_cc_min <- records_path[which.min(records_path$AOIcloudcoverpercentage),]  # select only 1 record per time stamp with min cloud cover
}) 

# save list of records 
write.csv(records_selected_df,"azraq_selected_records.csv")

# look at previews
getLandsat_preview(records_selected[[1]],on_map=F, show_aoi = F)
getLandsat_preview(records_selected[[2]],on_map=F, show_aoi = F)

#################################### 2. Data download #################################### 

# download selected data to external harddrive 
records_selected_df <- do.call("rbind",records_selected)
datasets <- getLandsat_data(records_selected_df,
                            dir_out = dirs$hard_dir,
                            source = "ESPA",
                            username = username,
                            password = password)


#################################### 3. Cropping data #################################### 

# create directory list for all scenes
dirs_full <- list.dirs(dirs$hard_dir)[-1]

# unzip files and save into the same subdirectory
for (i in 1:length(dirs_full)){
  cat("\nUnzip:",basename(dirs_full[i]),"\n")         # message on processing status
  file <- dir(dirs_full[i])                           # get the name for each file
  file_path <- paste(dirs_full[i],file,sep = "/")     # create path to each file folder
  exdir <- dirname(file_path)                         # define output 
  untar(file_path,compressed = "gzip",exdir = exdir)  # unzip files to their subdirectory
#}

# crop rasterbricks to the AOI polygon
# for (i in 1:length(dirs_full)){
  cat("\nCrop:",basename(dirs_full[i]),"\n")
  bands_list <- list.files(dirs_full[i], pattern = "band.*\\.tif",full.names = T) # list all bands of format .tif
  bands <- lapply(bands_list,function(x) raster(x))                               # read tif-files  
  stack <- raster::stack(bands)                                                   # create raster stack of bands
  aoi_proj <- spTransform(aoi,crs(stack))                                         # project aoi to the raster stack
  mask <- mask(crop(stack,aoi_proj),aoi_proj)                                     # mask/ crop raster raster stacks to the shape of the AOI polygon
  filename <- paste(basename(dirs_full[i]),"cropped",sep = "_")                   # create file name for output raster stack
  # new_dir <- file.path(out_dir,filename)
  # dir.create(new_dir)
  writeRaster(mask,file.path(dirs$out_dir,filename),                                   # save cropped raster stacks in output folder
              byLayer = T, 
              format="GTiff",
              overwrite=T,
              options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
}
