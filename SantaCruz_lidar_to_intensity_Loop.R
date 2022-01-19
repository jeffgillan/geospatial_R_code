library(raster)
library(lidR) 
library(rgdal) 
library(sf) 
library(rgeos) 
library(sp)
library(data.table)
library(doParallel)
library(gstat)

#Create list of all of the las files files to be processed within a folder
cooperlidar = list.files("Z:\\Pima_County_Santa_Cruz_River_2021_2022\\Feb_2021_Cooper_Aerial\\Feb_2021_Lidar_LAS\\LAS_denoise",pattern="*.las$", full.names=TRUE )


#Set up loop to process each las file to create lidar intensity raster
for(fileName in cooperlidar) {
  
  
  lidar_points = readLAS(fileName)
  
  #This will create a raster where each cell will get assigned the mean lidar intensity value. Cells with no lidar points will be NoData.
  intensity <- grid_metrics(lidar_points, ~mean(Intensity), 1)
  
  #This command will interpolate the NoData cells with mean intensity of 3x3 window. It will only apply to NoData points. 
   intensity <- focal(intensity, w = matrix(1, 3, 3), fun = function(intensity){mean(intensity, na.rm = TRUE)}, pad = TRUE, NAonly = TRUE)

  
  writeRaster(intensity,
              filename=paste('Z:\\Pima_County_Santa_Cruz_River_2021_2022\\Feb_2021_Cooper_Aerial\\intensity\\',substr(fileName, 97, 126), "_intensity"),
              format="GTiff",
              overwrite=TRUE)
}