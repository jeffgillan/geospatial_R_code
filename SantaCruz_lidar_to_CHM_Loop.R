library(raster)
library(lidR) 
library(rgdal) 
library(sf) 
library(rgeos) 
library(sp)
library(data.table)
library(doParallel)


#Create list of all of the las files files to be processed within a folder
cooperlidar = list.files("Z:\\Pima_County_Santa_Cruz_River_2021_2022\\Feb_2021_Cooper_Aerial\\Feb_2021_Lidar_LAS\\LAS_denoise",pattern="*.las$", full.names=TRUE )


#Set up loop to process each las file to create canopy height model raster
for(fileName in cooperlidar) {
  
  
  lidar_points = readLAS(fileName)
  
  
  #Identify Noise Points
  #identify_noise=classify_noise(lidar_points, sor(15, 7))
  
  #Filter out points classified as noise
  #las_denoise <- filter_poi(identify_noise, Classification != LASNOISE)
  
  #Separate the classified point cloud into a ground point cloud and canopy point cloud
  ground_points = filter_poi(lidar_points, Classification == 2)
  
  #Create a grided digital terrain model from ground points
  DTM = grid_terrain(ground_points, res = 1, algorithm = knnidw(k = 10, p = 2))
 
  #Create digital terrain model from all of the denoise points
  #DSM = grid_canopy(lidar_points, res = 1, algorithm = p2r(subcircle = 0, na.fill = knnidw()))
  
  #DEMstack = stack(DSM, DTM). I stopped using this command because my DSM and DTMs sometimes have slight extent differences. Stack will not work with extent differences.
  
  #Subtract DTM from DSM on cell-by-cell basis
  #CHM = DSM-DTM
  
  #Convert from feet to meter units
  #CHMmeters = CHM/3.281
  
  #filter out heights that are not logical 
  #CHM[CHM < 0] <- 0
  #CHMmeters[CHMmeters > 100] <- 0
  
  writeRaster(DTM,
              filename=paste('Z:\\Pima_County_Santa_Cruz_River_2021_2022\\Feb_2021_Cooper_Aerial\\Feb_2021_Lidar_LAS\\LAS_denoise\\',substr(fileName, 97, 126), "_DTM"),
              format="GTiff",
              overwrite=TRUE)
}
 


