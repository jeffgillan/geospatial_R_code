library(raster)
library(lidR) 
library(rgdal) 
library(sf) 
library(rgeos) 
library(sp)
library(data.table)
library(doParallel)

#Create list of all of the las files files to be processed within a folder
lidar2020 = list.files("F:\\Black_mypassport_1.8TB\\Smith_vanLeeuwen_work\\Bighorn_fire\\Bighorn_fire_data\\2020_lidar_data\\CHMs",pattern="*.las$", full.names=TRUE )

#Set up loop to process each las file to create canopy height model raster
for(fileName in lidar2020) {
  
  
  lidar_points = readLAS(fileName)
  
  #Change coordinate system to NAD83 UTM Zone 12N
  las2 = spTransform(lidar_points, sp::CRS(SRS_string = "EPSG:26912"))
  
  #Identify Noise Points
  identify_noise=classify_noise(las2, sor(15, 7))
  
  #Filter out points classified as noise
  las_denoise <- filter_poi(identify_noise, Classification != LASNOISE)
  
  #Separate the classified point cloud into a ground point cloud and canopy point cloud
  ground_points = filter_poi(las_denoise, Classification == 2)
  
  #Create a grided digital terrain model from ground points
  DTM = grid_terrain(ground_points, res = 0.914, algorithm = knnidw(k = 10, p = 2))
 
  #Create digital terrain model from all of the denoise points
  DSM = grid_canopy(las_denoise, res = 0.914, algorithm = p2r(subcircle = 0, na.fill = knnidw()))
  
  #DEMstack = stack(DSM, DTM). I stopped using this command because my DSM and DTMs sometimes have slight extent differences. Stack will not work with extent differences.
  
  #Subtract DTM from DSM on cell-by-cell basis
  CHM = DSM-DTM
  
  #Convert from feet to meter units
  CHMmeters = CHM/3.281
  
  #filter out heights that are not logical 
  CHMmeters[CHMmeters < 0] <- 0
  CHMmeters[CHMmeters > 100] <- 0
  
  writeRaster(CHMmeters,
              filename=paste('F:\\Black_mypassport_1.8TB\\Smith_vanLeeuwen_work\\Bighorn_fire\\Bighorn_fire_data\\2020_lidar_data\\CHMs\\',substr(fileName, 101, 127), "_2020_CHM"),
              format="GTiff",
              overwrite=TRUE)
}
 

