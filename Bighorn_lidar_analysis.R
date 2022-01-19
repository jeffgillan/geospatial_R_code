#The following R code was written by Jeffrey Gillan, University of Arizona, 2020. 
  
setwd("F:\\Black_mypassport_1.8TB\\Smith_vanLeeuwen_work\\Bighorn_fire\\Bighorn_fire_data\\2019_lidar_data")


library(raster)
library(lidR) 
library(rgdal) 
library(sf) 
library(rgeos) 
library(sp)

#Bring point cloud into Rstudio
points = readLAS("USGS_LPC_AZ_BrawleyRillito_FEMA_2018_12S_WA_1477.laz")

#Identify Noise Points
identify_noise=classify_noise(points, sor(15, 7))

#plot(identify_noise, color = "Classification")

#Filter out points classified as noise
las_denoise <- filter_poi(identify_noise, Classification != LASNOISE)

#export .LAS point cloud with noise removed
writeLAS(las_denoise, "F:\\Black_mypassport_1.8TB\\Smith_vanLeeuwen_work\\Bighorn_fire\\Bighorn_fire_data\\2019_lidar_data\\MtLemmon_2019_lidar_3382_denoise.las")


#Points have already been identified as ground or non-ground by the USGS

#Identify ground points using a cloth simulation filter algorithm
#points_classified = lasground(points, algorithm = csf(sloop_smooth = TRUE, class_threshold = 0.4, cloth_resolution =  0.75, rigidness = 2))

#Separate the classified point cloud into a ground point cloud and canopy point cloud

ground_points = filter_poi(las_denoise, Classification == 2)
canopy_points = filter_poi(las_denoise, Classification == 1)

#Create a grided digital terrain model
DTM = grid_terrain(ground_points, res = 0.914, algorithm = knnidw(k = 10, p = 2))

DSM = grid_canopy(las_denoise, res = 0.914, algorithm = p2r(subcircle = 0, na.fill = knnidw()))

DEMstack = stack(DSM, DTM) 
CHM = DSM-DTM

CHM[CHM < 0] <- 0
CHM[CHM > 100] <- 0

#Export the Canopy Height Model to a .tif

writeRaster(DSM, "F:\\Black_mypassport_1.8TB\\Smith_vanLeeuwen_work\\Bighorn_fire\\Bighorn_fire_data\\2019_lidar_data\\MtLemmon_2019_lidar_3382_DSM.tif", overwrite = TRUE)
writeRaster(DTM, "F:\\Black_mypassport_1.8TB\\Smith_vanLeeuwen_work\\Bighorn_fire\\Bighorn_fire_data\\2019_lidar_data\\MtLemmon_2019_lidar_3382_DTM.tif", overwrite = TRUE)

writeRaster(CHM, "F:\\Black_mypassport_1.8TB\\Smith_vanLeeuwen_work\\Bighorn_fire\\Bighorn_fire_data\\2019_lidar_data\\MtLemmon_2019_lidar_3382_CHM.tif", overwrite = TRUE)








