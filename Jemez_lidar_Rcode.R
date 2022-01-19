
setwd("D:\\Black_mypassport_1.8TB\\Jemez_TNC\\Jemez_lidar")
library(raster)
library(lidR)
library(rgdal)
library(sf)
library(rgeos)
library(spatstat)
library(sp)


#Bring point cloud into Rstudio
points = readLAS("USGS_lidar_Jemez_Area3_clip.las")

structures = readOGR("D:\\Black_mypassport_1.8TB\\Jemez_TNC\\GIS_data\\structures.shp")


#LAS points that fall within the 'structures' polygon will be attributed as 'False' in a new field called 'in_structure' 
#The goal is to remove lidar points that hit structures
y_test = merge_spatial(points, structures, "in_structure")
points_without_structures = lasfilter(y_test, in_structure == FALSE)


writeLAS(points_without_structures,"D:\\Black_mypassport_1.8TB\\Jemez_TNC\\GIS_data\\no_structures.las")


#plot(points, color = "Classification")

dtm_tin <- grid_terrain(points_without_structures, res = 0.5, algorithm = tin())

##canopy_points = lasfilter(points, Classification == 1)

nlas <- lasnormalize(points_without_structures, dtm_tin)

#Remove points that is below 0
nlas_clean = lasfilter(nlas, Z > 1.5)

#plot(canopy_points)

# Create Canopy Height Model 

#chm <- grid_canopy(nlas_clean, res = 0.25, algorithm = p2r())


# Khosravipour et al. pitfree algorithm

chm_pitfree <- grid_canopy(nlas, res = 0.25, pitfree(c(0,2,5,10,15), c(0, 1.5)))

#writeRaster(chm,"D:\\Black_mypassport_1.8TB\\Jemez_TNC\\Jemez_lidar\\CHM_2.tif",options=c('TFW=YES'), overwrite = TRUE)
writeRaster(chm_pitfree,"D:\\Black_mypassport_1.8TB\\Jemez_TNC\\Jemez_lidar\\CHM_pitfree.tif",options=c('TFW=YES'), overwrite = TRUE)


# Local Maxima Tree Locations

# Create function that selects with a variable area window

f <- function(x){
  y <- 2.6 * (-(exp(-0.1*(x-2)) - 1)) + 1
  y[x < 2] <- 0.5
  y[x > 20] <- 3.2
  return(y)
}

heights <- seq(0,30,0.5)

ws <- f(heights)

# view the variable area model

plot(heights, ws, type = "l",  ylim = c(0,5))

# Locate Tree tops

ttops <- tree_detection(chm_pitfree, lmf(ws=3, shape = "circular"))
writeOGR(ttops, dsn = "D:\\Black_mypassport_1.8TB\\Jemez_TNC\\Jemez_lidar", layer = "treeseg_lidar_tops", driver = "ESRI Shapefile", overwrite_layer = TRUE)

#plot(chm, col = height.colors(50))
#plot(ttops, add = TRUE)

treeseg_lidar= lastrees(nlas_clean, dalponte2016(chm = chm_pitfree, treetops = ttops, th_tree = 1.5, th_seed = 0.20,
                                                       th_cr = 0.20, max_cr = 35, ID = "treeID"))

metric = tree_metrics(treeseg_lidar, .stdtreemetrics)
hulls  = tree_hulls(treeseg_lidar)
hulls@data = dplyr::left_join(hulls@data, metric@data)

writeOGR(hulls, dsn = "D:\\Black_mypassport_1.8TB\\Jemez_TNC\\Jemez_lidar", layer = "treeseg_lidar4", driver = "ESRI Shapefile", overwrite_layer = TRUE)




