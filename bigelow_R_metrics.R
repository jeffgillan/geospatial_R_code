setwd("F:\\Bigelow_April2020\\products")

setwd("D:\\Black_mypassport_1.8TB\\Walnut_Gulch\\RG82_38mAGL\\SIF_comparison")

library(lidR)
library(raster)



bigelow_canopy = readLAS("bigelow_full_canopy_clip.las")

canopy = readLAS("kendal_canopy.las")



bigelow_DTM = raster("bigelow_full_dtm.tif")

DTM = raster("kendal_DTM.tif")


canopy_voxel = lasvoxelize(canopy, res = 0.10)
bigelow_canopy_voxel = lasvoxelize(bigelow_canopy, res = 0.10)


AGL_canopy = lasnormalize(canopy_voxel, DTM, copy=TRUE)
AGL_bigelow_canopy = lasnormalize(bigelow_canopy_voxel, bigelow_DTM, copy=TRUE)


AGL_canopy_clean = lasfilter(AGL_canopy, Z > 0)
AGL_bigelow_canopy_clean = lasfilter(AGL_bigelow_canopy, Z > 0)

bigelow_z = AGL_bigelow_canopy_clean$Z

AGL_canopy_clean



bins <- seq(0, 41, by=0.5)  
hist(bigelow_z, breaks=bins, col=rgb(1,0,0,0.5), xlim = c(0,40), ylim=c(0,3400000), xlab="Vegetation Heights (m)", main="Vegetation Structure")
hist(z, breaks=bins, col=rgb(0,0,1,0.5), xlim = c(0,40), add=T)
legend("topright", legend=c("Bigelow","Kendall"), col=c(rgb(1,0,0,0.5), 
                                                       rgb(0,0,1,0.5)), pt.cex=2, pch=15)



range(bigelow_z) 
       
plot(AGL_canopy)
plot(canopy_voxel)
plot(AGL_canopy_clean)



writeLAS(AGL_canopy_clean,"C:\\FUSION\\AGL_voxel_0.1.las")
writeLAS(canopy_voxel,"D:\\Black_mypassport_1.8TB\\Walnut_Gulch\\RG82_38mAGL\\SIF_comparison\\kendal_canopy_voxel_0.1.las")



z = AGL_canopy




treeseg = lastrees(AGL_voxel, li2012(), attribute = "treeID")

plot(treeseg, color = "treeID")

#Don't know the value of this yet
AGL_voxel_metrics = voxel_metrics(AGL_canopy, .stdmetrics_z, 0.1) 
AGL_grid_metrics = grid_metrics(AGL, .stdmetrics, res = 0.1)





library(rLiDAR)

AGL_voxel_df = readLAS("AGL_voxel.las", short=TRUE)


AGL_voxel_df <- data.frame(AGL_voxel_df)




####Landscape Metrics - Largest Patch Index

setwd("F:\\Bigelow_April2020\\classify")

library(landscapemetrics)
library(raster)

Bigelow_classify = raster("Bigelow_reclass.tif")
Bigelow_classify_LPI = lsm_c_lpi(Bigelow_classify, directions = 8)

setwd("D:\\Black_mypassport_1.8TB\\Walnut_Gulch\\RG82_38mAGL\\classify")

RG82_classify = raster("RG82_classified_clip.tif")

RG82_classify_LPI = lsm_c_lpi(RG82_classify, directions = 8)


