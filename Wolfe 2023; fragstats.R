#!/usr/bin/env Rscript
#to run from BASH cmd line type "R < script.r --no-save" to show errors & outputs or "./script.r" to not show errors & outputs
#WOLFE 2023; FRAGSTATS 4 MOVEMENT ECOLOGY PRJ
#scope: import home range data > perform FRAGSTATS analysis > export stats

#clear environment
rm(list=ls())

#set working directory
setwd('C:/Users/wolfe/Downloads/Cut/mvmnt final prj/Analysis/Data')

#install.packages
#install.packages(c('tidyverse','sf','sp','raster','landscapemetrics'),repos='mirror.las.iastate.edu/CRAN/')
library(tidyverse)
library(sp)
library(sf)
library(raster)
library(landscapemetrics)

#load & examine data
hr<-read.csv("../Results/HRstats.csv",sep=",",head=T)
head(hr)
tail(hr)
str(hr)

#make SpatialPointsDataFrame from HR data
hr_spdf<-st_as_sf(hr,coords=c("harm_east","harm_north"),crs=32615)

#re-project to Lamert Conic Projection
hr_prj<-st_transform(hr_spdf,crs="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

#load NLCD raster
img<-raster("./nlcd_2021_land_cover_l48_20230630/nlcd_2021_land_cover_l48_20230630.img")
#img<-raster("./nlcd2021_mask.tif")
names<-"landuse"

#mask NLCD raster
bcat_buff<-st_buffer(as(hr_prj,"sf"),dist=(1000*max(hr$radius95)))
crop_extent<-extent(bcat_buff)
nlcd_crop<-crop(img,crop_extent)

#reclassify NLCD to 5 classes
rc<-cbind(c(11,21:24,31,41:43,52,71,81:82,90,95),c(1,3,3,3,2,5,4,4,4,4,4,5,5,1,1))
landuse<-reclassify(nlcd_crop,rc)
names(landuse)<-"landuse"

# make a list of the landscape level metrics, can edit these figure out which ones to need. the fewer we can cut these down the easier
lmetrics<-c("lsm_l_ai","lsm_l_area_cv","lsm_l_area_mn","lsm_l_area_sd","lsm_l_contag","lsm_l_enn_cv","lsm_l_enn_mn","lsm_l_enn_sd","lsm_l_iji","lsm_l_lpi","lsm_l_lsi","lsm_l_np","lsm_l_para_cv","lsm_l_para_mn","lsm_l_para_sd","lsm_l_pd","lsm_l_pr")

# make a list of class level metrics
cmetrics<-c("lsm_c_area_cv","lsm_c_area_mn","lsm_c_area_sd","lsm_c_enn_cv","lsm_c_enn_mn","lsm_c_enn_sd","lsm_c_iji","lsm_c_lpi","lsm_c_lsi","lsm_c_np","lsm_c_pd","lsm_c_pland")

#make buffer around animal individual animal
bcat<-hr_prj[1,]
bcat_crcl<-st_buffer(as(bcat,"sf"),dist=(1000*hr[1,7]))

#crop nlcd to each cat
lu_crop<-crop(landuse,bcat_crcl)

#figure out landcover in cropped circle
lu<-mask(lu_crop,bcat_crcl,inverse=FALSE)
image(lu)

#calculate landscape level metrics (I need to make this more efficient before I make it a loop)
lu_land<-calculate_lsm(lu,what=lmetrics)
landscape_results<-spread(lu_land,key=metric,value=value)
landscape_results

#calculate class level metrics (I need to make this more efficient before I make it a loop)
lu_class<-calculate_lsm(lu,what=cmetrics)
class_results<-spread(lu_class,key=metric,value=value)

#end
