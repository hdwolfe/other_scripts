#!/usr/bin/env Rscript
#to run from BASH cmd line type "R < script.r --no-save" to show errors & outputs or "./script.r" to not show errors & outputs
#WOLFE 2023; HOME RANGE 4 MOVEMENT ECOLOGY PRJ
#scope: import location data > prepare it for analysis > perform Kernel Density Estimation for home range > export stats and shp files

#clear environment
rm(list=ls())

#set working directory
setwd('C:/Users/wolfe/Downloads/Cut/mvmnt final prj/Analysis/Data')

#install.packages
#install.packages(c('sf','sp','adehabitatHR','psych'),repos='mirror.las.iastate.edu/CRAN/')
library(sf)
library(sp)
library(adehabitatHR)
library(psych)

#load & examine data
bobcat<-read.csv('bobcat_fixes_ttl.csv',sep=",",head=T)
head(bobcat)
tail(bobcat)
str(bobcat)

#trim unnecessary fields
bobcat<-bobcat[c(1,2,4,5,6)]

#format date
bobcat[,2]<-as.Date(bobcat[,2],"%m/%d/%Y")

#make sex field
bobcat$sex<-substr(bobcat$animal_id,1,1)

#split data by UTM zone
bcat15<-bobcat[bobcat[,3]==15,]
bcat16<-bobcat[bobcat[,3]==16,]

#define UTM crs
bcat15_prj<-st_as_sf(bcat15,coords=c("easting","northing"),crs=32615)
bcat16_prj<-st_as_sf(bcat16,coords=c("easting","northing"),crs=32616)

#re-project UTM16 to UTM15
bcat15.2_prj<-st_transform(bcat16_prj,crs=32615)

#row bind the two sp objects
bcat_prj<-rbind(bcat15_prj,bcat15.2_prj)

#split by id for 
bcatXid<-split(bcat_prj,bcat_prj$animal_id)

#remove animals with less than 25 fixes
bcatXid<-bcatXid[sapply(bcatXid,function(x) nrow(x)>24)]

#make HRstats export df
HRstats<-data.frame(id=character(),n=numeric(),sex=character(),HR50=numeric(),HR95=numeric(),radius95=numeric(),harm_east=numeric(),harm_north=numeric())

######HR loop
for(i in 1:length(bcatXid)){

#fetch individual bobcat info
id<-bcatXid[[i]]$animal_id[1]
sex<-bcatXid[[i]]$sex[1]
n<-length(bcatXid[[i]]$animal_id)

#create export file
HRstats[i,"id"]<-as.character(id)
HRstats[i,"n"]<-n
HRstats[i,"sex"]<-sex

#fetch points for animal i and make SpatialPoints object
cat<-st_as_sf(bcatXid[[i]],coords=c("easting","northing"),crs=32615)
cat<-as(cat,"Spatial")

#KD home range
ud<-kernelUD(as(cat,"SpatialPoints"),h="href")

#get area estimates
ka<-kernel.area(ud,c(50,95),unin="m",unout="km2")
HRstats[i,"HR50"]<-ka[1]
HRstats[i,"HR95"]<-ka[2]

#get radius estimate of 95%
radius_95<-(pi*sqrt(ka[2]))
HRstats[i,"radius95"]<-radius_95

#get harmonic easting & northing
tmp<-bobcat[bobcat[,1]==id,]
harm_east<-harmonic.mean(tmp[,4])
harm_north<-harmonic.mean(tmp[,5])
HRstats[i,"harm_east"]<-harm_east
HRstats[i,"harm_north"]<-harm_north

#get HR vertices for 50% & 95%
bcat_vert50<-st_as_sf(getverticeshr(ud,50))
bcat_vert95<-st_as_sf(getverticeshr(ud,95))

#make shp file names
name50<-paste("./HR Shp Files/",id,"_50.shp",sep="")
name95<-paste("./HR Shp Files/",id,"_95.shp",sep="")

#write individual shp files
st_write(bcat_vert50,name50,append=F)
st_write(bcat_vert95,name95,append=F)
}

#export HRstats
write.csv(HRstats,"../Results/HRstats.csv")

#mann-whitney U test AKA wilcoxon rank sum test
results50<-wilcox.test(HR50~sex,data=HRstats)
results50
mean(HRstats[HRstats[,3]=="M",4])
mean(HRstats[HRstats[,3]=="F",4])

results95<-wilcox.test(HR95~sex,data=HRstats)
results95
mean(HRstats[HRstats[,3]=="M",5])
mean(HRstats[HRstats[,3]=="F",5])

#end