#!/usr/bin/env Rscript
#to run from BASH cmd line type "R < script.r --no-save" to show errors & outputs or "./script.r" to not show errors & outputs
#WOLFE 2023; FETCH & MASK PRISM DATA

#install packages
#install.packages(c("prism","terra","raster"),repos='mirror.las.iastate.edu/CRAN/')

#load packages
library(prism)
library(terra)
library(raster)

#set working directory
setwd('C:/Users/wolfe/Downloads/Cut/SDSU Bison/Data/')

#load sites shp file fields: site, name, lat, long
sites<-vect("sites_WGS84.shp")

#examine data
head(sites)
tail(sites)
str(sites)
sites

names(sites)<-c("a","b","c","d","e","f","g")

#set path where PRISM will download to
prism_set_dl_dir('C:/Users/wolfe/Downloads/Cut/SDSU Bison/Data/PRISM/')

#fetch PRISM data !!!CAUTION!!! don't get our office IP address blocked by PRISM: PRISM will read any request coming from our office's internet router as having the same IP address. external IP (outside network) is different that internal IP (within network). they will block our office's IP if you run these lines of code more than twice in one day. communicate with each other.
###CAUTION###get_prism_annual(type="tmean",year=2008:2022,keepZip=F)
###CAUTION###get_prism_annual(type="ppt",year=2008:2022,keepZip=F)

#load sample PRISM file, confirm coordinate reference system (CRS) is the same as sites
prism_sample_file<-"./PRISM/PRISM_tmean_stable_4kmM3_2022_bil/PRISM_tmean_stable_4kmM3_2022_bil.bil"
prism_sample<-rast(prism_sample_file)
crs(sites,describe=T)
crs(prism,describe=T)

######the CRS are different. PRISM files do not have an authority code. I'm still figuring out how to change CRS with no authority code. Depending on the location on Earth, WGS84 EPSG:4326 and NAD83 are, at most, going to be ~3m different. Therefore, having CRS different isn't going to change anything here. I'll run the code anyway.

#make array for years
years<-2008:2022

#loop to extract tmean and ppt from PRISM bil rasters
i=0
for(x in years)
{
######tmean
tmean_file<-paste("./PRISM/PRISM_tmean_stable_4kmM3_",x,"_bil/PRISM_tmean_stable_4kmM3_",x,"_bil.bil",sep="")
tmean_prism<-rast(tmean_file)

#extract points from tmean raster
tmean<-terra::extract(tmean_prism,sites)

#rename column
colnames(tmean)<-c("ID",x)

ifelse(i>0,tmean_out<-cbind(tmean_out,tmean[2]),tmean_out<-tmean)

######ppt
ppt_file<-paste("./PRISM/PRISM_ppt_stable_4kmM3_",x,"_bil/PRISM_ppt_stable_4kmM3_",x,"_bil.bil",sep="")
ppt_prism<-rast(ppt_file)

#extract points from ppt raster
ppt<-terra::extract(ppt_prism,sites)

#rename column
colnames(ppt)<-c("ID",x)

ifelse(i>0,ppt_out<-cbind(ppt_out,ppt[2]),ppt_out<-ppt)
i=i+1
}

#recode to site name
tmean_out$site<-c("WC","BK","DR","NC","KS","TP","SV")
ppt_out$site<-c("WC","BK","DR","NC","KS","TP","SV")

#reorder columns to make it easier to read
tmean_out<-tmean_out[,c(1,17,2:16)]
ppt_out<-ppt_out[,c(1,17,2:16)]

#write csv files
write.csv(tmean_out,"tmean_ttl.csv")
write.csv(ppt_out,"ppt_ttl.csv")

#END
