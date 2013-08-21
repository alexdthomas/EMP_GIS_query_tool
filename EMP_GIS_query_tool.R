#Develop tool to query EMP dataset based on global environmental 
#parameters and identify patterns

#start with example data set and a shiny app
library(shiny)
library(phyloseq)
library(sp)
library(rgdal)
library(raster)

data(GlobalPatterns)
GlobalPatterns
sample_variables(GlobalPatterns)
head(sample_data(GlobalPatterns))
rownames(sample_data(GlobalPatterns))

#hmm, well maybe best chance is to add the lat/long to GlobalPatterns

#subset environmental samples (not mock or human, can estimate lat/long)
sample_data(GlobalPatterns)$Description
rownames(sample_data(GlobalPatterns)) %in% rownames(sample_data(GlobalPatterns)[c(1:3,11:21), ])

gp<-subset_samples(GlobalPatterns, rownames(sample_data(GlobalPatterns)) %in% rownames(sample_data(GlobalPatterns)[c(1:3,11:21), ]))
gp
nrow(sample_data(GlobalPatterns)[c(1:3,11:21), ])

#ok, have subset
#google some lat/long coordinates
sample_data(gp)$Description

#location- lat long
#Calhoun South Carolina- N 34.092870 W 82.589746
#Cedar Creek Minnesota- 45.4086° N, 93.2008° W
#Sevilleta new Mexico- 34.3333° N, 106.8333° W
# Lake Mendota Minnesota- ...
#rogers lake mendota heights minnesota?- 44.87191 N, 93.13883 W
#Sparkling Lake Wisconsin 46.0100 N, -89.6996 W
#Allequash Creek 46.038330 N, -89.613892 W
#Newport Pier 33.6073° N, 117.9289° W
# Tijuana River Reserve 32.574,  -117.1

sample_data(gp)$Latitude<-c(34.092870, 45.4086, 34.3333, 44.87191, 46.0100,
														rep(46.038330, 3), rep(33.6073, 3), rep(32.574, 3))

sample_data(gp)$Longitude<-c(-82.589746, -93.2008, -106.8333, -93.13883, -89.6996, 
														 rep(-89.613892, 3), rep(-117.9289, 3), rep(-117.1, 3))

#import a world map (inappropriate as these are all in N. America, but ok for example)

borders<-readOGR(dsn="C:/Users/asus4/Documents/GIS/Data/Natural_Earth", 
								 layer="ne_50m_admin_0_countries")

class(borders)

#can make lat long coordinates in phyloseq class object?
#coordinates(gp) = c("LONGITUDE", "LATITUDE")
#nope, make a copy
gp.sp<-data.frame(sample_data(gp))
	
class(gp.sp)
head(gp.sp)
coordinates(gp.sp) = c("Longitude", "Latitude")
head(coordinates(gp.sp))

#need to check, set and transform spatial projection
proj4string(borders)
proj4string(gp.sp)
proj4string(gp.sp)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#visual check
plot(borders)
points(coordinates(gp.sp), col="green", pch=20)
#ok, looks reasonable now

#now grab some GIS data
#follow this tutorial, but substitute RODBC to access MS Access directly 

library(RODBC)

hwsd<-raster("C:/Users/asus4/Documents/GIS/Data/Harmonized_World_Soil_Database/HWSD_RASTER/hwsd.bil")
ncol(hwsd)
nrow(hwsd)
res(hwsd)
extent(hwsd)
projection(hwsd)
proj4string(hwsd)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

wc.alt<-raster("C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5/alt.bil")

plot(wc.alt)
#plotting this took a while, looks really nice though...

projection(wc.alt)
projection(gp.sp)
#I think these projections are totally compatable 
#here is noted datum=WGS84 sets the other arguments
#http://osgeo-org.1560.x6.nabble.com/Difference-between-same-projections-types-td4469571.html
#and BioClim sites states 
#latitude / longitude coordinate reference system (not projected) 
#and the datum is WGS84

#exctact values to points
gp.sp$wc.alt<-extract(wc.alt, coordinates(gp.sp))

#do the rest
wc.alt<-raster("C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5/alt.bil")


emp.border<-over(gp.sp, borders, returnList=FALSE)