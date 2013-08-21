#Develop tool to query EMP dataset based on global environmental 
#parameters and identify patterns

#start with example data set and a shiny app
library(shiny)
library(phyloseq)
library(sp)
library(rgdal)
library(raster)
library(maptools)

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

hwsd<-raster("C:/Users/asus4/Documents/GIS/Data/Harmonized_World_Soil_Database/HWSD_RASTER/hwsd.bil")
ncol(hwsd)
nrow(hwsd)
res(hwsd)
extent(hwsd)
projection(hwsd)
proj4string(hwsd)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#work on BioClim data
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

#probably should still convert see ?spTransform
#" In general +datum= is to be prefered to +ellps=, because the datum
#always fixes the ellipsoid, but the ellipsoid never fixes the datum."
wc.alt<-projectRaster(wc.alt, crs=proj4string(gp.sp))
#this takes a really long time and I don't think it really makes a 
#difference, do the rest later...
projection(wc.alt)
#R crashed using zoom, lost this projection..

#do they look different
plot(wc.alt)
plot(wc.prec, add=TRUE)

#exctact values to points
gp.sp$wc.alt<-extract(wc.alt, coordinates(gp.sp))

#do the rest
wc.bio<-list.files("C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5", 
					 pattern="bio.*\\.bil", full.names=TRUE)

wc.bio<-sapply(wc.bio, raster)
wc.bio<-stack(wc.bio)
#don't average bio, each one is seperate

wc.prec<-list.files("C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5", 
									 pattern="prec.*\\.bil", full.names=TRUE)
wc.prec<-sapply(wc.prec, raster)
head(wc.prec)
wc.prec<-stack(wc.prec)
wc.prec<-mean(wc.prec)

#tmax
wc.tmax<-list.files("C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5", 
										pattern="tmax.*\\.bil", full.names=TRUE)
wc.tmax<-sapply(wc.tmax, raster)
head(wc.tmax)
wc.tmax<-stack(wc.tmax)
#annual mean tmax
wc.tmax<-mean(wc.tmax)
#fix units
wc.tmax<-wc.tmax/10

#tmin
wc.tmean<-list.files("C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5", 
										 pattern="tmean.*\\.bil", full.names=TRUE)
wc.tmean<-sapply(wc.tmean, raster)
head(wc.tmean)
wc.tmean<-stack(wc.tmean)
wc.tmean<-mean(wc.tmean)
wc.tmean<-wc.tmean/10

#tmin
wc.tmin<-list.files("C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5", 
										pattern="tmin.*\\.bil", full.names=TRUE)
wc.tmin<-sapply(wc.tmin, raster)
head(wc.tmin)
wc.tmin<-stack(wc.tmin)
wc.tmin<-mean(wc.tmin)
wc.tmin<-wc.tmin/10

#add values to dataset
gp.sp.test<-gp.sp
gp.sp.test

#on wc.bio raster stack
wc.bio.test<-extract(wc.bio, coordinates(gp.sp.test), df=TRUE)
ncol(wc.bio.test)
colnames(wc.bio.test)<-(c("ID", paste("bio", 1:19, sep="")))
#note, regular cbind converts SpatialPointsDataFrame to data frame
#use spCbind from maptools instead
gp.sp.test<-spCbind(gp.sp.test, wc.bio.test[,2:20])


gp.sp.test$wc.prec<-extract(wc.prec, coordinates(gp.sp.test))
gp.sp.test$wc.tmax<-extract(wc.tmax, coordinates(gp.sp.test))
gp.sp.test$wc.tmin<-extract(wc.tmin, coordinates(gp.sp.test))
gp.sp.test$wc.tmean<-extract(wc.tmean, coordinates(gp.sp.test))

#back to the WHSD database, now in SQLite (hopefully...)
#according to 
#looks like can extract hwsd cell values to data frame
#then match to attribute data in SQLite
gp.sp.test$hwsd<-extract(hwsd, coordinates(gp.sp.test))
#right, the ocean samples get no soil cells, but nearest may still be helpful...
#oh, and will have no salinity...

#this should get the nearest cell
gp.sp.test$hwsd<-extract(hwsd, coordinates(gp.sp.test), buffer=600, small=TRUE)
#well, it works, but returns lists, kind of messy
gp.sp.test<-gp.sp.test[, -32]

#try RSAGA pick.from.saga.grid
library(RSAGA)
xy<-as.data.frame(coordinates(gp.sp.test))
colnames(xy)<-c("X.name", "Y.name")
gp.sp.test$hwsd<-pick.from.ascii.grid(xy, filename=hwsd)
#that's a pain, annoying error message, can't figure out...

#can I see it?
plot(hwsd)
points(coordinates(gp.sp[9:14,]), col="red", pch=20)
extent(coordinates(gp.sp.test[9:11,]))
zoom(hwsd, extent(c(-118.0, -117.0, 32.0, 33.0)))
click(hwsd)

#export points and see if I can find them in QGIS
writeOGR(gp.sp.test, dsn=getwd(), driver="ESRI Shapefile", layer="gp_sp_test")
#ok, this 0 values should be 4766...

#easier to extract with no buffers and edit...
gp.sp.test$hwsd<-extract(hwsd, coordinates(gp.sp.test))
#and have to convert
gp.sp.test<-data.frame(gp.sp.test)
gp.sp.test[9:11, "hwsd"]<-4766
coordinates(gp.sp.test) = c("Longitude", "Latitude")

#ok, now try to extract attributes from SQLite database
require(RSQLite)

m<-dbDriver("SQLite")
con<-dbConnect(m, dbname="C:/Users/asus4/Documents/GIS/Data/Harmonized_World_Soil_Database/HWSD")
dbListTables(con)							 

dbGetQuery(con, "pragma table_info(HWSD_DATA)")$name

hwsd.id<-gp.sp.test$hwsd

(display.fields <- c("ID", "MU_GLOBAL", "ISSOIL", "SHARE", "SU_CODE90", "SU_SYM90", "T_USDA_TEX_CLASS"))

tmp <- dbGetQuery(con, paste("select", paste(display.fields, collapse = ", "), "from HWSD_DATA"))
#print(tmp)
class(tmp)
head(tmp)
tmp[which(tmp$MU_GLOBAL%in% hwsd.id), ]

(hwsd.ex <- dbGetQuery(con, paste("select * from HWSD_DATA where MU_GLOBAL = ", 4885)))

#well, having a hard time getting this exact method to work, 
#but found one that does

#make a reasonable list of soil variables 
display.fields <- c("MU_GLOBAL", "ISSOIL", "T_USDA_TEX_CLASS", "T_GRAVEL", "T_SAND", "T_SILT", "T_CLAY", "T_BULK_DENSITY", "T_OC", "T_PH_H2O", "T_BS", "T_ECE")
#query the list
tmp <- dbGetQuery(con, paste("select", paste(display.fields, collapse = ", "), "from HWSD_DATA"))
#now it's a dataframe and I can match it
tmp<-tmp[which(tmp$MU_GLOBAL%in% hwsd.id), ]
#spCbind what I want to my data frame
#hmm, there are more than value per MU_GLOBAL ID
#not sure why, but reasonable short term solution is to average
tmp<-apply(tmp, 2, function(x) as.numeric(x))
tmp<-data.frame(tmp)
tmp<-stats::aggregate(. ~ MU_GLOBAL, data=tmp, mean)

gp.sp.test<-data.frame(gp.sp.test)
colnames(gp.sp.test)[32]<-"MU_GLOBAL"
library(plyr)
gp.sp.test<-join(gp.sp.test, tmp, by="MU_GLOBAL")
coordinates(gp.sp.test)= c("Longitude", "Latitude")

#grab NPP
#and another package for ArcInfo files
library(RArcInfo)
npp<-get.arcdata("C:/Users/asus4/Documents/GIS/Data/NASA/NPP/info")

#thad didn't work, try these
library(grid)
#npp<-readGDAL("C:/Users/asus4/Documents/GIS/Data/NASA/NPP/npp_geotiff/npp_geotiff.TIF")
#this created a SpatialGridDataFrame, couldn't extract...
npp<-raster("C:/Users/asus4/Documents/GIS/Data/NASA/NPP/npp_geotiff/npp_geotiff.TIF")
#raster works though, not file is .TIF not .tiff or .TIFF
#that worked
gp.sp.test$npp<-extract(npp, coordinates(gp.sp.test))

#what about ocean salinity?
#still waiting to receive that data...

#so try to feed the new sample data back into phyloseq
#try to query by data and make ordinations
gp.sp.test<-data.frame(gp.sp.test)
rownames(gp.sp.test)<-sample_names(gp)
sample_data(gp)<-gp.sp.test
sample_data(gp)

gp.dist<-UniFrac(gp)
gp.ord<-ordinate(gp, "MDS", gp.dist)
plot_ordination(gp, gp.ord, type="samples")

#export data to bring into shiny
write.table(as.matrix(gp.dist), "gp_dist.txt")
gp