#Develop tool to query EMP dataset based on global environmental 
#parameters and identify patterns

#start with example data set and a shiny app
library(shiny)
library(phyloseq)
library(sp)
library(rgdal)
library(raster)
library(maptools)
library(RSQLite)
library(vegan)
library(ggplot2)
library(grid)

def.par <- par(no.readonly = TRUE) # save default, for resetting...

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
#plot(borders)
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

#plot(wc.alt)
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
#wc.alt<-projectRaster(wc.alt, crs=proj4string(gp.sp))
#this takes a really long time and I don't think it really makes a 
#difference, do the rest later...
projection(wc.alt)
#R crashed using zoom, lost this projection..

#do they look different
#plot(wc.alt)
#plot(wc.prec, add=TRUE)

#exctact values to points
gp.sp$wc.alt<-extract(wc.alt, coordinates(gp.sp))

#do the rest
wc.bio<-list.files("C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5", 
					 pattern="bio.*\\.bil", full.names=TRUE)

wc.bio<-sapply(wc.bio, raster)
wc.bio<-stack(wc.bio)
#don't average bio, each one is seperate


# wc.prec<-list.files("C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5", 
# 									 pattern="prec.*\\.bil", full.names=TRUE)
# wc.prec<-sapply(wc.prec, raster)
# head(wc.prec)
# wc.prec<-stack(wc.prec)
# wc.prec<-mean(wc.prec)
# #exported this to bring in later, speed up script processing

#tmax
# wc.tmax<-list.files("C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5", 
# 										pattern="tmax.*\\.bil", full.names=TRUE)
# wc.tmax<-sapply(wc.tmax, raster)
# head(wc.tmax)
# wc.tmax<-stack(wc.tmax)
# #annual mean tmax
# wc.tmax<-mean(wc.tmax)
# #fix units
# wc.tmax<-wc.tmax/10
#exported this to bring in later, speed up script processing

#tmin
# wc.tmean<-list.files("C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5", 
# 										 pattern="tmean.*\\.bil", full.names=TRUE)
# wc.tmean<-sapply(wc.tmean, raster)
# head(wc.tmean)
# wc.tmean<-stack(wc.tmean)
# wc.tmean<-mean(wc.tmean)
# wc.tmean<-wc.tmean/10
# #exported this to bring in later, speed up script processing

#tmin
# wc.tmin<-list.files("C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5", 
# 										pattern="tmin.*\\.bil", full.names=TRUE)
# wc.tmin<-sapply(wc.tmin, raster)
# head(wc.tmin)
# wc.tmin<-stack(wc.tmin)
# wc.tmin<-mean(wc.tmin)
# wc.tmin<-wc.tmin/10
# #exported this to bring in later, speed up script processing

wc.prec<-raster("C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5/wc_prec_mean")
wc.tmax<-raster("C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5/wc_tmax_mean")
wc.tmin<-raster("C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5/wc_tmin_mean")
wc.tmean<-raster("C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5/wc_tmean_mean")

#add values to dataset

#on wc.bio raster stack
wc.bio.test<-extract(wc.bio, coordinates(gp.sp), df=TRUE)
ncol(wc.bio.test)
colnames(wc.bio.test)<-(c("ID", paste("bio", 1:19, sep="")))
#note, regular cbind converts SpatialPointsDataFrame to data frame
#use spCbind from maptools instead
gp.sp<-spCbind(gp.sp, wc.bio.test[,2:20])


gp.sp$wc.prec<-extract(wc.prec, coordinates(gp.sp))
gp.sp$wc.tmax<-extract(wc.tmax, coordinates(gp.sp))
gp.sp$wc.tmin<-extract(wc.tmin, coordinates(gp.sp))
gp.sp$wc.tmean<-extract(wc.tmean, coordinates(gp.sp))

#back to the WHSD database, now in SQLite (hopefully...)
#according to 
#looks like can extract hwsd cell values to data frame
#then match to attribute data in SQLite
#gp.sp$hwsd<-extract(hwsd, coordinates(gp.sp))
#right, the ocean samples get no soil cells, but nearest may still be helpful...
#oh, and will have no salinity...

#this should get the nearest cell
#gp.sp$hwsd<-extract(hwsd, coordinates(gp.sp), buffer=600, small=TRUE)
#well, it works, but returns lists, kind of messy
#gp.sp<-gp.sp[, -32]

#export points and see if I can find them in QGIS
#writeOGR(gp.sp, dsn=getwd(), driver="ESRI Shapefile", layer="gp_sp_test")
#ok, this 0 values should be 4766...

#easier to extract with no buffers and edit...
gp.sp$MU_GLOBAL<-extract(hwsd, coordinates(gp.sp))
#and have to convert
gp.sp<-data.frame(gp.sp)
gp.sp[9:11, "MU_GLOBAL"]<-4766
coordinates(gp.sp) = c("Longitude", "Latitude")

#ok, now try to extract attributes from SQLite database

m<-dbDriver("SQLite")
con<-dbConnect(m, dbname="C:/Users/asus4/Documents/GIS/Data/Harmonized_World_Soil_Database/HWSD")
dbListTables(con)							 

dbGetQuery(con, "pragma table_info(HWSD_DATA)")$name

hwsd.id<-gp.sp$MU_GLOBAL

#(display.fields <- c("ID", "MU_GLOBAL", "ISSOIL", "SHARE", "SU_CODE90", "SU_SYM90", "T_USDA_TEX_CLASS"))

#tmp <- dbGetQuery(con, paste("select", paste(display.fields, collapse = ", "), "from HWSD_DATA"))
#print(tmp)
#class(tmp)
#head(tmp)
#tmp[which(tmp$MU_GLOBAL%in% hwsd.id), ]

#(hwsd.ex <- dbGetQuery(con, paste("select * from HWSD_DATA where MU_GLOBAL = ", 4885)))

#well, having a hard time getting this exact method to work, 
#but found one that does

#make a reasonable list of soil variables 
display.fields <- c("MU_GLOBAL", "ISSOIL", "T_USDA_TEX_CLASS", 
										"T_GRAVEL", "T_SAND", "T_SILT", "T_CLAY", 
										"T_BULK_DENSITY", "T_OC", "T_PH_H2O", "T_BS", 
										"T_ECE")
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

gp.sp<-data.frame(gp.sp)
library(plyr)
gp.sp<-join(gp.sp, tmp, by="MU_GLOBAL")
coordinates(gp.sp)= c("Longitude", "Latitude")

#this created a SpatialGridDataFrame, couldn't extract...
npp<-raster("C:/Users/asus4/Documents/GIS/Data/NASA/NPP/npp_geotiff/npp_geotiff.TIF")
#raster works though, not file is .TIF not .tiff or .TIFF
#that worked
gp.sp$npp<-extract(npp, coordinates(gp.sp))

#what about ocean salinity?
#still waiting to receive that data...

#so try to feed the new sample data back into phyloseq
#try to query by data and make ordinations
gp.sp<-data.frame(gp.sp)
rownames(gp.sp)<-sample_names(gp)
sample_data(gp)<-gp.sp
sample_data(gp)

#this works, takes a really long time, exported to use later
#gp.dist<-UniFrac(gp)
#import the exported file
gp.dist<-as.dist(read.table("C:/Users/asus4/Documents/EarthMicrobiomeProject/R/EMP_GIS_query_tool/gp_dist.txt"))
#test makes sure works
gp.ord<-ordinate(gp, "MDS", gp.dist)
plot_ordination(gp, gp.ord, type="samples")

#export data to bring into shiny

#exporting the distance matrix as a table/ importing and using as.dist works!
#write.table(as.matrix(gp.dist), "gp_dist.txt")

write.table(gp.sp, "gp_sp.txt")

coordinates(gp.sp) = c("Longitude", "Latitude")
wc_tmean_crop<-crop(wc.tmean, (extent(gp.sp)+50))

#looks ok
#(wc.tmean.crop)
#points(coordinates(gp.sp), col="red", pch=20)

wc_prec_crop<-crop(wc.prec, (extent(gp.sp)+50))
npp_crop<-crop(npp, (extent(gp.sp)+50))

#write averaged bioClim rasters for later use
# writeRaster(wc.prec, "C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5/wc_prec_mean")
# writeRaster(wc.tmax, "C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5/wc_tmax_mean")
# writeRaster(wc.tmin, "C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5/wc_tmin_mean")
# writeRaster(wc.tmean, "C:/Users/asus4/Documents/GIS/Data/WorldClim_World_Climate_Data/generic_2_5/wc_tmean_mean")

#clean up workspace
rm(wc.alt)
rm(wc.bio)
rm(wc.prec)
rm(wc.tmax)
rm(wc.tmin)
rm(hwsd)

#increase cell size for plotting efficieancy 
#tested aggregating by 2-8, 4 still looks ok and plots faster
wc_tmean_crop<-raster::aggregate(wc_tmean_crop, 4, mean)
wc_prec_crop<-raster::aggregate(wc_prec_crop, 4, mean)

#npp has a much smaller raster size, I think it's fine as is
#npp_crop<-raster::aggregate(npp_crop, 4, mean)


writeRaster(wc_tmean_crop, "wc_tmean_crop", overwrite=TRUE)
writeRaster(wc_prec_crop, "wc_prec_crop", overwrite=TRUE)
writeRaster(npp_crop, "npp_crop", overwrite=TRUE)

#test first try at simple map
runApp()

#subsetting the points
#right, have to do this before I convert to SpatialPointsDataFrame
#or convert back to data frame, subset, and convert back, but thats silly

#ok, cool
gp.sp[(gp.sp[,"wc.prec"] >20 & gp.sp[,"wc.prec"] < 30), ]

#now to get the optional min and max for the sliders can use
#conditional panels 
max(gp.sp[,c("wc.prec", "wc.tmean", "npp")])
range(data.frame(gp.sp[,"wc.prec"]))
range(data.frame(gp.sp[,"wc.tmean"]))
range(data.frame(gp.sp[,"npp"]))

test<-data.frame(gp.sp)
test<-test[(test[,"wc.prec"] >20 & test[,"wc.prec"] < 30), ]
coordinates(test) = c("Longitude", "Latitude")
proj4string(test)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#plot(wc_prec_crop)
#plot(test, col="red", pch=20, cex=2, add=TRUE)

#mess around with ordination
#look at code form zooplankton and see how I did things there
class(gp.ord)
#so I already have the ordination, should be able to run envfit from there

#this may be a useful function
#from here http://joey711.github.io/phyloseq-demo/phyloseq-demo.html
veganotu = function(physeq) {
	require("vegan")
	OTU = otu_table(physeq)
	if (taxa_are_rows(OTU)) {
		OTU = t(OTU)
	}
	return(as(OTU, "matrix"))
}

gp.veganotu<-veganotu(gp)
dim(gp.veganotu)

#ah gp.ord was calculated with pcoa from "ape", no scores method
#redo with cmdscale
gp.cmdscale<-cmdscale(gp.dist, nrow(gp.veganotu)-1, eig=TRUE)

gp.sp.edit<-data.frame(gp.sp)
gp.sp.edit<-gp.sp.edit[,c(6:34, 36:46)]

gp.envfit<-envfit(gp.cmdscale, gp.sp.edit)

spe.wa<-wascores(gp.cmdscale$points[,1:2], gp.veganotu)
head(spe.wa)
#maybe can come up with way to select OTU most associated with samples or something...

#modify this code from here 
#http://stackoverflow.com/questions/14711470/plotting-envfit-vectors-vegan-package-in-ggplot2


gp.cmdscale.pts <- as.data.frame(scores(gp.cmdscale, display = "sites"))
gp.cmdscale.pts$SampleType<-gp.sp.edit$SampleType
unlist(lapply(gp.cmdscale.pts$SampleType, function(x) col.table[x %in% col.table$SampleType, "Col"]))

gp.cmdscale.pts$Col<-c(rep("#F50583", 3), rep("#05E9E9", 2), 
											 rep("#26F605", 3), rep("#2B27EF", 3), rep("#9E025D", 3))

#gp.envfit.plot<-as.matrix(gp.envfit$vectors$r)

gp.envfit.v <- as.data.frame(scores(gp.envfit, display = "vectors"))
gp.envfit.v <- cbind(gp.envfit.v, var = rownames(gp.envfit.v))
#spp.scrs <- spp.scrs[names(gp.envfit.plot[gp.envfit.plot>0.7,]),]

p <- ggplot(gp.cmdscale.pts) +
	geom_point(mapping = aes(x = Dim1, y = Dim2, colour = SampleType)) +
	coord_fixed() + ## need aspect ratio of 1!
	geom_segment(data = gp.envfit.v,
							 aes(x = 0, xend = Dim1, y = 0, yend = Dim2),
							 arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
	geom_text(data = gp.envfit.v, aes(x = Dim1, y = Dim2, label = var),
						size = 3)

p
#still not super legable, but use it for now

# ordiplot(scores(gp.cmdscale)[,c(1,2)], type="t", main="PCoA with 'species'")
# abline(0,0, h=0, v=0, col="grey", lty=2)
# #text()
# plot(gp.envfit, choices=c(1,2), col="blue", p.max=0.002, cex=0.7)

#subset points to plot by what is selected?
test<-read.table("C:/Users/asus4/Documents/EarthMicrobiomeProject/R/EMP_GIS_query_tool/gp_sp.txt")
rownames(test)
test<-rownames(test[(test[,"wc.prec"] >20 & test[,"wc.prec"] < 30), ])

test2<-gp.cmdscale.pts[which(rownames(gp.cmdscale.pts)%in% test), ]

ggplot(test2) +
	geom_point(mapping = aes(x = Dim1, y = Dim2, colour = SampleType)) +
	coord_fixed() #+ ## need aspect ratio of 1!
	geom_segment(data = gp.envfit.v,
							 aes(x = 0, xend = Dim1, y = 0, yend = Dim2),
							 arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
	geom_text(data = gp.envfit.v, aes(x = Dim1, y = Dim2, label = var),
						size = 3)

#reduce size of gp.sp
gp.sp<-as.data.frame(gp.sp)
colnames(gp.sp)

gp.sp2<-gp.sp[,c(1:8, 28, 31, 33:34, 43, 45:46)]
head(gp.sp2)
write.table(gp.sp2, "gp_sp.txt")
colnames(gp.sp2)
write.table(as.data.frame(gp.sp), "gp_sp.txt")
colnames(as.data.frame(gp.sp))
class(gp.sp)

#add in marspec data
#biogeo08 is mean annual sea surface salinity 
#http://people.duke.edu/~mej14/MARSPEC/Data_files/Table1.pdf
marspec<-raster("C:/Users/asus4/Documents/GIS/Data/marspec/MARSPEC_2o5m/Core_Variables_2o5m/biogeo08_2o5m")
plot(marspec)

#got a color blind friendly pallete and ggplot2 tutorial here
#http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p <- ggplot(gp.cmdscale.pts[1:3,]) +
	geom_point(mapping = aes(x = Dim1, y = Dim2, colour = SampleType)) +
	scale_colour_manual(values=c("Freshwater"= "#56B4E9", "Freshwater (creek)" = "#009E73", 
															 " Ocean" = "#0072B2", "Sediment (estuary)" = "#CC79A7", 
															 "Soil" = "#E69F00")) +
	coord_fixed() + ## need aspect ratio of 1!
	geom_segment(data = gp.envfit.v,
							 aes(x = 0, xend = Dim1, y = 0, yend = Dim2),
							 arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
	geom_text(data = gp.envfit.v, aes(x = Dim1, y = Dim2, label = var),
						size = 3);
print(p)

par(mar=c(0.1, 0.1, 0.1, 0.1))
plot(wc_tmean_crop, box=FALSE, axes=FALSE)
plot(gp.spInput(), col="red", pch=20, cex=2, add=TRUE, box=FALSE)

#should make a color pallete table for plotting 
levels(as.data.frame(gp.sp)$SampleType)
plot(data.frame(x=1:8, y=1:8), col=cbPalette, pch=20)

col.table<-data.frame(SamplyType=levels(as.data.frame(gp.sp)$SampleType), 
											col=c("#56B4E9", "#009E73", "#0072B2", "#CC79A7", "#E69F00"))

col.table[col.table$SamplyType %in% gp.sp$SampleType, "col"]
unlist(lapply(gp.sp$SampleType, function(x) col.table[col.table$SamplyType %in% x, "col"]))

gp.sp$SampleType