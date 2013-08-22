require(shiny)
require(phyloseq)
require(sp)
require(rgdal)
require(raster)
require(maptools)

#import GlobalPatterns
data(GlobalPatterns)
#subset
gp<-subset_samples(GlobalPatterns, rownames(sample_data(GlobalPatterns)) %in% rownames(sample_data(GlobalPatterns)[c(1:3,11:21), ]))
#may not need these

#import UniFrac distance matrix of subset samples
gp.dist<-as.dist(read.table("C:/Users/asus4/Documents/EarthMicrobiomeProject/R/EMP_GIS_query_tool/gp_dist.txt"))
#import sample data with GIS environmental variables added
gp.sp<-read.table("C:/Users/asus4/Documents/EarthMicrobiomeProject/R/EMP_GIS_query_tool/gp_sp.txt")

#import rasters to plot
wc_tmean_crop<-raster("C:/Users/asus4/Documents/EarthMicrobiomeProject/R/EMP_GIS_query_tool/wc_tmean_crop")
wc_prec_crop<-raster("C:/Users/asus4/Documents/EarthMicrobiomeProject/R/EMP_GIS_query_tool/wc_prec_crop")
npp_crop<-raster("C:/Users/asus4/Documents/EarthMicrobiomeProject/R/EMP_GIS_query_tool/npp_crop")

#checked in clean environement and this works
#gp.ord<-ordinate(gp, "MDS", as.dist(gp.dist))
#plot_ordination(gp, gp.ord, type="samples")

#define server
shinyServer(function(input, output){
	rastInput <-reactive({
		switch(input$rast,
					 "Mean Annual Precipitation" = wc_prec_crop,
					 "Mean Annual Temperature" = wc_tmean_crop, 
					 "Net Primary Productivity" = npp_crop)
})
	
	#subset the global patterns environmental data frame by user input
	gp.spInput<-reactive({
		gp.sp.sub<-gp.sp[(gp.sp[,input$col1] > input$col1.min & 
											gp.sp[,input$col1] < input$col1.max), ]
		#convert to a SpatialPointsDataFrame
		#coordinates(gp.sp.sub) = c("Longitude", "Latitude")
		#proj4string(gp.sp.sub)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
		#gp.sp.sub<-spTransform(gp.sp.sub, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
		gp.sp.sub<-SpatialPointsDataFrame(gp.sp.sub[,c("Longitude", "Latitude")], 
																			proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
																			data=gp.sp.sub)
		})
	#do this outside of reactive?
	#coordinates(gp.spInput) = c("Longitude", "Latitude")
	#proj4string(gp.spInput)<- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
	
	
	#try to see what is happening to this data frame when it is subset
	output$gp.sp.head<- renderPrint({
		head(gp.spInput())
	})
	
	#return text for printing caption for map
	#output$caption<- renderText({
	#	formulaText()
	#})
	
	#generate plot of requrest raster
	output$gp.spPlot<- renderPlot({
		#coordinates(gp.spInput) = c("Longitude", "Latitude")
		#proj4string(gp.spInput)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
			plot(rastInput())
			plot(gp.spInput(), col="red", pch=20, cex=2, add=TRUE)
	})
	
	
	
})

