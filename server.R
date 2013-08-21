library(shiny)
require(phyloseq)
library(sp)
library(rgdal)
library(raster)
library(maptools)

#import GlobalPatterns
data(GlobalPatterns)
#subset
gp<-subset_samples(GlobalPatterns, rownames(sample_data(GlobalPatterns)) %in% rownames(sample_data(GlobalPatterns)[c(1:3,11:21), ]))
#may not need these

#import UniFrac distance matrix of subset samples
gp.dist<-as.dist(read.table("C:/Users/asus4/Documents/EarthMicrobiomeProject/R/EMP_GIS_query_tool/gp_dist.txt"))
#import sample data with GIS environmental variables added
gp.sp<-read.table("C:/Users/asus4/Documents/EarthMicrobiomeProject/R/EMP_GIS_query_tool/gp_sp.txt")
coordinates(gp.sp) = c("Longitude", "Latitude")

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
								"wc_prec_crop" = wc_prec_crop, 
								"wc_tmean_crop" = wc_tmean_crop, 
								"npp_crop" = npp_crop,
					 wc_tmean_crop)
})
	
	#return text for printing caption for map
	#output$caption<- renderText({
	#	formulaText()
	#})
	
	#generate plot of requrest raster
	output$gp.spPlot<- renderPlot({
		rast<-rastInput()
		
		plot(rast)
		#points(coordinates(gp.sp), col="red", pch=20)
	})
	
})

