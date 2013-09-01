require(shiny)
require(phyloseq)
require(sp)
require(rgdal)
require(raster)
require(maptools)
require(ggplot2)
require(grid)
require(vegan)

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

#set up for the ordination plot
gp.cmdscale<-cmdscale(gp.dist, nrow(gp.veganotu)-1, eig=TRUE)
gp.sp.edit<-gp.sp
gp.sp.edit<-gp.sp.edit[,c(1:8, 28, 31, 33:34, 43, 45:46)]
colnames(gp.sp.edit)[8:15]<-c("Elev.", "Precip.", "Temp.", "Long.", "Lat.", "pH", "Salinity", "NPP")
gp.envfit<-envfit(gp.cmdscale, gp.sp.edit)

gp.cmdscale.pts <- as.data.frame(scores(gp.cmdscale, display = "sites"))
gp.cmdscale.pts$SampleType<-gp.sp.edit$SampleType

gp.envfit.v <- as.data.frame(scores(gp.envfit, display = "vectors"))
gp.envfit.v <- cbind(gp.envfit.v, var = rownames(gp.envfit.v))

#set color palette
col.table<-data.frame(SamplyType=levels(as.data.frame(gp.sp)$SampleType), 
											col=c("#00F0F0", "#03FFAC", "#2300FF", "#F000C2", "#FF0D00"))
#define server
shinyServer(function(input, output){
	rastInput <-reactive({
		switch(input$rast,
					 "Mean Annual Precipitation" = wc_prec_crop,
					 "Mean Annual Temperature" = wc_tmean_crop, 
					 "Net Primary Productivity" = npp_crop)
})
	
	#subset the global patterns environmental data frame by user input
	gp.sub<-reactive({
		gp.sp.sub<-gp.sp[(gp.sp[,input$col1] >= input$col1.min & 
							        gp.sp[,input$col1] <= input$col1.max), ]
	})
	
	#sub names by user input
	gp.subNames<-reactive({
		gp.sp.sub<-rownames(gp.sp[(gp.sp[,input$col1] >= input$col1.min & 
												gp.sp[,input$col1] <= input$col1.max), ])
	})
	
	#create SpatialPointsDataFrame by user input
	gp.spInput<-reactive({
		gp.sp.sub<-gp.sp[(gp.sp[,input$col1] >= input$col1.min & 
											gp.sp[,input$col1] <= input$col1.max), ]		
		
		#convert to spatial points data frame
		gp.sp.sub<-SpatialPointsDataFrame(gp.sp.sub[,c("Longitude", "Latitude")], 
																			proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
																			data=gp.sp.sub)
		})
	
	#get map colors
	gp.mapCol<-reactive({
		Col<-gp.sp[(gp.sp[,input$col1] >= input$col1.min & 
												gp.sp[,input$col1] <= input$col1.max), ]
		
		Col<-as.character(unlist(lapply(Col$SampleType, function(x) col.table[col.table$SamplyType %in% x, "col"])))
	
	})
		#subset the points plotted in ordination by user input
		gp.ordInput<-reactive({
			gp.ord.sub<-gp.cmdscale.pts[which(rownames(gp.cmdscale.pts)%in% print(gp.subNames())), ]
			
		})
	

	
	#generate plot of requested raster and selected points
	output$gp.spPlot<- renderPlot({
		par(mar=c(0.1, 0.1, 0.1, 0.1))
			plot(rastInput(), box=FALSE, axes=FALSE)
			plot(gp.spInput(), col=gp.mapCol(),
					 pch=20, cex=2, add=TRUE, box=FALSE)
	})
	
	#generate ordination plot with subset points
	output$gp.ordPlot<-renderPlot({
		p <- ggplot(gp.ordInput()) +
			geom_point(mapping = aes(x = Dim1, y = Dim2, colour = SampleType)) +
			scale_colour_manual(values=c("Freshwater"= "#00F0F0", "Freshwater (creek)" = "#03FFAC", 
																	 "Ocean" = "#2300FF", "Sediment (estuary)" = "#F000C2", 
																	 "Soil" = "#FF0D00")) +	
			coord_fixed() + ## need aspect ratio of 1!
			geom_segment(data = gp.envfit.v,
									 aes(x = 0, xend = Dim1, y = 0, yend = Dim2),
									 arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
			geom_text(data = gp.envfit.v, aes(x = Dim1, y = Dim2, label = var),
								size = 3);
		print(p)
	})
	
	#generate table of attributes for selected points
	output$gp.tablePrint<-renderTable({
		gp.sub()
	})
	
	#test stuff in app
# 	output$test<-renderPrint({
# 		test<-gp.ordInput()
# 		print(test)
# 	})
})

