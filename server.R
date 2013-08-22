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
gp.sp.edit<-gp.sp.edit[,c(6:34, 36:46)]
gp.envfit<-envfit(gp.cmdscale, gp.sp.edit)

gp.cmdscale.pts <- as.data.frame(scores(gp.cmdscale, display = "sites"))
gp.cmdscale.pts$SampleType<-gp.sp.edit$SampleType

gp.envfit.v <- as.data.frame(scores(gp.envfit, display = "vectors"))
gp.envfit.v <- cbind(gp.envfit.v, var = rownames(gp.envfit.v))

#define server
shinyServer(function(input, output){
	rastInput <-reactive({
		switch(input$rast,
					 "Mean Annual Precipitation" = wc_prec_crop,
					 "Mean Annual Temperature" = wc_tmean_crop, 
					 "Net Primary Productivity" = npp_crop)
})
	
	#subset the global patterns environmental data frame by user input
	gp.subNames<-reactive({
		gp.sp.sub<-rownames(gp.sp[(gp.sp[,input$col1] >= input$col1.min & 
												gp.sp[,input$col1] <= input$col1.max), ])
	})
	gp.spInput<-reactive({
		gp.sp.sub<-gp.sp[(gp.sp[,input$col1] >= input$col1.min & 
											gp.sp[,input$col1] <= input$col1.max), ]
		
		#convert to spatial points data frame
		gp.sp.sub<-SpatialPointsDataFrame(gp.sp.sub[,c("Longitude", "Latitude")], 
																			proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
																			data=gp.sp.sub)
		})
	
		gp.ordInput<-reactive({
			gp.ord.sub<-gp.cmdscale.pts[which(rownames(gp.cmdscale.pts)%in% print(gp.subNames())), ]
			
		})
	
	#generate plot of requrest raster
	output$gp.spPlot<- renderPlot({
			plot(rastInput())
			plot(gp.spInput(), col="red", pch=20, cex=2, add=TRUE)
	})
	
	#generate ordination plot with subset points
	output$gp.ordPlot<-renderPlot({
		p <- ggplot(gp.ordInput()) +
			geom_point(mapping = aes(x = Dim1, y = Dim2, colour = SampleType)) +
			coord_fixed() + ## need aspect ratio of 1!
			geom_segment(data = gp.envfit.v,
									 aes(x = 0, xend = Dim1, y = 0, yend = Dim2),
									 arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
			geom_text(data = gp.envfit.v, aes(x = Dim1, y = Dim2, label = var),
								size = 3);
		print(p)
	})
	
	output$gp.spTable<-renderTable({
		
	})
	
	#test stuff in app
	output$test<-renderPrint({
		test<-gp.ordInput()
		print(test)
	})
	
})

