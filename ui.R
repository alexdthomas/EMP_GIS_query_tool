library(shiny)
library(phyloseq)
library(sp)
library(rgdal)
library(raster)
library(maptools)

#set up page with side bar
shinyUI(pageWithSidebar(
	
	#Applicatoin title
	headerPanel('Earth Microbiome Project Explorer'), 
	
	#Sidebar allows querying conditions of different variables 
	sidebarPanel(
		#select column variable in the sample data 
		selectInput("col1", "Subset Points By:", 
								list("Mean Annual Temperature" = "wc.tmean",
										 "Mean Annual Precipitation"="wc.prec",  
										 "Net Primary Productivity" = "npp")), 
		#use slider to subset sample data, default 0 and 20
		numericInput("col1.min", "Minimum Value:", 0),
		numericInput("col1.max", "Maximum Value:", 20),
		br(),
		
	#select raster do display
	selectInput("rast", "Plot Raster:", 
							choices=c("Mean Annual Temperature",
												"Mean Annual Precipitation", 
									 			"Net Primary Productivity"))
	),
	#set up tabs for map, PCoA and attribute table 
	mainPanel(
		tabsetPanel(
			tabPanel("Map", plotOutput("gp.spPlot")),
			tabPanel("PCoA", plotOutput("gp.ordPlot")),
			tabPanel("Attribute Table", tableOutput("gp.tablePrint"))
			#tabPanel("test", verbatimTextOutput("test"))
		)
	)
	))