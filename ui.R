library(shiny)
library(phyloseq)
library(sp)
library(rgdal)
library(raster)
library(maptools)

emp.logo<-"C:/Users/asus4/Documents/EarthMicrobiomeProject/EMP-logos/EMP-green-small.png"
#define user interface

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
		#use slider to subset sample data
		numericInput("col1.min", "Minimum Value:", 0),
		numericInput("col1.max", "Maximum Value:", 20),
		br(),
# 		selectInput("co2", "Variable:", 
# 								list("Mean Annual Precipitation"="wc.prec", 
# 										 "Mean Annual Temperature" = "wc.tmean", 
# 										 "Net Primary Productivity" = "npp")), 
# 		sliderInput("range", "Range:", 
# 								min= 0, max= 100, value= c(45, 55)),
# 		br(),
# 		selectInput("co3", "Variable:", 
# 								list("Mean Annual Precipitation"="wc.prec", 
# 										 "Mean Annual Temperature" = "wc.tmean", 
# 										 "Net Primary Productivity" = "npp")), 
# 		sliderInput("range", "Range:", 
# 								min= 0, max= 100, value= c(45, 55)),
# 	br(),
	selectInput("rast", "Plot Raster:", 
							choices=c("Mean Annual Temperature",
												"Mean Annual Precipitation", 
									 			"Net Primary Productivity"))
	),
	#plot map with points 
	mainPanel(
		tabsetPanel(
			tabPanel("Map", plotOutput("gp.spPlot")),
			tabPanel("PCoA", plotOutput("gp.ordPlot")),
			tabPanel("Attribute Table", tableOutput("gp.tablePrint"))
			#tabPanel("test", verbatimTextOutput("test"))
		)
	)
	))