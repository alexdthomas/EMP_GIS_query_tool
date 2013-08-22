library(shiny)
require(phyloseq)
library(sp)
library(rgdal)
library(raster)
library(maptools)

#define user interface

shinyUI(pageWithSidebar(
	
	#Applicatoin title
	headerPanel('header'), 
	
	#Sidebar allows querying conditions of different variables 
	sidebarPanel(
		#select column variable in the sample data 
		selectInput("col1", "Subset Points By:", 
								list("Mean Annual Precipitation"="wc.prec", 
										 "Mean Annual Temperature" = "wc.tmean", 
										 "Net Primary Productivity" = "npp")), 
		#use slider to subset sample data
		numericInput("col1.min", "Minimum Value:", 20),
		numericInput("col1.max", "Maximum Value:", 30),
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
							choices=c("Mean Annual Precipitation", 
									 "Mean Annual Temperature", 
									 "Net Primary Productivity"))
	),
	#plot map with points 
	mainPanel(
		tabsetPanel(
			tabPanel("Map", plotOutput("gp.spPlot")),
			tabPanel("PCoA", plotOutput("gp.ordPlot")),
			tabPanel("test", verbatimTextOutput("test"))
		)
	)
	))