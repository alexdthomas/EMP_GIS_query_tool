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
# 		selectInput("co1", "Variable:", 
# 								list("Mean Annual Precipitation"="wc.prec", 
# 										 "Mean Annual Temperature" = "wc.tmean", 
# 										 "Net Primary Productivity" = "npp")), 
# 		#use slider to subset sample data
# 		sliderInput("range", "Range:", 
# 								min= 0, max= 100, value= c(45, 55)),
# 		br(),
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
	selectInput("rast", "Plot Data:", 
							choices=c("Mean Annual Precipitation", 
									 "Mean Annual Temperature", 
									 "Net Primary Productivity"))
	),
	#plot map with points 
	mainPanel(
		#h3(textOutput("caption")), 
		
		plotOutput("gp.spPlot")
	)
	))