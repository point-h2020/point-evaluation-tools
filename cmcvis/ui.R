#####
# 
# CMC Evaluation Framework - GUI edition
# 
# Author: Janne Riihijärvi (Institute for Networked Systems, RWTH Aachen University)
#
# Description: User interface definition for the Shiny GUI
#
#####


shinyUI(fluidPage(theme = shinytheme("journal"), titlePanel(list("CMC Evaluator", HTML('<a href="http://www.inets.rwth-aachen.de"><img src="inets-logo.png" align="right" height="70"/></a>')), windowTitle="CMC Evaluator"), sidebarLayout(
						sidebarPanel(h4("Network model"), selectInput("dataset", "Select dataset:", choices = c("ATT US Core", "Bell Canada", "Geant")),
									 p(), br(),
									 h4("Request model"),
									 sliderInput("users", "Active users in time interval:", value = 35, min = 10, max = 2500),
									 br(),
									 sliderInput("items", "Catalogue size:", value = 9000, min = 1, max = 25000),
									 br(),
									 sliderInput("alpha", "Popularity decay:", value = 1.8, min = 1, max = 5.0, step = 0.1),
									 br(),
									 h4("Simulation parameters"),
									 sliderInput("n", "Number of Monte Carlo iterations:", value = 50, min = 10, max = 1000),
									 br()),
						mainPanel(tabsetPanel(tabPanel("Network model", plotOutput("topology", height = "750px")),
											  tabPanel("Example multicast tree", plotOutput("tree", height = "750px")),
											  tabPanel("Distribution of CMC gains", plotOutput("gains", height = "750px")))))))
											  