#####
# 
# CMC Evaluation Framework - GUI edition
# 
# Author: Janne Riihijärvi (Institute for Networked Systems, RWTH Aachen University)
#
# Description: Loads needed data sets and launches the Shiny server at port 7777 on localhost.
#
#####

require(shiny)
require(shinythemes)

load("CMC_demo_datasets.Rdata")
load("WorldPopulation.Rdata")

runApp(launch.browser = FALSE, port = 7777, host = "0.0.0.0")