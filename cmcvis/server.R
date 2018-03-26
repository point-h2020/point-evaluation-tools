#####
# 
# CMC Evaluation Framework - GUI edition
# 
# Author: Janne Riihijärvi (Institute for Networked Systems, RWTH Aachen University)
#
# Description: Server side logic for providing data to the GUI to visualize
#
#####

require(VGAM, quietly = TRUE)
require(spatstat, quietly = TRUE)
require(igraph, quietly = TRUE)
require(Cairo, quietly = TRUE)

# Use Cairo for improved graphics quality
options(shiny.usecairo=T)

# The main shiny server function
shinyServer(function(input, output) {

	# Switch between the three preloaded data sets
	datasetInput <- reactive({
    	switch(input$dataset,
           	"ATT US Core" = dataATT,
           	"Bell Canada" = dataBell,
           	"Geant" = dataGeant)
  	})

	# Set the size of the plots produced as output  	  	
  	plotWidth <- 800
  	plotHeight <- 750
  	
  	# Plot the network topology together with the underlying world population data 
  	output$topology <- renderPlot({ plot_dataset(datasetInput()$topo, datasetInput()$pop) }, width = plotWidth, height = plotHeight)

	# Plot current realization of MC tree created by simulate_round()
  	output$tree <- renderPlot({ simulate_round(datasetInput()$topo, datasetInput()$pop, input$users, input$items, input$alpha, do.plot = TRUE) },
  							  width = plotWidth, height = plotHeight)

	# Plot histogram of CMC gains from Monte Carlo simulation done by simulate_n_rounds()
  	output$gains <- renderPlot({ df <- simulate_n_rounds(input$n, datasetInput()$topo, datasetInput()$pop, input$users, input$items, input$alpha) ;
  								 plot(hist(df$gain), xlab = "Multicast gain", main = "", cex.axis = 1.25, cex.lab = 1.25) },
  								 width = plotWidth, height = plotHeight)
})

#
# Plots the network topology overlaid on top of world population density, with area of the vertices being proportional to
# the total population weight in the service area.
# 
# Arguments:
# 
# topology: 		list with elements G (igraph giving network structure) and pp (spatstat ppp object giving vertex locations)
# pops_with_SAs: 	list with elements SAs (spatstat tessellation of service areas) and pops (numeric vector of population weights)
# 

plot_dataset <- function(topology, pops_with_SAs) {
	# Get world population as an image with logarithmic color scale
	Image <- WorldPopulation[boundingbox(topology$pp$window), drop = FALSE, tight = TRUE]
	Image$v <- log(Image$v + 1)

	# Approximate an equal area projection for plotting
	yscale <- abs(1/cos(2*pi*median(topology$pp$y)/360))
	
	# Plot the population data and prepare topology$pp for plotting
	plot(affine(Image, mat = diag(c(1, yscale))), main = "", ribbon = FALSE)
	pp <- affine(topology$pp, mat = diag(c(1, yscale)))
	marks(pp) <- (pops_with_SAs$pops)^0.5
		

	# First plot the edges of topology$G
	Connections <- ends(topology$G, E(topology$G))
	
	for (i in 1:length(Connections[, 1])) {
		p1 <- pp[Connections[i, 1]]
		p2 <- pp[Connections[i, 2]]
		lines(c(p1$x, p2$x), c(p1$y, p2$y), col = "white", lwd = 1.5)
	}

	# Then add vertices
	plot(pp, add = TRUE, bg = "white", maxsize = 4)
}

#
# Creates one Monte Carlo realisation of the CMC tree
# 
# Arguments:
# 
# topology: 		list with elements G (igraph giving network structure) and pp (spatstat ppp object giving vertex locations)
# pops_with_SAs: 	list with elements SAs (spatstat tessellation of service areas) and pops (numeric vector of population weights)
# total_users:		mean number of users per synchronicity interval
# num_items:		size of the content catalogue
# alpha:			power law exponent of the content popularity decay of the catalogue
# do.plot:			Boolean indicating whether to plot the tree and statistics
#
# Returns: 			list with shortest paths from the server to the requesting clients, CMC group size and load, equivalent unicast load, and
#					number of users per service area
#

simulate_round <- function(topology, pops_with_SAs, total_users, num_items, alpha, do.plot = FALSE) {
	users <- rand_users_from_content_popularity_pareto(total_users, num_items, alpha)
	
	users_per_sa <- assign_users_for_timeslot(pops_with_SAs$pops, users)
	
	to_remove <- users_per_sa == 0

	server <- sample(V(topology$G), 1)

	paths <- shortest_paths(topology$G, from = server, to = V(topology$G)[which(users_per_sa > 0)])

	g1 <- topology$G
	E(g1)$color <- "grey"
	
	for (p in paths$vpath) {
		E(g1, path=p)$color <- "red"
	}
	
	if (do.plot) {
		g1 <- set_vertex_attr(g1, "label", value = as.character(users_per_sa))
		g1 <- set_vertex_attr(g1, "color", which(users_per_sa == 0), "gray")
		g1 <- set_vertex_attr(g1, "color", which(users_per_sa > 0), "white")
		g1 <- set_vertex_attr(g1, "color", index = server, value = "green")
		plot(g1, layout = cbind(topology$pp$x, topology$pp$y), vertex.size = 9.5, vertex.label.cex = 0.95, edge.width = 1.5)
	}
	
	mc_load <- sum(get.edge.attribute(g1, "color") == "red")
	uc_load <- sum((unlist(lapply(paths$vpath, length)) - 1) * users_per_sa[users_per_sa > 0])
	mc_group_size <- sum(users_per_sa > 0)
	
	xmax <- max(topology$pp$x)
	ymax <- max(topology$pp$y)

	if (uc_load == 0)
		mc_gain <- 1
	else
		mc_gain <- round(uc_load/mc_load, digits = 1)
	
	if (do.plot)
		text(0.8, 0.9, labels = paste("Unicast load: ", uc_load, "\n", "Multicast load: ",
		                              mc_load, "\n", "Gain: ", mc_gain, sep = ""), cex = 1.25)
	
	list(paths = paths, mc_group_size = mc_group_size, mc_load = mc_load, uc_load = uc_load, users_per_sa = users_per_sa)
}

#
# Simulates n CMC trees and collects statistics into a data frame
# 
simulate_n_rounds <- function(n, topology, pops_with_SAs, total_users, num_items, alpha) {
	gain <- numeric(length = n)
	group_size <- numeric(length(n))
	
	for (i in 1:n) {
		out <- simulate_round(topology, pops_with_SAs, total_users, num_items, alpha)
		if (out$uc_load == 0 & out$mc_load == 0) {
			gain[i] <- 1
		} else {
			gain[i] <- out$uc_load / out$mc_load			
		}
		group_size[i] <- out$mc_group_size
	}
	
	data.frame(gain = gain, group_size = group_size, total_users = rep(total_users, n), num_items = rep(num_items, n), alpha = rep(alpha, n))
}

# Draw a sample from pareto distributed number of users per content item
rand_users_from_content_popularity_pareto <- function(total_users, num_items, alpha) {
	if (num_items == 1)
		return(total_users)
		
	sample(total_users * dpareto(1:num_items, shape = alpha),
	       size = 1,
	       prob = dpareto(1:num_items, shape = alpha))/sum(dpareto(1:num_items, shape = alpha))
}

# Draw Poisson distributed user counts per network location provided there are total_users in total in the system
assign_users_for_timeslot <- function(pop_per_sa, total_users) {
	total_pop <- sum(pop_per_sa)

	unlist(lapply(pop_per_sa, function (pop) { rpois(1, round(total_users * (pop/total_pop)))}))
}





