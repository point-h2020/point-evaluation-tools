#  graphs.R set of network graph functions
# Copyright (C) 2017-2018  Mays AL-Naday
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' Load Zoo graphs
#'
#' @description Load list of zoo graphs, if stored. otherwise, read, store then load
#'
#' @author Mays AL-Naday
#' @param read.path path to read graph objects in either gml or graphml format
#' @param write.path path to write the R list of all read graphs:node_population object
#' @param extension graph format (zoo provides either 'gml' or 'graphml')
#' @return list of \code{{g, u}} objects of the zoo list of topology files. \code{g} graphNEL, \code{u} nodes population
#' @seealso \code{\link{GraphImportZoo}}
GraphLoadZoo <- function(read.path, write.path, ext = "graphml")
{
  #obtain the network graph
  tryCatch(load(file=paste0(write.path, "zoo.rda")),
           error=function(e) {
             cat("model: zoo graphs are not stored, import graph objects from:", read.path, "\n");
             GraphImportZoo(read.path = read.path, write.path = write.path, ext = ext)
           })
  load(file = paste0(write.path, "zoo.rda"))
  zoo
}
#' Import Zoo topologies
#'
#' @description Import Zoo topology graphs and extrapolate node population of each node in every graph, based on the WorldPopulation
#'
#' @author Mays AL-Naday
#' @param read.path path to read graph objects in either gml or graphml format
#' @param write.path path to write the R list of all read graphs:node_population object
#' @param extension graph format (zoo provides either 'gml' or 'graphml')
#' @return list of \code{{g, u}} objects of the zoo list of topology files. \code{g} graphNEL, \code{u} nodes population
#' @seealso \code{\link{GraphLoadZoo}}
GraphImportZoo <- function(read.path, write.path,  ext = "graphml")
{
  load(file = paste0(read.path, 'wp/WorldPopulation.Rdata'))
  pattern = paste0("*.", ext)
  files <- list.files(read.path, pattern = pattern)
  files <- paste0(read.path, files)
  print(files)
  graphs <- lapply(files, JarGraphGetNetworkTopology)
  graphs <- lapply(graphs, function(graph) {graph$g <- as.directed(graph$g); return(graph)})
  graphs <- graphs[-135] #troubled graph, causes Error x+1: invalid operation for non-binary
  graphs <- lapply(graphs, function(graph) {graph$g <- igraph.to.graphNEL(graph$g); return(graph)})
  graphs <- lapply(graphs, function(g, WorldPopulation){
    population <- JarGraphGetPopulationPerServiceArea(g, WorldPopulation)$pops;
    print(length(population));
    return(list(g = g$g, u = population))},
    WorldPopulation = WorldPopulation
    )
  #filter out graphs where population is NULL
  is.valid <- unlist(lapply(graphs, function(x){length(x$u[!(is.na(x$u))]) == numNodes(x$g)}))
  graphs <- graphs[is.valid]
  files <- files[is.valid]
  # print(graphs)
  zoo <- list(files=files, graphs=graphs)
  save(zoo, file = paste0(write.path, 'zoo.RData'))
  print("zoo graphs saved")
  zoo
}
#' Augument access network as a balanced tree
#' @description Augument a core network topology that has population associated with it into access/core network.
#' The augumentation takes the population associated with a core node and use it to create a k-ary tree access network
#' other design parameters that define the tree is \code{k} the arity parameter (i.e. the max number of children for each parent in the tree), and
#' \code{m} the max number of users to be associated with any leaf node.
#'
#' @author Mays AL-Naday
#' @param read.path path to read list of zoo graph object(s) of core node(s). This can be the imported zoo graphs using GraphImportZoo
#' @param write.path path to write the R list of all augumented graphs. filename is azoo.RData
#' @param augument.params \code{k} the tree-arity parameter defining the number of childer per parent. \code{m} maximum number of users that can be handled by a leaf node
#' @return \code{files} list of file names of zoo topologies that have been successfuly augumented.
#' @return \code{graphs} list of \code{g} the core/access augumented graph. \code{u} the number of active users of every node in the augument graph, core and aggregation nodes have \code{0} population as they don't have users attached to them. \code{access} list of graph objects of all the access trees attached to the core graph.
#' @seealso \code{\link{GraphLoadZoo}}, \code{\link{GraphImportZoo}}
GraphAugumentTreeAccessZoo <- function(read.path , write.path, augument.params = c(k = 20, m = 1000))
{
  zoo <- GraphLoadZoo(read.path = read.path, write.path = write.path)
  graphs <- zoo$graphs
  files <- zoo$files
  augumented.graphs <- lapply(graphs, GraphAugumentTreeAccessNetwork, k = augument.params['k'], m = augument.params['m'])
  azoo <- list(files=files, graphs=augumented.graphs)
  save(azoo, file = paste0(write.path, 'azoo.RData'))
  print("azoo graphs saved")
  azoo
}
#' convert igraph to graphNEL
#'
#' @author Mays AL-Naday
#' @param g igraph object
#' @param directed boolean \code{0} undirected, \code{1} directed graph
#' @return graphNEL object
IgraphToGraphNEL <- function(g, directed = 1) {
  convert.graph <- function(directed)
  {
    gn <- switch(directed,
                igraph::as.directed(g, mode = "mutual"),
                igraph::as.undirected(g, mode="mutual")
    )
  }
  g <- convert.graph(directed)
  gn <- igraph::igraph.to.graphNEL(g)
}
#' Convert a graphNEL to an edge matrix
#'
#' @description converts graphNEL to an edge matrix in the format of \code{{from, to}}
#' @param g: a graphNEL object
#' @return Edge Matrix
GraphToEdgeMatrix <- function(g) {
  em <- t(graph::edgeMatrix(g))
  em <- cbind(em, ri = c(1:nrow(em)))
  em
}
#' assing popularity to graph nodes
#' @description assign popularity metirc to each node based on a node property, e.g. closeness of node
#'
#' @param g graph object of type igraph
#' @return dataframe of (node, popularity)
NodePopularityCloseness <- function(g) {
  p <- igraph::closeness(g)
  n <- as.integer(names(p))
  r <- data.frame(n,p)
  r
}

#' Set capacity on graph links
#' @description Assign randomly generated capacity values to the edge matrix of a graph. Notice the matrix must have a column \code{capacity} already
#'
#' @param eM edge matrix, possibly generated using GraphToEdgeMatrix(g), including a column with column name \code{capacity}
#' @param min minimum capacity value on any edge
#' @param max maximum capacity on any edge
#' @return edge matrix with capacity set randomly between min and max
SetCapacityValueToEdgeMatrix <- function(eM, min = 5, max = 10) {
  num <- nrow(eM)
  eC <- trunc(runif(num, min, max))
  eM[,"capacity"] <- eC
  eM
}
#' Extend the edgeMatrix of a graph with a set of edge attributes
#'
#' @param eM edge matrix, possibly generated using GraphToEdgeMatrix(g)
#' @param attrs a character vector of the attributes to be added to the edge matrix (or the graph in that sense)
#' @return updated edge matrix including added columns, one for each of the attributes of \code{attrs}
AssignAttrsToEdgeMatrix <- function(eM, attrs=c("capacity", "weight", "pheromon", "rphermon")) {
  rnum <- nrow(eM)
  cnum <- length(attrs)
  newEM <- matrix(0, rnum, cnum)
  colnames(newEM) <- attrs
  newEM <- cbind(eM, newEM)
  newEM
}
#' Convert graph to edge matrix with edge attributes
#'
#' @param g network graph
#' @param attrs vector of strings, listing the edge attributies
#' @return edge matrix with added columns for edge attributes
GraphToEdgeMatrixWithAttributes <- function(g, attrs=c("capacity", "load", "weight"))
{
  eM <- GraphToEdgeMatrix(g)
  eM <- AssignAttrsToEdgeMatrix(eM, attrs = attrs)
  eM
}
#' Set the edge attribute of graph to value
#'
#' @param eM edge matrix containing graph attribute to be set
#' @param val value to set the edge attribute, accepts a vector but cannot be NULL, NA or NaN
#' @return Edge matrix with attribue set to val
GraphSetEdgeAttr <- function(eM, val = 1.0, attr) {
  if(missing(attr))
  {
    cat('Error: attribute not specified, exiting...\n')
    return(-1)
  }
  eM[,attr] <- val
  eM
}
#' Convert graph to N x N adjacency matrix where nxm cell gives the load on edge n->m
#'
#' @param g network graph
#' @param attrs vector of strings, listing the edge attributies
#' @return edge matrix with added columns for edge attributes
GraphToAdjMatrix <- function(g)
{
  methods::as(g, 'matrix')
}
#' Set the edge load in a NxN adjacency matrix
#'
#' @param eM adjaceny matrix
#' @param val value to set the edge load, accepts a vector but cannot be NULL, NA or NaN
#' @return adjacency matrix with load set to val
GraphSetLoadOnAdjMatrix <- function(eM, val = 1.0, attr) {
  eM[eM > 0] <- val
  eM
}
JarGraphGetNetworkTopology <- function(filename, ext = "graphml") {
  G <- igraph::read.graph(filename, format = c(ext))
  to_remove <- is.na(igraph::get.vertex.attribute(G, "Latitude")) | is.na(igraph::get.vertex.attribute(G, "Longitude"))
  G <- igraph::delete.vertices(G, V(G)[to_remove])
  x <- igraph::get.vertex.attribute(G, "Longitude")
  y <- igraph::get.vertex.attribute(G, "Latitude")
  if(is.null(x) | is.null(y)){
    pp <- NULL
    return(list(g=G, pp=pp))
  }
  W <- spatstat::bounding.box.xy(x, y)
  W <- spatstat::dilation(W, 0.2 * sqrt(spatstat::area.owin(W)))
  pp <- spatstat::ppp(x, y, window = W)
  list(g = G, pp = pp)
}
JarGraphGetPopulationPerServiceArea <- function(topology, WorldPopulation) {
  SAs <- NULL
  pops <- NULL
  tryCatch(SAs <- spatstat::dirichlet(topology$pp),
           error = function(e) {return(list(SAs = NULL, pops = NULL))}
           )
  if(!is.null(SAs)){
    pops <- unlist(lapply(tiles(SAs), function(tile) { sum(WorldPopulation[spatstat::intersect.owin(topology$pp$window, tile)]) } ))
  }
  #scale down factor to get realistic
  pops <- pops * 0.005
  list(SAs = SAs, pops = pops)
}
#' Augument Access network in the form of a k-ary tree, with {k,h} parameters.
#'
#' @param gu \code{g} graph object of the core network, to be augumented. \code{u} a numeric vector of the number of active users per each core node.
#' @param k tree-arity, i.e. the maximum number of children each node can have
#' @param m numeric value defining the number of users per access node, u/m gives the total number of leafs
#' @return augument graph with access and core network nodes
GraphAugumentTreeAccessNetwork <- function(gu, k, m)
{
  au <- c()
  ag <- list()
  access <- list()
  u <- gu$u
  u <- u[u > 0] * 0.005
  g <- gu$g
  N <- names(u)
  l <- length(N)
  for(n in N)
  {
    # cat("n: ", n, " u[", n, "]: ", u[n], "\n")
    a.net <- GraphCreateTreeNetwork(l, u[n], m, k)
    ag$g <- graph::graphNEL(nodes = a.net$nodes, edgemode = "directed")
    ag$g <- graph::addEdge(from=a.net$edges[,1], to=a.net$edges[,2], ag$g)
    ag$u <- a.net$u
    access[[n]] <- ag
    au <- c(au, a.net$u)  #updated popultion vector for the core/access augumented graph
    #add nodes to the graph
    g <- graph::join(g, ag$g)
    l <- l + length(a.net$nodes) -1
  }
  retL <- list(g = g, u = au, access = access)
  retL
}
#' Create a k-ary tree access network for a set of active users
#'
#' @param n the number of nodes in the graph. used as a starting index for numbering the nodes in the access network to avoid node id clash with the core graph
#' @param v a (name,value) number where name is the node id of a core node, and value is the number of active users associated with the node
#' @param c design parameter of the maximum number of users that can be served by an access (leaf) node
#' @param k tree-arity design parameter, i.e. maximum number of childer that can be aggregated by one parent in the tree
#' @return R list of: \code{h}: the hight of the tree, \code{parents}: the number of parents at each level of the tree, \code{edges} edge matrix of the tree graph, \code{nodes} vector of nodes in the tree graph, \code{u} vector of populations per node in the tree, \code{sum(u) == v}. Notice that the core node and parents (i.e. aggregate nodes) have a population of \code{zero}.
GraphCreateTreeNetwork <- function(n, v, c, k)
{
  au <- c()
  a.nodes <- list()
  total.edges <- cbind(from=c(), to=c())
  tree <- list(h=0, parents=NULL, edges=NULL, nodes=NULL)
  loop <- TRUE
  v.name <- names(v)
  leaves <- ceiling(v/c) #number of leave nodes = total population of core node / maximum number of users that can be handled by a node. c is a configuration param.
  h <- 1
  parents <- c(leaves)
  p <- parents
  m <- p + n
  a.nodes[[h]] <- as.character(c((n+1):m))
  au <- rep(0, p)
  nodes.with.max <- trunc(v/c)
  residual <- v %% c
  au[1:nodes.with.max] <- c
  au[p] <- residual
  names(au) <- a.nodes[[h]]
  while(loop)
  {
    n <- n + p
    p <- ceiling(p/k)
    p[p == 0] <- 1
    m <- n + p
    h <- h + 1
    parents[h] <- p
    a.nodes[[h]] <- as.character(c((n+1):m))
    if(p == 1)
    {
      a.nodes[[h]] <- v.name
      loop <- FALSE
    }
    au.h <- rep(0, p)
    names(au.h) <- a.nodes[[h]]
    au <- c(au, au.h)
    from <- rep(a.nodes[[h]], each=k)
    from <- from[1:length(a.nodes[[h-1]])]
    edges <- cbind(from = from, to = a.nodes[[h-1]])
    total.edges <- rbind(total.edges, edges)
  }
  r.edges <- cbind(from = total.edges[,2], to = total.edges[,1])
  total.edges <- rbind(total.edges, r.edges)
  names(parents) <- c((h-1):0)
  parents <- sort(parents)
  tree$parents <- parents
  tree$edges <- total.edges
  tree$nodes <- unlist(a.nodes)
  tree$u <- au
  tree$h <- h-1
  tree
}
#' Augument the graph to add access node to each node with capacity between the original node and the access node = total edge capacity of the original node.
#' This means the access node cannot push to the origin node any more traffic that the sum of capacities on all of its edges.
#'
#' @param g core network graph
#' @param eM core edge Matrix of the graph with capacities
#' @return augumented graph including core + access nodes
#' @return augumented edge Matrix of core + access edges
GraphAugumentAccessNodes <- function(g, eM)
{
  nodes <- graph::nodes(g) # get the nodes of the core graph, these represent the core nodes
  nn <- length(nodes)  #number of nodes is the start index for the added nodes
  ne <- nrow(eM) # number of core edges
  ids <- as.integer(nodes) #get the ids of the nodes for the eM
  a.ids <- seq(nl + 1, 2 * nn, 1) #access nodes: node Ids start from the index of the last core node

  a.nodes <- as.character(a.ids)
  for (i in 1:nl) {
    c.n <- nodes[i]
    a.n <- a.nodes[i]
    c.id <- ids[i]
    a.id <- a.ids[i]
    ae.capacity <- sum(eM[eM[, 'from'] == c.n, 'capacity']) #access <-> core capacity = sum(core edge capacity of the core node)
    g <- graph::addNode(a.n, g)
    g <- graph::addEdge(f = c(a.n, c.n), t = c(c.n, a.n), g, ae.capacity)
    # update the edge Matrix
    a.e <- matrix(0, nrow = 2, ncol = ncol(eM))
    colnames(a.e) <- colnames(eM)
    a.e[ ,'from'] <- c(a.id, c.id)
    a.e[ ,'to'] <- c(c.id, a.id)
    a.e[, 'capacity'] <- ae.capacity #set edge capacity
    a.e[, 'type'] <- 1 #set edge type to 1 (i.e. access), 0 means core
    a.e[, 'ri'] <- c(ne +1, ne+2)
    eM <- rbind(eM, a.e)
    ne <- ne + 2
  }
  r <- list(g = g, eM = eM)
  r
}
