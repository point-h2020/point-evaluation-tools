#  algorithms.R provides a set of Pub/Sub based service routing algorithms
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


# Dijkstra ----------------------------------------------------------------

#' Calculate shortest paths
#' @description Calculate all shortest paths, through every neighbour of a node.
#'
#' @export
#' @param g network graph
#' @param d vector of edge weights
AlgoDijNeighbours <- function(g)
{
  D <- edgeWeights(g)
  W <- unlist(D)
  N <- graph::nodes(g)
  c <- length(N)
  p <- array(0, dim=c(c,c,c))
  rownames(p) <- N
  colnames(p) <- N
  dimnames(p)[[3]] <- N
  for(n in N)
  {
    M <- names(D[[n]])
    for(m in M)
    {
      fe <- paste0(n, ".", M)
      be <- paste0(M, ".", n)
      f <- W[fe]
      b <- W[be]
      W[fe] <- Inf
      W[be] <- Inf
      sp <- RBGL::dijkstra.sp(g, start=m, eW = W)$distance
      sp[sp != 0] <- sp[sp != 0] + 1
      sp[n] <- 1
      p[n,m,] <- sp
      W[fe] <- f
      W[be] <- b
    }
  }
  p
}
#' Calculate shortest paths
#' @description Calculate all shortest paths from each node to every other node in a graph g
#'
#' @export
#' @param g network graph
#' @param d vector of edge weights
#' @return N x N matrix of distances from each node to every other node. dataframe of all the SPR from each node to every other node
AlgoDijAllPaths <- function(g, d){
  N <- graph::nodes(g)
  c <- length(N)
  D <- matrix(0, nrow = c, ncol = c)
  R <- data.frame(row.names = N)
  base::rownames(D) <- N
  base::colnames(D) <- N
    for(s in N){
      sp <- RBGL::dijkstra.sp(g, start=s, eW = d)
      D[s,] <- sp$distance
      pens <- N[sp$penult]
      names(pens) <- names(sp$penult)
      R[[s]] <- lapply(N, function(x,s,pens){ExtractPath(s,x,pens) }, s, sp$penult)
      names(R[[s]]) <- N
    }
  # p
  list(D = D, R = R)
}
#' Dijkstra weighted shortest tree(s) multicast algorithm
#' @description The algorithm calculates a shortest path from each publisher to each subscriber. and acorrdingly group togheter (in a multicast tree) paths originating from the same publisher towards mutliple subscribers
#'
#' @export
#' @param g network graph, used by dijkstra
#' @param eM edgeMatrix of the graph, providing edge: capacity, load, and hence weight
#' @param i Information item demand to be routed
#' @return list of: d: distance matrix (publisher x subscribers) - used for debugging, pen: list of pens of each publisher - used for debugging, l: list of paths from each publisher to each subscriber - used for debugging, ts: multicast tress from the nearest publisher(s) to groups of subscribers, p: nearest publishers
AlgoDij <- function(g, eM, i) {
  r <- list()
  sp <- list()
  l <- list()
  pl <- length(i$p)
  sl <- length(i$s)
  publishers <- i$p
  subscribers <- names(i$s) #get only the nodes names, not the number of subscriptions per node.
  d <- matrix(0, nrow = pl, ncol = sl)
  rownames(d) <- publishers
  colnames(d) <- subscribers
  for(p in publishers){
    p.cost <- ((i$l[p]) * 1) + ((!(i$l[p])) * 2) #if i is cached by p, his cost is half the cost if not cached.
    eW <- eM[,'weight'] * p.cost
    r[[p]] <- RBGL::dijkstra.sp(g,start=p,eW=eW)
    d[p,] <- r[[p]]$distance[subscribers]
    l[[p]] <- lapply(subscribers, function(x,s,pens){ r <- RBGL::extractPath(s,x,pens); r }, p, r[[p]]$penult)
  }
  sd <- apply(d, 2, min) #get the shortest distance for each subscriber: sd
  np.i <- apply(d, 2, which.min) #get the index of the nearest publisher having the shortest distance sd, np.i: nearest publisher index
  np <- publishers[np.i] # nearest publisher
  unp <- unique(np) #get the unique set of publishers
  length(sp) <- length(unp) #set the size and names of list of shortest path(sp)
  names(sp) <- unp
  unp.i <- rep(1, length(unp)) #have an index vector to track the count of unicast path lists belonging to the elements
  names(unp.i) <- unp
  k <- 1
  for(s in subscribers){
    sp[[np[k]]][[s]] <- c(0,0) #initialise the list first as list with a vector, don't do c(0) as it will result in 'list' not 'list of list'. I know stupid !!
    sp[[np[k]]][[s]] <- l[[np[k]]][[k]]
    k <- k + 1
  }
  ret <- list(d=d, pen = r, l=l, ts = sp, p = unp)
  ret
}
#' Route demand on Dijkstra's cached routes
#' @description Select service point and pre-calculated Dijkstra weighted shortest tree(s) multicast algorithm.
#' @description The algorithm selects the nearest service point and extracts the pre-calcuated dijkstra shortest tree.
#' @description if two service points at the same distance, favour would be given to the one with a cached copy of the item.
#' @description The algorithm route a streaming demand to the nearest surrogate, following a streaming CDN approach, whereby: For a first request, a surrogate is selected to serve the demand, the surrogate then is routed to an origin to fetch the stream.
#' @description But the surrogate would not act as a local origin (i.e. cache the cotent) for other surrogates! (i.e. it will not participate in the second surrogate-to-origin routing) this is important distinction from VoD CDN, because using the surrogate as an origin may incure additional delay, due to path stretch and the fact that the stream may not be cached any longer in the surrogate.
#' @description However, the surrogate does act as local origin for any requests arriving directly from subscribers in the sub-to-surrogate routing (NOT the surrogate-to-origin routing)
#'
#' @export
#' @param i Information item demand to be routed
#' @param D N x N shortest distance matrix
#' @param w cost reduction when selecting a caching vs. non-caching publisher
#' @return list of: d: distance matrix (publisher x subscribers) - used for debugging, pen: list of pens of each publisher - used for debugging, l: list of paths from each publisher to each subscriber - used for debugging, ts: multicast tress from the nearest publisher(s) to groups of subscribers, p: nearest publishers
AlgoSelectFromCachedDijkstraRoutes <- function(i, D, w = 0.01)
{
  P <- c()
  tree <- list()
  O <- i$p[i$l == 1] #' originators, or cachers
  B <- i$l * 0 #' Balance counter, initialised to zero
  C <- i$l * w
  W <- D
  W[i$p, names(i$s)] <- W[i$p, names(i$s)] - C
  for(s in names(i$s))
  {
    h <- B[which(W[i$p, s] == min(W[i$p, s]))]
    # logging::logdebug('ALGO: i: %s, select sp for %s from %s', i$i, s, h)
    p <- sample(names(h[h == min(h)]), size = 1) #' select the service point with the least number of requests, this ensures load balancing across multiple sp of the same distance
    if(!(p %in% P))
      P <- c(P, p)
    B[p] <- B[p] + 1
    d <- D[p, s]
    names(d) <- s
    tree[[p]] <- c(tree[[p]], d)
  }
  #' route edge point without a cached copy to origins. perform same operations
  for(e in P[!(P %in% O)])
  {
    h <- B[which(W[O, e] == min(W[O, e]))]
    # logging::logdebug('ALGO: i: %s,  select op for %s from %s', i$i, e, O)
    p <- sample(names(h[h == min(h)]), size = 1) #' select the service point with the least number of requests, this ensures load balancing across multiple sp of the same distance
    B[p] <- B[p] + 1
    d <- D[p, e]
    names(d) <- e
    tree[[p]] <- c(tree[[p]], d)
  }
  tree
}
#' Dijkstra based Relay-to-Origin
#' @description the algorithm calculates a shortest path from each publisher to each subscriber.
#' @description and acorrdingly group togheter (in a multicast tree) paths originating from the same publisher towards mutliple subscribers
#' @description The algorithm route a streaming demand to the nearest surrogate, following a streaming CDN approach, whereby:
#' @description For a first request, a surrogate is selected to serve the demand, the surrogate then is routed to an origin to fetch the stream.
#' @description But the surrogate would not act as a local origin (i.e. cache the cotent) for other surrogates! (i.e. it will not participate in the second surrogate-to-origin routing) this is important distinction from VoD CDN, because using the surrogate as an origin may incure additional delay, due to path stretch and the fact that the stream may not be cached any longer in the surrogate.
#' @description However, the surrogate does act as local origin for any requests arriving directly from subscribers in the sub-to-surrogate routing (NOT the surrogate-to-origin routing)
#'
#' @export
#' @param g network graph, used by dijkstra
#' @param eM edgeMatrix of the graph, providing edge: capacity, load, and hence weight
#' @param i Information item demand to be routed
#' @param w cost reduction when selecting a caching vs. non-caching publisher
#' @return list of: d: distance matrix (publisher x subscribers) - used for debugging, pen: list of pens of each publisher - used for debugging, l: list of paths from each publisher to each subscriber - used for debugging, ts: multicast tress from the nearest publisher(s) to groups of subscribers, p: nearest publishers
AlgoDijkstraRelayToOrigin <- function(g, eM, i, w=0.01) {
  P <- c()
  R <- list()
  SP <- list()
  L <- list()
  C <- i$p #candidates
  Clen <- length(C)
  O <- C[i$l == 1] #originators, or cachers
  E <- C[i$l == 0] #non Originators
  B <- rep(0, Clen) #Balance counter
  names(B) <- C
  S <- names(i$s) #Subscribers
  ES <- unique(c(S, E))
  ESlen <- length(ES)
  D <- matrix(0, nrow = Clen, ncol = ESlen )
  rownames(D) <- C
  colnames(D) <- ES
  for(c in C){
    c.cost <- ((i$l[c]) * w) #if i is cached by c, his cost is reduced by the local weight value (w).
    eW <- eM[,'weight']
    R[[c]] <- RBGL::dijkstra.sp(g,start=c,eW=eW)
    D[c,] <- R[[c]]$distance[ES] - c.cost
    L[[c]] <- sapply(ES, function(x,s,pens){ r <- RBGL::extractPath(s,x,pens); r }, c, R[[c]]$penult, simplify = FALSE)
  }
  for(s in S){
    cmin <- which(D[,s] == min(D[,s])) #get the index of the nearest publisher(s) having the shortest distance
    Cs <- C[cmin]
    Bs <- B[Cs]
    p <- Cs[Bs == min(Bs)][1]
    P <- c(P, p)
    SP[[p]][[s]] <- c(0,0) #initialise the list first as list with a vector, don't do c(0) as it will result in 'list' not 'list of list'. I know stupid !!
    SP[[p]][[s]] <- L[[p]][[s]]
    B[p] <- B[p] + 1
  }
  P <- unique(P)
  EP <- P[P %in% E]
  if(length(EP) > 0)
  {
    DO <- as.matrix(D[O, EP])
    colnames(DO) <- EP
    for(p in EP)
    {
      k <- which(DO[,p] == min(DO[,p]))
      Co <- O[k]
      Bo <- B[Co]
      o <- Co[Bo == min(Bo)][1]
      P <- c(P, o)
      SP[[o]][[p]] <- c(0,0) #initialise the list first as list with a vector, don't do c(0) as it will result in 'list' not 'list of list'. I know stupid !!
      SP[[o]][[p]] <- L[[o]][[p]]
      B[o] <- B[o] + 1
    }
    P <- unique(P)
  }
  ret <- list(d=D, pen = R, l=L, ts = SP, p = P)
  ret
}
#' Route demand on Dijkstra's cached routes
#' @description Route a demand for item (non-live or non-streaming) according to conventional CDN based on selecting surrogates (or edge servers) that are nearest to the DNS
#' @description the algorithm calculates the shortest path from a client's access node (i.e. subscriber in the ICN world) to the surrogate selected through DNS mapping
#' @description Notice here there is only one surrogate that maps to each client, whereas the edge server has multiple choices but eventually will select one
#'
#' @export
#' @param i item demand in the form of <i, s, p, ..>, although only one p is used for each client node in s
#' @param D N x N matrix of the shortest distance between each pair of nodes, for all pairs
#' @param ce client-edge mapping, provided through DNS mapping in function DnsMapClientsToSurrogates
#' @param eo edge-origin mapping, providing edge server (surrogate) mapping to an origin - this comes through server selection policy, decoupled in IP from routing.
AlgoSelectFromCachedDijkstraRoutesForDnsMaps <- function(i, D, ce, eo)
{
  P <- c()
  tree <- list()
  O <- i$p[i$l == 1] #' originators, or cachers
  for(s in names(i$s))
  {
    j <- sample(which(ce[,'n'] == s), size = 1)
    if(!(ce[j, 'p'] %in% P))
      P <- c(P, ce[j, 'p'])
    d <- D[ce[j, 'p'], s]
    names(d) <- s
    tree[[ce[j, 'p']]] <- c(tree[[ce[j, 'p']]], d)
  }
  for(e in P[!(P %in% O)])
  {
    j <- sample(which(eo[,'e'] == e), size = 1)
    if(!(eo[j, 'o'] %in% P))
      P <- c(P, eo[j, 'o'])
    d <- D[eo[j, 'o'], e]
    names(d) <- e
    tree[[eo[j, 'o']]] <- c(tree[[eo[j, 'o']]], d)
  }
  tree
}
#' Route Surrogate to Origin, selected by DNS
#' @description Route a demand for item (non-live or non-streaming) according to conventional CDN based on selecting surrogates (or edge servers) that are nearest to the DNS
#' @description the algorithm calculates the shortest path from a client's access node (i.e. subscriber in the ICN world) to the surrogate selected through DNS mapping
#' @note there is only one surrogate that maps to each client, whereas the edge server has multiple choices but eventually will select one
#'
#' @export
#' @param g network graph
#' @param eM edge matrix of g
#' @param i item demand in the form of <i, s, p, ..>, although only one p is used for each client node in s
#' @param ce client-edge mapping, provided through DNS mapping in function DnsMapClientsToSurrogates
#' @param eo edge-origin mapping, providing edge server (surrogate) mapping to an origin - this comes through server selection policy, decoupled in IP from routing.
AlgoDijDnsEdgeToOrigin <- function(g, eM, i, ce, eo)
{
  P <- c()
  R <- list()
  SP <- list()
  eW <- eM[,'weight']
  S <- names(i$s)
  ceS <- matrix(ce[ce[,'n'] %in% S,], ncol = 2) #filter only records concerning item consumers (equivelent to subscribers in the ICN world)
  colnames(ceS) <- c('n', 'p')
  C <- unique(ceS[,'p']) #get edge servers maped to the item consumers
  E <- C[i$l[C] == 0] #only edge servers (mapped to the consumers of the item) in the DNS map that does not have the item cached locally
  El <- length(E)
  #route clients' nodes to their respective edge servers
  for (c in C)
  {
    R[[c]] <- RBGL::dijkstra.sp(g, start=c, eW=eW)
    Sc <- ceS[ceS[, 'p'] == c,'n']
    SP[[c]][Sc] <- c(0,0)
    SP[[c]][Sc] <- lapply(Sc, function(x,s,pens){ r <- RBGL::extractPath(s,x,pens); r }, c, R[[c]]$penult)
  }
  #route servers with non-cached items to an origin with the cached item. first calculate the routes from the remaining origins
  if(El > 0)
  {
    #retrieve the origins for the surrogates that don't have the information cached.
    eoS <- matrix(eo[eo[,'e'] %in% E,], ncol = 2)
    O <- unique(eoS[,2]) #' second column is the origin node.
    for (o in O)
    {
      R[[o]] <- RBGL::dijkstra.sp(g, start=o, eW=eW)
      Eo <- eoS[eoS[,2] == o, 1]
      SP[[o]][Eo] <- lapply(Eo, function(x,s,pens){ r <- RBGL::extractPath(s,x,pens); r }, o, R[[o]]$penult)
    }
  }
  P <- names(SP)
  ret <- list(ts = SP, p = P)
  ret
}
#' Route Surrogate to Origin from cached routes, selected by DNS
#' @description Route a demand for item (non-live or non-streaming) according to conventional CDN based on selecting surrogates (or edge servers) that are nearest to the DNS
#' @description the algorithm calculates the shortest path from a client's access node (i.e. subscriber in the ICN world) to the surrogate selected through DNS mapping
#' @note there is only one surrogate that maps to each client, whereas the edge server has multiple choices but eventually will select one
#'
#' @export
#' @param g network graph
#' @param eM edge matrix of g
#' @param i item demand in the form of <i, s, p, ..>, although only one p is used for each client node in s
#' @param ce client-edge mapping, provided through DNS mapping in function DnsMapClientsToSurrogates
#' @param eo edge-origin mapping, providing edge server (surrogate) mapping to an origin - this comes through server selection policy, decoupled in IP from routing.
AlgoCachedDijDnsEdgeToOrigin <- function(g, eM, i, ce, eo)
{
  P <- c()
  R <- list()
  SP <- list()
  eW <- eM[,'weight']
  S <- names(i$s)
  ceS <- matrix(ce[ce[,'n'] %in% S,], ncol = 2) #filter only records concerning item consumers (equivelent to subscribers in the ICN world)
  colnames(ceS) <- c('n', 'p')
  C <- unique(ceS[,'p']) #get edge servers maped to the item consumers
  E <- C[i$l[C] == 0] #only edge servers (mapped to the consumers of the item) in the DNS map that does not have the item cached locally
  El <- length(E)
  #route clients' nodes to their respective edge servers
  for (c in C)
  {
    R[[c]] <- RBGL::dijkstra.sp(g, start=c, eW=eW)
    Sc <- ceS[ceS[, 'p'] == c,'n']
    SP[[c]][Sc] <- c(0,0)
    SP[[c]][Sc] <- lapply(Sc, function(x,s,pens){ r <- RBGL::extractPath(s,x,pens); r }, c, R[[c]]$penult)
  }
  #route servers with non-cached items to an origin with the cached item. first calculate the routes from the remaining origins
  if(El > 0)
  {
    #retrieve the origins for the surrogates that don't have the information cached.
    eoS <- matrix(eo[eo[,'e'] %in% E,], ncol = 2)
    O <- unique(eoS[,2]) #' second column is the origin node.
    for (o in O)
    {
      R[[o]] <- RBGL::dijkstra.sp(g, start=o, eW=eW)
      Eo <- eoS[eoS[,2] == o, 1]
      SP[[o]][Eo] <- lapply(Eo, function(x,s,pens){ r <- RBGL::extractPath(s,x,pens); r }, o, R[[o]]$penult)
    }
  }
  P <- names(SP)
  ret <- list(ts = SP, p = P)
  ret
}

#' Map Surrogate to Origin
#' @description map surrogates to origins, that is the origin that the surrogate should contact to retrieve a copy of whatever items not cached
#' @description The map here is created by assigning to each surrogate the nearest origin
#'
#' @export
#' @param D NxN distance matrix from each node to every other node.
#' @param O set of servers acting as origin servers
#' @param E set of edge servers
#' @return a mapping of each e in E to some o in O
AlgoAssignSurrogateToOrigin <- function(D, O, E)
{
  Re <- D[O, E]
  Re.i <- base::apply(Re, 2, which.min)
  Oe <- base::rownames(Re[Re.i, ])
  map <- base::cbind(e = E, o=Oe)
  map
}
#' Map clients to surrogates
#'  @description assign equal (roughly) number of client nodes to each surrogate.
#'  @description Better assignment will be implemented in the future that takes into consideration number of requests (subscriptions in ICN) per node
#'
#' @export
#' @param dns map in the form of <d, n, p> where d is the dns node, n is the set of clients' nodes and p is the set of surrogates' nodes
#' @return n,p array mapping n to p
AlgoAssignClientsToSurrogates <- function(dns)
{
  j <- length(dns$n)
  k <- length(dns$p)
  rp <- ceiling(j/k)
  t <- rep(dns$p, each= rp)[1:j]
  map <- cbind(n = dns$n, p = t)
  map
}

#' Find the nearest service point (publisher) to the subscriber
#'
#' @export
#' @param s character giving the subscriber
#' @param P character vector giving set of publisher
#' @param g network graph
#' @param eW numeric vector giving edge weights
#' @return np nearest publisher
AlgoDijNearestPop <- function(s, P, g)
{
  r <- list()
  sp <- list()
  l <- list()
  pl <- length(P)
  d <- rep(0, pl)
  names(d) <- P
  sp <- paste0(P, ":", s)
  ss <- paste0(s, ":", s)
  r <- sp.between(g, P, s)
  l <- vapply(r, "[[", double(1), "length")
  t <- sapply(r, "[[", "path_detail")
  names(t) <- P
  names(l) <- P
  if(s %in% P)
    l[ss] <- 0
  lmin <- which(l == min(l))
  np <- P[lmin]
  st <- t[lmin]
  lp <- l[lmin]
  retL <- list(p=np, r=st, l=lp)
  retL
}

# Swing -------------------------------------------------------------------

#' Select surrogates according to the 'swing' algorithm.
#'  @description The swing algorithm, orders the set of candidate surrogates according to their closeness to every other node. closeness is defined as the sum of all paths from the node to every other node
#'  @description It then selects the k surrogates by swinging between the nodes with shortest paths and nodes with longest paths, to ensure maximum coverage of the graph.
#'
#' @export
#' @param u vector giving per node population
#' @param k number of surrogates
#' @param D matrix of the distance from each node to every other node
#' @return set of k or more surrogates
AlgoSwing <- function(u, k = NULL, D, e)
{
  p <- c()
  n <- names(u)
  d <- sapply(n, AlgoSwingCandidateConnector, D = D, e = e, USE.NAMES = FALSE, simplify = FALSE)
  #extract the primary set of candidates. those are nodes that surely connect some other nodes to the rest of the graph
  a <- sort(unlist(sapply(d, "[[", "p")))
  s <- unique(names(a))
  l <- length(s)
  #extract secondary set of candidates. this is complementray to the primary set - i.e. all nodes who didn't make it to the primary cut.
  f <- sort(unlist(sapply(d, "[[", "bp")))
  m <- unique(names(f))
  #exclude any elements that already exists in the primary set
  m <- m[!(m %in% s)]
  if(is.null(k))
    return(s)
  # if k != null, then select based on k value
  k <- ifelse((length(u) - length(e)) < k, length(u) - length(e), k)
  for(i in 1:k)
  {
    if(l > 0)
    {
      p[i] <- s[1]
      s <- s[-1]
      l <- l - 1
      s <- rev(s)
    }
    else
    {
      p[i] <- m[1]
      m <- m[-1]
      m <- rev(m)
    }
  }
  s <- p
  s
}
#' Select candidate surrogates for each node. that would be the
#'
#' @export
#' @param n nodes in the network
#' @param D shortest distance matrix
#' @param e nodes to be excluded from selection
AlgoSwingCandidateConnector <- function(n, D, e = c())
{
  m <- colnames(D)
  e <- c(e, n)
  m <- m[!(m %in% e)]
  c <- D[n,m]
  mc <- names(c[c == min(c)]) #get neighbours of n
  p <- colSums(as.matrix(D[, mc]))
  names(p) <- mc
  #get p with min value, i.e. one with total min cost paths and maximum node degree
  a <- p[p == min(p)]
  b <- p[!(p %in% a)]
  r <- list(p = a, bp = b)
  r
}

# Pop/Cls -----------------------------------------------------------------

#' Find k-nodes with the highest closeness in the graph
#'
#' @export
#' @param g graphNEL object
#' @param k k-closest nodes
#' @param e set of nodes to be excluded. primarly used to exclude CDN origins when selecting surrogates
#' @return character vector of the node(s) name
AlgoClosest <- function(g, k, e = c())
{
  n <- c()
  c <- igraph::centr_clo(igraph::igraph.from.graphNEL(g))$res
  ns <- graph::nodes(g)
  names(c) <- ns
  c <- c[!(ns %in% e)]
  c <- sort(c, decreasing = TRUE)
  ns <- names(c)
  nn <- length(ns)
  if(k > nn)
    k <- nn
  if(k > 0)
    n <- sample(ns, size = k , prob = c)
  n
}
#' Find k-nodes with the highest population in the graph
#'
#' @export
#' @param p a numeric vector of nodes' populations
#' @param k k-largest nodes
#' @param e(exclude) set of nodes to be excluded. primarly used to exclude CDN PoPs when selecting surrogates
#' @return \code{n}: character vector of the node(s) name
AlgoLargest <- function(p, k, e = c())
{
  n <- c()
  p <- sort(p, decreasing = TRUE)
  ns <- names(p)
  p <- p[!(ns %in% e)]
  ns <- names(p)
  # n <- names(p)[1:k]
  pr.n <- p/sum(p)
  nn <- length(ns)
  if(k > nn)
    k <- nn
  if(k > 0){
    n <- sample(ns, size = k, prob = pr.n)
  }
  n
}

