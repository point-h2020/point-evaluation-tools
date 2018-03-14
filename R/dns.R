#  dns.R construct DNS-based mapping state
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

#' Assign DNS in the network
#'
#' @export
#' @param gu (g, u) list, where g is graphNEL of the  network and u is the population of each node
#' @param D NxN distance matrix
#' @param k number of DNS servers
#' @param opt numeric vector giving option of assignment algorithm. 1 == assign k largest population, 2 == assign k nodes with highest 'closeness' centrality
#' @return list of nodes assinged as DNS points, whether local or public doesn't matter
Dns <- function(gu, D, k, opt)
{
  dns <- switch(opt,
                AlgoLargest(gu$u, k),
                AlgoClosest(gu$g, k),
                AlgoSwing(gu$u, k, D, NULL)
  )
  dns
}
#' Assign clients to DNS points
#' @description assign network nodes to DNS nodes, this resembles assigning users to DNS servers
#'
#' @export
#' @param D NxN matrix of distances from each node to every other node in the network graph
#' @param d vector of nodes acting as DNS
#' @param c boolean determining whether or not to create or update a globale variable for the DNS map
#' @return <DNS, node> map associating each node with a DNS
DnsAssignClients <- function(D, d, c = FALSE)
{
  dn <- list()
  N <- colnames(D)
  Rdns <- D[d,]
  mins <- matrixStats::colMins(Rdns)
  for(n in N)
  {
    c <- which(Rdns[,n] == min(Rdns[,n]))
    sp <- sample(d[c], size = 1)
    dn[[sp]] <- c(dn[[sp]], n)
  }
  d <- names(dn)
  dns <- mapply(SetDnsMap, d = d, n = dn, SIMPLIFY = FALSE)
  if(c)
    assign("DNS", dns, envir = .GlobalEnv)
  dns
}
#' Assing clients to existing DNS Map
#' @description assign access nodes to DNS existing but incomplete state (e.g. state only consists of DNS nodes and surrogates), this resembles assigning users to DNS servers
#'
#' @export
#' @param dns incomplete DNS state, it maps DNS to surrogates but not yet to access nodes
#' @param D NxN matrix of distances from each node to every other node in the network graph
#' @param c boolean determining whether or not to create or update a globale variable for the DNS map
#' @return <DNS, n, p> map associating each DNS with a set of nodes and a set of surrogates
DnsAssignClientsToExistingMap <- function(dns, D, c = FALSE)
{
  dn <- list()
  length(dn) <- length(dns)
  d <- names(dns)
  names(dn) <- d
  N <- colnames(D)
  Rdns <- D[d,]
  mins <- matrixStats::colMins(Rdns)
  for(n in N)
  {
    c <- which(Rdns[,n] == min(Rdns[,n]))
    sp <- sample(d[c], size = 1)
    dn[[sp]] <- c(dn[[sp]], n)
  }
  dns <- mapply(SetDnsUsers, d = dns, n = dn, SIMPLIFY = FALSE)
  if(c)
    assign("DNS", dns, envir = .GlobalEnv)
  dns
}

#' Map clients to DNS and update the subscribers state
#' @description Update subscribers state - i.e. the subscribers list created in subs.R, which includes the list of items requested by end uesers, this resembles assigning users to DNS servers
#'
#' @export
#' @param d vector of nodes acting as DNS
#' @param S subscribers state in the form <s, v, i>
#' @param D NxN matrix of distances from each node to every other node in the network graph
#' @param c boolean determining whether or not to update the globale list SUBSCRIBERS in the global enviroment
#' @return updated subscribers state in the form <s,v, i, dns>
DnsMapClientsAndUpdateSubscribersList <- function(d, S, D, c=TRUE)
{
  n <- colnames(D)
  Rdns <- D[,d]
  ds <- apply(Rdns, 1, function(x){y <- which(x == min(x)); y <- names(y)[1]; y})
  S <- lapply(S, function(x, dns=NULL){ x$dns <- dns[x$s]; x}, dns = ds)
  if(c)
    SUBSCRIBERS <<- S
  S
}

#' Find surrogate (edge server) closest to local DNS (LDNS).
#' @description This resembels how the mapping system of traditional CDN performs global load balancing and local load balancing, by first selecting a CDN cluster then selecting servers within the cluster. the LDNS is then provided with identifiers for those servers.
#' @description See the paper "End-User Mapping: Next Generation Request Routing for Content Delivery" for a more comprehensive description of how CDN mapping works
#'
#' @export
#' @param d vector of nodes acting as DNS
#' @param p vector of nodes acting as edge servers (surrogates)
#' @param D NxN matrix of distances from each node to every other node in the network graph
#' @param c boolean determining whether or not to create a globale variable for the DNS map
#' @return \code{<DNS, n, p>} map associating each DNS with a set of nodes and a set of surrogates
DnsAssignSurrogates <- function(d, p, D, c = FALSE)
{
  dp <- list()
  length(dp) <- length(d)
  names(dp) <- d
  for(e in d)
  {
    dp[[e]] <- names(D[p,e][D[p,e] == min(D[p,e])]) #' for each dns server, find the set of surrogates that are nearest to it. notice that in the end some surrogates might not be selected for any dns. this is a design issue when selecting dns positions, not an assignment issue here.
  }
  dns <- mapply(SetDnsMap, d = d, p = dp, SIMPLIFY = FALSE)
  if(c)
    assign("DNS", dns, envir = .GlobalEnv)
  dns
}
#' Find surrogate (edge server) closest to local DNS (LDNS).
#' @description This resembels how the mapping system of traditional CDN performs global load balancing and local load balancing, by first selecting a CDN cluster then selecting servers within the cluster. the LDNS is then provided with identifiers for those servers.
#' @description See the paper "End-User Mapping: Next Generation Request Routing for Content Delivery" for a more comprehensive description of how CDN mapping works
#'
#' @export
#' @param dns incomplete DNS map, having only DNS assignment to access nodes but surrogates not assigned to DNS.
#' @param p vector of nodes acting as edge servers (surrogates)
#' @param D NxN matrix of distances from each node to every other node in the network graph
#' @param c boolean determining whether or not to create a globale variable for the DNS map
#' @return <DNS, n, p> map associating each DNS with a set of nodes and a set of surrogates
DnsAssignSurrogateToExistingMap <- function(dns, p, D, c = FALSE)
{
  dp <- list()
  length(dp) <- length(dns)
  d <- names(dns)
  names(dp) <- d
  for(e in d)
  {
    dp[[e]] <- names(D[p,e][D[p,e] == min(D[p,e])]) #' for each dns server, find the set of surrogates that are nearest to it. notice that in the end some surrogates might not be selected for any dns. this is a design issue when selecting dns positions, not an assignment issue here.
  }
  dns <- mapply(SetDnsSurrogates, d = dns, p = dp, SIMPLIFY = FALSE)
  if(c)
    assign("DNS", dns, envir = .GlobalEnv)
  dns
}
#' Map clients (subscribers) to surrogates
#' @description Map clients to surrogates in the DNS map, when there are multiple surrogates associated with a DNS,
#' @description Perform a simple round robin algorithm to load balance the surrogates.
#'
#' @export
#' @param dns list mapping client nodes (equivelent to subscribers in the ICN domain) to a DNS node and mapping surrogates to DNS
#' @return  a map of client nodes to surrogate nodes. this is equivelent to the function of the mapping system by conventional CDN employing DNS redirects with local and global balancing
DnsMapClientsToSurrogates <- function(dns)
{
  j <- 1
  k <- 1
  map <- lapply(dns, AlgoAssignClientsToSurrogates)
  map <- do.call(rbind, map)
  map
}
#' Construct DNS map
#' @description Generate DNS map, start by selecting DNS nodes, then assign client's nodes to DNS, assing surrogates and origins to DNS node, and finally map client's to surrogates and origins.
#'
#' @export
#' @param D NxN matrix of distance from each node to every other node in the graph. if missing calculate inside the function
#' @param k number of DNS servers
#' @param opt numeric vector giving option of assignment algorithm. 1 == assign k largest population, 2 == assign k nodes with highest 'closeness' centrality
#' @param p vector of nodes acting as edge servers (surrogates)
#' @param c boolean determining whether or not to create a globale variable for the DNS map
#' @return list mapping DNS nodes to client and surrogate nodes, and an array directly mapping clients to surrogates/origins
DnsMap <- function(gu, D, k, opt, p, c = FALSE)
{
  if(missing(D))
  {
    ed <- unlist(edgeWeights(gu$g))
    D <- AlgoDijAllPaths(g = gu$g, d = ed)
  }
  # logdebug('DNS: publishers: %s', p)
  d <- Dns(gu = gu, D = D, k = k, opt = opt)
  dns <- DnsAssignClients(D = D, d = d, c = c)
  dns <- DnsAssignSurrogateToExistingMap(dns = dns, p = p, D = D, c = c)
  ce <- DnsMapClientsToSurrogates(dns)
  list(dns = dns, ce = ce)
}
