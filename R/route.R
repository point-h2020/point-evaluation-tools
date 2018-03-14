#  route.R set of functions for routing service demand list
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


# ICN Unicast -------------------------------------------------------------

#' Route ICN unicast demands with Dijkstra
#' @description Route ICN subscriptions (demands) of all items using dijkstra's algorithm for min hop-count path. subscriptions here are assumed completely desynchronous, hence routed as unicast. Each demand is a tuple of \code{<i, s, p, v>}
#' @note There is no knowledge here if multiple publishers have equal distance to multiple subscribers that only one of them will be selected.
#' this will come in the improved dijkstra
#'
#' @param iL list of Pub/Sub state for each item in catalogue
#' @param g network graph list, including (at least): graph object and Edge Matrix
#' @return list of: \code{trees} satisfying the demand, \code{added} link capacity to accommodate the demand and \code{total} demand that was introduced.
RouteIcnUnicastDijkstra <- function(iL, g)
{
  logging::logdebug('ROUTE: ICN UC demands...')
  EM <<- g$eM   #' restore the global Edge Matrix to the original graph edge matrix, so that load can be calculated for each demand list based on empty network
  o <- list(trees = list(), added = list(), total = c())
  for(i in iL)
  {
    o$trees[[i$i]] <- AlgoSelectFromCachedDijkstraRoutes(i = i, D = g$D)
    p <- i$l[i$l == 0]
    p <- p[!(names(p) %in% names(i$s))]
    p <- replace(p, p==0, 1)
    s <- c(i$s, p)  #' don't want to contaminate the demands at the edge (i.e. \code{total}) with edge-origin demand. hence different var
    for(t in names(o$trees[[i$i]]))
    {
      o$added[[i$i]][[t]] <- mapply(HelperUpdatePathLoadOnAm, g$R[[t]][names(o$trees[[i$i]][[t]])], (s * i$v)[names(o$trees[[i$i]][[t]])], SIMPLIFY = FALSE)
    }
    o$total[i$i] <- sum(i$s * i$v)
  }
  o$eM <- EM
  # logging::logdebug('ROUTE: ICN UC demands complete.')
  o
}
#' Route ICN demands with Dijkstra
#' @description  Route ICN subscriptions (demand) of a single item, update the link weights in the global EM and report admission ratio. subscriptions here are assumed completely desynchronous, hence routed as unicast.
#'
#' @param iL demands of a single item in the format of a tuple \code{<i, f, s, p, v>}, i: id(or rank), p: set of publishers, s: set of subscribers, v: volume, f: probability of occurance
#' @param D N x N shortest distance matrix
#' @param R N x N dataframe of shortest routes (min hop count)
#' @return list of: \code{trees} satisfying the demand, \code{added} link capacity to accommodate the demand and \code{total} demand that was introduced.
RouteIcnUnicastOneDemandDijkstra <- function(iL, D, R)
{
  o <- list(trees = list(), added = list(), total = c())
  o$trees <- AlgoSelectFromCachedDijkstraRoutes(i = iL, D = D)
  p <- iL$l[iL$l == 0]
  p <- p[!(names(p) %in% names(iL$s))]
  p <- replace(p, p==0, 1)
  iL$s <- c(iL$s, p)
  for(t in names(o$trees))
  {
    o$added[[t]] <- HelperUpdatePathLoadOnAm(t = R[[t]][names(o$trees[[t]])], l = (iL$s * iL$v)[names(o$trees[[t]])])
  }
  o$total <- sum(iL$s * iL$v)
  o
}

# ICN Multicast -----------------------------------------------------------

#' Route ICN multicast demands with Dijkstra
#' @description Route multicast demands of a single item, update the link weights in the global EM and report admission ratio. Subscribers are assumed to be quasi-synchronous, hence routed as multicast. Number of multicast groups depends on \code{t} and \code{L}
#'
#' @param iL demands of a single item in the format of a tuple <i, f, s, p, v>, i: id(or rank), p: set of publishers, s: set of subscribers, v: volume, f: probability of occurance
#' @param D N x N shortest distance matrix
#' @param R N x N dataframe of shortest routes (min hop count)
#' @param t aggregation (or catchment) interval - number of seconds to wait for multicast group to build up from arriving requests before responding
#' @param L total transmission/streaming period
#' @return list of: \code{trees} satisfying the demand, \code{added} link capacity to accommodate the demand and \code{total} demand that was introduced.
RouteIcnMulticastOneDemandDijkstra <- function(iL, D, R, t, L)
{
  o <- list(trees = list(), added = list(), total = c())
  o$trees <- AlgoSelectFromCachedDijkstraRoutes(i = iL, D = D)
  p <- iL$l[iL$l == 0]
  p <- p[!(names(p) %in% names(iL$s))]
  p <- replace(p, p==0, 1)
  iL$s <- c(iL$s, p)
  n <- trunc((L + L/iL$s)/(t + L/iL$s))
  #' update multicast load on each tree
  for(t in names(o$trees))
  {
    o$added[[t]] <- HelperUpdateTreeLoadOnAM(R[[t]][names(o$trees[[t]])], (n * iL$v)[names(o$trees[[t]])])
  }
  o$total <- sum(i$s * iL$v)
  o
}
#' Route ICN multicast demands with Dijkstra
#'
#' @description Route all ICN demands according to dijkstra's algorithm. Subscribers are assumed to be quasi-synchronous, hence routed as multicast. Number of multicast groups depends on \code{t} and \code{L}
#' @note There is no knowledge here if multiple publishers have equal distance to multiple subscribers that only one of them will be selected.
#' this will come in the improved dijkstra
#'
#' @param iL list of items pub/sub matches
#' @param g network graph list, including (at least): graph object and Edge Matrix
#' @param t numerical value of the catchment interval
#' @note \code{t} must be less than or equal \code{L}
#' @return list of: \code{trees} satisfying the demand, \code{added} link capacity to accommodate the demand and \code{total} demand that was introduced.
RouteIcnMulticastDijkstra <- function(iL, g, t, L)
{
  logging::logdebug('ROUTE: ICN MC demands...')
  EM <<- g$eM   #' restore the global Edge Matrix to the original graph edge matrix, so that load can be calculated for each demand list based on empty network
  o <- list(trees = list(), added = list(), total = c())
  for(i in iL)
  {
    o$trees[[i$i]] <- AlgoSelectFromCachedDijkstraRoutes(i = i, D = g$D)
    p <- i$l[i$l == 0]
    p <- p[!(names(p) %in% names(i$s))]
    p <- replace(p, p==0, 1)
    s <- c(i$s, p)
    n <- trunc((L + L/s)/(t + L/s))
    for(r in names(o$trees[[i$i]]))
    {
      o$added[[i$i]][[r]] <- HelperUpdateTreeLoadOnAM(g$R[[r]][names(o$trees[[i$i]][[r]])], (n * i$v)[names(o$trees[[i$i]][[r]])])
    }
    o$total[i$i] <- sum(i$s * i$v)
  }
  o$eM <- EM
  # logging::logdebug('ROUTE: ICN MC demands complete.')
  o
}

# IP ----------------------------------------------------------------------

#' Route IP unicast demands with Dijkstra
#'
#' @description Route IP demand for a single stream, provided DNS map between clients and surrogates (edge servers), and mapping between surrogates and origin servers. This represnets conventional CDN routing towards a surrogate selected through DNS redirect
#'
#' @param g network graph
#' @param iL demands of a single item in the format of a tuple <i, f, s, p, v>, i: id(or rank), p: set of publishers, s: set of subscribers, v: volume, f: probability of occurance
#' @param ce client-edge server (surrogate) mapping through DNS
#' @param eo edge-origin mapping for retrieving non-cached items
#' @return list of: \code{trees} satisfying the demand, \code{added} link capacity to accommodate the demand and \code{total} demand that was introduced.
RouteIpUnicastOneDemandDijkstra <- function(iL, D, R, ce, eo)
{
  o <- list(trees = list(), added = list(), total = c())
  o <- AlgoSelectFromCachedDijkstraRoutesForDnsMaps(iL, D, ce, eo)
  #add the load introduced by origin-to-surrogate downstream, that is the bandwidth of a single flow from the origin to the surrogate, to provide the latter with a fresh copy of the item.
  p <- iL$l[iL$l == 0]
  p <- p[!(names(p) %in% names(iL$s))]
  p <- replace(p, p==0, 1)
  s <- c(iL$s, p)
  for(t in names(o$trees))
  {
    o$added[[t]] <- HelperUpdatePathLoadOnAm(t = R[[t]][names(o$trees[[t]])], l = (s * iL$v)[names(o$trees[[t]])])
  }
  o$total <- sum(iL$s * iL$v)
  o
}
#' Route all IP demands with Dijkstra's algorithm
#' @description Route all IP demands according to dijkstra's algorithm. Here unicast is assumed regardless of the sych state of clients
#' @note there is no knowledge here if multiple publishers have equal distance to multiple subscribers that only one of them will be selected. This will come in the improved dijkstra
#'
#' @param g network graph in graphNEL format.
#' @param iL list of items pub/sub matches
#' @param ce client-edge server (surrogate) mapping through DNS
#' @param eo edge-origin mapping for retrieving non-cached items
#' @return list of: \code{trees} satisfying the demand, \code{added} link capacity to accommodate the demand and \code{total} demand that was introduced.
RouteIpUnicastDijkstra <- function(g, iL, ce, eo)
{
  logging::logdebug('ROUTE: IP demands starts...')
  o <- list(trees = list(), added = list(), total = c())
  EM <<- g$eM
  for(i in iL)
  {
    o$trees[[i$i]] <- AlgoSelectFromCachedDijkstraRoutesForDnsMaps(i, D = g$D, ce, eo)
    p <- i$l[i$l == 0]
    p <- p[!(names(p) %in% names(i$s))]
    p <- replace(p, p==0, 1)
    s <- c(i$s, p)
    for(t in names(o$trees[[i$i]]))
    {
      o$added[[i$i]][[t]] <- mapply(HelperUpdatePathLoadOnAm, g$R[[t]][names(o$trees[[i$i]][[t]])], (s * i$v)[names(o$trees[[i$i]][[t]])], SIMPLIFY = FALSE)
    }
    o$total[i$i] <- sum(i$s * i$v)
  }
  o$eM <- EM
  # logging::logdebug('ROUTE: IP demands complete.')
  o
}

# valiation ---------------------------------------------------------------

#' Testing total presented demands
#' @description testing function to see how much is the demand introduced by a stored list of Pub/Sub state.
#'
#' @param cc.fn file name of the cc-g_xxx object to be loaded. notice this is pub/sub state not mere catalogue.
RouteTestTotal <- function(cc.fn)
{
  total <- list()
  load(cc.fn)
  for (t in 1:length(cc[[1]]))
  {
    total[[t]] <- c(0)
    for(i in cc[[1]][[t]])
    {
      total[[t]] <- c(total[[t]], sum(i$s * i$v))
    }
  }
  total
}

