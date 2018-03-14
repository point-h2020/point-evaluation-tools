#  pubs.R handle publishers (service points) lists
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

#' Construct list of origins
#' @description Select a subset of network nodes to publish all the items in a catalogue, in a CDN this equates to 'entry point' servers
#'
#' @param cc content catalogue as a list objects, each in the form of <i, p, s, f, l, ..>
#' @param gud (g, u, d) list, where g is graphNEL of the core network and u is the population of each core node (before distributed over access nodes), d: DM Distanc Matrix
#' @param k number of origins to be selected
#' @param a minimum storage capacity that can be placed in a node
#' @param opt numeric vector giving option of assignment algorithm. 1 == assign k largest population (\code{AlgoLargest}), 2 == assign k nodes with highest 'closeness' (\code{AlgoClosest}), 3 == assign using 'Swing' (\code{AlgoSwing})
#' @param e set of nodes to be excluded from selection. Typically because these nodes are already selected as origins.
#' @param c create? <FALSE | TRUE>, default FALSE, decided whether or not to create the global PUBLISHERS list
#' @return list of origins in the form of <p, v, i, c>, where p is the node id of the publisher, v is the total volume of all items, i is the set of items and c is the cost of placing an origin at the node. c is not exploited at the moment.
OriginPublishers <- function(cc, gud, k, a, opt, e = c(), c = FALSE)
{
  o <- switch(opt,
              AlgoLargest(gud$u, k, e),
              AlgoClosest(gud$g, k, e),
              AlgoSwing(gud$u, k = k, D = gud$D, e = e)
  )
  v <- sum(vapply(cc, "[[", numeric(1), "v"))
  v <- ceiling(v/a) * a
  I <- rep(1, length(cc))
  names(I) <- names(cc)
  O <- lapply(o, SetItemPublishersList, v =  v, i = I)
  names(O) <- o
  if(c)
    assign("PUBLISHERS", O, envir = .GlobalEnv)
  O
}
#' Construct list of surrogates
#' @description Select a subset of nodes to act as surrogates, who will advertise a subset of items from the content catalogue and have local copies, while having other items advertised but not available locally.
#' @description Items that are not available locally, can be retreived from another surrogate or an origin.
#' @description Items that are available locally will appear in the set of items p$i and the surrogate will appeare in the set of providers of an item i$p with availability boolean set to \code{TRUE}.
#' @description Items that are NOT available locally will not apprear in the set of items p$i and the surrogate will be marked with availability boolean FALSE in an item object i$p
#'
#' @param cc content catalogue as a list objects, each in the form of <i, p, s, f, l, ..>
#' @param gu (g, u) graph, population list, where g is graphNEL of the either the core, access or core/access augumented network and u is the population of each node
#' @param k number of surrogates to be selected
#' @param a minimum storage capacity that can be placed in a node
#' @param D matrix of the distance from each node to every other node
#' @param opt numeric vector giving option of assignment algorithm. 1 == assign k largest population, 2 == assign k nodes with highest 'closeness' centrality
#' @param e set of nodes to be excluded from selection. Typically because these nodes are already selected as origins.
#' @param c create?<FALSE|TRUE>, default FALSE, decided whether or not to create the global PUBLISHERS list
#' @return list of surrogates in the form of <p, v, i, c>, where p is the node id of the publisher, v is the total volume of all items, i is the set of items and c is the cost of placing an origin at the node. c is not exploited at the moment.
SurrogatePublishers <- function(cc, gu, k = NULL, a, D = NULL, opt, e = c(), c = FALSE)
{
  vmax <- sum(vapply(cc, "[[", numeric(1), "v"))
  p <- switch(opt,
           AlgoLargest(gu$u, k, e = e),
           AlgoClosest(gu$g, k, e = e),
           AlgoSwing(gu$u, k = k, D = D, e = e)
  )
  vp <- vapply(p, SetPublisherCapacity, numeric(1), u = gu$u, v = vmax, a = a)
  #get the probability of occurance of each item derived from its popularity - in Zipf this is the fraction of time the item may appeare in a set.
  #sample Items to be cached in the surrogate according to the popularity distribution of the catalogue
  Ip <- mapply(CatalogueSampleByVolume, v = vp, MoreArgs = list(I = cc), SIMPLIFY = FALSE)
  Ap <- mapply(GetComplementaryItems, Ip, MoreArgs = list(I = cc), SIMPLIFY = FALSE) #Ap: advertised publications
  Ip <- mapply(PublishersCombineCachedWithNonCached, Ip, Ap, SIMPLIFY = FALSE)
  P <- mapply(SetItemPublishersList, x = p, v = vp, i = Ip, SIMPLIFY = FALSE)
  names(P) <- p
  if(c)
    assign("PUBLISHERS", P, envir = .GlobalEnv)
  P
}
#' Combine cached items with non cached items in one vector
#'
#' @param c cached items
#' @param nc non cached items
#' @return vector of 1s and 0s , with 1s names set to cached items and 0s names set to non cached items.
PublishersCombineCachedWithNonCached <- function(c, nc)
{
  i <- rep(1, length(c))
  names(i) <- c
  j <- rep(0, length(nc))
  names(j) <- nc
  i <- c(i, j)
  i
}
