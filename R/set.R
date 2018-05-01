#  set.R sets the values of variables
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

#' Set enviroment with configuration arguments
#'
#' @param env enviroment to set the arguments within
SetEvnArgs <- function(env, ...)
{
  # env <- new.env(parent = globalenv())
  args <- list(...)
  names <- names(args)
  mapply(function(x,y){assign(x, y, envir = env)},names, args)
}

# Catalogue ---------------------------------------------------------------

#' Set attributes of a catalogue element (item)
#' @description create items list of demands in the form of (i,f, S,P) - i.e. <item, fraction of total occurances, Subscribers:#Subscriptions, Publishers>, volume is assumed 1
#'
#' @param x item id - currently the rank is also the item id
#' @param f (0,1) frequency or probability of occurance following a popularity distribution model
#' @param s subscribers or consumers of the item
#' @param p publishers or providers of the item
#' @param l boolean giving locality of the item within each publisher - i.e. 1 item is stored locally, 0 item is not stored locally
#' @param v volume of the item in Gbps
SetItemsList <- function(x, f, s=c(), p=c(), l=c(), v=c())
{
  ifelse(is.null(v), v <- runif(1,0.1, 0.5), v)
  x <- list(i = x, f = f, s = s, p = p, l = l, v = v)
  x
}
#' Set publisher of item
#'
#' @param x item object
#' @param p publisher
#' @param l localizing publishers (i.e. caching)
SetItemPublisher <- function(x, p, l) {
  x$p <- p
  x$l <- l
  x
}
#' Append publisher to list of publishers of item
#'
#' @param x item object
#' @param p publisher(s)
#' @param l locally available or not.
SetAdditionalItemPublisher <- function(x, p, l) {
  x$p <- c(x$p, p)
  if(!missing(l))
  {
    names(l) <- p
    x$l <- c(x$l, l)
  }
  x
}
#' Set publisher capacity
#' @description Set publisher capacity in terms of item volumes it can publish. Decision here is based on the ratio of the node population to the largest node population
#'
#' @param p publisher node
#' @param u numeric vector of population per node in the network
#' @param v volume to be approtioned to the surrogate
#' @param a minimum volume that can be placed in a node, this is a dimensioning parameter that depends on server specs
SetPublisherCapacity <- function(p, u, v, a)
{
  up <- u[p]
  umax <- max(u)
  r <- ceiling((up/umax) * v)
  r <- ceiling(r/a) * a
  r
}
#' Construct list of publishers
#'
#' @param node label of the publisher
#' @param v both storage and bandwideth capacity of the publisher, defined as sum of the volumes of published items
#' @param i set of advertised items
#' @param c cost of the publisher. i.e. cost of setting up a publisher in the node.
SetItemPublishersList <- function(x, v = Inf, i = c(), c=NULL){
  x <- list(p = x, v = v, i = i, c = c)
  x
}
#' Set subscriber of item
#'
#' @param x item object
#' @param s subscriber
SetItemSubscriber <- function(x, s){
  x$s <- s
  x
}
#' Append subscriber to set of item's subscribers
#' @description append subscriber to the set of subscribers of item in list of demands to the list
#'
#' @param x item object
#' @param v volume (number of subscriptions)
#' @param s subscriber
SetAdditionalItemSubscriber <- function(x, v, s) {
  names(v) <- s
  x$s <- c(x$s, v)
  x
}

#' Set attributes of subscribers list
#' @description Create subscribers list of demands in the form of (s,v,i) - i.e. <subscriber, volume, set of items>
#'
#' @param x node label of the subsciber
#' @param v number of subscriptions
#' @param i set of 'subscribed to' items
#' @return list object of the subscriber
SetSubscribersList <- function(x, v, i)
{
  x <- list(s = x, v = v, i = i)
  x
}

# DNS ---------------------------------------------------------------------

#' Set DNS users
#'
#' @param d DNS state
#' @param n set of nodes
#' @return updated state with d$n = n
SetDnsUsers <- function(d, n)
{
  d$n <- n
  d
}
#' Set DNS surrogates
#'
#' @param d DNS state
#' @param p set of nodes
#' @return updated state with d$n = n
SetDnsSurrogates <- function(d, p)
{
  d$p <- p
  d
}
#' Set DNS Map with clients and surrogates
#'
#' @param d DNS state
#' @param n set of clients nodes
#' @param p set of surrogaes nodes
SetDnsMap <- function(d, n=c(), p=c())
{
  x <- list(d = d, n = n, p = p)
  x
}
#' Set item reflector
#' @description Item here represents a live stream item in streaming CDN
#'
#' @param i item list object in the format <i, f, p, s, v,...>
#' @param R set of reflectors
#' @return updated item list
SetItemReflector <- function(i, R)
{
  i$r <- R
  i
}

# Misc --------------------------------------------------------------------

#' Substitue numeric value of column with a human-readable name
#'
#' @export
#' @param df dataframe of the cloumn
#' @param cols column names where the replacement will be
#' @param ov old value, i.e. existing value. the position of a value here MUST match the position of the name in the nv param (next)
#' @param nv new value, the new value to replace ov. The position of each element in nv must correspond to the same position in ov
SetReplacementValue <- function(df, cols, ov, nv)
{
  l <- length(ov)
  cl <- length(cols)
  for(i in 1:cl)
  {
    for(j in 1:l)
    {
      df[,cols[i]][df[,cols[i]] == ov[j]] <- nv[j]
    }
  }
  df
}

