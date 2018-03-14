#  items.R handle service/content/information catalogues and items within a catalogue
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

#' Construct content/service catalogue
#' @description Generate a list of item objects representing a content catalogue, each element is an item object in the form of <i, f, s, p, v>, where: i is the item identifier (also acts as the rank), f is the probability the item occurs in a dataset, s is set of subscribers and the number of their subscriptions, p is the set of publishers, v is the capacity requirement of the item.
#'
#' @export
#' @param N the number of ranks and when assuming 1 item per rank, N also means the number of items.
#' @param d character value, giving the probability distribution of the items. 'zipf' referes to zipf distribution.
#' @param d.parm numeric vector giving the distribution parameters, e.g. for zipf: \code{s} exponent
#' @param v volume or capacity requirements of the item.
#' @param c create? <FALSE | TRUE>, default FALSE, decided whether or not to create the global ITEMS list
#' @return content catalogue in the form of a list of objects, each of which is <i, f, s, p, v>
Catalogue <- function(N, d = 'zipf', d.param = c(s = 1.0, ig = 0), v = 1.0, c = FALSE)
{
  ifelse(d == 'zipf', f <- GenZipfPmf(d.param['s'], N, d.param['ig']), f <- NA)
  r <- names(f)
  v <- sample(v, N, TRUE) #randomised item volumes
  C <- mapply(SetItemsList, x = r, f = f, v = v, MoreArgs = list(s = c(), p = c(), l = c()), SIMPLIFY = FALSE)
  names(C) <- r
  if(c)
    assign("ITEMS", C, envir = .GlobalEnv)
  list(I = C, Z = f)
}

#' Sample catalogue by volume
#' @description select a subset of items, for which the total volume of all selected items, equals a defined value
#'
#' @export
#' @param I content catalogue
#' @param v total volume of all selected items
#' @return Iv subset of I for which the total volume is v
CatalogueSampleByVolume <- function(I, v)
{
  vi <- 0
  Iv <- c()
  res <- v - vi
  I.prob <- sapply(I, "[[", "f")
  I.names <- names(I)
  while((res > 0) & (length(I.names) > 0))
  {
    i <- sample(I.names, size=1, prob = I.prob, replace = FALSE)
    Iv <- c(Iv, i)
    res <- res - I[[i]]$v
    I.names <- I.names[I.names != i]
    I.prob <- I.prob[I.names]
  }
  Iv
}
#' Append pub/sub to catalogue
#' @description Update the content catalogue by adding publishers and/or subscribers information. This is equivelent to constructing the Information space of the RV
#'
#' @export
#' @param P set of publishers (i.e. providers of information)
#' @param S set of subscribers (i.e. consumers)
#' @param I content catalogue
#' @param c update?<FALSE|TRUE>, default FALSE, decided whether or not to update the global ITEMS list
#' @return updated content catalogue with publisher and/or subscriber information
CatalogueAppendPubSub <- function(P, S, I, c=FALSE)
{
  #' update
  if(!missing(S))
  {
    for(s in S)
    {
      sI.names <- names(s$i)
      sI <- s$i
      I[sI.names] <- mapply(SetAdditionalItemSubscriber, I[sI.names], sI, MoreArgs = list(s = s$s), SIMPLIFY = FALSE)
    }
    if(c)
      ITEMS <<- I
  }
  else
    logwarn("Subscribers (S) not provided")
  if(!missing(P))
  {
    for(p in P)
    {
      pI.names <- names(p$i)
      I[pI.names] <- mapply(SetAdditionalItemPublisher, I[pI.names], l = p$i, MoreArgs = list(p = p$p), SIMPLIFY = FALSE)
    }
    if(c)
      ITEMS <<- I
  }
  else
    logwarn("Publishers (P) not provided")
  #' remove list elements where there are no subscribers, as they are elements with no demand. it happens with some graphs and some light load
  I[sapply(I, function(x){is.null(x$s)})] <- NULL
  I
}
#' Append reflectors to catalogue
#' @description Assign reflectors to each streaming item in the content catalogue. This means the catalogue represents a collection of live streams, for each i in ITEMS, the names i is the STREAM IDENTIFIER.
#' @description Reflectors don't cache information locally, hence they are represneted by a standalone element in the 'i' list, not included in i$p or i$l
#'
#' @export
#' @param I content catalogue
#' @param R set of reflectors
#' @param c update? <FALSE | TRUE>, default FALSE, decided whether or not to update the global ITEMS list
#' @return updated content catalogue to include reflectors
CatalogueAppendReflectors <- function(I, R, c)
{
  I <- lapply(I, SetItemReflector, R)
  if(c)
    ITEMS <<- I
  I
}
