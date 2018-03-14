#  helper.R set of misc functions to help in modelling
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

#' calculate node centrality
#' @description calculate the centrality of all nodes in graph
#'
#' @export
#' @param g network graph (igraph object)
#' @return vector of cenrality for each node
HelperCentralNode <- function(g)
{
  c <- igraph::betweenness(g)
  names(c) <- V(g)
  n <- names(c[c == max(c)])
  n
}
#' modified version of RBGL extractPath function
#'
#' @description to include wavelength augumented graphs
#'
#' @export
#' @param s source of the path
#' @param f destination of the path
#' @param pens pens of the source s
#' @return vector of nodes giving the path from s to f
ExtractPath <- function (s, f, pens)
{
  path <- f
  maxl <- length(pens)
  i <- 0
  #     print(pens[f])
  while (path[1] != s) {
    if (i > maxl) {
      path <- "NA"
      break
    }
    path <- c(pens[f], path)
    f <- pens[f]
    # 	path <- c(names(pens[f]), path)
    i <- i + 1
  }
  path
}
#' Update path load
#' @description Update the load on the edges of a path in the global EdgeMatrix EM.
#' @note CAREFUL!!!, this function updates the global variable EM
#'
#' @export
#' @param peM matrix of [from, to] edges of a path (branch)
#' @param l number of subscriptions of item of node.
#' @return updated global EM
HelperUpdatePathLoadOnEM <- function(peM, l)
{
  ri <- which(paste0(EM[, 'from'], '|', EM[,'to']) %in% paste0(peM[, 'from'], '|', peM[, 'to']))
  EM[ri, 'load'] <<- EM[ri,'load'] + l
  #update the edge weights of es in the global EM!!!, don't worry about NAs in es, they will be ignored as they have no match in eM
  length(ri) * l
}
#' Update path load
#' @description Update the load on the edges of a path in the global AdjacencyMatrix EM
#' @note CAREFUL!!!, this function updates the global variable EM
#' @note DO NOT use diag()!!!!
#'
#' @param r sequence of nodes representing a path in the network.
#' @param l number of subscriptions of item of node.
#' @return updated global EM
HelperUpdatePathLoadOnAm <- function(r, l)
{
  rl <- length(r)
  if (rl == 1)  #' this also includes r = NA, as the previous condition was if (all(is.na(r)))
    return(0)
  EM[r[1:(rl-1)], r[2:rl]] <<- EM[r[1:(rl-1)], r[2:rl]] + diag(l, rl - 1)
  (rl - 1) * l
}
#' Update tree load
#' @description Update the load on the edges of a multicast tree in the global AdjacencyMatrix EM
#' @note CAREFUL!!!, this function updates the global variable EM
#'
#' @export
#' @param r sequence of nodes representing a path in the network.
#' @param l number of subscriptions of item of node.
#' @return updated global EM
HelperUpdateTreeLoadOnAm <- function(t, l)
{
  added <- 0
  et <- unique(unlist(t))
  if(length(et) == 1)
    return(added) #' cached node only, retrun 0, no additions
  tm <- matrix(data = 0,nrow = length(et), ncol = length(et), dimnames = list(a = et, b=et))
  for(b in t)
  {
    bl <- length(b)
    if(bl > 1)
    {
      dval <- as.matrix(tm[b[1:(bl-1)], b[2:bl]])
      dval <- dval[row(dval) == col(dval)] #' get the diagonal values only, as they represent links between edges
      tm[b[1:(bl-1)], b[2:bl]] <- diag(
        ((dval < l[b[bl]]) * l[b[bl]]) +
          ((!(dval < l[b[bl]])) * dval),
        bl - 1
        )
    }
  }
  added <- sum(tm)
  EM[et, et] <<- EM[et, et] + tm
  added
}
#' Update tree load
#' @description Extract the set of edges in a tree list. Common edges will be merged into one edge indicating multicast
#'
#' @export
#' @param t tree in the form of list, each element represent a branch in the tree
#' @param l load
#' @param iv item volume
#' @return matrix of edges in the tree
HelperUpdateTreeLoadonEM <- function(t, r, l, iv)
{
  added <- c()
  el <- lapply(t, HelperPathToEdges)
  s <- names(el)
  leafs <- s[s != r]
  el <- do.call(rbind, el)
  el.na <- el[is.na(el[,1]),]
  el.not.na <- unique(el[!is.na(el[,1]),])
  trunk.el <- matrix(el[duplicated(el),], ncol=2) #' trunk edges are shared between leafs of the tree
  colnames(trunk.el) <- c('from', 'to')
  trunk.load <- max(l[leafs])
  leaf.el <- matrix(el[(!duplicated(el)) & (!is.na(el[,'to'])) & (!(el[,'to'] %in% el[, 'from'])),], ncol=2) #' extract leaf edges as their load would be different
  colnames(leaf.el) <- c('from', 'to')
  leaf.load <- l[as.character(leaf.el[, 'to'])]
  added <- HelperUpdatePathLoadOnEM(leaf.el, leaf.load) #, iv
  added <- added + HelperUpdatePathLoadOnEM(trunk.el, trunk.load) #, iv
  added
}
#' convert graph nodes from character to integer
#'
#' @export
#' @param c character
#' @return integer
HelperConvertCharToInt <- function(c)
{
  i <- tryCatch(
    {
    as.integer(c)
    }, error = function(e) {
      cat("wrong input\n")
    }
  )
  i
}
#' convert graph nodes from integer to character
#'
#' @export
#' @param c character
#' @return integer
HelperConvertIntToChar <- function(i)
{
  c <- tryCatch(
    {
      as.character(i)
    }, error = function(e) {
      cat("wrong input\n")
    }
  )
  c
}
#' Round up a number
#' @export
#' @description taken from http://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
HelperRoundUp <- function(x,
                    nice=c(1,2,4,5,6,7,8,9,10)
                    )
  {
  if(length(x) != 1) stop("'x' must be of length 1")
  10 ^ floor(log10(x)) * nice[[which(x <= 10 ^ floor(log10(x)) * nice)[[1]]]]
}

#' Round up a number
#' @export
#' @description adapted from the the function HelperRoundUp for finer granularity
HelperRoundUpGranular <- function(x,
                           nice=c(1,2,4,5,6,7,8,9,10)
)
{
  if(length(x) != 1) stop("'x' must be of length 1")
  y <- 10 ^ floor(log10(x))
  for(i in nice )
  {
    z <- x %% y
    z <- 10 ^ floor(log10(z))
    y <- y + z
  }
  z <- x %% y
  z <- 10 ^ floor(log10(z)) *  nice[[which(z <= 10 ^ floor(log10(z)) * nice)[[1]]]]
  y <- y + z
  y
}

#' Summarizes data
#'
#' @description Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
#' @references The function is taken from Rcookbook online at: \link{http://www.cookbook-r.com/Manipulating_data/Summarizing_data/}
#'
#' @export
#' @param data a data frame.
#' @param measurevar the name of a column that contains the variable to be summariezed
#' @param groupvars a vector containing names of columns that contain grouping variables
#' @param na.rm a boolean that indicates whether to ignore NA's
#' @param conf.interval the percent range of the confidence interval (default is 95%)
HelperSummarySE <- function(data=NULL,
                      measurevar,
                      groupvars=NULL,
                      na.rm=FALSE,
                      conf.interval=.95,
                      .drop=TRUE) {
  #' New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  #' This is does the summary; it's not easy to understand...
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                 .fun= function(xx, col, na.rm) {
                   c( N    = length2(xx[,col], na.rm=na.rm),
                      mean = mean   (xx[,col], na.rm=na.rm),
                      sd   = sd     (xx[,col], na.rm=na.rm)
                   )
                 },
                 measurevar,
                 na.rm)

  #' Rename the "mean" column
  datac <- rename(datac, c("mean"=measurevar))
  datac$se <- datac$sd/ sqrt(datac$N)  #' Calculate standard error of the mean
  #' Confidence interval multiplier for standard error
  #' Calculate t-statistic for confidence interval:
  #' e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- stats::qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}
