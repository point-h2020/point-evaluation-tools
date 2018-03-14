#  subs.R handle subscribers (consumption points) lists
# Copyright (C) 2017-2018  Mays AL-Naday and Janne Riihijärvi
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

#' Subscribe every node to a set of items
#' @description Generate subscriptions per node, i.e. associate a set of items (subset of the global set of items) with a node.
#'
#' @param u number of active users associated with the node
#' @param I a sorted set, representing the global set of items in the network. I is the probabilities of ITEMS, the global list of items.
#' @param c create?<FALSE|TRUE>, default FALSE, decided whether or not to create the global SUBSCRIBERS list
#' @return list of subscribers, each of which is a tuple object (s,v,i), where s is subscriber id, v numeric vector of subscriptions per item, i is character vector of items Ids
SubscribePerNode <- function(u, I, c = FALSE)
{
  s <- names(u)
  u <- round(u)
  sI <- lapply(u, SubscribePerItemPerNode, I)
  subscribers <- mapply(SetSubscribersList, s, u, sI, SIMPLIFY = FALSE)
  if(c)
    assign("SUBSCRIBERS", subscribers, envir = .GlobalEnv)
  subscribers
}
#' Subscribe a node to set of items
#' @description Generate subscriptions per item per node by approtioning the total number of subscriptions according to some probability distribution such as Zipf
#'
#' @param u access population, i.e. the number of active users associated with an access node
#' @param I a sorted set, representing the global set of items in the network. I is the probabilities of ITEMS, the global list of items.
#' @return list of subscriptions per item per node
SubscribePerItemPerNode <- function(u, I)
{
  s.id <- names(u) # nodes with users are subscribers
  I.id <- names(I)
  sI <- sample(I.id, size = u, replace = TRUE, prob = I)
  sI <- table(sI, dnn = NULL) # NULL prevents subnaming of the table
  sI
}

#' Generate Poisson distributed number of users
#' @description Generate Poisson distributed subscription counts per network location provided there are total_users in total in the system
#'
#' @author Janne Riihijärvi
#' @param nodes.population a vector nodes populations within the network
#' @param network.population total population of all nodes in the network, i.e.sum(nodes.population)
#' @param total.users total number of active users in the network <= network.population
#' @return numeric value selected from a Poisson distribution
JarSubsAssignUsersForTimeSlot <- function(nodes.population, network.population, total.users) {
  unlist(lapply(nodes.population, function (pop) { stats::rpois(1, round(total.users * (pop/network.population)))}))
}
