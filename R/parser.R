#  parser.R parse yml-based config files to config objects known to the evaluator
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

#' Parse configuration file in YAML format
#' @description parse all the elements in the YAML configuration file and return a list of grouped subsets of configurations
#' @details This function calles a number of smaller functions that looks for pre-defined groups in the yml, if found, then the elements within the group is retruned. Currently, only groups that are defined in the template \code{config.yml} are included, to include new groups, new atom parsing function need to be developed and added to this function.
#'
#' @param config.file configuration file in YAML format
#' @param config.active configuration enviroment of the logger
#' @param config.env configuration enviroment inside the YAML file, default to 'default'
#' @export
#' @return a list of grouped configurations. Currently, the list includes configs of: \code{dir} - i.e. directories to be used in the simulation, \code{graph}, \code{catalogue} - i.e. CDN items catalogue, \code{origin} - i.e. content originators, \code{surrogate}, i.e. edge servers, \code{dns} - i.e. DNS and \code{permutation} - i.e. number of simulation rounds, and load on the network. the permutations here are multiplicative of the parameters.
ParseConfig <- function(config.file, config.active = 'default', config.env = 'default')
{
  #set logging level
  Sys.setenv(R_CONFIG_ACTIVE = config.active)
  logging::loginfo('PAR: config enviroment: %s', config.env)
  #create configs enviroment
  cfgs <- new.env(parent = emptyenv())
  cfgs$cores <- ParseCores(config.file, config.env = config.env)
  cfgs$paths <- ParsePaths(config.file, config.env = config.env)
  #parse the various configurations from the yml config file
  cfgs$icfg <- ParseCatalogue(config.file = config.file, config.env = config.env)
  logging::logdebug('PAR: Catalogue: N:%s, d:%s, dparam:%s, v:%s ', cfgs$icfg$N, cfgs$icfg$d, cfgs$icfg$d.param, cfgs$icfg$v)
  cfgs$ocfg <- ParseOrigins(config.file = config.file, config.env = config.env)
  logging::logdebug('PAR: Origins: k:%s, a:%s, opt:%s', cfgs$ocfg$k, cfgs$ocfg$a, cfgs$ocfg$opt)
  cfgs$ecfg <- ParseSurrogates(config.file = config.file, config.env = config.env)
  logging::logdebug('PAR: Surrogate: k:%s, a:%s, opt:%s', cfgs$ecfg$k, cfgs$ecfg$a, cfgs$ecfg$opt)
  cfgs$dcfg <- ParseDns(config.file = config.file, config.env = config.env)
  logging::logdebug('PAR: DNS: k:%s, opt:%s', cfgs$dcfg$k, cfgs$dcfg$opt)
  #' generate Edge-Origin and DNS-Edge-Origin expression permutatins
  cfgs$eoexpr <- expand.grid(eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr)
  cfgs$deoexpr <- expand.grid(dexpr = cfgs$dcfg$expr, eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr)
  cfgs$tcfg <- ParsePermutation(config.file = config.file, config.env = config.env)
  logging::logdebug('PAR: Permutations: tests:%s, load:%s', cfgs$tcfg$tests, cfgs$tcfg$load)
  cfgs$gcfg <- ParseGraph(config.file = config.file, config.env = config.env)
  #load network graphs
  gun.set <- GetGraph(cfg = cfgs$gcfg, paths = cfgs$paths)
  logging::logdebug('PAR: graph, users/node, graph.name, Distance & Adj Matrices: ')
  cfgs$gcfg$gun.set <- base::lapply(gun.set, function(x){
    #' get the adjacency matrix of the graph
    eM <- GraphToAdjMatrix(x$g)
    #' Calculate the Distance matrix of g
    ed <- eM[eM > 0]
    res <- AlgoDijAllPaths(x$g, ed)
    x$eM <- eM
    x$D <- res$D
    x$R <- res$R
    logging::logdebug('PAR: %s', x$g.name)
    x
  })
  cfgs
}
#' Parse number of cores in YAML configuration file
#' @description parse a YAML config file to extract number of CPU cores to be utilised in parallel processing
#'
#' @param config.file full path to a .yml config file containing modelling graph
#' @param config.env configuration enviroment, default to 'default'
#' @return numeric number of cores
#' @export
ParseCores <- function(config.file, config.env = 'default')
{
  cores <- config::get(value = 'cores', config = config.env, file = config.file)
  logging::loginfo('PAR: #CPU cores: %s ', cores)
  cores
}
#' parse a YAML config file to extract directory paths
#'
#' @param config.file full path to a .yml config file containing a default template
#' @param config.env configuration enviroment, default to 'default'
#' @return directory paths
#' @export
ParsePaths <- function(config.file, config.env = 'default')
{
  dirs <- config::get(value = 'dirs', config = config.env, file = config.file)
  dir.names <- with(dirs, names(dirs))
  if(is.null(dir.names))
  {
    logging::logerror('PAR: missing common configuration paramter, dirs (i.e. directories)')
    return(-1)
  }
  home.dir <- with(dirs, home)
  logging::loginfo('PAR: Home path is: %s ', home.dir)
  sub.dirs <- dirs[dir.names[dir.names != 'home']]
  logging::loginfo('PAR: Sub-directories in home are: %s', dir.names[dir.names != 'home'])
  paths <- file.path(home.dir, sub.dirs, fsep = "")
  names(paths) <- dir.names[dir.names != 'home']
  #add the home path
  paths <- c(paths, home = home.dir)
  paths
}
#' parse modelling permutations from a YAML config file
#'
#' @param config.file full path to a .yml config file containing modelling permutations
#' @param config.env configuration enviroment, default to 'default'
#' @return modelling permutations
#' @export
ParsePermutation <- function(config.file, config.env = 'default')
{
  perms <- config::get(value = 'permutation', config = config.env, file = config.file)
  params <- names(perms)
  if(!('load' %in% params))
    logging::logwarn('PAR: Missing load parameter')
  else
    names(perms$load) <- c('lb', 'ub', 'st')
  sexpr <- paste0(perms$load, collapse = "_")
  perms$expr <- paste0('-scale_', sexpr, '-tests_', perms$tests)
  perms
}
#' parse graph configurations from a YAML config file
#'
#' @param config.file full path to a .yml config file containing modelling graph
#' @param config.env configuration enviroment, default to 'default'
#' @return graph configurations
#' @export
ParseGraph <- function(config.file, config.env = 'default')
{
  gcfg <- config::get(value = 'graph', config = config.env, file = config.file)
  gcfg
}
#' parse the catalogue of items from a YAML config file
#' @description This is listed under the 'catalogue' section of the config file
#' @note this function is similar to \code{ParseItems}, except the latte is used to parse the catalogue section under the alternative name \code{items}
#'
#' @param config.file .yml configuration file containing a section named items with essential elements including: {size = set size, a = exponent of distribution, iv = item volume or volumes to choose from, ig = item ignorance (i.e. fraction of popularity distribution to be ignored or assumed cached)}
#' @param config.env configuration enviroment, default to \code{default}
#' @return design parameters of the content catalogue
#' @export
ParseCatalogue <- function(config.file, config.env = 'default')
{
  icfg <- config::get(value = 'catalogue', config = config.env, file = config.file)
  #rename the size parameter to 'N'
  icfg$N <- icfg$size
  icfg$size <- NULL
  #convert the distribution parameters into a dataframe of combinations, then break it down to list elements
  dp <- expand.grid(icfg$d.param)
  dp.names <- colnames(dp)  #get the names of the distribution params
  dp <- as.data.frame(t(dp))
  names(dp) <- NULL
  dparam <- as.list(dp)
  dparam <- lapply(dparam, function(x,y){names(x) <- y; x}, dp.names)
  icfg$d.param <- dparam
  #generate combinations and directory names vectors
  combo <- expand.grid(dparam = dparam, d = icfg$d, N = icfg$N, v = list(icfg$v), stringsAsFactors = FALSE)
  icfg$expr <- mapply(GetCataloguePath, N = combo$N, d = combo$d, dparams = dparam, v=combo$v)
  combo$expr <- icfg$expr
  icfg$combo <- combo
  icfg
}
#' parse modeling origins from a YAML config file
#' @description This is grouped under the \code{origins} list
#'
#' @param config.file .yml configuration file containing a section named origins
#' @param config.env configuration enviroment, default to 'default'
#' @return dimensioning parameters of origins in the network - this equivelent to dimensioning CDN entry points
#' @export
ParseOrigins <- function(config.file, config.env = 'default')
{
  ocfg <- config::get(value = 'origin', config = config.env, file = config.file)
  combo <- expand.grid(ocfg)
  ocfg$expr <- apply(combo, 1, function(x){paste0(names(x), "_", x, collapse = "-")})
  combo$expr <- ocfg$expr
  ocfg$combo <- combo
  ocfg
}
#' parse modeling surrogates from a YAML config file
#' @description This is grouped under the \code{surrogate} list
#'
#' @param config.file .yml configuration file containing a section named surrogate
#' @param config.env configuration enviroment, default to 'default'
#' @return dimensioning parameters of surrogates in the network - this equivelent to dimensioning CDN edge servers
#' @export
ParseSurrogates <- function(config.file, config.env = 'default')
{
  ecfg <- config::get(value = 'surrogate', config = config.env, file = config.file)
  combo <- expand.grid(ecfg)
  ecfg$expr <- apply(combo, 1, function(x){paste0(names(x), "_", x, collapse = "-")})
  combo$expr <- ecfg$expr
  ecfg$combo <- combo
  ecfg
}
#' parse modeling DNS from a YAML config file
#' @description This is grouped under the \code{dns} list
#'
#' @param config.file .yml configuration file containing a section named dns
#' @param config.env configuration enviroment, default to 'default'
#' @return dimensioning parameters of DNS in the network - this equivelent to dimensioning CDN local DNS points
#' @export
ParseDns <- function(config.file, config.env = 'default')
{
  dcfg <- config::get(value = 'dns', config = config.env, file = config.file)
  combo <- expand.grid(dcfg)
  dcfg$expr <- apply(combo, 1, function(x){paste0(names(x), "_", x, collapse = "-")})
  combo$expr <- dcfg$expr
  dcfg$combo <- combo
  dcfg
}
