#  construct.R constructs regular expression strings such as filenames and paths based on configuration object
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

#' construct full filenames
#' @description  construct full filename from cfgs object and depending on the filename.
#'
#' @export
#' @param cfgs configuration object, given in a list of elements
#' @param obj character vector giving the name of the object. This will be the first part of the filename
#' @param dir character vector of the directory within cfgs
#' @param env configuration enviroment used to parse cfgs, e.g. 'default' for data generation or 'simulate' for simulation data
ConstructFullFileName <- function(cfgs, obj, dir = '', env = 'simulate')
{
  #' construct combinations of expressions, will be used to create file names of results objects
  gnames <- GetGraphName(cfgs$gcfg$gun.set)

  if(env == 'simulate') {
    #' set the number of tests to be simulated, if not present in cfgs$tcfg (i.e. it has not be set in config.yml)
    cfgs$tcfg$sim.tests <- ifelse(is.null(cfgs$tcfg$sim.tests), cfgs$tcfg$tests, cfgs$tcfg$sim.tests)
    #' make sure the expression name includes the simulation number of tests, not the generation number.
    cfgs$tcfg$sim.expr <- paste0('-scale_', cfgs$tcfg$load['lb'], '_', cfgs$tcfg$load['ub'], '_', cfgs$tcfg$load['st'], '-tests_', cfgs$tcfg$sim.tests)
    icn.expr <- expand.grid(texpr = cfgs$tcfg$sim.expr, eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr, gnames = gnames, iexpr = cfgs$icfg$expr)
  }
  if(env == 'default' | env == 'generate')
  {
    icn.expr <- expand.grid(texpr = cfgs$tcfg$expr, eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr, gnames = gnames, iexpr = cfgs$icfg$expr)
  }
  if({obj == 'icn.uc'} | {obj == 'icn.upl'})
    return(file.path(dir, obj, '-g_', icn.expr$gnames, '-o_', icn.expr$oexpr, '-e_', icn.expr$eexpr, icn.expr$texpr, '.RData', fsep = ''))
  if(obj == 'icn.mc') {
    icn.expr <- expand.grid(texpr = cfgs$tcfg$sim.expr, ca = cfgs$tcfg$t, L = cfgs$tcfg$L, eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr, gnames = gnames, iexpr = cfgs$icfg$expr)
    return(file.path(dir, obj, '-g_', icn.expr$gnames, '-o_', icn.expr$oexpr, '-e_', icn.expr$eexpr, '-ca_', icn.expr$ca, '-L_', icn.expr$L, icn.expr$texpr, '.RData', fsep = ''))
  }
  if({obj == 'ip'} | {obj == 'ip.pl'}) {
    ip.expr <- expand.grid(texpr = cfgs$tcfg$sim.expr, dexpr = cfgs$dcfg$expr, eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr, gnames = gnames, iexpr = cfgs$icfg$expr)
    return(file.path(dir, obj, '-g_', ip.expr$gnames, '-o_', ip.expr$oexpr, '-e_', ip.expr$eexpr, '-d_', ip.expr$dexpr, ip.expr$texpr, '.RData', fsep = ''))
  }
}

#' construct filenames
#' @description  construct filename from cfgs object and depending on the filename. This is only filenames within a directory (without the full path)
#'
#' @export
#' @param cfgs configuration object, given in a list of elements
#' @param obj character vector giving the name of the object. This will be the first part of the filename
#' @param env configuration enviroment used to parse cfgs, e.g. 'default' for data generation or 'simulate' for simulation data
ConstructFileName <- function(cfgs, obj, env = 'simulate')
{
  #' construct combinations of expressions, will be used to create file names of results objects
  gnames <- GetGraphName(cfgs$gcfg$gun.set)

  if(env == 'simulate') {
    #' set the number of tests to be simulated, if not present in cfgs$tcfg (i.e. it has not be set in config.yml)
    cfgs$tcfg$sim.tests <- ifelse(is.null(cfgs$tcfg$sim.tests), cfgs$tcfg$tests, cfgs$tcfg$sim.tests)
    #' make sure the expression name includes the simulation number of tests, not the generation number.
    cfgs$tcfg$sim.expr <- paste0('-scale_', cfgs$tcfg$load['lb'], '_', cfgs$tcfg$load['ub'], '_', cfgs$tcfg$load['st'], '-tests_', cfgs$tcfg$sim.tests)
    icn.expr <- expand.grid(texpr = cfgs$tcfg$sim.expr, eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr, gnames = gnames, iexpr = cfgs$icfg$expr)
  }
  if(env == 'default' | env == 'generate')
  {
    icn.expr <- expand.grid(texpr = cfgs$tcfg$expr, eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr, gnames = gnames, iexpr = cfgs$icfg$expr)
  }

  file.path(obj, '-g_', icn.expr$gnames, '-o_', icn.expr$oexpr, '-e_', icn.expr$eexpr, icn.expr$texpr, '.RData', fsep = '')
}
