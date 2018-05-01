#  simulate.R simulate/model a scenario for all input permutations
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

#' simulate routing ICN demands in a native ICN or *-over-ICN network
#'
#' @export
#' @param cfgs configurations as parsed from config file in YAML format, config/config.yml
SimIcnUnicast <- function(cfgs)
{
  #' define output containers
  icn <- list()
  doParallel::registerDoParallel(cfgs$cores)
  dir.create(cfgs$paths['result'], showWarnings = FALSE)
  gset <- cfgs$gcfg$gun.set
  cnum <- length(cfgs$icfg$expr)
  #' set the number of tests to be simulated, if not present in cfgs$tcfg (i.e. it has not be set in config.yml)
  cfgs$tcfg$sim.tests <- ifelse(is.null(cfgs$tcfg$sim.tests), cfgs$tcfg$tests, cfgs$tcfg$sim.tests)
  #' make sure number of simulation tests is smaller or equal to the number of generation tests, otherwise, its invalide data situation
  if(cfgs$tcfg$sim.tests > cfgs$tcfg$tests)
  {
    logging::logerror('Number of simulation tests: %s larger than number of generation tests: %s. Not enought input data to be simulated!', cfgs$tcfg$sim.tests, cfgs$tcfg$tests)
    return(-1)
  }
  #' make sure the expression name includes the simulation number of tests, not the generation number.
  cfgs$tcfg$sim.expr <- paste0('-scale_', cfgs$tcfg$load['lb'], '_', cfgs$tcfg$load['ub'], '_', cfgs$tcfg$load['st'], '-tests_', cfgs$tcfg$sim.tests)
  #' create subdirectories for the results of each catalogue
  ccdir <- file.path(cfgs$paths['store'], cfgs$icfg$expr, fsep = '')
  resdir <- file.path(cfgs$paths['result'], cfgs$icfg$expr, fsep = '')
  for(i in 1:cnum)
  {
    dir.create(ccdir[i], showWarnings = FALSE)
    dir.create(resdir[i], showWarnings = FALSE)
    #' create results directory
    for(g in gset)
    {
      logging::loginfo('SIM: ICN UNICAST catalogue:%s graph:%s', cfgs$icfg$expr[i],  g$g.name)
      cc <- list()
      #' construct results filenames
      icn.fn <- file.path(resdir[i], 'icn.uc', '-g_', g$g.name, '-o_', cfgs$eoexpr$oexpr, '-e_', cfgs$eoexpr$eexpr, cfgs$tcfg$sim.expr, '.RData', fsep = '')
      cc.fn <- file.path(ccdir[i], 'cc', '-g_', g$g.name, '-o_', cfgs$eoexpr$oexpr, '-e_', cfgs$eoexpr$eexpr, cfgs$tcfg$expr, '.RData', fsep = "")
      # logging::logdebug('SIM: ICN UC: home: %s, g: %s, oexp: %s, eexp: %s perm: %s', resdir[i], g$g.name, cfgs$eoexpr$oexpr, cfgs$eoexpr$eexpr, cfgs$cfgs$tcfg$sim.expr)
      sim.not.exit <- which(file.access(icn.fn, mode = 4) < 0)
      logging::logdebug('SIM: ICN UC %s instances out of %s total', length(sim.not.exit), length(icn.fn))
      foreach::foreach(j = sim.not.exit) %dopar%
      {
        logging::logdebug('SIM: ICN UC instance %s',j)
        #' cc[[g]] is 3-Level list:
        #'  EO.combo
        #'  |-- scale.index
        #'      |-- test.number
        load(file = cc.fn[j])
        SimIcnUnicastInstance(cc = cc, g = g, scale = cfgs$tcfg$load, tests = cfgs$tcfg$sim.tests, path = icn.fn[j])
      }
    }
  }
}
#' simulate ICN or *-over-ICN while input data generation is in process
#'
#' @export
#' @param cfgs
GSimIcnUnicast <- function(cfgs)
{
  icn <- list()
  doParallel::registerDoParallel(cfgs$cores)
  dir.create(cfgs$paths['result'], showWarnings = FALSE)
  gset <- cfgs$gcfg$gun.set
  cnum <- length(cfgs$icfg$expr)

  #' set the number of tests to be simulated, if not present in cfgs$tcfg (i.e. it has not be set in config.yml)
  cfgs$tcfg$sim.tests <- ifelse(is.null(cfgs$tcfg$sim.tests), cfgs$tcfg$tests, cfgs$tcfg$sim.tests)

  #' make sure number of simulation tests is smaller or equal to the number of generation tests, otherwise, its invalide data situation
  if(cfgs$tcfg$sim.tests > cfgs$tcfg$tests)
  {
    logging::logerror('Number of simulation tests: %s larger than number of generation tests: %s. Not enought input data to be simulated!', cfgs$tcfg$sim.tests, cfgs$tcfg$tests)
    return(-1)
  }
  #' make sure the expression name includes the simulation number of tests, not the generation number.
  cfgs$tcfg$sim.expr <- paste0('-scale_', cfgs$tcfg$load['lb'], '_', cfgs$tcfg$load['ub'], '_', cfgs$tcfg$load['st'], '-tests_', cfgs$tcfg$sim.tests)

  #' create subdirectories for the results of each catalogue
  ccdir <- file.path(cfgs$paths['store'], cfgs$icfg$expr, fsep = '')
  resdir <- file.path(cfgs$paths['result'], cfgs$icfg$expr, fsep = '')
  for(i in 1:cnum)
  {
    #' create subdirectories for the results of each catalogue
    dir.create(ccdir[i], showWarnings = FALSE)
    dir.create(resdir[i], showWarnings = FALSE)
    #' create results directory
    for(g in gset)
    {
      logging::loginfo('SIM: ICN UNICAST catalogue:%s graph:%s', cfgs$icfg$expr[i],  g$g.name)
      cc <- list()
      #' construct results filenames
      icn.fn <- file.path(resdir[i], 'icn.uc', '-g_', g$g.name, '-o_', cfgs$eoexpr$oexpr, '-e_', cfgs$eoexpr$eexpr, cfgs$tcfg$sim.expr, '.RData', fsep = '')
      cc.fn <- file.path(ccdir[i], 'cc', '-g_', g$g.name, '-o_', cfgs$eoexpr$oexpr, '-e_', cfgs$eoexpr$eexpr, cfgs$tcfg$expr, '.RData', fsep = "")
      # logging::logdebug('SIM: ICN UC: home: %s, g: %s, oexp: %s, eexp: %s perm: %s', resdir[i], g$g.name, cfgs$eoexpr$oexpr, cfgs$eoexpr$eexpr, cfgs$cfgs$tcfg$sim.expr)
      sim.not.exit <- which(file.access(icn.fn, mode = 4) < 0)
      logging::logdebug('SIM: files: %s', icn.fn[1])
      logging::logdebug('SIM: ICN UC %s instances out of %s total', length(sim.not.exit), length(icn.fn))
      while (length(sim.not.exit) > 0) {
        foreach::foreach(j = sim.not.exit) %dopar%
        {
          #' cc[[g]] is 3-Level list:
          #'  EO.combo
          #'  |-- scale.index
          #'      |-- test.number
          sim <- tryCatch(load(cc.fn[j]), error = function(e) { return("")})
          if (sim == 'cc')
          {
            logging::logdebug('SIM: ICN UC instance %s',j)
            SimIcnUnicastInstance(cc = cc, g = g, scale = cfgs$tcfg$load, tests = cfgs$tcfg$sim.tests, path = icn.fn[j])
          }
        }
        sim.not.exit <- which(file.access(icn.fn, mode = 4) < 0)
      }
    }
  }
}
#' simulate provisioned capacity for ICN unicast traffic
#'
#' @export
#' @param cc pub/sub state for a single content catalogue
#' @param g network graph list including graph object, per node population, name, distance matrix and edge matrix
#' @param scale ratio of population as active users
#' @param tests number of tests to run
#' @param path full path as to where to store the data, including the filename
SimIcnUnicastInstance <- function(cc, g, scale, tests, path)
{
  si <- seq(scale['lb'], scale['ub'], scale['st']) #' scale index
  sl <- length(si)
  icn.uc <- list()
  for(s in 1:sl)
  {
    icn.uc[[s]] <- vapply(cc[[s]][1:tests], RouteIcnUnicastDijkstra, as.list(c(1:4)), g = g)
  }
  icn.uc$ocfg <- cc$ocfg
  icn.uc$ecfg <- cc$ecfg
  save(icn.uc, file = path)
  logging::logdebug('SIM: ICN UC complete, store:%s', path)
}

# ICN Multicast -----------------------------------------------------------

#' simulate routing ICN demands in a native ICN or *-over-ICN network
#'
#' @export
#' @param cfgs configurations as parsed from config file in YAML format, config/config.yml
SimIcnMulticast <- function(cfgs)
{

  #' define output containers
  icn <- list()
  #' define prallel cluster of workers
  doParallel::registerDoParallel(cfgs$cores)
  #' construct permutations names and load relevant input data
  expr <- expand.grid(eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr, ca = cfgs$tcfg$t, L = cfgs$tcfg$L)
  gset <- cfgs$gcfg$gun.set
  cnum <- length(cfgs$icfg$expr)
  #' set the number of tests to be simulated, if not present in cfgs$tcfg (i.e. it has not be set in config.yml)
  cfgs$tcfg$sim.tests <- ifelse(is.null(cfgs$tcfg$sim.tests), cfgs$tcfg$tests, cfgs$tcfg$sim.tests)
  #' make sure number of simulation tests is smaller or equal to the number of generation tests, otherwise, its invalide data situation
  if(cfgs$tcfg$sim.tests > cfgs$tcfg$tests)
  {
    logging::logerror('Number of simulation tests: %s larger than number of generation tests: %s. Not enought input data to be simulated!', cfgs$tcfg$sim.tests, cfgs$tcfg$tests)
    return(-1)
  }
  #' make sure the expression name includes the simulation number of tests, not the generation number.
  cfgs$tcfg$sim.expr <- paste0('-scale_', cfgs$tcfg$load['lb'], '_', cfgs$tcfg$load['ub'], '_', cfgs$tcfg$load['st'], '-tests_', cfgs$tcfg$sim.tests)
  #' create subdirectories for the results of each catalogue
  ccdir <- file.path(cfgs$paths['store'], cfgs$icfg$expr, fsep = '')
  resdir <- file.path(cfgs$paths['result'], cfgs$icfg$expr, fsep = '')
  for(i in 1:cnum)
  {
    dir.create(ccdir[i], showWarnings = FALSE)
    dir.create(resdir[i], showWarnings = FALSE)
    #' create results directory
    for(g in gset)
    {
      logging::loginfo('SIM: ICN MULTICAST catalogue:%s graph:%s', cfgs$icfg$expr[i],  g$g.name)
      #' construct results filenames
      icn.fn <- file.path(resdir[i], 'icn.mc', '-g_', g$g.name, '-o_', expr$oexpr, '-e_', expr$eexpr, '-ca_', expr$ca, '-L_', expr$L, cfgs$tcfg$sim.expr, '.RData', fsep = '')
      cc.fn <- file.path(ccdir[i], 'cc', '-g_', g$g.name, '-o_', expr$oexpr, '-e_', expr$eexpr, cfgs$tcfg$expr, '.RData', fsep = "")
      sim.not.exit <- which(file.access(icn.fn, mode = 4) < 0)
      logging::logdebug('SIM: ICN MULTICAST %s instances out of %s total', length(sim.not.exit), length(icn.fn))
      foreach::foreach(j = sim.not.exit) %dopar%
      {
        logging::logdebug('SIM: ICN MC instance %s', j)
        #' cc[[g]] is 3-Level list:
        #'  EO.combo
        #'  |-- scale.index
        #'      |-- test.number
        load(file = cc.fn[j])
        SimIcnMulticastInstance(cc = cc, g = g, scale = cfgs$tcfg$load, tests = cfgs$tcfg$sim.tests, t = expr$ca[j], L = expr$L[j], path = icn.fn[j])
      }
    }
  }
}

#' simulate routing ICN demands in a native ICN or *-over-ICN network
#'
#' @export
#' @param cfgs configurations as parsed from config file in YAML format, config/config.yml
GSimIcnMulticast <- function(cfgs)
{

  #' define output containers
  icn <- list()
  #' define prallel cluster of workers
  doParallel::registerDoParallel(cfgs$cores)
  #' construct permutations names and load relevant input data
  expr <- expand.grid(eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr, ca = cfgs$tcfg$t, L = cfgs$tcfg$L)
  gset <- cfgs$gcfg$gun.set
  cnum <- length(cfgs$icfg$expr)
  #' set the number of tests to be simulated, if not present in cfgs$tcfg (i.e. it has not be set in config.yml)
  cfgs$tcfg$sim.tests <- ifelse(is.null(cfgs$tcfg$sim.tests), cfgs$tcfg$tests, cfgs$tcfg$sim.tests)
  #' make sure number of simulation tests is smaller or equal to the number of generation tests, otherwise, its invalide data situation
  if(cfgs$tcfg$sim.tests > cfgs$tcfg$tests)
  {
    logging::logerror('Number of simulation tests: %s larger than number of generation tests: %s. Not enought input data to be simulated!', cfgs$tcfg$sim.tests, cfgs$tcfg$tests)
    return(-1)
  }
  #' make sure the expression name includes the simulation number of tests, not the generation number.
  cfgs$tcfg$sim.expr <- paste0('-scale_', cfgs$tcfg$load['lb'], '_', cfgs$tcfg$load['ub'], '_', cfgs$tcfg$load['st'], '-tests_', cfgs$tcfg$sim.tests)
  #' create subdirectories for the results of each catalogue
  ccdir <- file.path(cfgs$paths['store'], cfgs$icfg$expr, fsep = '')
  resdir <- file.path(cfgs$paths['result'], cfgs$icfg$expr, fsep = '')
  for(i in 1:cnum)
  {
    dir.create(ccdir[i], showWarnings = FALSE)
    dir.create(resdir[i], showWarnings = FALSE)
    #' create results directory
    for(g in gset)
    {
      logging::loginfo('SIM: ICN MULTICAST catalogue:%s graph:%s', cfgs$icfg$expr[i],  g$g.name)
      #' construct results filenames
      icn.fn <- file.path(resdir[i], 'icn.mc', '-g_', g$g.name, '-o_', expr$oexpr, '-e_', expr$eexpr, '-ca_', expr$ca, '-L_', expr$L, cfgs$tcfg$sim.expr, '.RData', fsep = '')
      cc.fn <- file.path(ccdir[i], 'cc', '-g_', g$g.name, '-o_', expr$oexpr, '-e_', expr$eexpr, cfgs$tcfg$expr, '.RData', fsep = "")
      sim.not.exit <- which(file.access(icn.fn, mode = 4) < 0)
      while(length(sim.not.exit) > 0) {
        logging::logdebug('SIM: ICN MULTICAST %s/%s instances', length(sim.not.exit), length(icn.fn))
        foreach::foreach(j = sim.not.exit) %dopar%
        {
          logging::logdebug('SIM: ICN MC instance %s', j)
          #' cc[[g]] is 3-Level list:
          #'  EO.combo
          #'  |-- scale.index
          #'      |-- test.number
          sim <- tryCatch(load(cc.fn[j]), error = function(e) { return("")})
          if(sim == 'cc') {
            logging::logdebug('SIM: ICN MC instance %s',j)
            SimIcnMulticastInstance(cc = cc, g = g, scale = cfgs$tcfg$load, tests = cfgs$tcfg$sim.tests, t = expr$ca[j], L = expr$L[j], path = icn.fn[j])
          }
        }
        sim.not.exit <- which(file.access(icn.fn, mode = 4) < 0)
      }
    }
  }
}

#' Simulate routing ICN demands in a native ICN or *-over-ICN network
#' @description simulate provisioned capacity for ICN multicast traffic, for a single catchement interval and single length of running
#'
#' @export
#' @param cc pub/sub state for a single content catalogue
#' @param g network graph list including graph object, per node population, name, distance matrix and edge matrix
#' @param scale ratio of population as active users
#' @param tests number of tests to run
#' @param t catchement interval in seconds
#' @param L total length in seconds
#' @param path full path as to where to store the data, including the filename
SimIcnMulticastInstance <- function(cc, g, scale, tests, t, L, path)
{
  si <- seq(scale['lb'], scale['ub'], scale['st']) #' scale index
  sl <- length(si)
  icn.mc <- list()
  for(s in 1:sl)
  {
    icn.mc[[s]] <- vapply(cc[[s]][1:tests], RouteIcnMulticastDijkstra, as.list(c(1:4)), g = g, t = t, L = L)
  }
  icn.mc$ocfg <- cc$ocfg
  icn.mc$ecfg <- cc$ecfg
  save(icn.mc, file = path)
  logging::logdebug('SIM: ICN MC complete, store:%s', path)
}

# IP ----------------------------------------------------------------------

#' simulate routing ICN demands in a native ICN or *-over-ICN network
#'
#' @export
#' @param cfgs configurations as parsed from config file in YAML format, config/config.yml
SimIp <- function(cfgs)
{
  #' define prallel cluster of workers
  doParallel::registerDoParallel(cfgs$cores)
  #' construct permutations names and load relevant input data
  gset <- cfgs$gcfg$gun.set
  cnum <- length(cfgs$icfg$expr)
  #' create subdirectories for the results of each catalogue
  ccdir <- file.path(cfgs$paths['store'], cfgs$icfg$expr, fsep = '')
  resdir <- file.path(cfgs$paths['result'], cfgs$icfg$expr, fsep = '')
  #' set the number of tests to be simulated, if not present in cfgs$tcfg (i.e. it has not be set in config.yml)
  cfgs$tcfg$sim.tests <- ifelse(is.null(cfgs$tcfg$sim.tests), cfgs$tcfg$tests, cfgs$tcfg$sim.tests)
  #' make sure number of simulation tests is smaller or equal to the number of generation tests, otherwise, its invalide data situation
  if(cfgs$tcfg$sim.tests > cfgs$tcfg$tests)
  {
    logging::logerror('Number of simulation tests: %s larger than number of generation tests: %s. Not enought input data to be simulated!', cfgs$tcfg$sim.tests, cfgs$tcfg$tests)
    return(-1)
  }
  #' make sure the expression name includes the simulation number of tests, not the generation number.
  cfgs$tcfg$sim.expr <- paste0('-scale_', cfgs$tcfg$load['lb'], '_', cfgs$tcfg$load['ub'], '_', cfgs$tcfg$load['st'], '-tests_', cfgs$tcfg$sim.tests)
  for(i in 1:cnum)
  {
    #' create results directory
    dir.create(ccdir[i], showWarnings = FALSE)
    dir.create(resdir[i], showWarnings = FALSE)
    for(g in gset)
    {
      #' construct results filenames
      ip.fn <- file.path(resdir[i], 'ip', '-g_', g$g.name, '-o_', cfgs$deoexpr$oexpr, '-e_', cfgs$deoexpr$eexpr, '-d_', cfgs$deoexpr$dexpr, cfgs$tcfg$sim.expr, '.RData', fsep = '')
      dns.fn <- file.path(ccdir[i], 'DNS', '-g_', g$g.name, '-', cfgs$deoexpr$dexpr, '-o_', cfgs$deoexpr$oexpr, '-e_', cfgs$deoexpr$eexpr, '-tests_', cfgs$tcfg$tests, '.RData', fsep = '')
      eo.fn  <- file.path(ccdir[i], 'EO', '-g_', g$g.name, '-o_', cfgs$deoexpr$oexpr, '-e_', cfgs$deoexpr$eexpr, '-tests_', cfgs$tcfg$tests, '.RData', fsep = '')
      cc.fn <- file.path(ccdir[i], 'cc', '-g_', g$g.name, '-o_', cfgs$deoexpr$oexpr, '-e_', cfgs$deoexpr$eexpr, cfgs$tcfg$expr, '.RData', fsep = '')
      #' cc RV state only reflects the combinations of origins and surrogates, hence the list length is length(origins combo) x length(surrogate combo).
      #' To evaluate IP allocation of the cc state for different DNS combo, each element need to be repeated length(DNS combo) times
      sim.not.exit <- which(file.access(ip.fn, mode = 4) < 0)
      logging::loginfo('SIM: IP catalogue:%s graph:%s', cfgs$icfg$expr[i],  g$g.name)
      logging::logdebug('SIM: IP %s instances out of %s total', length(sim.not.exit), length(ip.fn))
      foreach::foreach(j = sim.not.exit) %dopar%
      {
        logging::logdebug('SIM: IP instance %s', j)
        #' cc[[g]] is 3-Level list:
        #'  EO.combo
        #'  |-- scale.index
        #'      |-- test.number
        cc.dns.eo <- GetStoredObjects(filenames = c(cc.fn[j], dns.fn[j], eo.fn[j]))
        cemap <- lapply(cc.dns.eo[[2]], "[[", 'ce')   #' extract cemap information from the DNS list.
        SimIpInstance(cc = cc.dns.eo[[1]], cemap = cemap, eomap = cc.dns.eo[[3]], path = ip.fn[j],
                               g = g, scale = cfgs$tcfg$load, tests = cfgs$tcfg$sim.tests)
      }
    }
  }
}

#' simulate routing ICN demands in a native ICN or *-over-ICN network
#'
#' @export
#' @param cfgs configurations as parsed from config file in YAML format, config/config.yml
GSimIp <- function(cfgs)
{
  #' define prallel cluster of workers
  doParallel::registerDoParallel(cfgs$cores)
  #' construct permutations names and load relevant input data
  gset <- cfgs$gcfg$gun.set
  cnum <- length(cfgs$icfg$expr)
  #' create subdirectories for the results of each catalogue
  ccdir <- file.path(cfgs$paths['store'], cfgs$icfg$expr, fsep = '')
  resdir <- file.path(cfgs$paths['result'], cfgs$icfg$expr, fsep = '')
  #' set the number of tests to be simulated, if not present in cfgs$tcfg (i.e. it has not be set in config.yml)
  cfgs$tcfg$sim.tests <- ifelse(is.null(cfgs$tcfg$sim.tests), cfgs$tcfg$tests, cfgs$tcfg$sim.tests)
  #' make sure number of simulation tests is smaller or equal to the number of generation tests, otherwise, its invalide data situation
  if(cfgs$tcfg$sim.tests > cfgs$tcfg$tests)
  {
    logging::logerror('Number of simulation tests: %s larger than number of generation tests: %s. Not enought input data to be simulated!', cfgs$tcfg$sim.tests, cfgs$tcfg$tests)
    return(-1)
  }
  #' make sure the expression name includes the simulation number of tests, not the generation number.
  cfgs$tcfg$sim.expr <- paste0('-scale_', cfgs$tcfg$load['lb'], '_', cfgs$tcfg$load['ub'], '_', cfgs$tcfg$load['st'], '-tests_', cfgs$tcfg$sim.tests)
  for(i in 1:cnum)
  {
    #' create results directory
    dir.create(ccdir[i], showWarnings = FALSE)
    dir.create(resdir[i], showWarnings = FALSE)
    for(g in gset)
    {
      #' construct results filenames
      ip.fn <- file.path(resdir[i], 'ip', '-g_', g$g.name, '-o_', cfgs$deoexpr$oexpr, '-e_', cfgs$deoexpr$eexpr, '-d_', cfgs$deoexpr$dexpr, cfgs$tcfg$sim.expr, '.RData', fsep = '')
      dns.fn <- file.path(ccdir[i], 'DNS', '-g_', g$g.name, '-', cfgs$deoexpr$dexpr, '-o_', cfgs$deoexpr$oexpr, '-e_', cfgs$deoexpr$eexpr, '-tests_', cfgs$tcfg$tests, '.RData', fsep = '')
      eo.fn  <- file.path(ccdir[i], 'EO', '-g_', g$g.name, '-o_', cfgs$deoexpr$oexpr, '-e_', cfgs$deoexpr$eexpr, '-tests_', cfgs$tcfg$tests, '.RData', fsep = '')
      cc.fn <- file.path(ccdir[i], 'cc', '-g_', g$g.name, '-o_', cfgs$deoexpr$oexpr, '-e_', cfgs$deoexpr$eexpr, cfgs$tcfg$expr, '.RData', fsep = '')
      #' cc RV state only reflects the combinations of origins and surrogates, hence the list length is length(origins combo) x length(surrogate combo).
      #' To evaluate IP allocation of the cc state for different DNS combo, each element need to be repeated length(DNS combo) times
      sim.not.exit <- which(file.access(ip.fn, mode = 4) < 0)
      logging::loginfo('SIM: IP catalogue:%s graph:%s', cfgs$icfg$expr[i],  g$g.name)
      while(length(sim.not.exit) > 0) {
      logging::logdebug('SIM: IP %s/%s instances', length(sim.not.exit), length(ip.fn))
        foreach::foreach(j = sim.not.exit) %dopar%
        {
          logging::logdebug('SIM: IP instance %s', j)
          #' cc[[g]] is 3-Level list:
          #'  EO.combo
          #'  |-- scale.index
          #'      |-- test.number
          cc.dns.eo <- tryCatch(GetStoredObjects(filenames = c(cc.fn[j], dns.fn[j], eo.fn[j])), error = function(e) { return("")})
          if(cc.dns.eo != "") {
            cemap <- lapply(cc.dns.eo[[2]], "[[", 'ce')   #' extract cemap information from the DNS list.
            SimIpInstance(cc = cc.dns.eo[[1]], cemap = cemap, eomap = cc.dns.eo[[3]], path = ip.fn[j],
                          g = g, scale = cfgs$tcfg$load, tests = cfgs$tcfg$sim.tests)
          }
        }
        sim.not.exit <- which(file.access(ip.fn, mode = 4) < 0)
      }
    }
  }
}
#' simulation one instance for the permutation configuration and the load
#'
#' @param cc Pub/Sub states for content catalogues of specific configuration set
#' @param g network graph list, including: graph object, population/node, graph.name, Distance Matrix, Edge Matrix
#' @param cemap including \code{ce}: client-to-surrogate(edge server) mapping according to DNS redirect
#' @param eomap surrogate-to-origin mapping
#' @param scale scaling load on the network
#' @param tests number of permutations
#' @param path full path - including filename - to store the results
#' @param return list of provisioned capacity
SimIpInstance <- function(cc, g, cemap, eomap, scale, tests, path)
{
  #' define dataframes for results
  ip <- list() #' Route list
  si <- seq(scale['lb'], scale['ub'], scale['st']) #' scale index
  sl <- length(si)
  for(s in 1:sl)
  {
    logging::logdebug('SIM: IP start admitting demands for load: %s', si[s])
    ip[[s]] <- mapply(RouteIpUnicastDijkstra, iL = cc[[s]][1:tests], ce = cemap[1:tests], eo = eomap[1:tests],
                      MoreArgs = list(g = g))
  }
  ip$ocfg <- cc$ocfg
  ip$ecfg <- cc$ecfg
  ip$g <- g$g.name
  save(ip, file = path)
  logging::logdebug('SIM: IP complete, store: %s', path)
}
