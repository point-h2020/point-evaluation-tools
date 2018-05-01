#  capacity.R process raw modelling/simulation results to generate link capacity knowledge
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

#' Process ICN unicast capacity results
#' @description process stored capacity for ICN unicast, if not stored, return notification to generate or wait until generated.
#' @note This function currently assumes only one catalogue permutation.
#'
#' @export
#' @param cfgs configurations, following a YAML template in config/config.yml which have been used to simulate the network behaviour
ProcIcnUnicastCapacity <- function(cfgs)
{
  #' Todo: extend the function to plot for multiple catalogues
  logging::loginfo('Proc: ICN UC')
  doParallel::registerDoParallel(cfgs$cores)
  #' construct combinations of expressions, will be used to create file names of results objects
  gnames <- GetGraphName(cfgs$gcfg$gun.set)
  #' set the number of tests to be simulated, if not present in cfgs$tcfg (i.e. it has not be set in config.yml)
  cfgs$tcfg$sim.tests <- ifelse(is.null(cfgs$tcfg$sim.tests), cfgs$tcfg$tests, cfgs$tcfg$sim.tests)

    #' make sure the expression name includes the simulation number of tests, not the generation number.
  cfgs$tcfg$sim.expr <- paste0('-scale_', cfgs$tcfg$load['lb'], '_', cfgs$tcfg$load['ub'], '_', cfgs$tcfg$load['st'], '-tests_', cfgs$tcfg$sim.tests)
  icn.expr <- expand.grid(texpr = cfgs$tcfg$sim.expr, eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr, gnames = gnames, iexpr = cfgs$icfg$expr)
  icn.expr.len <- nrow(icn.expr)

  icn.fns <- file.path(cfgs$paths['result'], icn.expr$iexpr, 'icn.uc', '-g_', icn.expr$gnames, '-o_', icn.expr$oexpr, '-e_', icn.expr$eexpr, icn.expr$texpr, '.RData', fsep = '')
  ons <- names(cfgs$ocfg)
  ons <- paste0('o', ons[!(ons %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  ens <- names(cfgs$ecfg)
  ens <- paste0('e', ens[!(ens %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  logging::logwarn('Proc: Meta Data construction assumes fixed 3 parameteres defining origins and 3 params defining surrogates.')
  #' Todo: comeup with a more generic construction of meta data for any number of parameteres for origins and surrogates.
  icn.meta <- base::expand.grid(cfgs$ecfg[[1]], cfgs$ecfg[[2]], cfgs$ecfg[[3]], #' surrogate meta
                          cfgs$ocfg[[1]], cfgs$ocfg[[2]], cfgs$ocfg[[3]], #' origina meta
                          gnames = gnames, #' graph meta
                          N = cfgs$icfg$N, d = cfgs$icfg$d, d.param = cfgs$icfg$d.param #' Catalogue meta
  )
  colnames(icn.meta)[1:6] <- c(ens, ons)
  icn.fns.exist <- base::which(file.access(icn.fns, mode = 4) == 0)
  icn.cu <- foreach::foreach(j = icn.fns.exist, .combine = rbind, .multicombine = TRUE) %dopar%
  {
    logging::logdebug('GET ICN UC: %s, fn:%s', j, icn.fns[j])
    tmp <- GetIcnUnicastEmResults(filename = icn.fns[j])
    tmp <- cbind(tmp, icn.meta[j,])
  }
  icn.uc <- cbind(icn.cu[, c('uc', 't', 'ek', 'ok', 'eopt', 'oopt')],
                  type = 'unicast', ca = '', L = '')[, c('ek', 'uc', 't', 'eopt', 'oopt', 'ok', 'ca', 'L', 'type')]
  colnames(icn.uc) <- c('surrogates', 'provisioned', 'demand', 'surrogates_policy', 'origins_policy', 'origins', 'catchment_interval', 'stream_length', 'type')
  icn.uc <- SetReplacementValue(icn.uc, cols = c('surrogates_policy', 'origins_policy'), ov = c(1,2,3), nv = c('Pop', 'Cls', 'Swing'))
  icn.uc
}

#' Process ICN unicast capacity results
#' @description process stored capacity for ICN unicast, if not stored, return notification to generate or wait until generated.
#' @note This function currently assumes only one catalogue permutation.
#'
#' @export
#' @param cfgs configurations, following a YAML template in config/config.yml which have been used to simulate the network behaviour
ProcIcnUnicastCapacityOfOneCatalogue <- function(cfgs)
{
  logging::loginfo('Proc: ICN UC')
  doParallel::registerDoParallel(cfgs$cores)

  #' construct combinations of expressions, will be used to create file names of results objects
  gnames <- GetGraphName(cfgs$gcfg$gun.set)

  #' set the number of tests to be simulated, if not present in cfgs$tcfg (i.e. it has not be set in config.yml)
  cfgs$tcfg$sim.tests <- ifelse(is.null(cfgs$tcfg$sim.tests), cfgs$tcfg$tests, cfgs$tcfg$sim.tests)

  #' make sure the expression name includes the simulation number of tests, not the generation number.
  cfgs$tcfg$sim.expr <- paste0('-scale_', cfgs$tcfg$load['lb'], '_', cfgs$tcfg$load['ub'], '_', cfgs$tcfg$load['st'], '-tests_', cfgs$tcfg$sim.tests)
  icn.expr <- expand.grid(texpr = cfgs$tcfg$sim.expr, eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr, gnames = gnames, iexpr = cfgs$icfg$expr)
  icn.expr.len <- nrow(icn.expr)

  #' construct directory paths and full filenames
  resdir <- file.path(cfgs$paths['result'], cfgs$icfg$expr, fsep = '')
  presdir <- file.path(resdir, 'processed/', fsep = '')
  dir.create(resdir, showWarnings = FALSE)
  dir.create(presdir, showWarnings = FALSE)

  icn.fns <- file.path(resdir, 'icn.uc', '-g_', icn.expr$gnames, '-o_', icn.expr$oexpr, '-e_', icn.expr$eexpr, icn.expr$texpr, '.RData', fsep = '')
  icn.pfns <- file.path(presdir, 'icn.uc', '-g_', icn.expr$gnames, '-o_', icn.expr$oexpr, '-e_', icn.expr$eexpr, icn.expr$texpr, '.RData', fsep = '')

  ons <- names(cfgs$ocfg)
  ons <- paste0('o', ons[!(ons %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  ens <- names(cfgs$ecfg)
  ens <- paste0('e', ens[!(ens %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  logging::logwarn('Proc: Meta Data construction assumes fixed 3 parameteres defining origins and 3 params defining surrogates.')
  #' Todo: comeup with a more generic construction of meta data for any number of parameteres for origins and surrogates.
  icn.meta <- base::expand.grid(cfgs$ecfg[[1]], cfgs$ecfg[[2]], cfgs$ecfg[[3]], #' surrogate meta
                                cfgs$ocfg[[1]], cfgs$ocfg[[2]], cfgs$ocfg[[3]], #' origina meta
                                gnames = gnames, #' graph meta
                                N = cfgs$icfg$N, d = cfgs$icfg$d, d.param = cfgs$icfg$d.param, #' Catalogue meta
                                stringsAsFactors = FALSE
  )
  colnames(icn.meta)[1:6] <- c(ens, ons)

  #' define output container template, will be subset later for each combo
  template <- cbind(c = 0, t= 0 , r = 0, icn.meta[rep(seq_len(nrow(icn.meta)), each=cfgs$tcfg$sim.tests),],
                    dopt = '', dk = 0, type = 'unicast', ca = 0, L = 0, stringsAsFactors = FALSE )[, c('ek', 'c', 't', 'r', 'eopt', 'oopt', 'ok', 'dopt', 'dk', 'ca', 'L', 'type')]
  colnames(template) <- c('surrogates', 'provisioned', 'demand', 'ratio', 'surrogates_policy',
                          'origins_policy', 'origins', 'dnss_policy', 'dnss', 'catchment_interval', 'stream_length', 'type')
  template <- SetReplacementValue(template, cols = c('surrogates_policy', 'origins_policy'),
                                  ov = c(1,2,3), nv = c('Pop', 'Cls', 'Swing'))
  sim.exist <- 0
  while(length(sim.exist) < icn.expr.len)
  {
    sim.exist <- which(file.access(icn.fns, mode = 4) == 0)
    # foreach::foreach(j = sim.exist) %dopar%
    for(j in sim.exist)
    {
      logging::logdebug('PROC: ICN UC: %s, fn: %s', j, icn.pfns[j])
      pos <- (j - 1) * cfgs$tcfg$sim.tests + 1
      icn.uc <- template[pos:(pos + cfgs$tcfg$sim.tests - 1),]
      icn.uc[, c('provisioned', 'demand', 'ratio')] <- GetIcnUnicastEmResults(filename = icn.fns[j])
      save(icn.uc, file = icn.pfns[j])
    }
  }
}

#' Process ICN multicast capacity results
#' @description process stored backhaul capacity for ICN multicast, if not stored, return notification to generate or wait until generated.
#' @note This function currently assumes only one catalogue permutation.
#'
#' @export
#' @param cfgs configurations, following a YAML template in config/config.yml which have been used to simulate the network behaviour
ProcIcnMulticastCapacity <- function(cfgs)
{
  #' Todo: extend the function to plot for multiple catalogues
  logging::loginfo('Proc: ICN MC')
  doParallel::registerDoParallel(cfgs$cores)
  #' construct combinations of expressions, will be used to create file names of results objects
  gnames <- GetGraphName(cfgs$gcfg$gun.set)
  #' make sure the expression name includes the simulation number of tests, not the generation number.
  cfgs$tcfg$sim.expr <- paste0('-scale_', cfgs$tcfg$load['lb'], '_', cfgs$tcfg$load['ub'], '_', cfgs$tcfg$load['st'], '-tests_', cfgs$tcfg$sim.tests)
  icn.expr <- expand.grid(texpr = cfgs$tcfg$sim.expr, ca = cfgs$tcfg$t, L = cfgs$tcfg$L, eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr, gnames = gnames, iexpr = cfgs$icfg$expr)
  icn.expr.len <- nrow(icn.expr)
  icn.fns <- file.path(cfgs$paths['result'], icn.expr$iexpr, 'icn.mc', '-g_', icn.expr$gnames, '-o_', icn.expr$oexpr, '-e_',
                       icn.expr$eexpr, '-ca_', icn.expr$ca, '-L_', icn.expr$L, icn.expr$texpr, '.RData', fsep = '')
  ons <- names(cfgs$ocfg)
  ons <- paste0('o', ons[!(ons %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  ens <- names(cfgs$ecfg)
  ens <- paste0('e', ens[!(ens %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  logging::logwarn('Proc: Meta Data construction assumes fixed 3 parameteres defining origins and 3 params defining surrogates.')
  #' Todo: comeup with a more generic construction of meta data for any number of parameteres for origins and surrogates.
  icn.meta <- expand.grid(ca = cfgs$tcfg$t, L = cfgs$tcfg$L,
                          cfgs$ecfg[[1]], cfgs$ecfg[[2]], cfgs$ecfg[[3]], #' surrogate meta
                          cfgs$ocfg[[1]], cfgs$ocfg[[2]], cfgs$ocfg[[3]], #' origina meta
                          gnames = gnames, #' graph meta
                          N = cfgs$icfg$N, d = cfgs$icfg$d, d.param = cfgs$icfg$d.param #' Catalogue meta
  )
  colnames(icn.meta)[3:8] <- c(ens, ons)
  icn.fns.exist <- which(file.access(icn.fns, mode = 4) == 0)
  icn.cu <- foreach::foreach(j = icn.fns.exist, .combine = rbind, .multicombine = TRUE) %dopar%
  {
    logging::logdebug('GET ICN MC: %s, fn:%s', j, icn.fns[j])
    tmp <- GetIcnMulticastEmResults(filename = icn.fns[j])
    tmp <- cbind(tmp, icn.meta[j,])
  }
  icn.mc <- cbind(icn.cu[, c('ek', 'mc', 't', 'ok', 'eopt', 'oopt', 'ca', 'L')],
                  type = 'multicast')[, c('ek', 'mc', 't', 'eopt', 'oopt', 'ok', 'ca', 'L', 'type')]
  colnames(icn.mc) <- c('x', 'y', 't', '.var1', '.var2', '.var3', '.var4', '.var5', '.var6')
  icn.mc$.var4 <- as.character(icn.mc$.var4)
  icn.mc$.var5 <- as.character(icn.mc$.var5)
  icn.mc <- SetReplacementValue(icn.mc, cols = c('.var1', '.var2'), ov = c(1,2,3), nv = c('Pop', 'Cls', 'Swing'))
  icn.mc
}
#' Process ICN multicast capacity results
#' @description process stored backhaul capacity for ICN multicast, if not stored, return notification to generate or wait until generated.
#' @note This function currently assumes only one catalogue permutation.
#'
#' @export
#' @param cfgs configurations, following a YAML template in config/config.yml which have been used to simulate the network behaviour
ProcIcnMulticastCapacityOfOneCatalogue <- function(cfgs)
{
  #' Todo: extend the function to plot for multiple catalogues
  logging::loginfo('Proc: ICN MC')
  doParallel::registerDoParallel(cfgs$cores)

  #' construct combinations of expressions, will be used to create file names of results objects
  gnames <- GetGraphName(cfgs$gcfg$gun.set)

  #' set the number of tests to be simulated, if not present in cfgs$tcfg (i.e. it has not be set in config.yml)
  cfgs$tcfg$sim.tests <- ifelse(is.null(cfgs$tcfg$sim.tests), cfgs$tcfg$tests, cfgs$tcfg$sim.tests)

  #' make sure the expression name includes the simulation number of tests, not the generation number.
  cfgs$tcfg$sim.expr <- paste0('-scale_', cfgs$tcfg$load['lb'], '_', cfgs$tcfg$load['ub'], '_', cfgs$tcfg$load['st'], '-tests_', cfgs$tcfg$sim.tests)
  icn.expr <- expand.grid(texpr = cfgs$tcfg$sim.expr, ca = cfgs$tcfg$t, L = cfgs$tcfg$L, eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr, gnames = gnames, iexpr = cfgs$icfg$expr)
  icn.expr.len <- nrow(icn.expr)

  #' construct directory paths and full filenames
  resdir <- file.path(cfgs$paths['result'], cfgs$icfg$expr, fsep = '')
  presdir <- file.path(resdir, 'processed/', fsep = '')
  dir.create(resdir, showWarnings = FALSE)
  dir.create(presdir, showWarnings = FALSE)

  icn.fns <- file.path(resdir, 'icn.mc', '-g_', icn.expr$gnames, '-o_', icn.expr$oexpr, '-e_', icn.expr$eexpr,
                       '-ca_', icn.expr$ca, '-L_', icn.expr$L, icn.expr$texpr, '.RData', fsep = '')
  icn.pfns <- file.path(presdir, 'icn.mc', '-g_', icn.expr$gnames, '-o_', icn.expr$oexpr, '-e_', icn.expr$eexpr,
                        '-ca_', icn.expr$ca, '-L_', icn.expr$L, icn.expr$texpr, '.RData', fsep = '')

  ons <- names(cfgs$ocfg)
  ons <- paste0('o', ons[!(ons %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  ens <- names(cfgs$ecfg)
  ens <- paste0('e', ens[!(ens %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  logging::logwarn('Proc: Meta Data construction assumes fixed 3 parameteres defining origins and 3 params defining surrogates.')
  #' Todo: comeup with a more generic construction of meta data for any number of parameteres for origins and surrogates.
  icn.meta <- expand.grid(ca = cfgs$tcfg$t, L = cfgs$tcfg$L,
                          cfgs$ecfg[[1]], cfgs$ecfg[[2]], cfgs$ecfg[[3]], #' surrogate meta
                          cfgs$ocfg[[1]], cfgs$ocfg[[2]], cfgs$ocfg[[3]], #' origina meta
                          gnames = gnames, #' graph meta
                          N = cfgs$icfg$N, d = cfgs$icfg$d, d.param = cfgs$icfg$d.param, #' Catalogue meta
                          stringsAsFactors = FALSE
  )
  colnames(icn.meta)[3:8] <- c(ens, ons)

  #' define output container template, will be subset later for each combo
  template <- cbind(c = 0, t= 0 , r = 0, icn.meta[rep(seq_len(nrow(icn.meta)), each=cfgs$tcfg$sim.tests),],
                    dopt = '', dk = 0, type = 'multicast', stringsAsFactors = FALSE)[, c('ek', 'c', 't', 'r', 'eopt', 'oopt', 'ok', 'dopt', 'dk', 'ca', 'L', 'type')]
  colnames(template) <- c('surrogates', 'provisioned', 'demand', 'ratio', 'surrogates_policy',
                          'origins_policy', 'origins', 'dnss_policy', 'dnss', 'catchment_interval', 'stream_length', 'type')
  template <- SetReplacementValue(template, cols = c('surrogates_policy', 'origins_policy'),
                                  ov = c(1,2,3), nv = c('Pop', 'Cls', 'Swing'))

  #' findout which processed results files are missing
  icn.pfns.not.exist <- which(file.access(icn.pfns, mode = 4) < 0)

  #' process results for which there are raw results files, and save using the processed filenames icn.pfns
  while (length(icn.pfns.not.exist) > 0) {
    #' select raw results for which there is no processed results
    icn.fns.exist <- which(file.access(icn.fns, mode = 4) == 0)
    for (j in icn.fns.exist)
    {
      logging::logdebug('PROC: ICN MC: %s, fn: %s', j, icn.pfns[j])
      pos <- (j - 1) * cfgs$tcfg$sim.tests + 1
      icn.mc <- template[pos:(pos + cfgs$tcfg$sim.tests - 1),]
      icn.mc[, c('provisioned', 'demand', 'ratio')] <- GetIcnMulticastEmResults(filename = icn.fns[j])
      save(icn.mc, file = icn.pfns[j])
    }
    icn.pfns.not.exist <- which(file.access(icn.pfns, mode = 4) < 0)
  }
}
#' Process ICN both unicast and multicast capacity results
#' @description process stored backhaul capacity for ICN unicast and multicast, if not stored, return notification to generate or wait until generated.
#' @note This function currently assumes only one catalogue permutation.
#'
#' @export
#' @param cfgs configurations, following a YAML template in config/config.yml which have been used to simulate the network behaviour
ProcIcnCapacity <- function(cfgs)
{
  #' Todo: extend the function to plot for multiple catalogues
  icn.uc <- ProcIcnUnicastCapacity(cfgs)
  icn.mc <- ProcIcnMulticastCapacity(cfgs)
  icn.c <- rbind(icn.uc, icn.mc)
  #' substitue numeric identifiers of selection policies with human-readable names
  icn.c <- SetReplacementValue(icn.c, cols = c('.var1', '.var2'), ov = c(1,2,3), nv = c('Pop', 'Cls', 'Swing'))
  icn.c
}

#' Process IP backhaul capacity results
#' @description process stored backhaul capacity for IP, if not stored, return notification to generate or wait until generated.
#' @note This function currently assumes only one catalogue permutation.
#'
#' @export
#' @param cfgs configurations, following a YAML template in config/config.yml which have been used to simulate the network behaviour
ProcIpCapacity <- function(cfgs)
{
  #' Todo: extend the function to plot for multiple catalogues
  ip.cu <- data.frame() #'output dataframe
  doParallel::registerDoParallel(cfgs$cores)
  #' construct combinations of expressions, will be used to create file names of results objects
  gnames <- GetGraphName(cfgs$gcfg$gun.set)
  #' make sure the expression name includes the simulation number of tests, not the generation number.
  cfgs$tcfg$sim.expr <- paste0('-scale_', cfgs$tcfg$load['lb'], '_', cfgs$tcfg$load['ub'], '_', cfgs$tcfg$load['st'], '-tests_', cfgs$tcfg$sim.tests)
  ip.expr <- expand.grid(texpr = cfgs$tcfg$sim.expr, dexpr = cfgs$dcfg$expr, eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr, gnames = gnames, iexpr = cfgs$icfg$expr)
  ip.expr.len <- nrow(ip.expr)
  #' create results filenames (to be loaded)
  ip.fns <- file.path(cfgs$paths['result'], ip.expr$iexpr, 'ip', '-g_', ip.expr$gnames, '-o_', ip.expr$oexpr, '-e_', ip.expr$eexpr, '-d_', ip.expr$dexpr, ip.expr$texpr, '.RData', fsep = '')
  #' construct combinations of metadata, length of this dataframe MUST equal length of the corresponding 'x.expr' dataframe
  ons <- names(cfgs$ocfg)
  ons <- paste0('o', ons[!(ons %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  ens <- names(cfgs$ecfg)
  ens <- paste0('e', ens[!(ens %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  logging::logwarn('Proc: Meta Data construction assumes fixed 3 parameteres defining origins and 3 params defining surrogates.')
  #' Todo: comeup with a more generic construction of meta data for any number of parameteres for origins and surrogates.
  dns <- names(cfgs$dcfg)
  dns <- paste0('d', dns[!(dns %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  ip.meta <- expand.grid(cfgs$dcfg[[1]], cfgs$dcfg[[2]],
                         cfgs$ecfg[[1]], cfgs$ecfg[[2]], cfgs$ecfg[[3]], #' surrogate meta
                         cfgs$ocfg[[1]], cfgs$ocfg[[2]], cfgs$ocfg[[3]], #' origina meta
                         gnames = gnames, #' graph meta
                         N = cfgs$icfg$N, d = cfgs$icfg$d, d.param = cfgs$icfg$d.param #' Catalogue meta
  )
  colnames(ip.meta)[1:8] <- c(dns, ens, ons)
  ip.fns.exist <- which(file.access(ip.fns, mode = 4) == 0)
  ip.cu <- foreach::foreach(j = ip.fns.exist, .combine = rbind, .multicombine = TRUE) %dopar%
    {
      logging::logdebug('GET IP: %s, fn:%s', j, ip.fns[j])
      tmp <- GetIpEmResults(filename = ip.fns[j])
      tmp <- cbind(tmp, ip.meta[j,])
    }
  ip.c <- ip.cu[, c('ek', 'c', 't', 'dopt', 'eopt', 'oopt', 'ok', 'dk')]
  colnames(ip.c) <- c('x', 'y', 't', '.var1', '.var2', '.var3', '.var4', '.var5')  #' specific colnames are required by the plotting function
  #' substitue numeric identifiers of selection policies with human-readable names
  ip.c <- SetReplacementValue(ip.c, cols = c('.var1', '.var2', '.var3'), ov = c(1,2,3), nv = c('Pop', 'Cls', 'Swing'))
  ip.c
}

#' Process IP backhaul capacity results
#' @description process stored backhaul capacity for IP, if not stored, return notification to generate or wait until generated.
#' @note This function currently assumes only one catalogue permutation.
#'
#' @export
#' @param cfgs configurations, following a YAML template in config/config.yml which have been used to simulate the network behaviour
ProcIpCapacityOfOneCatalogue <- function(cfgs)
{
  #' Todo: extend the function to plot for multiple catalogues
  ip.cu <- data.frame() #'output dataframe
  doParallel::registerDoParallel(cfgs$cores)

  #' construct combinations of expressions, will be used to create file names of results objects
  gnames <- GetGraphName(cfgs$gcfg$gun.set)

  #' set the number of tests to be simulated, if not present in cfgs$tcfg (i.e. it has not be set in config.yml)
  cfgs$tcfg$sim.tests <- ifelse(is.null(cfgs$tcfg$sim.tests), cfgs$tcfg$tests, cfgs$tcfg$sim.tests)

  #' make sure the expression name includes the simulation number of tests, not the generation number.
  cfgs$tcfg$sim.expr <- paste0('-scale_', cfgs$tcfg$load['lb'], '_', cfgs$tcfg$load['ub'], '_', cfgs$tcfg$load['st'], '-tests_', cfgs$tcfg$sim.tests)
  ip.expr <- expand.grid(texpr = cfgs$tcfg$sim.expr, dexpr = cfgs$dcfg$expr, eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr, gnames = gnames, iexpr = cfgs$icfg$expr)
  ip.expr.len <- nrow(ip.expr)

  #' construct directory paths and full filenames
  resdir <- file.path(cfgs$paths['result'], cfgs$icfg$expr, fsep = '')
  presdir <- file.path(resdir, 'processed/', fsep = '')
  dir.create(resdir, showWarnings = FALSE)
  dir.create(presdir, showWarnings = FALSE)

  #' create results filenames (to be loaded)
  ip.fns <- file.path(resdir, 'ip', '-g_', ip.expr$gnames, '-o_', ip.expr$oexpr,
                      '-e_', ip.expr$eexpr, '-d_', ip.expr$dexpr, ip.expr$texpr, '.RData', fsep = '')
  ip.pfns <- file.path(presdir, 'ip', '-g_', ip.expr$gnames, '-o_', ip.expr$oexpr,
                       '-e_', ip.expr$eexpr, '-d_', ip.expr$dexpr, ip.expr$texpr, '.RData', fsep = '')

  #' construct combinations of metadata, length of this dataframe MUST equal length of the corresponding 'x.expr' dataframe
  ons <- names(cfgs$ocfg)
  ons <- paste0('o', ons[!(ons %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  ens <- names(cfgs$ecfg)
  ens <- paste0('e', ens[!(ens %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  logging::logwarn('Proc: Meta Data construction assumes fixed 3 parameteres defining origins and 3 params defining surrogates.')
  #' Todo: comeup with a more generic construction of meta data for any number of parameteres for origins and surrogates.
  dns <- names(cfgs$dcfg)
  dns <- paste0('d', dns[!(dns %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  ip.meta <- expand.grid(cfgs$dcfg[[1]], cfgs$dcfg[[2]],
                         cfgs$ecfg[[1]], cfgs$ecfg[[2]], cfgs$ecfg[[3]], #' surrogate meta
                         cfgs$ocfg[[1]], cfgs$ocfg[[2]], cfgs$ocfg[[3]], #' origina meta
                         gnames = gnames, #' graph meta
                         N = cfgs$icfg$N, d = cfgs$icfg$d, d.param = cfgs$icfg$d.param, #' Catalogue meta
                         stringsAsFactors = FALSE
  )
  colnames(ip.meta)[1:8] <- c(dns, ens, ons)

  #' define output container template, will be subset later for each combo
  template <- cbind(c = 0, t= 0 , r = 0, ip.meta[rep(seq_len(nrow(ip.meta)), each=cfgs$tcfg$sim.tests),],
                    type = 'ip', ca = 0, L = 0, stringsAsFactors = FALSE)[, c('ek', 'c', 't', 'r', 'eopt', 'oopt', 'ok', 'dopt', 'dk', 'ca', 'L', 'type')]
  colnames(template) <- c('surrogates', 'provisioned', 'demand', 'ratio', 'surrogates_policy',
                          'origins_policy', 'origins', 'dnss_policy', 'dnss', 'catchment_interval', 'stream_length', 'type')
  template <- SetReplacementValue(template, cols = c('surrogates_policy', 'origins_policy', 'dnss_policy'),
                                  ov = c(1,2,3), nv = c('Pop', 'Cls', 'Swing'))

  #' check how many files are non existent yet and process accordingly.
  ip.pfns.not.exist <- which(file.access(ip.pfns, mode = 4) < 0)

  while(length(ip.pfns.not.exist) > 0) {
    ip.fns.exist <- which(file.access(ip.fns, mode = 4) == 0)
    for(j in ip.fns.exist)
    {
      logging::logdebug('PROC: IP UC: %s, fn: %s', j, ip.pfns[j])
      pos <- (j - 1) * cfgs$tcfg$sim.tests + 1
      ip <- template[pos:(pos + cfgs$tcfg$sim.tests - 1),]
      ip[, c('provisioned', 'demand', 'ratio')] <- GetIpEmResults(filename = ip.fns[j])
      save(ip, file = ip.pfns[j])
    }
    ip.pfns.not.exist <- which(file.access(ip.pfns, mode = 4) < 0)
  }
}

#' Process ICN path length results
#' @description process path length and generate ECDF of path lengths of all tests for which there are stored data. If not stored, return notification to generate or wait until generated.
#'
#' @export
#' @param cfgs configurations, following a YAML template in config/config.yml which have been used to simulate the network behaviour
ProcIcnUnicastPathLength <- function(cfgs, chosen.cols = c("l", "e", 'eopt', 'oopt', 'ek', 'ok'))
{
  cores <- detectCores() - 2 #' number of cores to use is always -2, leaving cores for other processes in the machine
  doParallel::registerDoParallel(cores)
  #' construct combinations of expressions, will be used to create file names of results objects
  gnames <- GetGraphName(cfgs$gcfg$gun.set)
  #' make sure the expression name includes the simulation number of tests, not the generation number.
  cfgs$tcfg$sim.expr <- paste0('-scale_', cfgs$tcfg$load['lb'], '_', cfgs$tcfg$load['ub'], '_', cfgs$tcfg$load['st'], '-tests_', cfgs$tcfg$sim.tests)
  icn.expr <- expand.grid(texpr = cfgs$tcfg$sim.expr, eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr, gnames = gnames, iexpr = cfgs$icfg$expr)
  icn.expr.len <- nrow(icn.expr)
  icn.fns <- file.path(cfgs$paths['result'], icn.expr$iexpr, 'icn.uc', '-g_', icn.expr$gnames, '-o_', icn.expr$oexpr, '-e_', icn.expr$eexpr, icn.expr$texpr, '.RData', fsep = '')
  ons <- names(cfgs$ocfg)
  ons <- paste0('o', ons[!(ons %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  ens <- names(cfgs$ecfg)
  ens <- paste0('e', ens[!(ens %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  logging::logwarn('Proc: Meta Data construction assumes fixed 3 parameteres defining origins and 3 params defining surrogates.')
  #' Todo: comeup with a more generic construction of meta data for any number of parameteres for origins and surrogates.
  icn.meta <- expand.grid(cfgs$ecfg[[1]], cfgs$ecfg[[2]], cfgs$ecfg[[3]], #' surrogate meta
                          cfgs$ocfg[[1]], cfgs$ocfg[[2]], cfgs$ocfg[[3]], #' origina meta
                          gnames = gnames, #' graph meta
                          N = cfgs$icfg$N, d = cfgs$icfg$d, d.param = cfgs$icfg$d.param #' Catalogue meta
  )
  colnames(icn.meta)[1:6] <- c(ens, ons)
  icn.fns.exist <- which(file.access(icn.fns, mode = 4) == 0)
  icn.uc <- foreach::foreach(j = icn.fns.exist, .combine = rbind, .multicombine = TRUE) %dopar%
  {
    logging::logdebug('GET ICN UC path length: %s, fn:%s', j, icn.fns[j])
    l <- GetIcnUnicastPathLengthEcdfResults(filename = icn.fns[j])
    l <- cbind(l, icn.meta[j, ])
  }
  icn.uc <- icn.uc[, chosen.cols]
  colnames(icn.uc) <- c('x', 'y', '.var1', '.var2', '.var3', '.var4')
  icn.uc <- SetReplacementValue(icn.uc, cols = c('.var1', '.var2'), ov = c(1,2,3), nv = c('Pop', 'Cls', 'Swing'))
  icn.uc
}
#' Process ICN path length results
#' @description process path length and generate ECDF of path lengths of all tests for which there are stored data. If not stored, return notification to generate or wait until generated.
#'
#' @export
#' @param cfgs configurations, following a YAML template in config/config.yml which have been used to simulate the network behaviour
ProcIcnUnicastPathLengthOfOneCatalogue <- function(cfgs, chosen.cols = c("l", "e", 'eopt', 'oopt', 'ek', 'ok'))
{
  # cores <- detectCores() - 2 #' number of cores to use is always -2, leaving cores for other processes in the machine
  doParallel::registerDoParallel(cfgs$cores)
  #' construct combinations of expressions, will be used to create file names of results objects
  gnames <- GetGraphName(cfgs$gcfg$gun.set)

  #' set the number of tests to be simulated, if not present in cfgs$tcfg (i.e. it has not be set in config.yml)
  cfgs$tcfg$sim.tests <- ifelse(is.null(cfgs$tcfg$sim.tests), cfgs$tcfg$tests, cfgs$tcfg$sim.tests)

  #' make sure the expression name includes the simulation number of tests, not the generation number.
  cfgs$tcfg$sim.expr <- paste0('-scale_', cfgs$tcfg$load['lb'], '_', cfgs$tcfg$load['ub'], '_', cfgs$tcfg$load['st'], '-tests_', cfgs$tcfg$sim.tests)
  icn.expr <- expand.grid(texpr = cfgs$tcfg$sim.expr, eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr, gnames = gnames, iexpr = cfgs$icfg$expr)
  icn.expr.len <- nrow(icn.expr)

  #' construct directory paths and full filenames
  resdir <- file.path(cfgs$paths['result'], cfgs$icfg$expr, fsep = '')
  presdir <- file.path(resdir, 'processed/', fsep = '')
  dir.create(resdir, showWarnings = FALSE)
  dir.create(presdir, showWarnings = FALSE)

  #' file names
  # icn.fns <- file.path(cfgs$paths['result'], icn.expr$iexpr, 'icn.uc', '-g_', icn.expr$gnames, '-o_', icn.expr$oexpr, '-e_', icn.expr$eexpr, icn.expr$texpr, '.RData', fsep = '')

  icn.fns <- file.path(resdir, 'icn.uc', '-g_', icn.expr$gnames, '-o_', icn.expr$oexpr, '-e_', icn.expr$eexpr, icn.expr$texpr, '.RData', fsep = '')
  icn.pfns <- file.path(presdir, 'icn.upl', '-g_', icn.expr$gnames, '-o_', icn.expr$oexpr, '-e_', icn.expr$eexpr, icn.expr$texpr, '.RData', fsep = '')

  ons <- names(cfgs$ocfg)
  ons <- paste0('o', ons[!(ons %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  ens <- names(cfgs$ecfg)
  ens <- paste0('e', ens[!(ens %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  logging::logwarn('Proc: Meta Data construction assumes fixed 3 parameteres defining origins and 3 params defining surrogates.')
  #' Todo: comeup with a more generic construction of meta data for any number of parameteres for origins and surrogates.
  icn.meta <- base::expand.grid(cfgs$ecfg[[1]], cfgs$ecfg[[2]], cfgs$ecfg[[3]], #' surrogate meta
                                cfgs$ocfg[[1]], cfgs$ocfg[[2]], cfgs$ocfg[[3]], #' origina meta
                                gnames = gnames, #' graph meta
                                N = cfgs$icfg$N, d = cfgs$icfg$d, d.param = cfgs$icfg$d.param, #' Catalogue meta
                                stringsAsFactors = FALSE
  )
  colnames(icn.meta)[1:6] <- c(ens, ons)

  #' define output container template, will be subset later for each combo
  template <- cbind(icn.meta, dopt = '', dk = 0, type = 'unicast', ca = 0, L = 0, stringsAsFactors = FALSE )[, c('ek', 'eopt', 'oopt', 'ok', 'dopt', 'dk', 'ca', 'L', 'type')]
  colnames(template) <- c('surrogates', 'surrogates_policy',
                          'origins_policy', 'origins', 'dnss_policy', 'dnss', 'catchment_interval', 'stream_length', 'type')
  template <- SetReplacementValue(template, cols = c('surrogates_policy', 'origins_policy'),
                                  ov = c(1,2,3), nv = c('Pop', 'Cls', 'Swing'))

  #' findout which processed results files are missing
  icn.pfns.not.exist <- which(file.access(icn.pfns, mode = 4) < 0)

  #' process results for which there are raw results files, and save using the processed filenames icn.pfns
  while (length(icn.pfns.not.exist) > 0) {
    #' select raw results for which there is no processed results
    icn.fns.exist <- which(file.access(icn.fns, mode = 4) == 0)
    for (j in icn.fns.exist)
    {
      logging::logdebug('PROC: ICN UC PL: %s, fn: %s', j, icn.pfns[j])
      icn.upl <- cbind(GetIcnUnicastPathLengthEcdfResults(icn.fns[j]), template[j,])
      save(icn.upl, file = icn.pfns[j])
    }
    icn.pfns.not.exist <- which(file.access(icn.pfns, mode = 4) < 0)
  }
}

#' Process IP path length results
#' @description process path length and generate ECDF of path lengths of all tests for which there are stored data. If not stored, return notification to generate or wait until generated.
#'
#' @export
#' @param cfgs configurations, following a YAML template in config/config.yml which have been used to simulate the network behaviour
ProcIpPathLength <- function(cfgs, chosen.cols = c("l", "e", 'dopt', 'eopt', 'oopt', 'ek', 'ok', 'dk'))
{
  ip <- list()
  doParallel::registerDoParallel(cfgs$cores)
  #' construct combinations of expressions, will be used to create file names of results objects
  gnames <- GetGraphName(cfgs$gcfg$gun.set)
  #' make sure the expression name includes the simulation number of tests, not the generation number.
  cfgs$tcfg$sim.expr <- paste0('-scale_', cfgs$tcfg$load['lb'], '_', cfgs$tcfg$load['ub'], '_', cfgs$tcfg$load['st'], '-tests_', cfgs$tcfg$sim.tests)
  ip.expr <- expand.grid(texpr = cfgs$tcfg$sim.expr, dexpr = cfgs$dcfg$expr, eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr, gnames = gnames, iexpr = cfgs$icfg$expr)
  ip.expr.len <- nrow(ip.expr)
  #' create results filenames (to be loaded)
  ip.fns <- file.path(cfgs$paths['result'], ip.expr$iexpr, 'ip', '-g_', ip.expr$gnames, '-o_', ip.expr$oexpr, '-e_', ip.expr$eexpr, '-d_', ip.expr$dexpr, ip.expr$texpr, '.RData', fsep = '')
  ons <- names(cfgs$ocfg)
  ons <- paste0('o', ons[!(ons %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  ens <- names(cfgs$ecfg)
  ens <- paste0('e', ens[!(ens %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  logging::logwarn('Proc: Meta Data construction assumes fixed 3 parameteres defining origins and 3 params defining surrogates.')
  #' Todo: comeup with a more generic construction of meta data for any number of parameteres for origins and surrogates.
  dns <- names(cfgs$dcfg)
  dns <- paste0('d', dns[!(dns %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  ip.meta <- expand.grid(cfgs$dcfg[[1]], dopt = cfgs$dcfg[[2]],
                         ek = cfgs$ecfg[[1]], eopt = cfgs$ecfg[[2]], ea = cfgs$ecfg[[3]], #' surrogate meta
                         ok = cfgs$ocfg[[1]], oopt = cfgs$ocfg[[2]], oa = cfgs$ocfg[[3]], #' origina meta
                         gnames = gnames, #' graph meta
                         N = cfgs$icfg$N, d = cfgs$icfg$d, d.param = cfgs$icfg$d.param #' Catalogue meta
  )
  colnames(ip.meta)[1:8] <- c(dns, ens, ons)
  ip.fns.exist <- which(file.access(ip.fns, mode = 4) == 0)
  ip <- foreach::foreach(j = ip.fns.exist, .combine = rbind, .multicombine = TRUE) %dopar%
  {
    logging::logdebug('GET IP path length: %s, fn:%s', j, ip.fns[j])
    l <- GetIpPathLengthEcdfResults(filename = ip.fns[j])
    l <- cbind(l, ip.meta[j,])
  }
  ip <- ip[, chosen.cols]
  colnames(ip) <- c('x', 'y', '.var1', '.var2', '.var3', '.var4', '.var5', '.var6')
  ip <- SetReplacementValue(ip, cols = c('.var1', '.var2', '.var3'), ov = c(1,2,3), nv = c('Pop', 'Cls', 'Swing'))
  ip
}

#' Process IP path length results
#' @description process path length and generate ECDF of path lengths of all tests for which there are stored data. If not stored, return notification to generate or wait until generated.
#'
#' @export
#' @param cfgs configurations, following a YAML template in config/config.yml which have been used to simulate the network behaviour
ProcIpPathLengthOfOneCatalogue <- function(cfgs, chosen.cols = c("l", "e", 'dopt', 'eopt', 'oopt', 'ek', 'ok', 'dk'))
{
  ip <- list()
  doParallel::registerDoParallel(cfgs$cores)

  #' construct combinations of expressions, will be used to create file names of results objects
  gnames <- GetGraphName(cfgs$gcfg$gun.set)

  #' set the number of tests to be simulated, if not present in cfgs$tcfg (i.e. it has not be set in config.yml)
  cfgs$tcfg$sim.tests <- ifelse(is.null(cfgs$tcfg$sim.tests), cfgs$tcfg$tests, cfgs$tcfg$sim.tests)

  #' make sure the expression name includes the simulation number of tests, not the generation number.
  cfgs$tcfg$sim.expr <- paste0('-scale_', cfgs$tcfg$load['lb'], '_', cfgs$tcfg$load['ub'], '_', cfgs$tcfg$load['st'], '-tests_', cfgs$tcfg$sim.tests)
  ip.expr <- expand.grid(texpr = cfgs$tcfg$sim.expr, dexpr = cfgs$dcfg$expr, eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr, gnames = gnames, iexpr = cfgs$icfg$expr)
  ip.expr.len <- nrow(ip.expr)

  #' construct directory paths and full filenames
  resdir <- file.path(cfgs$paths['result'], cfgs$icfg$expr, fsep = '')
  presdir <- file.path(resdir, 'processed/', fsep = '')
  dir.create(resdir, showWarnings = FALSE)
  dir.create(presdir, showWarnings = FALSE)

  #' create results filenames (to be loaded)
  ip.fns <- file.path(resdir, 'ip', '-g_', ip.expr$gnames, '-o_', ip.expr$oexpr,
                      '-e_', ip.expr$eexpr, '-d_', ip.expr$dexpr, ip.expr$texpr, '.RData', fsep = '')
  ip.pfns <- file.path(presdir, 'ip.pl', '-g_', ip.expr$gnames, '-o_', ip.expr$oexpr,
                       '-e_', ip.expr$eexpr, '-d_', ip.expr$dexpr, ip.expr$texpr, '.RData', fsep = '')

  #' construct combinations of metadata, length of this dataframe MUST equal length of the corresponding 'x.expr' dataframe
  ons <- names(cfgs$ocfg)
  ons <- paste0('o', ons[!(ons %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  ens <- names(cfgs$ecfg)
  ens <- paste0('e', ens[!(ens %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  logging::logwarn('Proc: Meta Data construction assumes fixed 3 parameteres defining origins and 3 params defining surrogates.')
  #' Todo: comeup with a more generic construction of meta data for any number of parameteres for origins and surrogates.
  dns <- names(cfgs$dcfg)
  dns <- paste0('d', dns[!(dns %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  ip.meta <- expand.grid(cfgs$dcfg[[1]], cfgs$dcfg[[2]],
                         cfgs$ecfg[[1]], cfgs$ecfg[[2]], cfgs$ecfg[[3]], #' surrogate meta
                         cfgs$ocfg[[1]], cfgs$ocfg[[2]], cfgs$ocfg[[3]], #' origina meta
                         gnames = gnames, #' graph meta
                         N = cfgs$icfg$N, d = cfgs$icfg$d, d.param = cfgs$icfg$d.param, #' Catalogue meta
                         stringsAsFactors = FALSE
  )
  colnames(ip.meta)[1:8] <- c(dns, ens, ons)

  #' define output container template, will be subset later for each combo
  template <- cbind(ip.meta, type = 'ip', ca = 0, L = 0, stringsAsFactors = FALSE)[, c('ek', 'eopt', 'oopt', 'ok', 'dopt', 'dk', 'ca', 'L', 'type')]
  colnames(template) <- c('surrogates', 'surrogates_policy',
                          'origins_policy', 'origins', 'dnss_policy', 'dnss', 'catchment_interval', 'stream_length', 'type')
  template <- SetReplacementValue(template, cols = c('surrogates_policy', 'origins_policy', 'dnss_policy'),
                                  ov = c(1,2,3), nv = c('Pop', 'Cls', 'Swing'))

  #' check how many files are non existent yet and process accordingly.
  ip.pfns.not.exist <- which(file.access(ip.pfns, mode = 4) < 0)

  while(length(ip.pfns.not.exist) > 0) {
    ip.fns.exist <- which(file.access(ip.fns, mode = 4) == 0)
    for(j in ip.fns.exist)
    {
      logging::logdebug('PROC: IP PL: %s, fn: %s', j, ip.pfns[j])
      ip.pl <- cbind(GetIpPathLengthEcdfResults(ip.fns[j]), template[j,])
      save(ip.pl, file = ip.pfns[j])
    }
    ip.pfns.not.exist <- which(file.access(ip.pfns, mode = 4) < 0)
  }
}

#' Process Storage capacity
#' @description Calculate storage capacity required to cach all items of a catalogue.
#'
#' @export
#' @param cfgs configurations, following a YAML template in config/config.yml which have been used to simulate the network behaviour
ProcTotalStorage <- function(cfgs, chose.cols = c('c', 'nc', 'ek', 'eopt', 'oopt', 'ok'))
{
  doParallel::registerDoParallel(cfgs$cores)
  #' construct combinations of expressions, will be used to create file names of results objects
  gnames <- GetGraphName(cfgs$gcfg$gun.set)
  icn.expr <- expand.grid(texpr = cfgs$tcfg$expr, eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr, gnames = gnames, iexpr = cfgs$icfg$expr)
  icn.expr.len <- nrow(icn.expr)
  cc.fns <- file.path(cfgs$paths['store'], icn.expr$iexpr, 'cc', '-g_', icn.expr$gnames, '-o_', icn.expr$oexpr, '-e_', icn.expr$eexpr, icn.expr$texpr, '.RData', fsep = '')
  ons <- names(cfgs$ocfg)
  ons <- paste0('o', ons[!(ons %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  ens <- names(cfgs$ecfg)
  ens <- paste0('e', ens[!(ens %in% c('expr', 'combo'))]) #' take out exprestions and combinations elements, as they are merely a reflection of the other params
  logging::logwarn('Proc: Meta Data construction assumes fixed 3 parameteres defining origins and 3 params defining surrogates.')
  #' Todo: comeup with a more generic construction of meta data for any number of parameteres for origins and surrogates.
  icn.meta <- expand.grid(cfgs$ecfg[[1]], cfgs$ecfg[[2]], cfgs$ecfg[[3]], #' surrogate meta
                          cfgs$ocfg[[1]], cfgs$ocfg[[2]], cfgs$ocfg[[3]], #' origina meta
                          gnames = gnames, #' graph meta
                          N = cfgs$icfg$N, d = cfgs$icfg$d, d.param = cfgs$icfg$d.param #' Catalogue meta
  )
  colnames(icn.meta)[1:6] <- c(ens, ons)
  cc.fns.exist <- which(file.access(cc.fns, mode = 4) == 0)
  sc <- foreach::foreach(j = cc.fns.exist, .combine = rbind, .multicombine = TRUE) %dopar%
  {
    tmp <- GetStorageResults(cc.fns[j])
    tmp <- cbind(tmp, icn.meta[j,])
  }
  sc <- sc[, chose.cols]
  colnames(sc)[colnames(sc) %in% c('ek', 'eopt', 'oopt', 'ok')] <- c('x', '.var1', '.var2', '.var3')
  sc <- SetReplacementValue(sc, cols = c('.var1', '.var2'), ov = c(1,2,3), nv = c('Pop', 'Cls', 'Swing'))
  sc
}
