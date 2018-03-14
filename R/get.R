#  get.R retrieves or generate and store missing input data
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

#' get Pub/Sub (i.e. PCF) state for a catalogue(s)
#'
#' @export
#' @param cc.dir subdirectory of the content catalogue, name of the dir contain all the configurations (metadata) defining the catalogue
#' @param cc the catalogue in the list of catalogues. The list is loaded into a separate ArgsEnv enviroment. default to NULL to indicate no number is selected
#' @param P list of publishers permutations, including \code{O} origins, \code{E} edges (surrogates) and \code{P} the merge of both
#' @param S list of subscription permutations
#' @param cfgs common configs, as parsed by \code{ParseConfig}, must include: \code{paths} to where to place/retrieve data, \code{cores} number of cpus to use in running, \code{eoexpr} combinations of regular expressions used in naming files and \code{tcfg} for load configs
#' @return pub/sub state of the catalogue, i.e. added publishers and subscribers to the catalogue object
GetPubSubState <- function(cc.dir, cc = NULL, P, S, gund, cfgs)
{
  logging::loginfo('GET: pub/sub state for cataloge in %s', cc.dir)
  doParallel::registerDoParallel(cfgs$cores)
  fn <- file.path(cfgs$paths['store'], cc.dir, 'cc', '-g_', gund$g.name, '-o_', cfgs$eoexpr$oexpr, '-e_', cfgs$eoexpr$eexpr, cfgs$tcfg$expr, '.RData', fsep = "")
  fn.exists <- sum(file.access(fn, mode = 4))
  if(fn.exists == 0)
  {
    logging::logdebug('GET: All pub/sub states exist for catalogue in: %s , returing to upper function', cc.dir)
    return(0)
  }
  logging::loginfo('GET: some Pub/sub states not stored. To generate state first load catalogue from: %s', cc.dir)
  if(is.null(cc))
  {
    cc.file <- file.path(cfgs$paths['store'], cc.dir, 'cc.RData', fsep = '')
    cc.exist <- file.access(cc.file, mode = 4)
    if(cc.exist < 0) {
      logging::logerror('GET: catalogue cannot be loaded to generate pub/sub state, exiting')
      return(-1)
    }
    cc <- GetStoredObjects(cc.file)[[1]]
    cc$path <- file.path(cfgs$paths['store'], cc.dir, fsep = '')
  }
  logging::logdebug('GET: loaded catalogue: N %s, d %s, s %s', cc$N, cc$d, cc$d.param['s'])
  #' check which files are missing, generate and store them.
  ccgen <- which(file.access(fn, mode = 4) < 0)
  logging::logdebug('GET: %s/%s missing Pub/Sub State', length(ccgen), length(fn))
  foreach::foreach(j = ccgen) %dopar%
  {
    logging::logdebug('GET: missing Pub/Sub state for Origins: %s , Edges: %s, generate...', cfgs$eoexpr$oexpr[j], cfgs$eoexpr$eexpr[j])
    GenPubSubState(.P = P$P[[j]], .cc = cc$I, .S = S, .scale = cfgs$tcfg$load, .tests = cfgs$tcfg$tests, .path = fn[j])
  }
  logging::logdebug('GET: Complete for catalogue: N: %s, d: %s, s: %s', cc$N, cc$d, cc$d.param['s'])
}
#' Get DNS based maps
#' @description Get DNS based maps, including: DNS-based client-to-surrogate and surrogate-to-origin mappings
#' @description This resembels DNS redirect in native IP-based CDN
#' @note Edge to Origin mapping is not bound by the number of LDNSes, because CDN management handle that independently from LDNSes
#'
#' @export
#' @param cc.dir subdirectory of the content catalogue, name of the dir contain all the configurations (metadata) defining the catalogue
#' @param cc the catalogue in the list of catalogues. The list is loaded into a separate ArgsEnv enviroment. default to NULL to indicate no number is selected
#' @param P list of publishers permutations, including \code{O} origins, \code{E} edges (surrogates) and \code{P} the merge of both
#' @param gund network graph, population per node graph name and Distance matrix - i.e <g, u, g.name, D>
#' @param cfgs common configs, as parsed by \code{ParseConfig}
GetDnsAndSurrogateToOriginMaps <- function(cc.dir, cc = NULL, P, gund, cfgs)
{
  logging::loginfo('GET: DNS and Edge-to-Origin Maps')
  maps <- list()
  doParallel::registerDoParallel(cfgs$cores)
  elen <- length(cfgs$ecfg$expr)
  dlen <- length(cfgs$dcfg$expr)
  dnsfn <- file.path(cfgs$paths['store'], cc.dir, 'DNS', '-g_', gund$g.name, '-', cfgs$deoexpr$dexpr, '-o_', cfgs$deoexpr$oexpr, '-e_', cfgs$deoexpr$eexpr, '-tests_', cfgs$tcfg$tests, '.RData', fsep = '')
  eofn  <- file.path(cfgs$paths['store'], cc.dir, 'EO', '-g_', gund$g.name, '-o_', cfgs$eoexpr$oexpr, '-e_', cfgs$eoexpr$eexpr, '-tests_', cfgs$tcfg$tests, '.RData', fsep = '')
  dns.exist <- file.access(dnsfn, mode = 4)
  eo.exist <- file.access(eofn, mode = 4)
  dns.and.eo.exists <- ((sum(dns.exist) == 0) & (sum(eo.exist) == 0))
  if(dns.and.eo.exists)
  {
    logging::logdebug('GET: All DNS maps exist for catalogue in: %s , returing to upper function', cc.dir)
    return(0)
  }
  logging::loginfo('GET: missing DNS maps. To generate state first load catalogue from: %s', cc.dir)
  if(is.null(cc))
  {
    ccfn <- file.path(cfgs$paths['store'], cc.dir, 'cc.RData', fsep = '')
    cc.exist <- file.access(ccfn, mode = 4)
    if(cc.exist < 0) {
      logging::logerror('GET: catalogue cannot be loaded to generate pub/sub state, exiting')
      return(-1)
    }
    cc <- GetStoredObjects(ccfn)[[1]]
    cc$path <- file.path(cfgs$paths['store'], cc.dir, fsep = '')
  }
  logging::logdebug('GET: loaded catalogue: N %s, d %s, s %s', cc$N, cc$d, cc$d.param['s'])
  dnsgen <- which(file.access(dnsfn, mode = 4) < 0)
  eogen <- which(file.access(eofn, mode = 4) < 0)
  logging::logdebug('GET: %s/%s missing DNS maps, generate...', length(dnsgen),length(dnsfn))
  foreach::foreach(j = dnsgen) %dopar%
  {
    oidx <- trunc(j / (dlen * elen)) + (j%%(dlen * elen) > 0)
    #' rotate over surrogates, first move to the chunck of rows of particular origins expr (combination). \code{j -1} is to avoid special cases where \code{j == (dlen * elen)}, which would result in \code{k == 0}
    oofset <- trunc((j - 1)/(dlen * elen)) * (dlen * elen)
    k <- j - oofset #' find out the surrogate index within this chunck that corresponds to an origin expression
    eidx <- trunc(k / dlen) +  (k%%dlen > 0) #' rotate based on number of LDNSes
    pidx <- ((oidx - 1) * elen) + eidx #' reconstruct the publishers index = combination of surrogate and origin indexes
    didx <- (j %% dlen) + (! j %% dlen) * dlen   #' rotating over LDNSes
    Pnames <- GetNamesAndCfgs(P$P[[pidx]], tests = cfgs$tcfg$tests)
    logging::logdebug('GET: first generate %s', dnsfn[j])
    GenDnsMap(P = Pnames, opt = cfgs$dcfg$combo$opt[didx], k = cfgs$dcfg$combo$k[didx], D = gund$D, gun = gund, tests = cfgs$tcfg$tests, dns.path = dnsfn[j])
  }
  logging::logdebug('GET: %s/%s missing Edge-to-Origin maps, generate...', length(eogen), length(eofn))
  foreach::foreach(j = eogen) %dopar%
  {
    oidx <- trunc(j/elen) + (j%%elen > 0) #' rotating over origins
    Onames <- GetNamesAndCfgs(P$O[[oidx]], tests = cfgs$tcfg$tests)
    Enames <- GetNamesAndCfgs(P$E[[j]], tests = cfgs$tcfg$tests)
    logging::logdebug('GET: now generate %s', eofn[j])
    GenEoMap(O = Onames, E = Enames, D = gund$D, tests = cfgs$tcfg$tests, eo.path = eofn[j])
  }
  logging::logdebug('GET: DNS and Edge-to-Origin maps complete')
}
#' Get publishers permutations
#' @description Get publishers permutations if stored, if not generate then store
#'
#' @export
#' @param cc content catalogue, including \code{dp} the directory path to the subdirectory of the content catalogue
#' @param gund network graph, population per node, graph name and Distance matrix - i.e <g, u, g.name, D>
#' @param cfgs various common cfgs, must include: \code{ocfg} for origin configs, \code{ecfg} edge (surrogate) configs, and \code{tcfg} - including \code{tcfg$tests} - for load configs.
GetPublishersPermutations <- function(cc, gund, cfgs)
{
  logging::loginfo('GET: Publishers for cc N_%s-d_%s-params_%s', cc$N, cc$d, cc$d.param)
  doParallel::registerDoParallel(cfgs$cores) #' register CPU cores to use in parallel foreach
  elen <- length(cfgs$ecfg$expr)
  olen <- length(cfgs$ocfg$expr)
  #' set up output containser
  P <- list()
  P$O <- list()
  length(P$O) <- olen
  P$E <- list()
  length(P$E) <- elen * olen
  P$P <- list()
  length(P$P) <- elen * olen
  #' -------------------------
  #' verify origins first
  ofn <- file.path(cc$path, 'O', '-g_', gund$g.name, '-', cfgs$ocfg$expr, '-tests_', cfgs$tcfg$tests, '.RData', fsep = '')
  ogen <- which(file.access(ofn, mode = 4) < 0)
  oload <- which(file.access(ofn, mode = 4) == 0)
  #verify surrogates
  efn <- file.path(cc$path, 'E', '-g_', gund$g.name, '-', cfgs$eoexpr$eexpr, '-o_', cfgs$eoexpr$oexpr, '-tests_', cfgs$tcfg$tests, '.RData', fsep = '')
  pfn <- file.path(cc$path, 'P', '-g_', gund$g.name, '-o_', cfgs$eoexpr$oexpr, '-e_', cfgs$eoexpr$eexpr, '-tests_', cfgs$tcfg$tests, '.RData', fsep = '')
  egen <- which(file.access(efn, mode = 4) < 0)
  eload <- which(file.access(efn, mode = 4) == 0)
  pgen <- which(file.access(pfn, mode = 4) < 0)
  pload <- which(file.access(pfn, mode = 4) == 0)
  logging::logdebug('GET: %s/%s origins are missing', length(ogen), olen)
  P$O[ogen] <- foreach::foreach(j = ogen) %dopar%
  {
    logging::logdebug('GET: missing origin of %s, generate...', cfgs$ocfg$expr[j])
    GenOriginsPermutations(.opt = cfgs$ocfg$combo$opt[j], .k = cfgs$ocfg$combo$k[j], .a = cfgs$ocfg$combo$a[j], .cc = cc, .gund = gund, .tests = cfgs$tcfg$tests, .path = ofn[j])
  }
  logging::logdebug('GET: already stored origins')
  P$O[oload] <- foreach::foreach(j = oload) %dopar%
  {
    GetStoredObjects(ofn[j])[[1]]
  }
  #' get/generate surrogates....
  logging::logdebug('GET: %s/%s surrogates are missing', length(egen), elen * olen)
  P$E[egen] <- foreach::foreach(j = egen) %dopar%
  {
    logging::logdebug('GET: missing surrogate of %s, generate...', cfgs$eoexpr$eexpr[j])
    oidx <- trunc(j/elen) + (j%%elen > 0) #' rotating over origins
    eidx <- (j%%elen) + (!j%%elen) * elen   #' rotating over surrogates
    e <- GetNamesAndCfgs(P$O[[oidx]], tests = cfgs$tcfg$tests)
    GenSurrogatePermutations(.opt = cfgs$ecfg$combo$opt[eidx], .k = cfgs$ecfg$combo$k[eidx], .a = cfgs$ecfg$combo$a[eidx], .cc = cc, .e = e, .tests = cfgs$tcfg$tests, .gund = gund, .path = efn[j])
  }
  logging::logdebug('GET: already stored surrogates')
  P$E[eload] <- foreach::foreach(j = eload) %dopar%
  {
    GetStoredObjects(efn[j])[[1]]
  }
  #' get/generate publishers (i.e. merge of origins and surrogates)
  logging::logdebug('GET: %s/%s Publishers are missing', length(pgen), elen * olen)
  P$P[pgen] <- foreach::foreach(j = pgen) %dopar%
  {
    logging::logdebug('GET: Publishers for O:%s, E:%s missing, generate...', cfgs$eoexpr$oexpr[j], cfgs$eoexpr$eexpr[j])
    oidx <- trunc(j / elen) + (j%%elen > 0) #' rotating over origins
    P <- GenMergedOriginsAndSurrogates(P$O[[oidx]], P$E[[j]], .gname = gund$g.name, .tests = cfgs$tcfg$tests, .path = pfn[j])
  }
  #' load already-stored publishers files
  P$P[pload] <- foreach::foreach(j = pload) %dopar%
  {
    P <- GetStoredObjects(pfn[j])[[1]]
  }
  logging::loginfo('GET: Publishers complete')
  P
}
#' Get subscribers permutations
#' @description Get subscribers permutations if stored, if not then generate and store
#'
#' @export
#' @param cc content catalogue, including \code{dp} the directory path to the subdirectory of the content catalogue
#' @param gun network graph, population per node and graph name - i.e <g, u, g.name>
#' @param users numeric vector of active users per node
#' @param cfgs load configs, must include: \code{load} for scale of load on the network and \code{tests} number of random permutations.
#' @param path character vector, gives the full path (including filename) to retreive or store and retreive the object
GetSubscribersPermutations <- function(cc, gun, users, cfgs, path)
{
  #' verify subscribers exisit
  s.exists <- file.access(path, mode = 4)
  ifelse(s.exists < 0,
         {
           logging::loginfo('GET: subscribers not stored, generate for: g:%s scale:%s tests:%s' , gun$g.name, cfgs$load, cfgs$tests)
           S <- GenSubsPermutations(.I = cc$Z, .gun = gun, .cfgs = cfgs, .users = users, .path = path)
         },
         {
           logging::logdebug('GET: subscribers: %s', path)
           load(path)
         }
  )
  S
}
#' Get active users in network
#' @description Get the list of active users per node in a graph. if not available then calculate it
#'
#' @param .gun network graph, population per node and graph name - i.e <g, u, g.name>
#' @param .cfgs load configs, must include: \code{load} for scale of load on the network and \code{tests} number of random permutations.
#' @param .path the directory path for where to save the demands
#' @return list of active users
#' @export
GetActiveUsers <- function(.gun, .cfgs, .path)
{
  logging::logdebug('GET: active users for load %s and tests %s', .cfgs$scale, .cfgs$tests)
  users.exists <- file.access(.path, mode = 4)
  ifelse(users.exists < 0,
         {
           logging::loginfo('GET: Active users not stored, caculating for: g: ', .gun$g.name, ' scale: ', .cfgs$scale, ' tests: ', .cfgs$tests, ' path: ', .path)
           users <- GenActiveUsers(gun = .gun, scale = .cfgs$load, tests = .cfgs$tests, path = .path)
         },
         {
           logging::logdebug('GET: Active users: %s', .path)
           load(.path)
         })
  users
}

#' get list of stored objects
#'
#' @export
#' @param  filenames
#' @return loaded objects
GetStoredObjects <- function(filenames)
{
  obj <- list()
  length(obj) <- length(filenames)
  obj <- lapply(filenames, function(x){y<-load(x); return(base::get(y))})
  obj
}
#' Get publishers' names
#' @description get origins, surrogates, publishers (both origins and surrogates), DNS or subscribers names, provided a list of origins.
#' @description The list structure is typically in the form O[[combo.row.id]][[test.num]]
#'
#' @export
#' @param O nested list of origins, each list in the list corresponds to a set of configuration params
#' @param tests the number of O permutations
#' @return nested list of origins names
GetNames <- function(O, tests)
{
  o <- list()
  for(i in 1:tests)
  {
    o[[i]] <- names(O[[i]])
  }
  o
}
#' Get publishers' names and configs
#' @description get origins, surrogates, publishers (both origins and surrogates), DNS or subscribers names and configs, provided a list of origins.
#' @description The list structure is typically in the form O[[combo.row.id]][[test.num]], O$cfg
#'
#' @export
#' @param O nested list of origins, each list in the list corresponds to a set of configuration params
#' @param tests the number of O permutations
#' @return nested list of origins names, in addition to origins cfg
GetNamesAndCfgs <- function(O, tests)
{
  o <- list()
  for(t in 1:tests)
  {
    o[[t]] <- names(O[[t]])
  }
  o$cfg <- O$cfg
  o
}
#' Get origins
#' @description Retrieve the stored permutations of Origins. if not stored, then calculate, store and return it.
#'
#' @note NOT COMPLETE YET!!
#' @export
#' @param opt numeric value indicating the selection policy, this corresponds to the service placement algorithm. currently there are 3 greedy algorithms: largest population first, highest closeness first and swing
#' @param k number of origins
#' @param a minimum storage unit in GB
#' @param gund network graph, population per node and graph name - i.e <g, u, g.name, DM, AM>, DM/AM: Distance/Adjacency Matrix
#' @param tests number of random permutations
#' @param path full path to retreive or write the origins list
GetOrigins <- function(opt, k, a, gund, tests, path)
{
  tryCatch(load(path), error = function(e){
    logging::loginfo('Origins are not stored for simulation ID, caculating..')
    GenOriginsPermutations(.opt = opt, .k = k, .a = a, .cc = cc, .gund = gund, .tests = test, .path = path)
  })
  load(fn)
  O
}
#' Get Catalogue
#' @description Get content catalogue, i.e. check if catalogue is stored and if not, then generate and store in a new directory of name made up from the catalogue config. params
#'
#' @export
#' @param N size of the catalogue
#' @param d popularity distribution
#' @param d.param parameters of the popularity distribution
#' @param v volume range of the items
#' @param path to where the catalogue directory and the catalogue object should be stored
GetCatalogue <- function(N, d, d.param, v, path)
{
  fn <- file.path(path , 'cc.RData', fsep = "")
  cc.exists <- file.access(fn, mode=4)
  ifelse(file.access(fn, mode=4) < 0,
         {
           logging::logdebug('GET: catalogue not stored, generate for: N:%s d:%s params:%s v:%s', N, d, d.param, v)
           cc <- GenCataloguePermutation(.N = N, .d = d, .d.param = d.param, .v = v, .path = path)
         },
         {
           logging::logdebug('GET: catalogue: %s', fn)
           load(fn)
           cc
         }
  )
  cc$path <- path
  cc
}
#' verctorised form of \code{GetCatalogue}
#'
#' @seealso \code{\link{GetCatalogue}}
vGetCatalogue <- Vectorize(GetCatalogue, vectorize.args = c('N', 'd', 'd.param', 'path'), SIMPLIFY = FALSE)
#' Get network graph(s)
#'
#' @export
#' @param cfg graph configurations, including: gdir where the graphs are stored, gset: index of graphs and augument, whether or not to augument the graph
#' @param paths string vector of at least 2 elements, first is paths['home'], indicating home directory and second paths['store'] indicating storage (write) directory
#' @return list of gun <g, u, n>, i.e. graph object, population per node, graph name
GetGraph <- function(cfg, paths)
{
  gpath <- file.path(paths['home'], cfg$gdir, '/', fsep = "")
  if(!exists('zoo'))
    zoo <- GraphLoadZoo(read.path = gpath, write.path = paths['store'])
  gs <- zoo$graphs[cfg$gset]
  gs.files <- strsplit(zoo$files[cfg$gset], ".", fixed = TRUE)
  gs <- mapply(function(x, y) {z <- strsplit(x[1], '/', fixed = TRUE)[[1]]; y$g.name <- z[length(z)]; y}, gs.files, gs, SIMPLIFY = FALSE)
  gs
}
#' Get graph name for list of graphs
#'
#' @export
#' @param gset list of graph objects, each of which includes element \code{g.name}
#' @return graph name as stated by $g.name
GetGraphName <- function(gset)
{
  gnames <- vapply(gset, "[[", character(1), "g.name")
  gnames
}
#' get the directory path to the content catalogue
#'
#' @export
#' @param cfgs list of config variables for a single catalogue
#' @param dir parent directory of the catalogue
#' @return path to the catalogue directory
GetCataloguePath <- function(N, d, dparams, v, dir = '')
{
  v.names <- paste0(v, collapse = "_")
  param.names <- paste0(names(dparams), "_", dparams, collapse = "_")
  dp <- file.path(dir, 'N_', N, '-d_', d, '-params_', param.names, '-v_', v.names, '/' , fsep = "")
  dp
}
#' Get complementary items
#' @description get complementary items in the content catalogue, i.e. items in the catalogue that does not appear in a subset
#'
#' @export
#' @param J subset of content items that belongs to I
#' @param I the content catalogue
#' @return vector of the complementary items
GetComplementaryItems <- function(J, I)
{
  I <- names(I)
  C <- I[!(I %in% J)]
  C
}
#' convert DNS-to-surrogate map format from list to array
#'
#' @export
#' @param d name of DNS node
#' @param p set of surrogates for a DNS node
#' @return array form of the dns list
GetDnsToSurrogateArrayMap <- function(d, p)
{
  da <- cbind(d = d, p = p)
  da
}

# Results -----------------------------------------------------------------
#' load Edge Matrix (eM) for the ICN results data of unicast
#'
#' @export
#' @param filename of the result data
GetIcnUnicastEmResults<- function(filename)
{
  uc <- c() #' unicast capacity
  t <- c() #' total ingress demand
  load(filename)
  for(i in 1:length(icn.uc[[1]]['added',]))
  {
    t[i] <- sum(icn.uc[[1]]['total',][[i]])
    uc[i] <- sum(unlist(icn.uc[[1]]['added',][[i]]))
  }
  cbind(uc = uc, t = t)
}
#' load Edge Matrix (eM) for the ICN results data of multicast
#'
#' @export
#' @param filename of the result data
GetIcnMulticastEmResults <- function(filename)
{
  mc <- c() #' unicast capacity
  t <- c() #' total ingress demand
  load(filename)
  for(i in 1:length(icn.mc[[1]]['added',]))
  {
    t[i] <- sum(icn.mc[[1]]['total',][[i]])
    mc[i] <-sum(unlist(icn.mc[[1]]['added',][[i]]))
  }
  cbind(mc = mc, t = t)
}
#' load Edge Matrix (eM) for the IP results data
#'
#' @export
#' @param filename of the result data
GetIpEmResults<- function(filename)
{
  c <- c() #' capacity
  t <- c() #' total ingress demand
  load(filename)
  for(i in 1:length(ip[[1]]['eM',]))
  {
    t[i] <- sum(ip[[1]]['total',][[i]])
    c[i] <- sum(unlist(ip[[1]]['added',][[i]]))
  }
  cbind(c = c, t = t)
}
#' load the paths of the ICN simulation results for unicast
#'
#' @export
#' @param filename of the results data
GetIcnUnicastPathLengthResults <- function(filename)
{
  load(filename)
  l <- unlist(icn.uc[[1]]['trees',])
  l
}
#' load the paths of the ICN simulation results for unicast
#'
#' @export
#' @param filename of the results data
#' @param D NxN matrix of all the routes
GetIcnUnicastPathLengthEcdfResults <- function(filename)
{
  l <- c()
  e <- c()
  load(filename)
  l <- stats::ecdf(unlist(icn.uc[[1]]['trees',]))
  bins <- length(stats::knots(l))
  for(a in stats::knots(l))
  {
    e <- c(e, l(a))
  }
  cbind(l = stats::knots(l), e = e)
}
#' load the tress of the IP simulation results
#'
#' @export
#' @param filename of the results data
GetIpPathLengthResults <- function(filename)
{
  load(filename)
  l <- unlist(ip[[1]]['trees',])
  l
}
#' load the tress of the IP simulation results
#'
#' @export
#' @param filename of the results data
GetIpPathLengthEcdfResults <- function(filename)
{
  l <- c()
  e <- c()
  load(filename)
  l <- stats::ecdf(unlist(ip[[1]]['trees',]))
  bins <- length(stats::knots(l))
  for(a in stats::knots(l))
  {
   e <- c(e, l(a))
  }
  cbind(l = stats::knots(l), e = e)
}
#' get the pub/sub state to obtain the storage capacity
#'
#' @export
#' @param filename of the pub/sub state
#' @param scale load on the network, this is the outer index of the nest list of pub/subs
#' @param tests number of simulations, that is the inner scale of the list
GetStorageResults <- function(filename)
{
  c <- c()
  nc <- c()
  t <- c()
  load(filename)
  cc$ocfg <- NULL
  cc$ecfg <- NULL
  ps <- cc[[1]]
  j <- length(ps)
  for (i in 1:j)
  {
    c[i] <- sum(vapply(ps[[i]], function(x){c <- sum(x$l) * x$v}, numeric(1)))
    nc[i] <- sum(vapply(ps[[i]], function(x){c <- length(x$l[x$l == 0]) * x$v}, numeric(1)))
  }
  ret <- cbind(c = c, nc = nc)
  gc() #' grabage collection, required to optimise memeory
  ret
}
# Get Multicast Trees from Unicast Paths, without dijkstra routing or service point reselection -----------------------------------------------------
#' Get ICN multicst capacity results
#' Get ICN provisioned capacity when co-incidental multicast is in place, given a catchment interval \code{t} seconds and a total running time of \code{L} seconds
#'
#' @export
#' @param cc pub/sub state for a content catalogue, required as it gives the number of subscriptions per item
#' @param o list object of the multicast tree - must include: trees: i.e. list of paths in the multicast trees, added: list of added values in unicast paths, this is merely a container so don't have to recreat it, and total: again a container for total values.
#' @param g network graph list including graph, population per node, name, distance matrix and edge matrix
#' @param t catchment interval in seconds
#' @param L total running period in seconds
GetIcnMcCapacity <- function(cc, o, g, t, L)
{
  logging::loginfo('GET: multicast trees instead of unicast paths. stream period: %s, catchment interval: %s', L, t)
  EM <<- g$eM
  for(i in cc)
  {
    p <- i$l[i$l == 0]
    p <- p[!(names(p) %in% names(i$s))]
    p <- replace(p, p==0, 1)
    i$s <- c(i$s, p)
    n <- trunc((L + L/i$s)/(t + L/i$s))
    for(r in names(o$trees[[i$i]]))
    {
      o$added[[i$i]][[r]] <- HelperUpdateTreeLoadOnEm(g$R[[r]][o$trees[[i$i]][[r]]], (n * i$v)[o$trees[[i$i]][[r]]])
    }
    o$total[i$i] <- sum(n * i$v)
  }
  logging::loginfo('GET: multicast complete...')
  o
}
GetIcnMcCapacityForOneDemand <- function(iL, o, t, L)
{
  p <- iL$l[iL$l == 0]
  p <- p[!(names(p) %in% names(iL$s))]
  p <- replace(p, p==0, 1)
  iL$s <- c(iL$s, p)
  n <- trunc((L + L/iL$s)/(t + L/iL$s))
  #' update multicast load on each tree
  for(t in names(o$trees))
  {
    o$added[[t]] <- HelperUpdateTreeLoadOnEm(g$R[[t]][o$trees[[t]]], (n * iL$v)[o$trees[[t]]])
  }
  o$total <- sum(n * iL$v)
  o
}

# Misc --------------------------------------------------------------------
GetUnsetDemands <- function(x)
{
  length(x$i) <= x$v
}

# Edges/Paths/Trees -------------------------------------------------------

#' Extract the set of edges in a tree list
#' @description common edges will be merged into one edge indicating multicast
#'
#' @export
#' @param t tree in the form of list, each element represent a branch in the tree
#' @return matrix of edges in the tree
GetEdgesOnMulticastTree <- function(t)
{
  el <- lapply(t, HelperPathToEdges)
  el <- do.call(rbind, el)
  el.na <- el[is.na(el[,1]),]
  el.not.na <- unique(el[!is.na(el[,1]),])
  el <- rbind(el.not.na, el.na)
  el
}
#' Extract the set of edges in a tree list
#' @description common edges will NOT be merged into one edge indicating individual unicast paths.
#'
#' @export
#' @param t tree in the form of list, each element represent a branch in the tree
#' @return matrix of edges in the tree
GetEdgesOnTreeOfUnicastPaths <- function(t)
{
  el <- lapply(t, HelperPathToEdges)
  el <- do.call(rbind, el)
  el
}
#' Convert path to set of edges
#'
#' @export
#' @param p path as a vector of nodes
#' @return matrix of edges in the form <from, to>
GetEdgesOnPath <- function(p)
{
  if (all(is.na(p))) {
    e <- cbind(from=NA, to=NA)
    return(e)
  }
  pl <- length(p)
  from <- p[1:(pl-1)]
  to <- p[2:pl]
  cbind(from, to)
}
#' get matching row indices between an Edge Matrix and a sub matrix of a path
#'
#' @export
#' @param eM edge matrix
#' @param es subset of edges in eM
GetMatchingRowIndex <- function(eM, es)
{
  # need to explicitly indicate the 'base' package because merge is function that exist in multiple packages and will conflict
  ri <- base::merge(es, eM, sort = FALSE)$ri #' improtant to keep sort = false, so that the order of es is respected.
  ri
}



