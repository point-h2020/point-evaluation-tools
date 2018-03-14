#  generate.R generate and store input data for modelling
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



# Overall Simulation Data -------------------------------------------------

#' Generation simulation data
#' @description Generate (and save) simulation data provided a simulation configuration file
#' @details the configs should include all the necessary groups required to generate the input data. Generated data will be stored with consisten name, whereby the catalogue cfgs will make up the name of the catalogue subdirectory. inside each catalogue directory, there will be the permutations generated for the catalgoue. The name of each data file follows the format \code{ObjectName-graph_params-object_Params.RData}
#'
#' @export
#' @param cfgs parsed configuration groups from a YAML file, see function \code{ParseConfig}
#' @param config.active configuration enviroment of the logger
GenSimData <- function(cfgs, config.active = 'default')
{
  #' set logging level
  Sys.setenv(R_CONFIG_ACTIVE = config.active)
  gund.set <- cfgs$gcfg$gun.set
  #' check stored data, if not generate then store. start first with input catalogue
  logging::logdebug('GEN: simulation data')
  cc.paths <- file.path(cfgs$paths['store'], cfgs$icfg$expr , fsep = "")
  cc <- vGetCatalogue(N=cfgs$icfg$combo$N, d = cfgs$icfg$combo$d, d.param = cfgs$icfg$combo$dparam, v = cfgs$icfg$v, path = cc.paths)
  for(gund in gund.set)
  {
    #' retreive users permutations depending on scale of load. generate if not already stored.
    users.file <- file.path(cfgs$paths['store'], "users", "-g_", gund$g.name, cfgs$tcfg$expr, ".RData", fsep = "")
    users <- GetActiveUsers(.gun = gund, .cfgs = cfgs$tcfg, .path = users.file)
    for(i in 1:length(cc.paths))
    {
      P <- GetPublishersPermutations(cc = cc[[i]], gund = gund, cfgs = cfgs)
      #' check if subscribers stored, if not generate and store
      s.file <- file.path(cc[[i]]$path, "S", "-g_", gund$g.name, cfgs$tcfg$expr, ".RData", fsep = "")
      S <- GetSubscribersPermutations(cc = cc[[i]], gun = gund, users = users, cfgs = cfgs$tcfg, path = s.file)
      GetPubSubState(cc.dir = cfgs$icfg$expr[i], cc = cc[[i]], P = P, S = S, gund = gund, cfgs = cfgs)
      GetDnsAndSurrogateToOriginMaps(cc.dir = cfgs$icfg$expr[i], cc = cc[[i]], P = P, gund = gund, cfgs = cfgs)
    }
  }
  logging::logdebug('GEN: simulation data generation complete')
}
#' Generate pub/sub state
#' @description generate (and save) pub/sub state according to saved permutations of clients (S), origins (O) and surrogates (E)
#' @description saved state can be loaded at the start of a simulation rather than be calculated then.
#'
#' @export
#' @param .P permutations of publishers generated for .cc for an snapshot of configurations, the length of the list should equal tests
#' @param .cc permutations of content catalogue
#' @param .S permutations of subscribers generated for .cc, the length of the list should equal \code{tests} x \code{#steps in scale}
#' @param .scale load of subscriptions
#' @param .tests number of permutations
#' @param .path path to store the generated data
GenPubSubState <- function(.P, .cc, .S, .scale, .tests, .path)
{
  cc <- list()
  logging::logdebug('GEN: pub/sub state for origins:{%s:%s} surrogates:{%s:%s}', names(.P$ocfg), .P$ocfg, names(.P$ecfg), .P$ecfg)
  si <- seq(.scale['lb'], .scale['ub'], .scale['st']) #scale index
  sl <- length(si)
  for(s in 1:sl)
  {
    #' mapply is the fastes approach from the 3 options: 1- serial for , 2- parallel foreach and 3- mapply
    cc[[s]] <- mapply(CatalogueAppendPubSub, P = .P[1:.tests], S = .S[[s]], MoreArgs = list(I = .cc, c = FALSE), SIMPLIFY = FALSE)
  }
  cc$ocfg <- .P$ocfg
  cc$ecfg <- .P$ecfg
  o.names <- paste0(names(.P$ocfg), "_", .P$ocfg, collapse = "-")
  e.names <- paste0(names(.P$ecfg), "_", .P$ecfg, collapse = "-")
  s.names <- paste0(.scale, collapse = "_")
  save(cc, file = .path)
}

# Catalogue Permutations --------------------------------------------------

#' Generate catalogue permutations
#' @description generate (and save) permutations of content catalogues for multiple tests and possibly multiple load values.
#' @description saved permutations can be loaded at the start of a simulation rather than be calculated then, to save simulation time
#'
#' @export
#' @param .N Size of the content catalogue, i.e. number of items in the catalogue
#' @param .d popularity distribution of the items (e.g. Zipf, lognormal,..), currently only Zipf is supported
#' @param .d.param distribution parameters, these are the factors that impact distribution shape. e.g. Zipf exponent and the ignore ratio (i.e. how much of the top level to ignore) (s = 1, ig = 0)
#' @param .v vector of double values, defining the possible volume per item
#' @param .path string giving the directory path for where to store the permutations
GenCataloguePermutation <- function(.N, .d, .d.param, .v, .path)
{
  cc <- list()
  logging::logdebug('GEN: content catalogue in path: %s', .path)
  cc <- Catalogue(N = .N, d = .d, d.param = .d.param, v = .v)
  cc$N <- .N
  cc$d <- .d
  cc$d.param <- .d.param
  dir.create(.path, showWarnings = FALSE)
  fn <- file.path(.path, 'cc.RData', fsep = "")
  cc$path <- .path
  save(cc, file = fn)
  cc
}

# Subscribers Permutations -------------------------------------------------------------

#' Generate active users
#' @description Generate scaling vectors of demand volumes, each scale has multiple random generated volumes in the count of 'tests'
#'
#' @export
#' @param gun network graphNEL
#' @param scale 0 > vector > 1.0 of the ratio of demands to capacity in the form of \code{(lb, ub, st)}, lb/ub: lower/upper bound, st: step
#' @param tests number of random permutations
#' @param path the directory path for where to save the demands
GenActiveUsers <- function(gun, scale, tests, path)
{
  users <- list()
  total.subs <- c(0) #' total number of users for a scaling up load
  nodes.population <- gun$u[gun$u > 0] #' filter out core and aggregate nodes, as they wont have direct users.
  nodes <- names(nodes.population)  #' get node names
  si <- seq(scale['lb'], scale['ub'], scale['st']) #' scale index
  sl <- length(si)  #' get the length of the scaling vector
  network.population <- sum(nodes.population)     #' get the overall network population
  #' print(nodes.population)
  for(s in 1:sl)
  {
    users[[s]] <- list()
    total.subs[s] <- si[s] * network.population   #' total number of subscribers in the network, depending on the network load
    for(t in 1:tests)
    {
      #' generate lognormal that is in the size of total.demand
      users[[s]][[t]] <- JarSubsAssignUsersForTimeSlot(nodes.population, network.population, total.subs[s])
    }
  }
  save(users, file = path)
  users
}
#' Generate subscribers' permutations
#' @description Generate randomised and scaling subscriptions for multiple tests and possibly multiple load values
#'
#' @export
#' @param .I a sorted set, representing the global set of items in the network. I is the probabilities of occurance of the items
#' @param .gun network graph, population per node and graph name - i.e <g, u, g.name>
#' @param .cfgs load configs, must include: \code{load} for scale of load on the network and \code{tests} number of random permutations.
#' @param .users list of active users per node, for each load value in \code{scale} and each test in \code{tests}. If this users is missing, the function will try to load from stored list or calculate the list if not stored using \code{\link{GenActiveUsers}}
#' @param .path the full path (including filename) for where to save the demands
GenSubsPermutations <- function(.I, .gun, .cfgs, .users, .path)
{
  S <- list()
  si <- seq(.cfgs$load['lb'], .cfgs$load['ub'], .cfgs$load['st']) #scale index
  sl <- length(si)  #get the length of the scaling vector
  #load the numbers of active users if stored, otherwise calculate then load them
  ifelse(missing(.users),
         .users <- GetActiveUsers(.gun, .cfgs, .path),
         .users
  )
  for(s in 1:sl)
  {
    u <- .users[[s]]
    subs <- lapply(u, SubscribePerNode, .I) #' looping for number of permutations (tests)
    S[[s]] <- subs
  }
  #create sim.id directory in case it has not been created before, no error if it already exists
  # fn <- file.path(.path, "S", "-g_", .gun$g.name, "-scale_", .scale['lb'], '_', .scale['ub'], '_', .scale['st'], "-tests_", .tests, ".RData", fsep = "")
  save(S, file = .path)
  S
}

# Publishers Permutations -------------------------------------------------

#' Generate origins permutations
#' @description Generate (and save) origins permutations, those are nodes holding copies of every item in the content catalogue, there are no restriction on their storage capacity
#' @description The saved permutations can be loaded at the start of a simulation rather than be calculated then, to save simulation time
#'
#' @export
#' @param .opt numeric value indicating the selection policy, this corresponds to the service placement algorithm. currently there are 3 greedy algorithms: largest population first, highest closeness first and swing
#' @param .k numeric value giving #origins
#' @param .a numberic value giving min storage size
#' @param .cc content catalogue instance. A list that includes <I, Z, N, d, d.params> whereby: I content objects, Z popularity distribution, N number of items, d name of popularity distribution, d.params parameters used to generate the distribution
#' @param .gund network graph, population per node, graph name and distance matrix - i.e <g, u, g.name, D>
#' @param .tests number of permutations
#' @param .path string giving the full paths to write the origins permutations, including the object name
GenOriginsPermutations <- function(.opt, .k, .a, .cc, .gund, .tests, .path)
{
  O <- list()
  logging::logdebug('GEN: origins K %s, policy %s, a %s', .k, .opt, .a)
  for(t in 1:.tests)
  {
    O[[t]] <- OriginPublishers(cc = .cc$I, gu = .gund, k = .k, a = .a, opt = .opt, c = FALSE)
  }
  O$cfg <- c(opt = .opt, k = .k, a = .a)
  O$path <- .path
  save(O, file = .path)
  O
}
#' Generate surrogate permutations
#' @description generate surrogate permutations for one origin, those are nodes holding copies of some of the items in the content catalogue but not all. They are restricted by a capacity constraint
#' @description The saved permutations can be loaded at the start of a simulation rather than be calculated then, to save simulation time
#'
#' @export
#' @param cfg config parameters, including \code{k} numeric value giving #surrogates, \code{a} numberic value giving min storage size and \code{opt} numeric value indicating the selection policy, this corresponds to the service placement algorithm. currently there are 3 greedy algorithms: largest population first, highest closeness first and swing
#' @param .cc content catalogue instance. A list that includes <I, Z, N, d, d.params> whereby: I content objects, Z popularity distribution, N number of items, d name of popularity distribution, d.params parameters used to generate the distribution
#' @param .gund  network graph, population per node and graph name - i.e <g, u, g.name>
#' @param .e list of origins names to be excluded, including (in addition) .e$cfg, which gives the config params of the excluded origins.
#' @param .tests number of permutations
#' @param .path string giving the paths to write the surrogate permutations
GenSurrogatePermutations <- function(.opt, .k, .a, .cc, .gund, .e, .tests, .path)
{
  E <- list()
  logging::logdebug('GEN: surrogate configs: k:%s opt:%s', .k, .opt)
  for(t in 1:.tests)
  {
    E[[t]] <- SurrogatePublishers(cc = .cc$I, gu = .gund, k = .k, a = .a, opt = .opt, D = .gund$D, e = .e[[t]], c = FALSE)
  }
  logging::logdebug('GEN: surrogate complete, save: %s', .path)
  E$cfg <- c(opt = .opt, k = .k, a = .a)
  E$path <- .path
  save(E, file = .path)
  E
}
#' vectorised wrapper of origins.permutations function
vGenSurrogatePermutations <- Vectorize(GenSurrogatePermutations, vectorize.args = c('.opt', '.k', '.a', '.path'), SIMPLIFY = FALSE)
#' vectorised wrapper of origins.permutations function
vGenOriginsPermutations <- Vectorize(GenOriginsPermutations, vectorize.args = c('.opt', '.k', '.a', '.path'), SIMPLIFY = FALSE)
#' Generate publishers' permutations
#' @description Generate (and save) origins and surrogates - publishers - permutations for a single content catalogue, origins are generated first therefore no nodes need to be excluded
#' @description Then for each origin instance, permutations of surrogates are generated according to surrogates configurations which excludes the origins nodes.
#'
#' @export
#' @param cc content catalogue in the form of a list that includes: the content catalogue items (I) and the popularity distribution of I (Z)
#' @param ocfg origin configurations
#' @param ecfg surrogate configurations
#' @param gund network graph, population per node, graph name and distance matrix - i.e <g, u, g.name, D>
#' @param tests number of permutations
#' @param path string giving the paths to write the publishers permutations
#' @return  list of origins and surrogates permutations, as well as the merge of both to give 'publishers' sets. All have been saved
GenPublishersPermutations <- function(cc, ocfg, ecfg, gund, tests, path)
{
  O <- list()
  E <- list()
  M <- nrow(ocfg$combo)
  N <- nrow(ecfg$combo)
  fn <- file.path(path, 'O', "-g_", gund$g.name, '-', ocfg$expr, "-tests_", tests, ".RData", fsep = "" )
  #generate origins permutations
  O <- vGenOriginsPermutations(.opt = ocfg$combo$opt, .k = ocfg$combo$k, .a = ocfg$combo$a, .cc = cc, .tests = tests, .gund = gund, .path = fn)
  #extract origin nodes
  e <- lapply(O, GetNamesAndCfgs, tests)
  #generate surrogates perms for each origin specification
  for(m in 1:M)
  {
    logging::logdebug('GEN: surrogates when origins: %s', ocfg$expr[m])
    fn <- file.path(path, 'E', "-g_", gund$g.name, '-', ecfg$expr, '-o_', ocfg$expr[m], "-tests_", tests, ".RData", fsep = "" )
    E[[m]] <- vGenSurrogatePermutations(.opt = ecfg$combo$opt, .k = ecfg$combo$k, .a = ecfg$combo$a, .cc = cc, .e = e[[m]], .tests = tests, .gund = gund, .path = fn)
  }
  #extract combinations of both origins and surrogates for publisher filename paths
  expr <- expand.grid(eexpr = ecfg$expr, oexpr = ocfg$expr)
  fn <- file.path(path, 'P', '-g_', gund$g.name, '-o_', expr$oexpr, '-e_', expr$eexpr, '-tests_', tests, '.RData', fsep = '')
  #unfold the origins and surrogate lists to have two equal length lists
  E <- unlist(E, recursive = FALSE)
  Oe <- rep(O, each = N)
  P <- mapply(GenMergedOriginsAndSurrogates, Oe, E, .path = fn, MoreArgs = list(.gname = gund$g.name, .tests = tests), SIMPLIFY = FALSE)
  ret <- list(O = O, E = E, P = P)
  ret
}
#' Generate merged list of origins and permutation
#' @description merge the list of origins with the list of surrogates in the one list of publishers
#'
#' @export
#' @param O list of origins
#' @param E list of surrogates
#' @param .gname network graph name
#' @param .tests numeric value giving number of permutations inside each list
#' @param .path string giving the paths to write the publishers permutations
GenMergedOriginsAndSurrogates <- function(O, E, .gname, .tests, .path)
{
  P <- list()
  for(t in 1:.tests)
  {
    P[[t]] <- c(O[[t]], E[[t]])
  }
  P$ocfg <- O$cfg
  P$ecfg <- E$cfg
  save(P, file=.path)
  P
}

# DNS Permutations --------------------------------------------------------

#' Generate DNS map permutations
#' @description Generate DNS map of client nodes to surrogate and origin nodes, also generate a surrogate to origin map
#'
#' @export
#' @param cfg DNS configurations, currently including: \code{k} number of LDNS points and \code{opt} DNS selection policy
#' @param P list of origins (O), surrogates(E) and their merge to publishers (P) permutations elements, provided by the function \code{GenPublishersPermutations}
#' @param gund network graph, population per node, graph name and distance matrix - i.e <g, u, g.name, D>
#' @param D NxN matrix of distance from each node to every other node
#' @param tests number of permutations
#' @param path string giving the paths to write the permutations of the DNS maps and surrogate-to-origin maps
GenDnsAndSurrogateToOriginMapPermutations <- function(cfg, P, gund, tests, path)
{
  logging::logdebug("GEN: catalogue, 1st path: %s", path[1])
  dnum <- nrow(cfg$combo)
  Orep <- length(P$E)/length(P$O)
  Oe <- rep(P$O, each = Orep)
  O.names <- sapply(Oe, GetNamesAndCfgs, tests, simplify = FALSE)
  E.names <- sapply(P$E, GetNamesAndCfgs, tests, simplify = FALSE)
  P.names <- sapply(P$P, GetNames, tests, simplify = FALSE)
  Pnum <- length(P.names)
  logging::logdebug('GEN: DNS: number of publisher configuration permutations: %s', Pnum)
  #' calculate file offset
  j <- 1
  k <- dnum
  for(i in 1:Pnum)
  {
    #' create DNS and surrogate-to-origin mapping for every combination of origins and surrogates
    mapply(GenDnsAndSurrogateToOriginMap, cfg$combo$opt, cfg$combo$k, dns.path = path$dns[j:k], eo.path = path$eo[j:k],
           MoreArgs = list(gu = gund, O = O.names[[i]], E = E.names[[i]], P = P.names[[i]], tests = tests))
    j <- j + dnum
    k <- k +dnum
  }
}
#' Generate DNS and surrogate-to-origins permutations
#' @description generate DNS and surrogate-to-origin mappings for permutations of origins and surrogates for a single set of configurations
#' @note, the function is intermidiary, as it should normally be called from within \code{GenDnsAndSurrogateToOriginMapPermutations}
#'
#' @export
#' @param opt DNS selection policy
#' @param k number of LDNS points
#' @param O names and configurations of origins permutation, length of the list equals tests
#' @param E names and configurations of of surrogates permutation, length of the list equals tests
#' @param P names of publishers (i.e. joined O and E) permutation, length of list equals tests
#' @param gun network graph, population per node and graph name - i.e <g, u, g.name>
#' @param D NxN matrix of distance from each node to every other node
#' @param tests number of permutations
#' @param dns.path string giving the path to write the permutations of the DNS maps
#' @param eo.path string giving the path to write the permutations of the surrogate-to-origin maps
GenDnsAndSurrogateToOriginMap <- function(opt, k, O, E, P, gund, tests, dns.path, eo.path)
{
  DNS <- lapply(P[1:tests], DnsMap, gu = gund, D = gund$D, k = k, opt = opt, c = FALSE)
  EO <- mapply(AlgoAssignSurrogateToOrigin, O = O[1:tests], E = E[1:tests], MoreArgs = list(D = D), SIMPLIFY = FALSE)
  save(DNS, file = dns.path)
  save(EO, file = eo.path)
}
#' Generate DNS map
#' @description generate DNS map for permutations of origins and surrogates for a single set of configurations
#'
#' @export
#' @param P names of publishers (i.e. joined O and E) permutation, length of list equals tests
#' @param opt DNS selection policy
#' @param k number of LDNS points
#' @param D NxN matrix of distance from each node to every other node
#' @param gun network graph, population per node and graph name - i.e <g, u, g.name>
#' @param tests number of permutations
#' @param dns.path string giving the path to write the permutations of the DNS maps
GenDnsMap <- function(P, opt, k, D, gun, tests, dns.path)
{
  DNS <- lapply(P[1:tests], DnsMap, gu = gun, D = D, k = k, opt = opt, c = FALSE)
  save(DNS, file = dns.path)
}
#' Generate surrogate-to-origins map
#' @description Generate surrogate-to-origin maps for permutations of origins and surrogates for a single set of configurations
#'
#' @export
#' @param O names and configurations of origins permutation, length of the list equals tests
#' @param E names and configurations of of surrogates permutation, length of the list equals tests
#' @param D NxN matrix of distance from each node to every other node
#' @param tests number of permutations
#' @param eo.path string giving the path to write the permutations of the surrogate-to-origin maps
GenEoMap <- function(O, E, D, tests, eo.path)
{
  EO <- mapply(AlgoAssignSurrogateToOrigin, O = O[1:tests], E = E[1:tests], MoreArgs = list(D = D), SIMPLIFY = FALSE)
  save(EO, file = eo.path)
}

# Popularity Distributions ------------------------------------------------

#' Generate a Zipf distribution
#' @description The generated value is the probablity of occurance in 100 events.
#'
#' @export
#' @param N the number of ranks when assuming 1 item per rank
#' @param s the distribution exponent
#' @param ig ignore ratio (0.0, 0.5): fraction of the top of the distribution to ignore
#' @return name/value numeric vector of zipf-based pmf, whereby name is the rank of an element and value is the fraction of time it appears in a dataset
GenZipfPmf <- function(s = 1.0, N, ig=0){
  #since the probablity is counted in terms of 100%
  fi <- ig * N
  fi <- fi + 1 #ensures that the distribtion starts from 1 when ig = 0
  k <- c(fi:N)
  hn <- 0
  #get the harmonic number of n, using the cumulative sum function, brilient!!
  hn <- cumsum(1/k^s)[N-fi]
  f <-  (1/(k^s * hn))
  names(f) <- k
  f
}
#' Generate an Exponential distribution
#' @description The generated value is the probablity of occurance in 100 events.
#'
#' @export
#' @param N the number of ranks when assuming 1 item per rank
#' @param s the distribution exponent
#' @param ig ignore ratio (0.0, 0.5): fraction of the top of the distribution to ignore
#' @return name/value numeric vector of zipf-based pmf, whereby name is the rank of an element and value is the fraction of time it appears in a dataset
GenNexpPmf <- function(N, ig=0){
  #since the probablity is counted in terms of 100%
  fi <- ig * N
  fi <- fi + 1 #ensures that the distribtion starts from 1 when ig = 0
  k <- c(fi:N)
  f <- exp(-k/N)/N
  names(f) <- k
  f
}

# Misc --------------------------------------------------------------------

#' simulate multiple instances
#'
#' @description Generate random unique identifier, used to link different objects of the same simulation to each other.
#' @description The function is taken from the R-blog: https://www.r-bloggers.com/generating-unique-random-ids/
#'
#' @export
#' @param x number of ids to generate
#' @return unique random string, consisting of letters and numbers
GenRndId <- function(x)
{
  max.val = x*1000
  count <- nchar(as.character(max.val))                       # find out how many 'numbers' each ID will have after the letter
  size <- paste("%0",count,"d",sep="")                        # set the variable to be fed into 'sprintf' to ensure we have leading 0's
  lets <- toupper(sample(letters,x, replace=T))               # randomising the letters
  nums <- sprintf(size,sample(1:max.val)[1:x])                # randominsing the numbers, and ensuing they all have the same number of characters
  ids <- paste(lets,nums,sep="")                              # joining them together
  return(ids)
}
