#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
# shinyServer(function(input, output) {
#
#   output$distPlot <- renderPlot({
#
#     # generate bins based on input$bins from ui.R
#     x    <- faithful[, 2]
#     bins <- seq(min(x), max(x), length.out = input$bins + 1)
#
#     # draw the histogram with the specified number of bins
#     hist(x, breaks = bins, col = 'darkgray', border = 'white')
#
#   })
#
# })

# get the names of graphs of the zoo list
gnames <- GetFileName(paths = zoo$files)
update.icu <<- TRUE
update.icm <<- TRUE
update.ip <<- TRUE


cfgs <- new.env(parent = emptyenv())
cfgs$icfg <- list()
ocfg <- list()
cfgs$ecfg <- list()
cfgs$dcfg <- list()
cfgs$tcfg <- list()

shiny::shinyServer(function(input, output, session) {

  #' Thread handlers
  ghandle <<- subprocess::spawn_process(R_binary(), c('--no-save'))
  shandle <<- subprocess::spawn_process(R_binary(), c('--no-save'))
  phandle <<- subprocess::spawn_process(R_binary(), c('--no-save'))
  plhandle <<- subprocess::spawn_process(R_binary(), c('--no-save'))


  update.capacity <<- shiny::eventReactive(input$update.live, c(IcnUnicast = input$update.live, IcnMulticast = input$update.live, Ip = input$update.live))
  update.pl <<- shiny::eventReactive(input$update.pl, c(IcnUnicast = input$update.pl, Ip = input$update.pl))
  update.mcgain <<- shiny::eventReactive(input$update.mcgain, update.mcgain)

  autoInvalidate <<- shiny::eventReactive(input$update.freq, reactiveTimer(input$update.freq * 1000))
  autoInvalidatePl <<- shiny::eventReactive(input$update.freq.pl, reactiveTimer(input$update.freq.pl * 1000))
  autoInvalidateMcGain <<- shiny::eventReactive(input$update.freq.mcgain, reactiveTimer(input$update.freq.mcgain * 1000))


  # set the size of graph plot
  gplotWidth <- 800
  gplotHeight <- 800


# Capacity Data -----------------------------------------------------------

  DataIcnUnicast <- function(cfgs, session)
  {
    logging::loginfo('Data: ICN UC')
    #' define plot container
    data <- data.frame()

    if(update.capacity[input$scn1] | update.capacity[input$scn2])
      autoInvalidate()

    presdir <- file.path(cfgs$paths['result'], cfgs$icfg$expr, 'processed/', fsep = '')
    dir.create(presdir, showWarnings = FALSE)
    presfiles <- ConstructFullFileName(cfgs = cfgs, obj = 'icn.uc', dir = presdir)

    sim.exist <- which(file.access(presfiles, mode = 4) == 0)
    logging::logdebug('Data: %s/%s existing files', length(sim.exist), length(presfiles))
    if(length(sim.exist) == 0)
      return(data.frame(x = 0, y = 0, t = 0, r = 0, var1 = 0, var2 = 0, var3 = 0,
                        var4 = 0, var5 = 0, var6 = 0, var7 = 0, var8 = 'unicast'))
    for(j in sim.exist) {
      logging::logdebug('Data: %s', presfiles[j])
      loaded <- ""
      while(loaded != 'icn.uc')
        loaded <- tryCatch(load(presfiles[j]), error = function(e){logging::logerror('PLOT: could not load file: %s', presfiles[j]); return("")})

      colnames(icn.uc)[1:4] <- c('x', 'y', 't', 'r')
      colnames(icn.uc)[5:ncol(icn.uc)] <- paste0('var', c(1:{ncol(icn.uc) - 4}))
      data <- rbind(data, icn.uc)
    }
    if(length(sim.exist) == length(presfiles))
      update.capacity['IcnUnicast'] <<- FALSE
    logging::logdebug('Data: complete, update: %s', update.capacity['IcnUnicast'])
    data
  }

  DataIcnMulticast <- function(cfgs, session)
  {
    logging::loginfo('Data: ICN MC')
    #' define plot container
    data <- data.frame()

    if(update.capacity[input$scn1] | update.capacity[input$scn2])
      autoInvalidate()

    presdir <- file.path(cfgs$paths['result'], cfgs$icfg$expr, 'processed/', fsep = '')
    dir.create(presdir, showWarnings = FALSE)
    presfiles <- ConstructFullFileName(cfgs = cfgs, obj = 'icn.mc', dir = presdir)

    sim.exist <- which(file.access(presfiles, mode = 4) == 0)
    logging::logdebug('Data: %s/%s existing files', length(sim.exist), length(presfiles))
    if(length(sim.exist) == 0)
      return(data.frame(x = 0, y = 0, t = 0, r = 0, var1 = 0, var2 = 0, var3 = 0,
                        var4 = 0, var5 = 0, var6 = 0, var7 = 0, var8 = 'multicast'))
    for(j in sim.exist) {
      logging::logdebug('Data: %s', presfiles[j])
      loaded <- ""
      while(loaded != 'icn.mc')
        loaded <- tryCatch(load(presfiles[j]), error = function(e){logging::logerror('PLOT: could not load file: %s', presfiles[j]); return("")})

      colnames(icn.mc)[1:4] <- c('x', 'y', 't', 'r')
      colnames(icn.mc)[5:ncol(icn.mc)] <- paste0('var', c(1:{ncol(icn.mc) - 4}))
      data <- rbind(data, icn.mc)
    }
    if(length(sim.exist) == length(presfiles))
      update.capacity['IcnMulticast'] <<- FALSE
    logging::logdebug('Data: complete, update: %s', update.capacity['IcnMulticast'])
    data
  }

  DataIp <- function(cfgs, session)
  {
    logging::loginfo('Data: IP Unicast')
    #' define plot container
    data <- data.frame()

    if(update.capacity[input$scn1] | update.capacity[input$scn2])
      autoInvalidate()

    presdir <- file.path(cfgs$paths['result'], cfgs$icfg$expr, 'processed/', fsep = '')
    dir.create(presdir, showWarnings = FALSE)
    presfiles <- ConstructFullFileName(cfgs = cfgs, obj = 'ip', dir = presdir)

    sim.exist <- which(file.access(presfiles, mode = 4) == 0)
    logging::logdebug('Data: %s/%s existing files', length(sim.exist), length(presfiles))
    if(length(sim.exist) == 0) {
      logging::logdebug('Data: %s', presfiles[1])
      return(data.frame(x = 0, y = 0, t = 0, r = 0, var1 = 0, var2 = 0, var3 = 0,
                        var4 = 0, var5 = 0, var6 = 0, var7 = 0, var8 = 'ip'))
    }
    for(j in sim.exist) {
      logging::logdebug('Data: %s', presfiles[j])
      loaded <- ""
      while(loaded != 'ip')
        loaded <- tryCatch(load(presfiles[j]), error = function(e){logging::logerror('PLOT: could not load file: %s', presfiles[j]); return("")})

      colnames(ip)[1:4] <- c('x', 'y', 't', 'r')
      colnames(ip)[5:ncol(ip)] <- paste0('var', c(1:{ncol(ip) - 4}))
      data <- rbind(data, ip)
    }
    if(length(sim.exist) == length(presfiles))
      update.capacity['Ip'] <<- FALSE
    logging::logdebug('Data: complete, update: %s', update.capacity['Ip'])
    data
  }


# Path Length -------------------------------------------------------------

  PathLengthIcnUnicast <- function(cfgs, session)
  {
    logging::loginfo('Data: ICN UC PL')
    #' define plot container
    data <- data.frame()

    if(update.pl[input$plscn1] | update.pl[input$plscn2])
      autoInvalidatePl()

    presdir <- file.path(cfgs$paths['result'], cfgs$icfg$expr, 'processed/', fsep = '')
    dir.create(presdir, showWarnings = FALSE)
    presfiles <- ConstructFullFileName(cfgs = cfgs, obj = 'icn.upl', dir = presdir)

    sim.exist <- which(file.access(presfiles, mode = 4) == 0)
    logging::logdebug('Data: filename %s', presfiles[1])

    logging::logdebug('Data: %s/%s existing files', length(sim.exist), length(presfiles))
    if(length(sim.exist) == 0)
      return(data.frame(x = 0, y = 0, var1 = 0, var2 = 0, var3 = 0,
                        var4 = 0, var5 = 0, var6 = 0, var7 = 0, var8 = 0, var9 = 'unicast'))
    for(j in sim.exist) {
      logging::logdebug('Data: %s', presfiles[j])
      loaded <- ""
      while(loaded != 'icn.upl')
        loaded <- tryCatch(load(presfiles[j]), error = function(e){logging::logerror('PLOT: could not load file: %s', presfiles[j]); return("")})

      colnames(icn.upl)[1:2] <- c('x', 'y')
      colnames(icn.upl)[3:ncol(icn.upl)] <- paste0('var', c(1:{ncol(icn.upl) - 2}))
      data <- rbind(data, icn.upl)
    }
    if(length(sim.exist) == length(presfiles))
      update.pl['IcnUnicast'] <<- FALSE
    logging::logdebug('Data: complete, update: %s', update.pl['IcnUnicast'])
    data
  }

  PathLengthIp <- function(cfgs, session)
  {
    logging::loginfo('Data: IP Unicast')
    #' define plot container
    data <- data.frame()
    logging::logdebug('input: %s, %s', update.pl[input$plscn1], update.pl[input$plscn2])
    if(update.pl[input$plscn1] | update.pl[input$plscn2])
      autoInvalidatePl()

    presdir <- file.path(cfgs$paths['result'], cfgs$icfg$expr, 'processed/', fsep = '')
    dir.create(presdir, showWarnings = FALSE)
    presfiles <- ConstructFullFileName(cfgs = cfgs, obj = 'ip.pl', dir = presdir)

    sim.exist <- which(file.access(presfiles, mode = 4) == 0)
    logging::logdebug('Data: %s/%s existing files', length(sim.exist), length(presfiles))
    if(length(sim.exist) == 0) {
      logging::logdebug('Data: %s', presfiles[1])
      return(data.frame(x = 0, y = 0, var1 = 0, var2 = 0, var3 = 0,
                        var4 = 0, var5 = 0, var6 = 0, var7 = 0, var8 = 0, var9 = 'ip'))
    }
    for(j in sim.exist) {
      logging::logdebug('Data: %s', presfiles[j])
      loaded <- ""
      while(loaded != 'ip.pl')
        loaded <- tryCatch(load(presfiles[j]), error = function(e){logging::logerror('PLOT: could not load file: %s', presfiles[j]); return("")})

      colnames(ip.pl)[1:2] <- c('x', 'y')
      colnames(ip.pl)[3:ncol(ip.pl)] <- paste0('var', c(1:{ncol(ip.pl) - 2}))
      data <- rbind(data, ip.pl)
    }
    if(length(sim.exist) == length(presfiles))
      update.pl['Ip'] <<- FALSE
    logging::logdebug('Data: complete, update: %s', update.pl['Ip'])
    data
  }


# Vectors of Functions ----------------------------------------------------

  Simulate <- c(IcnUnicast = ThreadSimulateIcnUnicast, IcnMulticast = ThreadSimulateIcnMulticast, Ip = ThreadSimulateIp)
  Process <- c(IcnUnicast = ThreadProcessIcnUnicastCapacity, IcnMulticast = ThreadProcessIcnMulticastCapacity, Ip = ThreadProcessIpCapacity)
  ProcessPathLength <- c(IcnUnicast = ThreadProcessIcnPathLength, Ip = ThreadProcessIpPathLength)
  Data <- c(IcnUnicast = DataIcnUnicast, IcnMulticast = DataIcnMulticast, Ip = DataIp)
  PathLength <- c(IcnUnicast = PathLengthIcnUnicast, Ip = PathLengthIp)


  graph <- shiny::reactive({
    gset <- which(gnames == input$g.name)
    g <- zoo$graphs[[gset]]
    g$g.name <- input$g.name
    g$eM <- GraphToAdjMatrix(g$g)
    ed <- g$eM[g$eM > 0]
    res <- AlgoDijAllPaths(g$g, ed)
    g$D <- res$D
    g$R <- res$R
    g
  })

  catalogue <- shiny::reactive({
    ParseCatalogue(ui.input = input)
  })

  paths <- shiny::reactive({
    home <- input$home
    store <- file.path(home, 'data/', fsep = '')
    results <- file.path(store, 'results/', fsep = '')
    c(home = home, store = store, result = results)
  })

  cores <- shiny::reactive({
    input$cores
  })

  origins <- shiny::reactive({
    ParseOrigins(ui.input = input)
  })

  surrogates <- shiny::reactive({
    ParseSurrogates(ui.input = input)
  })

  dns <- shiny::reactive({
    ParseDns(ui.input = input)
  })

  perms <- shiny::reactive({
    ParsePermutation(ui.input = input)
  })
  ParseConfigsFromShiny <- shiny::reactive({
    logging::basicConfig('DEBUG')

    cfgs$paths <- paths()
    cfgs$cores <- cores()
    cfgs$icfg <- catalogue()
    cfgs$ocfg <- origins()
    cfgs$ecfg <- surrogates()
    cfgs$dcfg <- dns()
    cfgs$tcfg <- perms()
    cfgs$gcfg <- list()
    cfgs$gcfg$gun.set <- list(graph())
    cfgs$eoexpr <- expand.grid(eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr)
    cfgs$deoexpr <- expand.grid(dexpr = cfgs$dcfg$expr, eexpr = cfgs$ecfg$expr, oexpr = cfgs$ocfg$expr)

    logging::logdebug('Shiny: Input Args changed')
    update.capacity <<- c(IcnUnicast = input$update.live, IcnMulticast = input$update.live, Ip = input$update.live)
    update.pl <<- c(IcnUnicast = input$update.pl, Ip = input$update.pl)
    update.mcgain <<- input$update.mcgain

    autoInvalidate <<- reactiveTimer(input$update.freq * 1000)
    autoInvalidatePl <<- reactiveTimer(input$update.freq.pl * 1000)
    autoInvalidateMcGain <<- reactiveTimer(input$update.freq.mcgain * 1000)

    #' save configurations
    cfg.path <- file.path(cfgs$paths['store'], 'cfgs.RData', fsep = '')
    save(cfgs, file = cfg.path)

    #' update relevant subprocesses
    ThreadGenerate(cfgs, ghandle)
    #' simulate
    Simulate[[input$scn1]](cfgs, shandle)
    Simulate[[input$scn2]](cfgs, shandle)
    if(!(input$plscn1 %in% c(input$scn1, input$scn2)))
      Simulate[[input$plscn1]](cfgs, shandle)
    if(!(input$plscn2 %in% c(input$scn1, input$scn2)))
      Simulate[[input$plscn2]](cfgs, shandle)

    #' process raw results
    Process[[input$scn1]](cfgs, phandle)
    Process[[input$scn2]](cfgs, phandle)

    ProcessPathLength[[input$plscn1]](cfgs, plhandle)
    ProcessPathLength[[input$plscn2]](cfgs, plhandle)
    cfgs
  })

  output$iuc.suma <- shiny::renderTable({print('No Simulation Results yet')})

  output$graph <- networkD3::renderForceNetwork({g <- PlotGraph(graph(), w = gplotWidth, h = gplotHeight)
                                     g$x$nodes$text <- round(graph()$u)
                                     g <- htmlwidgets::onRender(g, 'function(el, x){
                                       d3.selectAll(".node")
                                       .append("title")
                                       .text(function(d) {return d.text; })
                                     }')
                                     return(g)
                                     })
  # simulate results
  shiny::observeEvent(input$scn1, {
    logging::basicConfig('DEBUG')
    cfgs <- ParseConfigsFromShiny()
    update.capacity[input$scn1] <<- TRUE
    update.capacity[input$scn2] <<- TRUE

    #' #' simulate
    #' Simulate[[input$scn1]](cfgs, shandle)
    #' #' process raw results
    #' Process[[input$scn1]](cfgs, phandle)
  })

  shiny::observeEvent(input$scn2, {
    logging::basicConfig('DEBUG')
    cfgs <- ParseConfigsFromShiny()
    update.capacity[input$scn1] <<- TRUE
    update.capacity[input$scn2] <<- TRUE

    #' #' simulate
    #' Simulate[[input$scn2]](cfgs, shandle)
    #' #' process raw results
    #' Process[[input$scn2]](cfgs, phandle)

    # output$iuc.plot <- plotly::renderPlotly({
    #   logging::basicConfig('DEBUG')
    #
    #   data <- plotly::group_by(rbind(Data[[input$scn1]](cfgs, session), Data[[input$scn2]](cfgs, session), stringsAsFactors = FALSE), var8, add = TRUE)
    #   if(update.capacity[input$scn1] | update.capacity[input$scn2])
    #     autoInvalidate()
    #   p <- plotly::plot_ly(data = data, x = ~x, y = ~r, color = ~var8, colors = c('blue', 'red'), type = "box")
    #   # plotly::layout(p, xaxis = 'Number of Surrogates', yaxis = 'Relative Backhaul Required Capacity')
    #   p
    # })
  })

  shiny::observeEvent(input$plscn1, {
    logging::basicConfig('DEBUG')
    cfgs <- ParseConfigsFromShiny()
    update.pl[input$plscn1] <<- TRUE
    update.pl[input$plscn2] <<- TRUE
  })

  shiny::observeEvent(input$plscn2, {
    logging::basicConfig('DEBUG')
    cfgs <- ParseConfigsFromShiny()
    update.pl[input$plscn1] <<- TRUE
    update.pl[input$plscn2] <<- TRUE
  })

  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      "capacity_data.csv"
    },
    content = function(file) {
      cfgs <- ParseConfigsFromShiny()
      data <- rbind(Data[[input$scn1]](cfgs, session), Data[[input$scn2]](cfgs, session), stringsAsFactors = FALSE)
      write.csv(data, file, row.names = FALSE)
    }
  )

  output$downloadPl <- shiny::downloadHandler(
    filename = function() {
      "path_length_data.csv"
    },
    content = function(file) {
      cfgs <- ParseConfigsFromShiny()
      data <- rbind(PathLength[[input$plscn1]](cfgs, session), PathLength[[input$plscn2]](cfgs, session), stringsAsFactors = FALSE)
      write.csv(data, file, row.names = FALSE)
    }
  )


  output$iuc.plot <- plotly::renderPlotly({
    logging::basicConfig('DEBUG')

    cfgs <- ParseConfigsFromShiny()

    data <- plotly::group_by(rbind(Data[[input$scn1]](cfgs, session), Data[[input$scn2]](cfgs, session), stringsAsFactors = FALSE), var8, add = TRUE)
    # data1 <- Data[[input$scn1]](cfgs, session)
    # data2 <- Data[[input$scn2]](cfgs, session)
    # if(update.capacity[input$scn1] | update.capacity[input$scn2])
    #   autoInvalidate()
    p <- plotly::plot_ly(data = data, x = ~x, y = ~r, color = ~var8, colors = c('blue', 'red'), type = "box") #
    # plotly::layout(p, xaxis = list(title = 'Number of Surrogates', ticks = ~x), yaxis = list(title = 'Relative Backhaul Required Capacity', ticks = ~r), barmode = 'group')

    # p <- plotly::plot_ly(data = data1, x = ~x, y = ~r, name = ~var8, type = "box") #
    # p <- plotly::add_trace(p, y = data2$r, name = data2$var8)
    plotly::layout(p, xaxis = list(title = 'Number of Surrogates', ticks = ~x), yaxis = list(title = 'Relative Backhaul Required Capacity'), boxmode = 'group')
  })

  output$pl.plot <- plotly::renderPlotly({
    logging::basicConfig('DEBUG')

    cfgs <- ParseConfigsFromShiny()

    data <- plotly::group_by(rbind(PathLength[[input$plscn1]](cfgs, session), PathLength[[input$plscn2]](cfgs, session), stringsAsFactors = FALSE), var9, add = TRUE)
    p <- plotly::plot_ly(data = data, x = ~x, y = ~y, color = ~var9, colors = c('blue', 'red'), type = "bar") #
    plotly::layout(p, xaxis = list(title = 'Hop Count Per Path', ticks = ~x), yaxis = list(title = 'ECDF', ticks = ~y), barmode = 'group')
  })

  output$mcgain.plot <- plotly::renderPlotly({
    logging::basicConfig('DEBUG')

    cfgs <- ParseConfigsFromShiny()

    uc <- Data[['IcnUnicast']](cfgs, session)
    uc <- uc[rep(seq_len(nrow(uc)), each = length(input$t) * length(input$L)),]
    mc <- Data[['IcnMulticast']](cfgs, session)
    data <- mc
    if(nrow(uc) == nrow(mc)) {
      data$y <- uc$y/mc$y
      data$var7 <- factor(data$var7)
      data$var6 <- factor(data$var6)

      p <- plotly::plot_ly(data = data, x = ~var7, y = ~y, color = ~var6, colors = c('blue', 'red'), type = "bar") #
      plotly::layout(p, xaxis = list(title = 'Stream Length', ticks = ~var7), yaxis = list(title = 'Network Gain', ticks = ~y), barmode = 'group')
    }
  })

  #' terminate and clean up subprocesses
  #'
  #' @description  taken from https://stackoverflow.com/questions/23276491/how-to-implement-a-cleanup-routine-in-r-shiny

  cancel.onSessionEnded <- shiny::onSessionEnded(function() {
    if(subprocess::process_state(ghandle) == 'running') {
      subprocess::process_kill(ghandle)
      subprocess::process_terminate(ghandle)
    }
    if(subprocess::process_state(shandle) == 'running') {
      subprocess::process_kill(shandle)
      subprocess::process_terminate(shandle)

    }
    if(subprocess::process_state(phandle) == 'running') {
      subprocess::process_kill(phandle)
      subprocess::process_terminate(phandle)
    }
  })
  cancel.onSessionEnded()
})


SaveProccessedData <- function(data, name, cfgs)
{
  resdir <- file.path(cfgs$paths['result'], cfgs$icfg$expr, 'proccessed/', fsep = '')
  dir.create(resdir, showWarnings = FALSE)
  #' make sure the expression name includes the simulation number of tests, not the generation number.
  cfgs$tcfg$sim.expr <- paste0('-scale_', cfgs$tcfg$load['lb'], '_', cfgs$tcfg$load['ub'], '_', cfgs$tcfg$load['st'], '-tests_', cfgs$tcfg$sim.tests)
  filename <- file.path(resdir, name, '-g_', cfgs$gcfg$gun.set[[1]]$g.name, '-o_', cfgs$eoexpr$oexpr, '-e_', cfgs$eoexpr$eexpr, cfgs$tcfg$sim.expr, '.RData', fsep = '' )
  save(data, file = filename)
}

# Generate Thread ---------------------------------------------------------

ThreadGenerate <- function(cfgs, handle)
{
  if(missing(handle))
    handle <- subprocess::spawn_process(R_binary(), c('--no-save'))
  subprocess::process_write(handle, 'library(ice); logging::basicConfig("DEBUG")\n')
  subprocess::process_write(handle = handle, paste0( 'load("', file.path(cfgs$paths['store'], 'cfgs.RData', fsep = ''), '")\n'))
  subprocess::process_write(handle = handle, 'GenSimData(cfgs = cfgs)\n') #; quit(save = "no")
  assign('ghandle', handle, envir = .GlobalEnv)
}

# Sim Threads -------------------------------------------------------------

ThreadSimulateIcnUnicast <- function(cfgs, handle)
{
  if(missing(handle))
    handle <- subprocess::spawn_process(R_binary(), c('--no-save'))
  subprocess::process_write(handle, 'library(ice); logging::basicConfig("DEBUG")\n')
  subprocess::process_write(handle = handle, paste0( 'load("', file.path(cfgs$paths['store'], 'cfgs.RData', fsep = ''), '")\n'))
  subprocess::process_write(handle = handle, 'GSimIcnUnicast(cfgs = cfgs)\n') # ; quit(save = "no")
  assign('shandle', handle, envir = .GlobalEnv)
}
ThreadSimulateIcnMulticast <- function(cfgs, handle)
{
  if(missing(handle))
    handle <- subprocess::spawn_process(R_binary(), c('--no-save'))
  subprocess::process_write(handle, 'library(ice); logging::basicConfig("DEBUG")\n')
  subprocess::process_write(handle = handle, paste0( 'load("', file.path(cfgs$paths['store'], 'cfgs.RData', fsep = ''), '")\n'))
  subprocess::process_write(handle = handle, 'GSimIcnMulticast(cfgs = cfgs)\n') # ; quit(save = "no")
  assign('shandle', handle, envir = .GlobalEnv)
}

ThreadSimulateIp <- function(cfgs, handle)
{
  if(missing(handle))
    handle <- subprocess::spawn_process(R_binary(), c('--no-save'))
  subprocess::process_write(handle, 'library(ice); logging::basicConfig("DEBUG")\n')
  subprocess::process_write(handle = handle, paste0( 'load("', file.path(cfgs$paths['store'], 'cfgs.RData', fsep = ''), '")\n'))
  subprocess::process_write(handle = handle, 'GSimIp(cfgs = cfgs)\n') # ; quit(save = "no")
  assign('shandle', handle, envir = .GlobalEnv)
}


# Proc Threads ------------------------------------------------------------

ThreadProcessIcnUnicastCapacity <- function(cfgs, handle)
{
  if(missing(handle))
    handle <- subprocess::spawn_process(R_binary(), c('--no-save'))
  subprocess::process_write(handle, 'library(ice); logging::basicConfig("DEBUG")\n')
  subprocess::process_write(handle = handle, paste0( 'load("', file.path(cfgs$paths['store'], 'cfgs.RData', fsep = ''), '")\n'))
  subprocess::process_write(handle = handle, 'ProcIcnUnicastCapacityOfOneCatalogue(cfgs = cfgs)\n')
  assign('phandle', handle, envir = .GlobalEnv)
}
ThreadProcessIcnMulticastCapacity <- function(cfgs, handle)
{
  if(missing(handle))
    handle <- subprocess::spawn_process(R_binary(), c('--no-save'))
  subprocess::process_write(handle, 'library(ice); logging::basicConfig("DEBUG")\n')
  subprocess::process_write(handle = handle, paste0( 'load("', file.path(cfgs$paths['store'], 'cfgs.RData', fsep = ''), '")\n'))
  subprocess::process_write(handle = handle, 'ProcIcnMulticastCapacityOfOneCatalogue(cfgs = cfgs)\n')
  assign('phandle', handle, envir = .GlobalEnv)
}
ThreadProcessIpCapacity <- function(cfgs, handle)
{
  if(missing(handle))
    handle <- subprocess::spawn_process(R_binary(), c('--no-save'))
  subprocess::process_write(handle, 'library(ice); logging::basicConfig("DEBUG")\n')
  subprocess::process_write(handle = handle, paste0( 'load("', file.path(cfgs$paths['store'], 'cfgs.RData', fsep = ''), '")\n'))
  subprocess::process_write(handle = handle, 'ProcIpCapacityOfOneCatalogue(cfgs = cfgs)\n')
  assign('phandle', handle, envir = .GlobalEnv)
}

ThreadProcessIcnPathLength <- function(cfgs, handle)
{
  if(missing(handle))
    handle <- subprocess::spawn_process(R_binary(), c('--no-save'))
  subprocess::process_write(handle, 'library(ice); logging::basicConfig("DEBUG")\n')
  subprocess::process_write(handle = handle, paste0( 'load("', file.path(cfgs$paths['store'], 'cfgs.RData', fsep = ''), '")\n'))
  subprocess::process_write(handle = handle, 'ProcIcnUnicastPathLengthOfOneCatalogue(cfgs = cfgs)\n')
  assign('plhandle', handle, envir = .GlobalEnv)
}

ThreadProcessIpPathLength <- function(cfgs, handle)
{
  if(missing(handle))
    handle <- subprocess::spawn_process(R_binary(), c('--no-save'))
  subprocess::process_write(handle, 'library(ice); logging::basicConfig("DEBUG")\n')
  subprocess::process_write(handle = handle, paste0( 'load("', file.path(cfgs$paths['store'], 'cfgs.RData', fsep = ''), '")\n'))
  subprocess::process_write(handle = handle, 'ProcIpPathLengthOfOneCatalogue(cfgs = cfgs)\n')
  assign('plhandle', handle, envir = .GlobalEnv)
}


#' function taken from https://cran.r-project.org/web/packages/subprocess/vignettes/intro.html
is_windows <- function () (tolower(.Platform$OS.type) == "windows")

#' function taken from https://cran.r-project.org/web/packages/subprocess/vignettes/intro.html
R_binary <- function () {
  R_exe <- ifelse (is_windows(), "R.exe", "R")
  return(file.path(R.home("bin"), R_exe))
}


testThreads <- function()
{
  load('~/Documents/point-evaluation-tools/data/cfgs.RData')
  ghandle <- ThreadGenerate(cfgs = cfgs)
  subprocess::process_read(ghandle, timeout = 5)
  logging::logdebug('TEST: Gen Thrd')

  shandle <- ThreadSimulateIcnUnicast(cfgs)
  print(subprocess::process_read(shandle, timeout = 5))

  phandle <- ThreadProcessIcnUnicastCapacity(cfgs)
  plhandle <- ThreadProcessIpPathLength(cfgs)


  print(subprocess::process_read(phandle, timeout = 5))

  list(g = ghandle, s = shandle, p = phandle, pl = plhandle) #
}
