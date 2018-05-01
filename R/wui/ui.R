#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ice)

# get the names of graphs of the zoo list
gnames <- GetFileName(paths = zoo$files)

# Define UI for application that draws a histogram
fluidPage(
  # Theme
  theme = shinythemes::shinytheme("cyborg"),

  # Application title
  shiny::titlePanel("ICN Evaluator (ICE)"),

  # Sidebar with a slider input for number of bins
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::wellPanel(
        shiny::h5("Network"),
        shiny::selectInput(inputId = "g.name" , label = "Graph", choices = gnames),
        # Augumentation choices
        # shiny::radioButtons("augument", "Augumentation",
        # choices = c(None = "NULL",
        # Tree = "Tree",
        # Dag = "Dag"),
        # selected = NULL)
        value = 'gcfg'
      ),
      shiny::wellPanel(
        shiny::h5("Catalogue"),
        shiny::sliderInput("size", "Size:",
                           min = 100, max = 10000, value = 500, step = 100),
        shiny::sliderInput('v', 'Item Volume/Bitrate:',
                           min = 0.01, max = 0.1, value = c(0.02, 0.06), step = 0.02),
        shiny::radioButtons("d", 'Popularity Distribution: ',
                            choices = c(Zipf = 'zipf'), selected = 'zipf'),
        shiny::sliderInput('s', 'Exponent:',
                           min = 0.7, max = 2, value = 0.81, step = 0.1),
        shiny::sliderInput('ig', 'Ignore (top of distribution):',
                           min = 0.0, max = 0.2, value = 0, step = 0.05),
        value = 'icfg'
      ),
      shiny::wellPanel(shiny::h5("Service Points"),
                       shiny::tabsetPanel(
                         shiny::tabPanel("Origins",
                                         shiny::sliderInput('ok', 'Number of origins:',
                                                            min = 2, max = 32, value = c(2, 4), step = 2),
                                         shiny::selectInput("oopt", 'Selection Algorithm: ',
                                                            choices = c(Largest = 1, Closest = 2, Swing = 3), selected = 1, multiple=TRUE, selectize = FALSE),
                                         value = "ocfg"
                         ),
                         shiny::tabPanel("Edges",
                                         shiny::sliderInput('ek', 'Number of edges:',
                                                            min = 2, max = 32, value = c(2,4), step = 2),
                                         shiny::selectInput("eopt", 'Selection Algorithm: ',
                                                            choices = c(Largest = 1, Closest = 2, Swing = 3), selected = 1, multiple=TRUE, selectize = FALSE),
                                         value = "ecfg"
                         ),
                         shiny::tabPanel("DNSs",
                                         shiny::sliderInput('dk', 'Number of LDNSs:',
                                                            min = 2, max = 32, value = c(2,4), step = 2),
                                         shiny::selectInput("dopt", 'Selection Algorithm: ',
                                                            choices = c(Largest = 1, Closest = 2, Swing = 3), selected = 1, multiple=TRUE, selectize = FALSE),
                                         value = "dcfg"
                         )
                       )
      ),
      shiny::wellPanel(shiny::h5("Model"),
                       shiny::sliderInput("load", "Load:",
                                   min = 0.1, max = 1, value = c(0.4, 0.4), step = 0.1),
                       shiny::sliderInput("tests", "Generation Tests:",
                                   min = 1, max = 50, value = 10, step = 1),
                       shiny::sliderInput("sim.tests", "Simulation Tests:",
                                   min = 1, max = 50, value = 5, step = 1),
                       value = 'tcfg'
      ),
      shiny::wellPanel( shiny::h5("Others"),
        shiny::textInput('home', "Home Directory" , value = "~/Documents/point-evaluation-tools/", placeholder = "~/ice/"),
        shiny::numericInput('cores', 'Number of Cores', value = 2, min = 1, step = 1)
      ),
      width = 4
    ),
    # Show a plot of the network graph
    mainPanel(
      shiny::tabsetPanel(
          shiny::tabPanel("Network", networkD3::forceNetworkOutput("graph"),
                          shiny::tableOutput('data')),
          shiny::tabPanel("Backhaul", shiny::fluidRow(
            shiny::column(11, plotly::plotlyOutput("iuc.plot")),
            shiny::column(4, shiny::checkboxInput('update.live', 'Live Update', value = TRUE),
                          shiny::sliderInput('update.freq', 'Update Frequency :',
                                             min = 1, max = 5, value = 2, step = 1)),
            shiny::column(4, offset = 1,
                          shiny::selectInput('scn1', "Scenario",
                                             choices = c('ICN Unicast' = 'IcnUnicast', 'ICN Multicast' = 'IcnMulticast', 'IP DNS' = 'Ip'),
                                             selected = 'IcnUnicast'),
                          shiny::selectInput('scn2', "Scenario",
                                             choices = c('ICN Unicast' = 'IcnUnicast', 'ICN Multicast' = 'IcnMulticast', 'IP DNS' = 'Ip'),
                                             selected = 'IcnMulticast' )),
            shiny::column(4, shiny::downloadButton('downloadData', 'Download Results')),
            # shiny::column(4, shiny::actionButton('update.graph', 'Update Results')),
            value = 'bh')
          ),
          shiny::tabPanel("Path Length", shiny::fluidRow(
            shiny::column(11, plotly::plotlyOutput("pl.plot")),
            shiny::column(4, shiny::checkboxInput('update.pl', 'Live Update', value = TRUE),
                          shiny::sliderInput('update.freq.pl', 'Update Frequency :',
                                             min = 1, max = 5, value = 2, step = 1)),
            shiny::column(4, offset = 1,
                          shiny::selectInput('plscn1', "Scenario",
                                             choices = c('ICN Unicast' = 'IcnUnicast', 'IP DNS' = 'Ip'),
                                             selected = 'IcnUnicast'),
                          shiny::selectInput('plscn2', "Scenario",
                                             choices = c('ICN Unicast' = 'IcnUnicast', 'IP DNS' = 'Ip'),
                                             selected = 'Ip' )),
            shiny::column(4, shiny::downloadButton('downloadPl', 'Download Results')),
            value = 'pl')
            ),
          shiny::tabPanel('Network Mutlicast Gain', shiny::fluidRow(
            shiny::column(11, plotly::plotlyOutput("mcgain.plot")),
            shiny::column(4, shiny::checkboxInput('update.mcgain', 'Live Update', value = TRUE),
                          shiny::sliderInput('update.freq.mcgain', 'Update Frequency :',
                                             min = 1, max = 5, value = 2, step = 1)),
            shiny::column(4, offset = 1, shiny::selectInput("t", 'Catchment Interval (sec):',
                                                            choices = c(0.1, 1.0, 5.0), selected = 0.1, multiple=TRUE, selectize = FALSE),
                          shiny::selectInput("L", 'Stream Length (sec):',
                                             choices = c(900, 1800, 3600), selected = 900, multiple=TRUE, selectize = FALSE)),
            value = 'mcgain'
          ))
      )
    )
  )
)
