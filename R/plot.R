#' plot network graph
#' @description plot interactive network graph with networkD3 package
#'
#' @export
#' @param g graph object including population per node
#' @param w character vector for width of the plot area
#' @param h hight of the plot area
PlotGraph <- function(g, w = 500, h = 500)
{
  # calcualte the size of nodes based on relative node population to the largest population
  largest.population <- max(g$u)
  population <- g$u/largest.population

  # extract links and adjust links' indexes
  links <- data.frame(t(graph::edgeMatrix(g$g)))
  links$from <- links$from -1
  links$to <- links$to - 1
  # add link value column to define link width
  links$value <- 1
  # extract nodes
  nodes <- data.frame(nodes = graph::nodes(g$g), role = c(1), size = population)
  # plot the graph

  d3g <- networkD3::forceNetwork(Links = links, Nodes = nodes,
                          Source = "from", Target = "to",
                          Value = "value", NodeID = "nodes",
                          Group = "role", opacity = 0.9,
                          Nodesize = "size",
                          radiusCalculation = networkD3::JS(" d.nodesize * 30"),
                          linkWidth = networkD3::JS("function(d) { return Math.sqrt(d.value); }"),
                          width = w,
                          height = h,
                          bounded = TRUE,
                          fontSize = 12,
                          charge = -50,
                          legend = TRUE
                          )
}

#' plot a boxplot
#'
#' @export
#' @param data dataframe with colnames: x, y, t, r, var1, ..., varn. x, y are essential columns, the rest are optional, t is the reference data
#' @param group character vector, giving the set of columns (in colnames) that are used for grouping
#' @param dims named numeric vector in the format (h = , w = ) giving the plot dimensions
#' @param xlabel character vector giving x-axis label
#' @param ylabel character vector giving y-axis label
#' @param glabel character vector giving group label
#' @param ... theme parameters as defined by ggplot2 theme
PlotGroupedBoxPlots <- function(data, mapping, dims = c(h=4.75, w=7), xlabel = '',
                                ylabel = '', glabel = '', notch = FALSE, ...)
{
  args <- list(...)

  ggplot2::ggplot(data, mapping) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_bw() +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab(ylabel)
}
#' Black theme
#' @description Customized black theme for plotting results figures of ICE. taken from https://gist.github.com/jslefche/eff85ef06b4705e6efbc
#'
#' @export
#' @param base_size font size
#' @param base_family font family
theme_black <- function(base_size = 12, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +

    ggplot2::theme(
      # Specify axis options
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
      axis.text.y = ggplot2::element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
      axis.ticks = ggplot2::element_line(color = "white", size  =  0.2),
      axis.title.x = ggplot2::element_text(size = base_size, color = "white", margin = ggplot2::margin(0, 10, 0, 0)),
      axis.title.y = ggplot2::element_text(size = base_size, color = "white", angle = 90, margin = ggplot2::margin(0, 10, 0, 0)),
      axis.ticks.length = ggplot2::unit(0.3, "lines"),
      # Specify legend options
      legend.background = ggplot2::element_rect(color = NA, fill = "black"),
      legend.key = ggplot2::element_rect(color = "white",  fill = "black"),
      legend.key.size = ggplot2::unit(1.2, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = ggplot2::element_text(size = base_size*0.8, color = "white"),
      legend.title = ggplot2::element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),
      legend.position = "right",
      legend.text.align = NULL,
      legend.title.align = NULL,
      legend.direction = "vertical",
      legend.box = NULL,
      # Specify panel options
      panel.background = ggplot2::element_rect(fill = "black", color  =  NA),
      panel.border = ggplot2::element_rect(fill = NA, color = "white"),
      panel.grid.major = ggplot2::element_line(color = "grey35"),
      panel.grid.minor = ggplot2::element_line(color = "grey20"),
      panel.margin = ggplot2::unit(0.5, "lines"),
      # Specify facetting options
      strip.background = ggplot2::element_rect(fill = "grey30", color = "grey10"),
      strip.text.x = ggplot2::element_text(size = base_size*0.8, color = "white"),
      strip.text.y = ggplot2::element_text(size = base_size*0.8, color = "white",angle = -90),
      # Specify plot options
      plot.background = ggplot2::element_rect(color = "black", fill = "black"),
      plot.title = ggplot2::element_text(size = base_size*1.2, color = "white"),
      plot.margin = ggplot2::unit(rep(1, 4), "lines")

    )
}
