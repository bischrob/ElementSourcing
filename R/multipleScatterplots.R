################################################################################
#' function to create scatterplots for all five obsidian elements in one display
#' input is a data frame formatted with ID, the 10 elements from the Bruker
#' obsidan calibration, followed by the columns "Source" (with name of source
#' or project) and "Type" with either "Artifact" or "Source" in the column.
#' Other columns after these are allowed.
#' Author: Robert J. Bischoff
#' Date Created: 2.2.2018
#' Last Updated: 2.2.2018
#' Contact: bischrob@gmail.com

################################################################################
# Function
scatterPlots <- function(x = "data frame", groupn = NULL){ 
  if (is.null(groupn)) groupn <- "Source"
  my.packages <- c("ggplot2", "GGally", "grid","gridExtra")
  usePackage <- function(p) 
  {
    if (!is.element(p, installed.packages()[,1]))
      install.packages(p, dep = TRUE)
    require(p, character.only = TRUE)
  }
  lapply(my.packages, usePackage)
  
  pm <- ggpairs(x, ggplot2::aes_(color = x[,groupn]), columns = 7:11)

    # create a function to create one shared plot
  grid_arrange_shared_legend <- function(...) {
    plots <- list(...)
    g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs 
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    grid.arrange(
      do.call(arrangeGrob, lapply(plots, function(x)
        x + theme(legend.position="none"))),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight))
  }
  p1 <- getPlot(pm, 2, 1)
  p2 <- getPlot(pm, 3, 1)
  p3 <- getPlot(pm, 3, 2)
  p4 <- getPlot(pm, 4, 1)
  p5 <- getPlot(pm, 4, 2)
  p6 <- getPlot(pm, 4, 3)
  p7 <- getPlot(pm, 5, 1)
  p8 <- getPlot(pm, 5, 2)
  p9 <- getPlot(pm, 5, 3)
  p10<- getPlot(pm, 5, 4)
  g <- grid_arrange_shared_legend(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)

    return(g)
}


