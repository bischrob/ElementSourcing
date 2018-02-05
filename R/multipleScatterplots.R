################################################################################
#' function to create scatterplots for all five obsidian elements in one display
#' input is a data frame formatted with ID, the 10 elements from the Bruker
#' obsidan calibration, followed by the columns "Source" (with name of source
#' or project) and "Type" with either "Artifact" or "Source" in the column.
#' Other columns after these are allowed.
#' Author: Robert J. Bischoff
#' Date Created: 2.2.2018
#' Last Updated: 2.5.2018
#' Contact: bischrob@gmail.com

################################################################################
# Function
scatterPlots <- function(df = "dataframe", groupName = NULL){ 
  if(!is.data.frame(df)){
    stop("Please include a dataframe object.")  
  }
  if (missing(groupName)) {
    groupName <- "Source"
  } 
  # load packages
  myPackages <- c("ggplot2", "gridExtra")
  usePackage <- function(p) 
  {
    if (!is.element(p, installed.packages()[,1]))
      install.packages(p, dep = TRUE)
    lapply(p, function(x) suppressMessages(require(x, character.only = TRUE,quietly=TRUE,warn.conflicts = FALSE)))
  }
  lapply(myPackages, usePackage)
  
  # get all plots into a list
  options(warn = -1) # turn off warnings temporarily
  pNames <- ""
    for(i in 1:10){
      g <- ggplot(df, aes_string(x = names(df[7]), y = names(df[8]), color = groupName)) +
        geom_point() + theme_bw() + theme(legend.title=element_blank())
      assign(paste0("p",i), g)
      pNames <- c(pNames,paste0("p",i))
    }
    pNames <- pNames[2:11]
    mPlots <- lapply(pNames,FUN = function(x) get(x))

  
  # create a function to create one shared plot
  grid_arrange_shared_legend <- function(plots) {
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
  g <- grid_arrange_shared_legend(mPlots)

    return(g)
}
