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
scatterPlots <- function(df = "dataframe", groupName = NULL, mColors = NULL){ 
  if(!is.data.frame(df)){
    stop("Please include a dataframe object.")  
  }
  if (missing("groupName")) {
    groupName <- "Source"
  } 
  if (missing("mColors")) {
    myColors <- readRDS("Data/Colors.Rds")
    mColors <-  myColors$Hex[1:length(unique(df$Source))]
  } 
  # load packages
  myPackages <- c("ggplot2", "grid","gridExtra")
  usePackage <- function(p) 
  {
    if (!is.element(p, installed.packages()[,1]))
      install.packages(p, dep = TRUE)
    lapply(p, function(x) suppressMessages(require(x, character.only = TRUE,quietly=TRUE,warn.conflicts = FALSE)))
  }
  lapply(myPackages, usePackage)
  
  # get all plots into a list
  options(warn = -1) # turn off warnings temporarily
  for(i in 7:11){
    for(j in 7:11){
      if(!identical(i,j)) {
        ratio.display <- 4/3
        ratio.values <- (max(df[,i])-min(df[,i]))/
          (max(df[sources,j])-min(df[sources,j]))
        p <- ggplot() +
          geom_point(data = df[artifacts,], aes(x = df[artifacts,i],
                                                y = df[artifacts,j],
                                                color = df$Source[artifacts])) +
          coord_fixed(ratio.values/ratio.display) +
          xlab(names(df)[i]) +
          ylab(names(df)[j]) +
          theme_minimal() +
          theme(legend.title=element_blank()) +
          scale_color_manual(values = mColors) + # used for manual colors
          stat_ellipse(data = df[sources,], aes(x = df[sources,i],
                                                y = df[sources,j],
                                                color = df$Source[sources]),
                       type = "norm",
                       level = .9,
                       lwd = 1) # this ellipse is based off the multivariate normal distribution
      assign(paste0("p",i,j),p)
      if(exists("pNames")) {
        pNames <- c(pNames,paste0("p",i,j))
      } else {
        pNames <- paste0("p",i,j)
      }
      }
    }
  }
  options(warn = 0)
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
