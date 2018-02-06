#' This script is a function to create and save PCA plots
#' comparing sourcing data for obsidian.
#' Original Created: 2.6.2018
#' Latest version: 2.6.2018
#' Author: Robert J. Bischoff
#' Dependent packages: ggbiplot

###################################################################################

plotPCA <- function(df,mColors, showSources = F, onlySources = F){
  if(showSources == F & onlySources == T){
    stop("Cannot choose showSources = F and onlySources = T")
  }
  stopifnot(is.data.frame(df))
  artifacts <- which(df$Type == "Artifact")
  sources <- c(which(df$Type == "Source"),which(df$Type == "Source Flake"))
  if(missing(mColors)){
    myColors <- readRDS("Data/Colors.Rds")
    mColors <-  myColors$Hex[1:length(unique(df$Source))]
  }
  options(warn = -1)
  suppressMessages(library(ggbiplot))
  options(warn = -1)
  dName <- unlist(strsplit(as.character(Sys.time())," ")) # Gives directory today's date
  dName <- paste0("Figures/Plots--", dName[1])
  if (dir.exists(dName) == F) dir.create(dName)
  
  # PCA
  oPCAAll <- prcomp(df[,7:11], center = T, scale. = T)
  oPCAArtifacts <- prcomp(df[artifacts,7:11], center = T, scale. = T)
  oPCASources <- prcomp(df[sources,7:11], center = T, scale. = T)
  summary(oPCASources)

  # plot PCA
  if (showSources == F){
  g <- ggbiplot(oPCAArtifacts, obs.scale = 1, groups = df$Source[artifacts],
                ellipse = F, size = 2) +
    scale_color_discrete(name = '') +
    theme(legend.direction = 'horizontal', 
          legend.position = 'top',
          legend.title=element_blank()) +
    scale_color_manual(values = mColors) + 
    theme_bw()
  g
  ggsave(filename = paste0(dName,"/PCA Artifacts.png"), 
        dpi = 300, plot = g, width = 6.5, units = "in")  
  } else if (onlySources == T) {
  g <- ggbiplot(oPCASources, obs.scale = .1, groups = df$Source[sources],
              ellipse = F, size = 1, ellipse.prob = .9) +
        theme(legend.title = element_blank()) +
        scale_color_manual(values = mColors) + 
        theme_bw() 
  g
  ggsave(filename = paste0(dName,"/PCA Sources and Artifacts.png"), 
       dpi = 300, plot = g, width = 6.5, units = "in")
  } else {
    g <- ggbiplot(oPCAAll, obs.scale = .1, groups = df$Source,
                  ellipse = F, size = 1, ellipse.prob = .9) +
      theme(legend.title = element_blank()) +
      scale_color_manual(values = mColors) + 
      theme_bw() 
    g
    ggsave(filename = paste0(dName,"/PCA Sources and Artifacts.png"), 
           dpi = 300, plot = g, width = 6.5, units = "in")
  }
}