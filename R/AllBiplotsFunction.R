#' This script is designed to a function to create biplots
#' comparing sourcing data for obsidian.
#' Original Created: 2.2.2018
#' Latest version: 2.2.2018
#' Author: Robert J. Bischoff

#############################################################################################
# Create all possible combination of biplots
allBiplots <- function(df,nColors){
  stopifnot(is.data.frame(df))
  artifacts <- which(df$Type == "Artifact")
  sources <- c(which(df$Type == "Source"),which(df$Type == "Source Flake"))
  if(missing(nColors)){
    myColors <- readRDS("Data/Colors.Rds")
    nColors <-  myColors$Hex[1:length(unique(df$Source))]
  }
  dName <- unlist(strsplit(as.character(Sys.time())," ")) # Gives directory today's date
  dName <- paste0("Figures/Plots--", dName[1],"/Biplots/")
  if (dir.exists(dName) == F) {dir.create(dName)}

  for(i in 7:11){
    for(j in i:11){
      if(identical(i,j)){} else {
        ratio.display <- 4/3
        ratio.values <- (max(df[sources,i])-min(df[sources,i]))/
            (max(df[sources,j])-min(df[sources,j]))
        g <- ggplot() +
          # geom_point(data = df[sources,], aes(x = df[sources,i],
          #                            y = df[sources,j],
          #                            color = df$Source[sources],
          #                            shape = df$Type[sources])) +
          geom_point(data = df[artifacts,], aes(x = df[artifacts,i],
                                     y = df[artifacts,j],
                                     color = df$Source[artifacts])) +
          coord_fixed(ratio.values/ratio.display) +
          xlab(names(df)[i]) +
          ylab(names(df)[j]) +
          theme_minimal() +
          theme(legend.title=element_blank()) +
          scale_color_manual(values = nColors) + # used for manual colors
          stat_ellipse(data = df[sources,], aes(x = df[sources,i],
                                              y = df[sources,j],
                                              color = df$Source[sources]),
              type = "norm",
              level = .9,
              lwd = 1) # this ellipse is based off the multivariate normal distribution
        g  
        ggsave(filename = paste0(dName,names(df)[i], "-",names(df)[j],".jpg"), 
              dpi = 300, plot = g, width = 6.5, units = "in")
    }}}
}
