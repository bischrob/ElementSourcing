#' This script is designed to a function to create ternary plots
#' comparing sourcing data for obsidian.
#' Original Created: 2.2.2018
#' Latest version: 2.2.2018
#' Author: Robert J. Bischoff

#############################################################################################
# Create all possible combination of ternary graphs
allTriplots <- function(df,nColors){
  stopifnot(is.data.frame(df))
  artifacts <- which(df$Type == "Artifact")
  sources <- c(which(df$Type == "Source"),which(df$Type == "Source Flake"))
  if(missing(nColors)){
    nColors <- readRDS("Data/Colors.Rds")
    nColors <-  myColors$Hex[1:length(unique(df$Source))]
  }
  dName <- unlist(strsplit(as.character(Sys.time())," ")) # Gives directory today's date
  dName <- paste0("Figures/Plots--", dName[1],"/Triplots/")
  if (dir.exists(dName) == F) {dir.create(dName)}
  for(i in 7:11){
    for(j in i:11){
      for(k in j:11){
        if(identical(i,j) | identical(i,k) | identical(j,k)){} else {
          g <- ggtern() +
            geom_point(data = df[sources,],
                       aes(x = df[sources,i],
                           y = df[sources,j],
                           z = df[sources,k],
                           color = df$Source[sources],
                           shape = df$Type[sources]),
                       size = 1) +
            geom_point(data = df[artifacts,],
                       aes(x = df[artifacts,i],
                           y = df[artifacts,j],
                           z = df[artifacts,k],
                           color = df$Source[artifacts],
                           shape = df$Type[artifacts]),
                       size = 1) +
            xlab(names(df)[i]) +
            ylab(names(df)[j]) +
            zlab(names(df)[k]) +
            theme_minimal() +
            theme(legend.title=element_blank()) +
            scale_color_manual(values = col1) +
            geom_confidence_tern(data = df[sources,], 
                                 aes(x = df[sources,i],
                                     y = df[sources,j],
                                     z = df[sources,k],
                                     color = df$Source[sources]),
                                 breaks = .9)
          
          ggsave(filename = paste0(dName,names(df)[i],
                                   "-",names(df)[j],
                                   "-",names(df)[k],".jpg"), 
              dpi = 300, plot = g, width = 6.5, units = "in")
    }}}
  }
}
