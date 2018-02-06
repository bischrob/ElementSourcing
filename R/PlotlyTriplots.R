#' This script is designed to a function to create ternary plots
#' comparing sourcing data for obsidian.
#' Original Created: 2.2.2018
#' Latest version: 2.6.2018
#' Author: Robert J. Bischoff

#############################################################################################
# Create all possible combination of ternary graphs
allTriplotsPlotly <- function(df,nColors){
  stopifnot(is.data.frame(df))
  artifacts <- which(df$Type == "Artifact")
  sources <- c(which(df$Type == "Source"),which(df$Type == "Source Flake"))
  if(missing(mColors)){
    myColors <- readRDS("Data/Colors.Rds")
    mColors <-  myColors$Hex[1:length(unique(df$Source))]
  }
  dName <- unlist(strsplit(as.character(Sys.time())," ")) # Gives directory today's date
  dName <- paste0("Figures/Plots--", dName[1])
  
  i = 7
  j = 8
  k = 9
  options(warn = -1)
  
  if (dir.exists(dName) == F) dir.create(dName)
  for(i in 7:11){
    for(j in i:11){
      for(k in j:11){
        if(identical(i,j) | identical(i,k) | identical(j,k)){} else {
          p <- plot_ly(data = df,
                  a = df[,i],
                  b = df[,j],
                  c = df[,k],
                  color = ~Source,
                  
                  type = 'scatterternary',
                  symbol = ~Type,
                  mode = 'markers',
                  text = ~paste("ANID: ",
                                ANID, '<br>Source:',
                                Source, '<br>Closest Source:',
                                Mahalanobis)) %>%
            layout(ternary = list(aaxis = list(title = names(df)[i]),
                                  baxis = list(title = names(df)[j]),
                                  caxis = list(title = names(df)[k])))
          
        }}}
  }
}
Sys.setenv("plotly_username"="bischrob")
Sys.setenv("plotly_api_key"="o1bkAWYNgvd6qeIDn4hI")
plotly_IMAGE(p, width = 800, height = 800, format = "png", out_file = "Figures/Test.png")
options(warn = 0)
