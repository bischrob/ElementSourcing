################################################################################
#' This script is used for element sourcing. 
#' This version has been updated to take user input from the
#' command line
#' Created: 2.2.18
#' Last Updated: 2.2.18
#' Robert J. Bischoff

################################################################################
# Create a variable storing packages used, installs the package if missing, and loads packages.
  my.packages <- c("xlsx", "ggplot2","MASS", "dplyr",
                 "rio","svDialogs", "plotly", "shiny", "devtools")
  usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
}
  lapply(my.packages, usePackage)
  if (!is.element("ggbiplot", installed.packages()[,1])) {
    install_github("ggbiplot", "vqv")
}
# source functions
  source("R/selectData.R")
  source("R/elemSource.R")
  source("R/scatterPlots.R")
  source("R/AllBiplotsFunction.R")
  source("R/plotPCA.R")
  source("R/runBiplotApp.R")
  source("R/runTriplotApp.R")
################################################################################
# load data
  df <- selectData()
# saveRDS(df,"Data/testdata.Rds")
# df <- readRDS('Data/testdata.Rds')

# Run sourcing function
  df <- elemSource(df,saveResults = F,prob = 1)

################################################################################
# plot results
# Display scatterplots
  windows(12,12) ; scatterPlots(df)

# saves all possible versions of biplots
 allBiplots(df, onlySources = F, showSources = T)

# Plot PCA
  plotPCA(df, showSources = T)
 
# View results interactively
# Biplot
  runBiplotApp()
  
# Triplot
  runTriplotApp()

