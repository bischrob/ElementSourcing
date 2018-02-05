################################################################################
#' This script is used for element sourcing. 
#' This version has been updated to take user input from the
#' command line
#' Created: 2.2.18
#' Last Updated: 2.2.18
#' Robert J. Bischoff

################################################################################
# Create a variable storing packages used, installs the package if missing, and loads packages.
my.packages <- c("xlsx", "ggplot2","MASS", "dplyr", "rio","svDialogs", "plotly", "shiny")
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  lapply(p, function(x) suppressMessages(require(x, character.only = TRUE,quietly=TRUE,warn.conflicts = FALSE)))
}
lapply(my.packages, usePackage)

# source functions
source("R/scatterPlots.R")
source("R/elemSource.R")
source("R/selectData.R")
source("R/AllBiplotsFunction.R")

################################################################################
# load data
# df <- selectData()
# saveRDS(df,"Data/testdata.Rds")
df <- readRDS('Data/testdata.Rds')

# Create key for rows that are either artifacts or sources
artifacts <- which(df$Type == "Artifact")
sources <- c(which(df$Type == "Source"),which(df$Type == "Source Flake"))

# Run sourcing function
df <- elemSource(df,saveResults = F,prob = 1)

################################################################################
# plot results
 allBiplots(df)
# allTriplots(plotDF)
# Display scatterplots
# windows(12,12); sp <- scatterPlots(df)

