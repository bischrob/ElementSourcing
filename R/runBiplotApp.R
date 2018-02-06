#' This script is a function to run the biplot shiny app.
#' Original Created: 2.6.2018
#' Latest version: 2.6.2018
#' Author: Robert J. Bischoff

#############################################################################################
runBiplotApp <- function (){
  source("R/Biplot App/app.R")
  shinyApp(uibi, serverbi)
}