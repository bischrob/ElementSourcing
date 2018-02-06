#' This script is a function to run the triplot shiny app.
#' Original Created: 2.6.2018
#' Latest version: 2.6.2018
#' Author: Robert J. Bischoff

#############################################################################################
runTriplotApp <- function (){
  source("R/Triplot App/app.R")
  shinyApp(uitri, servertri)
}