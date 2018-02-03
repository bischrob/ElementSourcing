################################################################################
#' This script is used for element sourcing. 
#' This version has been updated to take user input from the
#' command line
#' Created: 2.2.18
#' Last Updated: 2.2.18
#' Robert J. Bischoff

################################################################################
# Variables to update

file1 <- file.choose() # note look into using choose.file for multiple selections
file2 <- file.choose()

myGroup <- "Source"
myGroup2 <- "Type"
myProb <- .9 # Probability limit to accept sourcing
# grp1 <- c("American Falls", "Malad", "Browns Bench Sub A", "Browns Bench Sub B", "Toy Pass", "Big Southern Butte", "Ferguson Wash") # subset sources
# grp1 <- unique(mySources$Source)
# grp2 <- c("PRM") # subset groups to source
whichData <- 1 # 1, 2 or 3 - designates the use of all data in plots, or assigned or unassigned
elem1 <- 8
elem2 <- 9
elem3 <- 10
myResults <- "N"

################################################################################
# Create a variable storing packages used, installs the package if missing, and loads packages.
my.packages <- c("xlsx", "ggplot2","MASS", "dplyr", "rio")
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(my.packages, usePackage)

# source functions
source("R/multipleScatterplots.R")
source("R/ElementSourcingFunction.R")

################################################################################
mySources <- import(file1)
myData <- import(file2)

# Change column names
names(mySources)[1:13] <- c("ANID","Mn", "Fe", "Zn", "Ga", "Th", "Rb", "Sr", "Y", "Zr", "Nb", "Source", "Type")
names(myData)[1:13] <- c("ANID","Mn", "Fe", "Zn", "Ga", "Th", "Rb", "Sr", "Y", "Zr", "Nb", "Source", "Type")

# Choose which sources to keep
sourceList <- unique(mySources$Source) # unique sources
dataList <- unique(myData$Source) # groups from data to be source

# Create function to print lists
printList <- function(printList){
  stopifnot(!missing(printList))
  sourceListPrint <- paste0(1:length(printList), ": ", printList)
  print(sourceListPrint)
}

# Create function to take user input for subset
readinteger <- function(xlist){ 
  stopifnot(!missing(xlist))
    n <- readline(prompt="Enter an integer for each selection to include separated by a comma (1,2,3...): ")
  n <- as.numeric(unlist(strsplit(n,"\\,")) )
  grp <- xlist[n]
  return(grp)
}

printList(sourceList)
grp1 <- readinteger(sourceList) 
printList(dataList)
grp2 <- readinteger(dataList) 

# Subset data
myCData <- bind_rows(mySources[which(mySources[,myGroup] %in% grp1),],
                      myData[which(myData[,myGroup] %in% grp2),] )  
rm(myData,mySources)
plotDF <- elemSource(myCData)

################################################################################
# plot results
allBiplots(plotDF)
# allTriplots(plotDF)
# Display scatterplots
# windows(12,12); sp <- scatterPlots(plotDF, as.character(myGroup))

