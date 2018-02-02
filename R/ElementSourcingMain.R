#' This script is used for element sourcing. 
#' This version has been updated to take user input from the
#' command line
#' Created: 2.2.18
#' Robert J. Bischoff

#############################################################################################
# Variables to update

file1 <- file.choose() # note look into using choose.file for multiple selections
file2 <- file.choose()

myGroup <<- "Source"
myGroup2 <<- "Type"
myProb <<- .9 # Probability limit to accept sourcing
# grp1 <- c("American Falls", "Malad", "Browns Bench Sub A", "Browns Bench Sub B", "Toy Pass", "Big Southern Butte", "Ferguson Wash") # subset sources
# grp1 <- unique(mySources$Source)
# grp2 <- c("PRM") # subset groups to source
whichData <- 1 # 1, 2 or 3 - designates the use of all data in plots, or assigned or unassigned
elem1 <- 8
elem2 <- 9
elem3 <- 10
myResults <- "Y"

######################################################
# Create a variable storing packages used, installs the package if missing, and loads packages.
my.packages <- c("xlsx", "ggplot2","MASS", "dplyr", "rio")
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(my.packages, usePackage)

# source scatter plots function
source("multipleobsidianscatterplots.R")

######################################################
mySources <- import(file1)
myData <- import(file2)

# Change column names
names(mySources)[1:13] <- c("ANID","Mn", "Fe", "Zn", "Ga", "Th", "Rb", "Sr", "Y", "Zr", "Nb", "Source", "Type")
names(myData)[1:13] <- c("ANID","Mn", "Fe", "Zn", "Ga", "Th", "Rb", "Sr", "Y", "Zr", "Nb", "Source", "Type")

# Choose which sources to keep
readinteger <- function()
{ 
  n <- readline(prompt="Enter an integer for each selection to include separated by a comma (1,2,3...): ")
  return(n)
}
uSource <- unique(mySources$Source) # unique sources
lSource <- paste0(1:length(uSource), ": ", uSource)
print(lSource)
grp1 <- readinteger() 
grp1 <- as.numeric(unlist(strsplit(grp1,"\\,")) )
grp1 <- uSource[grp1]
uGroup <- unique(myData$Source)
lGroup <- paste0(1:length(uGroup), ": ", uGroup)
print(lGroup)
grp2 <- readinteger()
grp2 <- as.numeric(unlist(strsplit(grp2,"\\,")) )
grp2 <- uGroup[grp2] 
######################################################
mySources1 <- mySources[which(mySources[,myGroup] %in% grp1),] 
myData1 <- myData[which(myData[,myGroup] %in% grp2),] 
myCData <<- bind_rows(myData1,mySources1)  
rm(myData, myData1,mySources,mySources1)
source("Obsidian Sourcing Interactive v2.R")
rm(myCData)

# Assign correct data frame   
if(whichData == 1) {plotDF <- AllData}
if(whichData == 2) {plotDF <- rbind.data.frame(Assigned, AllData[sources,])}
if(whichData == 3) {plotDF <- rbind.data.frame(Unassigned, AllData[sources,])}

# Display scatterplots
windows(12,12); sp <- scatterPlots(plotDF, as.character(myGroup))

# Save results
if(myResults == "Y"){
  fileName <- gsub(".xlsx","",file2)
  
  write.xlsx(AllData[artifacts,],paste0(fileName," Analysis.xlsx"), sheetName = "All Artifacts",
             row.names = F)
  write.xlsx(LDAPosterior[artifacts,],paste0(fileName," Analysis.xlsx"),
             sheetName = "LDA Posterior Probabilities", row.names = F,
             append = T)
  write.xlsx(allMdistC[artifacts,],paste0(fileName," Analysis.xlsx"),
             sheetName = "Mahalanobis distances", row.names = F,
             append = T)
  if(!nrow(Assigned) == 0){
     write.xlsx(Assigned,paste0(fileName," Analysis.xlsx"),
             sheetName = "Assigned Artifacts", row.names = F,
             append = T)
  }
  if(!nrow(Unassigned) == 0){
    write.xlsx(Unassigned,paste0(fileName," Analysis.xlsx"),
             sheetName = "Unassigned Artifacts", row.names = F,
             append = T)
  }
}

