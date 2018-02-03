#' This script creates a function to source elements. Intially based on
#' obsidian sourcing.

#############################################################################################
elemSource <- function(df,whichData = 1){
stopifnot(is.data.frame(df))

# log and clean values
oLog <- df[,2:11]
oLog[get("oLog") < 0] <- 0
oLog <- log(oLog)
oLog <- apply(oLog,2, function(x) replace(x, is.infinite(x),0)) # Replace infinite values

df[,2:11] <- oLog

# List unique identifiers and split artifacts and sources
oSources <<- as.factor(df [,myGroup])
oTypes <<- as.factor(df[,myGroup2])
artifacts <<- which(df$Type == "Artifact")
sources <<- c(which(df$Type == "Source"),which(df$Type == "Source Flake"))

#############################################################################################
# Discriminant Analysis

# Run analysis on sources
fitLDA <- lda(oSources[sources] ~ ., data = df[sources,7:11],
           prior = rep(1,nlevels(oSources[sources]))/nlevels(oSources[sources]))

# Assess the accuracy of the prediction
testLDA <- predict(fitLDA, newdata = df[sources,7:11])
ct <- table(as.character(oSources[sources]), as.character(testLDA$class))
testAccuracy <- diag(prop.table(ct, 1))
print(testAccuracy) # percent correct for each Source
testAccuracyTotal <-sum(diag(prop.table(ct)))
print(testAccuracyTotal) # total percent correct

# Predict source for artifacts and assign to obsidian data frame
predictLDA <- predict(fitLDA, newdata = df[,7:11])
df$Discriminant <- as.character(predictLDA$class)
df$Posterior <- round(apply(predictLDA$posterior, 1, function(x) max(x)),3)
LDAPosterior <- cbind.data.frame(df[,1],df$Source,predictLDA$posterior)
names(LDAPosterior)[1:2] <- c("ANID", "Source")



#############################################################################################
# Mahalanobis Distance

# Calculate Mahalanobis distance from each source sample to each source group.
oGroups <- unique(sort(df$Source[sources]))

# Source mean and covariance
sourcesMdist <- NULL
for(i in 1:length(oGroups)){
  sMean <- colMeans(df[df$Source == oGroups[i],7:11])
  sCov <- cov(df[df$Source == oGroups[i],7:11])
  mDist <- mahalanobis(df[sources,7:11],sMean,sCov, tol = 1e-20)
  sourcesMdist <- cbind(sourcesMdist,mDist)
  colnames(sourcesMdist)[i] <- oGroups[i]
  }

# assess accuracy of sourced data
sourcesMdistMin <- cbind.data.frame(df$Source[sources],
                                    colnames(sourcesMdist)[apply(sourcesMdist,1,which.min)],
                                    round(apply(sourcesMdist, 1, function(x) min(x)),3),
                                    sourcesMdist)
colnames(sourcesMdistMin)[1:3] <- c("OriginalSource","ClosestSource", "MinimumDistance")
mDistAccuracy <- sum(sourcesMdistMin$ClosestSource %in% sourcesMdistMin$OriginalSource)/
                  nrow(sourcesMdistMin)
print(mDistAccuracy) # should = 1

# Determine source for all
oGroups <- unique(sort(df$Source[sources]))

# Source mean and covariance
allMdist <- NULL
for(i in 1:length(oGroups)){
  sMean <- colMeans(df[df$Source == oGroups[i],7:11])
  sCov <- cov(df[df$Source == oGroups[i],7:11])
  mDist <- mahalanobis(df[,7:11],sMean,sCov, tol = 1e-20)
  allMdist <- cbind(allMdist,mDist)
  colnames(allMdist)[i] <- oGroups[i]
}

# Obtain chi-squared probabilities
mProbs <- NULL
for (i in 1:ncol(allMdist)){
  p <- 1 - pchisq(allMdist[,i],length(allMdist[,i]))
  mProbs <- cbind(mProbs,p)
  colnames(mProbs)[i] <- colnames(allMdist)[i]
}

# combine the mahalanobis distances with the probabilities
allMdistC <- cbind(allMdist,mProbs)
allMdistC <- allMdistC[,order(colnames(allMdistC))]
allMdistC <- cbind.data.frame(df$ANID, df$Source, allMdistC)
names(allMdistC)[1:2] <- c("ANID", "Source")

sNames <- colnames(allMdist) [apply(allMdist,1,which.min)]
minDist <- round(apply(allMdist, 1, function(x) min(x)),3)
maxProb <- round(apply(mProbs, 1, function(x) max(x)),3)

# Assign Mahalanobis info to original data frame
df$Mahalanobis <- sNames
df$MahalDistance <- minDist
df$MahalProb <- maxProb

#############################################################################################
# Determines sources that are confidently sourced 
# artifacts.

# Sourced
artifactsConf <- which(df$MahalProb[artifacts] >= myProb
                & df$Posterior[artifacts] >= myProb & df$Mahalanobis[artifacts] == df$Discriminant[artifacts])
# Unsourced
artifactsUnk <- setdiff(artifacts,artifactsConf)
AllData <<- df
Assigned <<- df[artifacts,][artifactsConf,]
Unassigned <<- df[artifacts,][artifactsUnk,]

################################################################################
# Save results
if(myResults == "Y"){
  fileName <- gsub(".xlsx","",file2)
  
  write.xlsx(df[artifacts,],paste0(fileName," Analysis.xlsx"), 
             sheetName = "All Artifacts",
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

################################################################################
# Return results as list of dataframes
# Assign correct data frame   
if(whichData == 1) {results <- df}
if(whichData == 2) {results <- rbind.data.frame(Assigned,
                                               df[sources,])}
if(whichData == 3) {results <- rbind.data.frame(Unassigned,
                                               df[sources,])}

return(results)
}

