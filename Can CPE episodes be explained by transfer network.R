##############################################################
##### Can CPE episodes be explained by transfer network? #####
##############################################################

#################################
#### INSTALL & LOAD PACKAGES ####

cat("Load Packages\n")
library("igraph")
library("stringr")
library("foreach")
library("doSNOW")
library("snow")

#########################
#### SET ENVIRONMENT ####

cat("Set Working Environment\n")
envNN=T
envNNwindows=T
if(envNN){
  if(envNNwindows){
    currentwd=setwd("C:/Users/Narimane/Dropbox/CPE Transmission Chains/")
  }else{
    currentwd=setwd("/Users/narimanenekkab/Dropbox/CPE Transmission Chains/")
  }
}else{
  currentwd=setwd("/Users/pascalcrepey/Google Drive/1-EPC/stageNN/") 
}

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("CPETransmissionChains/Generation Time Sensitivity Analysis Functions.R", 
       local = FALSE, verbose = getOption("verbose"))

###########################
#### LOAD DEPT NETWORK ####

cat("Original or Transformed Weights\n")
Transformed=T

cat("Upload Department Contact Network\n")
if(Transformed){
  cat("Upload Department Network with Transformed Weights\n")
  load("Data/Department Network (Transformed).RData")
}else{
  cat("Upload Department Contact Network without Transformations\n")
  load("../Hospital_Network/HospitalNetwork/Data/Department Network.RData")
}

##########################
#### STEP 1: CPE DATA ####

# cat("Get CPE Data with Mechanism and Class Info\n")
# data=getCPEData()

# cat("Save Data\n")
# save(data, file = "Data/CPE Data with Mechanism Info.RData")

cat("Load Data\n")
load("Data/CPE Data with Mechanism Info.RData")

cat("Fix Department Variable")
data$Department=str_pad(data$Department, 2, pad = "0")

cat("Removing Episodes Occuring in Depts Other Than Depts in Network")
data=data[which(data$Department %in% V(directed.graph_Dept)$name),]

#########################################
#### STEP 2: GET POTENTIAL INFECTORS ####

cat("Set Maximum Number of Days to Test\n")
MaxDays=60

cat("By Mechanism or Class\n")
Mechanism=F

cat("Get Potential Infectors\n")
if(Mechanism){
  if(Transformed){
    # cat(paste("Get Potential Infectors for Day 1 to", MaxDays, "by Mechanism\n"))
    # PotentialInfectors_byMechanism=foreach(i=1:MaxDays) %do% getPotentialInfectors_byMechanism(i)
    cat("Save or load\n")
    # save(PotentialInfectors_byMechanism, file = "Data/PotentialInfectors_byMechanism (Transformed).RData")
    load("Data/PotentialInfectors_byMechanism (Transformed).RData")
    PotentialInfectors_byMechanism=PotentialInfectors_1to60Days
    rm(PotentialInfectors_1to60Days)
  }else{
    # cat(paste("Get Potential Infectors for Day 1 to", MaxDays, "by Mechanism\n"))
    # PotentialInfectors_byMechanism=foreach(i=1:MaxDays) %do% getPotentialInfectors_byMechanism(i)
    cat("Save or load\n")
    # save(PotentialInfectors_byMechanism, file = "Data/PotentialInfectors_byMechanism.RData")
    load("Data/PotentialInfectors_byMechanism.RData")
    PotentialInfectors_byMechanism=PotentialInfectors_1to60Days
    rm(PotentialInfectors_1to60Days)
  }
}else{
  if(Transformed){
    cat(paste("Get Potential Infectors for Day 1 to", MaxDays, "by Class\n"))
    PotentialInfectors_byClass=foreach(i=1:MaxDays) %do% getPotentialInfectors_byClass(i)
    cat("Save or load\n")
    save(PotentialInfectors_byClass, file = "Data/PotentialInfectors_byClass (Transformed).RData")
    # load("Data/PotentialInfectors_byClass (Transformed).RData")
  }else{
    # cat(paste("Get Potential Infectors for Day 1 to", MaxDays, "by Class\n"))
    # PotentialInfectors_byClass=foreach(i=1:MaxDays) %do% getPotentialInfectors_byClass(i)
    cat("Save or load\n")
    # save(PotentialInfectors_byClass, file = "Data/PotentialInfectors_byClass.RData")
    load("Data/PotentialInfectors_byClass.RData")
  }
}

cat("Get Potential Infectors (Range)\n")
if(Mechanism){
  if(Transformed){
    cat(paste("Get Potential Infectors for Day 1 to", MaxDays, "by Mechanism\n"))
    PotentialInfectors_byMechanism=foreach(i=1:MaxDays) %do% getPotentialInfectors_DayRange_byMechanism(i)
    cat("Save or load\n")
    save(PotentialInfectors_byMechanism, file = "Data/PotentialInfectors_byMechanism (Transformed) by Range.RData")
    # load("Data/PotentialInfectors_byMechanism (Transformed).RData")
    # PotentialInfectors_byMechanism=PotentialInfectors_1to60Days
    # rm(PotentialInfectors_1to60Days)
  }else{
    # cat(paste("Get Potential Infectors for Day 1 to", MaxDays, "by Mechanism\n"))
    # PotentialInfectors_byMechanism=foreach(i=1:MaxDays) %do% getPotentialInfectors_byMechanism(i)
    # cat("Save or load\n")
    # save(PotentialInfectors_byMechanism, file = "Data/PotentialInfectors_byMechanism.RData")
    # load("Data/PotentialInfectors_byMechanism.RData")
    # PotentialInfectors_byMechanism=PotentialInfectors_1to60Days
    # rm(PotentialInfectors_1to60Days)
  }
}else{
  if(Transformed){
    cat(paste("Get Potential Infectors for Day 1 to", MaxDays, "by Class\n"))
    PotentialInfectors_byClass=foreach(i=1:MaxDays) %do% getPotentialInfectors_DayRange_byClass(i)
    cat("Save or load\n")
    save(PotentialInfectors_byClass, file = "Data/PotentialInfectors_byClass (Transformed) and Range.RData")
    # load("Data/PotentialInfectors_byClass (Transformed).RData")
  }else{
    # cat(paste("Get Potential Infectors for Day 1 to", MaxDays, "by Class\n"))
    # PotentialInfectors_byClass=foreach(i=1:MaxDays) %do% getPotentialInfectors_byClass(i)
    # cat("Save or load\n")
    # save(PotentialInfectors_byClass, file = "Data/PotentialInfectors_byClass.RData")
    # load("Data/PotentialInfectors_byClass.RData")
  }
}

###################################
#### STEP 3: GET MIN DISTANCES ####

cat("Weighted or Un-Weighted Shortest Path Calculations\n")
Weighted=T
if(Weighted){
  cat("Set Weights and Algorithm to Calculate Weighted Shortest Paths\n")
  weights = E(directed.graph_Dept)$weight
  algorithm = "dijkstra"
}else{
  cat("Disable Weights and Algorithm to Calculate Shortest Paths\n")
  weights = NA
  algorithm = "automatic"
}

cat("Get Min Distances\n")
if(Mechanism){
  if(Transformed){
    if(Weighted){
      # cat("Get Weighted Transformed Distances for Potential Infectors by Mechanism\n")
      MinimumDistances_byMechanism=foreach(i=1:MaxDays) %do% getMinimumDistances(i, PotentialInfectors_byMechanism[[i]], weights = weights, algorithm = algorithm)
      save(MinimumDistances_byMechanism, file = "Data/MinimumDistances_byMechanism (Transformed) & Range.RData")
      # load("Data/MinimumDistances_byMechanism (Transformed) (Weighted).RData")
      # MinimumDistances_byMechanism=MinimumDistances_1to60Days_Weighted
      # rm(MinimumDistances_1to60Days_Weighted)
    }else{
      # cat("Get Un-Weighted Transformed Distances for Potential Infectors by Mechanism\n")
      # MinimumDistances_byMechanism=foreach(i=1:MaxDays) %do% getMinimumDistances(i, PotentialInfectors_byMechanism[[i]], weights = weights, algorithm = algorithm)
      # save(MinimumDistances_byMechanism, file = "Data/MinimumDistances_byMechanism (Transformed) (Unweighted).RData")
      # load("Data/MinimumDistances_byMechanism (Transformed) (Unweighted).RData")
    }
  }else{
    # cat("Get Weighted Transformed Distances for Potential Infectors by Mechanism\n")
    # MinimumDistances_byMechanism=foreach(i=1:MaxDays) %do% getMinimumDistances(i, PotentialInfectors_byMechanism[[i]], weights = weights, algorithm = algorithm)
    # save(MinimumDistances_byMechanism, file = "Data/MinimumDistances_byMechanism.RData")
    # load("Data/MinimumDistances_byMechanism.RData")
  }
}else{
  if(Transformed){
    cat("Get Weighted Transformed Distances for Potential Infectors by Class\n")
    MinimumDistances_byClass=foreach(i=1:MaxDays) %do% getMinimumDistances(i, PotentialInfectors_byClass[[i]], weights = weights, algorithm = algorithm)
    save(MinimumDistances_byClass, file = "Data/MinimumDistances_byClass (Transformed) and Range.RData")
    # load("Data/MinimumDistances_byClass (Transformed).RData")
  }else{
    # cat("Get Weighted Transformed Distances for Potential Infectors by Class\n")
    # MinimumDistances_byClass=foreach(i=1:MaxDays) %do% getMinimumDistances(i, PotentialInfectors_byClass[[i]], weights = weights, algorithm = algorithm)
    # save(MinimumDistances_byClass, file = "Data/MinimumDistances_byClass.RData")
    # load("Data/MinimumDistances_byClass.RData")
  }
}

#################################################
#### STEP 4a: GET RANDOM POTENTIAL INFECTORS ####

cat("Number of Simulations\n")
Nruns=100

cat("Get Random Potential Infectors\n")
if(Mechanism){
  if(Transformed){
    # cat(paste("Run", Nruns, "runs\n"))
    # AllRandomMinimumDistances_byMechanism <- foreach(runs=rep(1,Nruns), .verbose=T) %do% getRandomMinimumDistances(PotentialInfectors_byMechanism)
    # cat("Save runs or load\n")
    # save(AllRandomMinimumDistances_byMechanism, file = "Data/AllRandomMinimumDistances_byMechanism (Weighted).RData")
    load("Data/AllRandomMinimumDistances_byMechanism (Transformed) (Weighted).RData")
    AllRandomMinimumDistances_byMechanism=AllRandomSimulations
    rm(AllRandomSimulations)
  }else{
    cat(paste("Run", Nruns, "runs\n"))
    AllRandomMinimumDistances_byMechanism <- foreach(runs=rep(1,Nruns), .verbose=T) %do% getRandomMinimumDistances(PotentialInfectors_byMechanism)
    cat("Save runs or load\n")
    save(AllRandomMinimumDistances_byMechanism, file = "Data/AllRandomMinimumDistances_byMechanism.RData")
    # load("Data/AllRandomMinimumDistances_byMechanism.RData")
  }
}else{
  if(Transformed){
    cat(paste("Run", Nruns, "runs\n"))
    AllRandomMinimumDistances_byClass <- foreach(runs=rep(1,Nruns), .verbose=T) %do% getRandomMinimumDistances(PotentialInfectors_byClass)
    cat("Save runs or load\n")
    save(AllRandomMinimumDistances_byClass, file = "Data/AllRandomMinimumDistances_byClass (Weighted).RData")
    # load("Data/AllRandomMinimumDistances_byClass (Weighted).RData")
  }else{
    cat(paste("Run", Nruns, "runs\n"))
    AllRandomMinimumDistances_byClass <- foreach(runs=rep(1,Nruns), .verbose=T) %do% getRandomMinimumDistances(PotentialInfectors_byClass)
    cat("Save runs or load\n")
    save(AllRandomMinimumDistances_byClass, file = "Data/AllRandomMinimumDistances_byClass.RData")
    # load("Data/AllRandomMinimumDistances_byClass.RData")
  }
}

cat("Get Random Potential Infectors by Range\n")
if(Mechanism){
  if(Transformed){
    cat(paste("Run", Nruns, "runs\n"))
    AllRandomMinimumDistances_byMechanism <- foreach(runs=rep(1,Nruns), .verbose=T) %do% getRandomMinimumDistances_DayRange(PotentialInfectors_byMechanism)
    # cat("Save runs or load\n")
    save(AllRandomMinimumDistances_byMechanism, file = "Data/AllRandomMinimumDistances_byMechanism (Weighted) & Range.RData")
    # load("Data/AllRandomMinimumDistances_byMechanism (Transformed) (Weighted).RData")
    # AllRandomMinimumDistances_byMechanism=AllRandomSimulations
    # rm(AllRandomSimulations)
  }else{
    # cat(paste("Run", Nruns, "runs\n"))
    # AllRandomMinimumDistances_byMechanism <- foreach(runs=rep(1,Nruns), .verbose=T) %do% getRandomMinimumDistances(PotentialInfectors_byMechanism)
    # cat("Save runs or load\n")
    # save(AllRandomMinimumDistances_byMechanism, file = "Data/AllRandomMinimumDistances_byMechanism.RData")
    # load("Data/AllRandomMinimumDistances_byMechanism.RData")
  }
}else{
  if(Transformed){
    cat(paste("Run", Nruns, "runs\n"))
    AllRandomMinimumDistances_byClass <- foreach(runs=rep(1,Nruns), .verbose=T) %do% getRandomMinimumDistances_DayRange(PotentialInfectors_byClass)
    cat("Save runs or load\n")
    save(AllRandomMinimumDistances_byClass, file = "Data/AllRandomMinimumDistances_byClass (Weighted) and Range.RData")
    # load("Data/AllRandomMinimumDistances_byClass (Weighted).RData")
  }else{
    # cat(paste("Run", Nruns, "runs\n"))
    # AllRandomMinimumDistances_byClass <- foreach(runs=rep(1,Nruns), .verbose=T) %do% getRandomMinimumDistances(PotentialInfectors_byClass)
    # cat("Save runs or load\n")
    save(AllRandomMinimumDistances_byClass, file = "Data/AllRandomMinimumDistances_byClass.RData")
    # # load("Data/AllRandomMinimumDistances_byClass.RData")
  }
}

#############################################
#### STEP 4b: AVERAGE RANDOM SIMULATIONS ####

# cat("Get Average of Simulated Random Min Distances\n")
# if(Mechanism){
#   if(Transformed){
#     AverageRandomMinimumDistances_byMechanism=getAverageRandomSimulationsMinDistances(AllRandomMinimumDistances_byMechanism)
#   }else{
#     AverageRandomMinimumDistances_byMechanism=getAverageRandomSimulationsMinDistances(AllRandomMinimumDistances_byMechanism)
#   }
# }else{
#   if(Transformed){
#     AverageRandomMinimumDistances_byClass=getAverageRandomSimulationsMinDistances(AllRandomMinimumDistances_byClass)
#   }else{
#     AverageRandomMinimumDistances_byClass=getAverageRandomSimulationsMinDistances(AllRandomMinimumDistances_byClass)
#   }
# }

#########################################
#### WILCOXON PAIRED RANKED SUM TEST ####

# cat("Wilcoxon Rank Sum Test Results\n")
# if(Mechanism){
#   if(Transformed){
#     cat("Run Wilcoxon Paired Rank Sum Test\n")
#     CombinedMinimumDistances_WilcoxonPairedRankTestPValues=foreach(i=1:MaxDays) %do% getWilcoxonPairedRankTestPValues(i, MinimumDistances_byMechanism, AverageRandomMinimumDistances_byMechanism)
#     cat("Create Table of Statistically Significant and Non-Sig Results\n")
#     WilcoxonPairedRankTestPValues=unlist(CombinedMinimumDistances_WilcoxonPairedRankTestPValues)
#     Results=as.data.frame(WilcoxonPairedRankTestPValues)
#     Results$Days=1:60
#     Results$StatSigDiff=Results$WilcoxonPairedRankTestPValues < 0.05
#     #If stat. sig. p-value results (TRUE), reject H0 (meaning that the distributions differ)
#   }else{
#     cat("Run Wilcoxon Paired Rank Sum Test\n")
#     CombinedMinimumDistances_WilcoxonPairedRankTestPValues=foreach(i=1:MaxDays) %do% getWilcoxonPairedRankTestPValues(i, MinimumDistances_byMechanism, AverageRandomMinimumDistances_byMechanism)
#     cat("Create Table of Statistically Significant and Non-Sig Results\n")
#     WilcoxonPairedRankTestPValues=unlist(CombinedMinimumDistances_WilcoxonPairedRankTestPValues)
#     Results=as.data.frame(WilcoxonPairedRankTestPValues)
#     Results$Days=1:60
#     Results$StatSigDiff=Results$WilcoxonPairedRankTestPValues < 0.05
#     #If stat. sig. p-value results (TRUE), reject H0 (meaning that the distributions differ)
#   }
# }else{
#   if(Transformed){
#     cat("Run Wilcoxon Paired Rank Sum Test\n")
#     CombinedMinimumDistances_WilcoxonPairedRankTestPValues=foreach(i=1:MaxDays) %do% getWilcoxonPairedRankTestPValues(i, MinimumDistances_byClass, AverageRandomMinimumDistances_byClass)
#     cat("Create Table of Statistically Significant and Non-Sig Results\n")
#     WilcoxonPairedRankTestPValues=unlist(CombinedMinimumDistances_WilcoxonPairedRankTestPValues)
#     Results=as.data.frame(WilcoxonPairedRankTestPValues)
#     Results$Days=1:60
#     Results$StatSigDiff=Results$WilcoxonPairedRankTestPValues < 0.05
#     #If stat. sig. p-value results (TRUE), reject H0 (meaning that the distributions differ)
#   }else{
#     cat("Run Wilcoxon Paired Rank Sum Test\n")
#     CombinedMinimumDistances_WilcoxonPairedRankTestPValues=foreach(i=1:MaxDays) %do% getWilcoxonPairedRankTestPValues(i, MinimumDistances_byClass, AverageRandomMinimumDistances_byClass)
#     cat("Create Table of Statistically Significant and Non-Sig Results\n")
#     WilcoxonPairedRankTestPValues=unlist(CombinedMinimumDistances_WilcoxonPairedRankTestPValues)
#     Results=as.data.frame(WilcoxonPairedRankTestPValues)
#     Results$Days=1:60
#     Results$StatSigDiff=Results$WilcoxonPairedRankTestPValues < 0.05
#     #If stat. sig. p-value results (TRUE), reject H0 (meaning that the distributions differ)
#   }
# }
# 
# cat("Save Results\n")
# write.csv(Results, file="Data/Rank Sum test Results for by Mechanism with Transformed Weights 100 Simulations.csv", row.names = F)

#######################################
#### INTERPRETATION OF THE RESULTS ####

# OBJECTIVE: Determine if transfer network is linked to CPE episodes
# Step 1: Calculate the minimum network distances of all potential infectors
# Step 2: Does simulation of random networks give us a different result?
# Step 3: Results: some results do show a difference
# Step 4: Conclude: Department transfer network does not better explain CPE episodes


#############################################################################################
#### Using Distribution of 100 comulations: Compare where potential infector values fall ####
################ Calculate proportions, and values of shortest path #########################
#############################################################################################

cat("Get Proportion Tables\n")
if(Mechanism){
  if(Transformed){
    ProportionTables_byMechanism=get5thQuantiles(MinimumDistances_byMechanism, AllRandomMinimumDistances_byMechanism)
    MeanMinimumDistances_byMechanism=getMeanMinimumDistances(MinimumDistances_byMechanism, AllRandomMinimumDistances_byMechanism)
    ProportionTables_withMinDistances_byMechanism=cbind(ProportionTables_byMechanism[2,], MeanMinimumDistances_byMechanism)
    colnames(ProportionTables_withMinDistances_byMechanism)=c("ProportionLessThan5thPercentile", "MinimumDistances_MeansByNDays", "RandomSimulationsByDays_MeansByNDays")
  }else{
    ProportionTables_byMechanism=get5thQuantiles(MinimumDistances_byMechanism, AllRandomMinimumDistances_byMechanism)
  }
}else{
  if(Transformed){
    ProportionTables_byClass=get5thQuantiles(MinimumDistances_byClass, AllRandomMinimumDistances_byClass)
  }else{
    ProportionTables_byClass=get5thQuantiles(MinimumDistances_byClass, AllRandomMinimumDistances_byClass)
  }
}

write.csv(ProportionTables_withMinDistances_byMechanism, file = "Data/ProportionTables_withMinDistances_byMechanism.csv")
