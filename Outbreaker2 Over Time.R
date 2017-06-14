########################################################
#####CPE Transmission Chains: Outbreaker2 Over Time#####
########################################################

#################################
#### INSTALL & LOAD PACKAGES ####

# cat("Intall Outbreaker 2\n")
# devtools::install_github("thibautjombart/outbreaker2")
# install.packages("rmarkdown")
# devtools::install_github("thibautjombart/outbreaker2", build_vignettes = TRUE)

cat("Load Packages\n")
library("outbreaker2")
library("data.table")
library("igraph")
library("stringr")
library("ape")
library("adegenet")
library("R0")
library("visNetwork")
library("plyr")
library("combinat")
library("doSNOW")
library("snow")
library("foreach")
library("splitstackshape")

#########################
#### SET ENVIRONMENT ####

cat("Set Working Environment\n")
envNN=T
if(envNN){
  currentwd=setwd("C:/Users/Narimane/Dropbox/CPE Transmission Chains")
}else{
  currentwd=setwd("/Users/pascalcrepey/Google Drive/1-EPC/stageNN/") 
}

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("CPETransmissionChains/Generation Time Sensitivity Analysis Functions.R", 
       local = FALSE, verbose = getOption("verbose"))

###########################
#### LOAD DEPT NETWORK ####

cat("Upload Department Contact Network\n")
load("../Hospital_Network/HospitalNetwork/Data/Department Network.RData")

#########################################
#### GET DATA ON OXA-48 CPE EPISODES ####

cat("Choose start date\n")
startDate="2015-01-01"

cat("Choose end date\n")
endDate="2015-06-30"

cat("Load data\n")
dsorted=getData(startDate, endDate)

#########################
#### MEAN GT TO TEST ####

cat("MeanGT time\n")
meanGT=38

cat("Cutoff")
cutoff="2015-03-31"

###################

Network=buildNetworks(meanGT)

dsorted_original=dsorted
dsorted=dsorted[which(dsorted$DateEpisode <= as.Date(cutoff)),]
Network_FirstHalf=buildNetworks(meanGT)

dsorted=dsorted_original
dsorted=dsorted[which(dsorted$DateEpisode > as.Date(cutoff)),]
Network_LastHalf=buildNetworks(meanGT)

##############################
#### GET CONNECTED GRAPHS ####

myData=getCaseDataForPoissonTransformedDates(meanGT)

min_support=0.05

cat("Get Plot\n")
Network_Plot <- plot(Network, type = "network", min_support = min_support)
Network_Plot

cat("Get Plot\n")
Network_Plot1 <- plot(Network_FirstHalf, type = "network", min_support = min_support)
Network_Plot1

cat("Get Plot\n")
Network_Plot2 <- plot(Network_LastHalf, type = "network", min_support = min_support)
Network_Plot2


