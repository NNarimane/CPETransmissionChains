##########################################################################
#####CPE Transmission Chains: Can Outbreaker2 Identify Imported Cases#####
##########################################################################

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

source("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Generation Time Sensitivity Analysis Functions.R", 
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
endDate="2015-06-01"

cat("Load data\n")
dsorted=getData(startDate, endDate)

##############################################
#### GET NETWORKS WITHOUT ANCESTOR CONFIG ####

cat("Maximum meanGT time to test\n")
meanGT=rep(7, 5)

cat("Get Networks via lapply\n")
Networks_Results_NoAncestorConfig = lapply(meanGT, function(i){
  Network=buildNetworks_NoAncestorConfig(i)
  return(Network)
})

cat("Save Network Results\n")
# save(Networks_Results_NoAncestorConfig, file = paste0("CPETransmissionChains/Network Results/",startDate, " to ", endDate, " Networks for meanGT equals ",MeanGT[1], " with Poisson Distribution.RData"))

############################################
#### LOAD NETWORKS WITH ANCESTOR CONFIG ####

cat("Load Network Results for meanGT 1-60, Jan-June 2015\n")
# load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Network Results/2015-01-01 to 2015-06-01 Networks for meanGT 1 to 60 with Poisson Distribution.RData")

cat("Load Network Results for meanGT 1-60, June-Dec 2015\n")
# load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Network Results/2015-06-01 to 2015-12-01 Networks for meanGT 1 to 60 with Poisson Distribution.RData")

####################
#### GET GRAPHS ####

i=1
min_support=0.05

cat("Get Graph for Networks_Results_NoAncestorConfig\n")
Network_NoAncestorConfig=Networks_Results_NoAncestorConfig[[i]]
Network_Plot_NoAncestorConfig=plot(Network_NoAncestorConfig, type="network", min_support = min_support)
Network_Edgelist_NoAncestorConfig=as.data.frame(cbind(as.character(Network_Plot_NoAncestorConfig$x$edges$from), as.character(Network_Plot_NoAncestorConfig$x$edges$to)), stringsAsFactors = F)
# Graph_NoAncestorConfig=graph_from_edgelist(Network_Edgelist_NoAncestorConfig)
# ConnectedGraph_NoAncestorConfig=Graph_NoAncestorConfig-V(Graph_NoAncestorConfig)[degree(Graph_NoAncestorConfig)==0]

cat("Get Graph for Networks_Results\n")
Network=Networks_Results[[i]]
Network_Plot=plot(Network, type="network", min_support = min_support)
Network_Edgelist=as.data.frame(cbind(as.character(Network_Plot$x$edges$from), as.character(Network_Plot$x$edges$to)), stringsAsFactors = F)
# Graph=graph_from_edgelist(Network_Edgelist)
# ConnectedGraph=Graph-V(Graph)[degree(Graph)==0]

cat("Get Imported Cases Data\n")
cat(paste("Get Data: (Poisson) Transformation Function for meanGT =", meanGT[1], "\n"))
myData=getCaseDataForPoissonTransformedDates(meanGT)
myImportedCases=rownames(myData[which(myData$Imported == "O"),])
#100 imported cases

cat("How many imported cases are targets?\n")
count(Network_Edgelist[,2] %in% myImportedCases) #283 TRUE for min_supp=0.02; 19 TRUE for min_supp=0.05
count(Network_Edgelist_NoAncestorConfig[,2] %in% myImportedCases) #810 TRUE for min_supp=0.02; 60 TRUE for min_supp=0.05

cat("Which imported cases are targets?\n")
Network_Edgelist_Imported=cbind(Network_Edgelist, Network_Edgelist[,2] %in% myImportedCases)
ImportedCases_AsTarget=unique(Network_Edgelist_Imported[which(Network_Edgelist_Imported[,3]==TRUE),2]) 
#42 imported cases out of 100 are targets for min_supp=0.02; 8 imported cases out of 100 are targets for min_supp=0.05

Network_Edgelist_Imported_NoAncestorConfig=cbind(Network_Edgelist_NoAncestorConfig, Network_Edgelist_NoAncestorConfig[,2] %in% myImportedCases)
ImportedCases_AsTarget_NoAncestorConfig=unique(Network_Edgelist_Imported_NoAncestorConfig[which(Network_Edgelist_Imported_NoAncestorConfig[,3]==TRUE),2]) #42 total cases
#100 imported cases out of 100 are targets for min_supp=0.02; 17 imported cases out of 100 are targets for min_supp=0.05


########
##6 out of the 8 real imported cases are identified 
##6/17 are correct; error is 11/17=64.7% are wrong