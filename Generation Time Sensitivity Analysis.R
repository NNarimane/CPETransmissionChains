#######################################################################
#####CPE Transmission Chains: Generation Time Sensitivity Analysis#####
#######################################################################

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
# library("doSNOW")
# library("snow")
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
#### GET ONE NETWORK ####

# cat("MeanGT time to test\n")
# MeanGT=38
# 
# cat("Get Network\n")
# Network=buildNetworks(MeanGT)
# 
# cat("Save Network Results\n")
# save(Network, file = paste0("CPETransmissionChains/Network Results/",startDate, " to ", endDate, " Network for meanGT ",MaxMeanGT, " with Poisson Distribution.RData"))

######################
#### GET NETWORKS ####

# cat("Maximum meanGT time to test\n")
# MaxMeanGT=60
# 
# cat("Get Networks via lapply\n")
# Networks_Results = lapply(1:MaxMeanGT, function(i){
#   Network=buildNetworks(i)
#   return(Network)
# })
# 
# cat("Save Network Results\n")
# save(Networks_Results, file = paste0("CPETransmissionChains/Network Results/",startDate, " to ", endDate, " Networks for meanGT 1 to ",MaxMeanGT, " with Poisson Distribution.RData"))
# 

######################
#### GET NETWORKS ####

cat("Maximum meanGT time to test\n")
MaxMeanGT=60

#Poisson
cat("Get Networks via lapply\n")
Networks_Results_Poisson10 = lapply(1:MaxMeanGT, function(i){
  Network=buildNetworks_Poisson(i)
  return(Network)
})
cat("Save Network Results\n")
save(Networks_Results_Poisson10, file = paste0("CPETransmissionChains/Network Results/",startDate, " to ", endDate, " Networks for meanGT 1 to ",MaxMeanGT, " with Poisson Distribution10.RData"))

#NonTransformed
cat("Get Networks via lapply\n")
Networks_Results_NonTransformed10 = lapply(1:MaxMeanGT, function(i){
  Network=buildNetworks_NonTransformed(i)
  return(Network)
})
cat("Save Network Results\n")
save(Networks_Results_NonTransformed10, file = paste0("CPETransmissionChains/Network Results/",startDate, " to ", endDate, " Networks for meanGT 1 to ",MaxMeanGT, " with NonTransformed Distribution10.RData"))

#AddingRandomly
cat("Get Networks via lapply\n")
Networks_Results_AddingRandomly10 = lapply(1:MaxMeanGT, function(i){
  Network=buildNetworks_AddingRandomly(i)
  return(Network)
})
cat("Save Network Results\n")
save(Networks_Results_AddingRandomly10, file = paste0("CPETransmissionChains/Network Results/",startDate, " to ", endDate, " Networks for meanGT 1 to ",MaxMeanGT, " with AddingRandomly Distribution10.RData"))

#Normal
cat("Get Networks via lapply\n")
Networks_Results_Normal10 = lapply(1:MaxMeanGT, function(i){
  Network=buildNetworks_Normal(i)
  return(Network)
})
cat("Save Network Results\n")
save(Networks_Results_Normal10, file = paste0("CPETransmissionChains/Network Results/",startDate, " to ", endDate, " Networks for meanGT 1 to ",MaxMeanGT, " with Normal Distribution10.RData"))

############################################

# cat("Maximum meanGT time to test\n")
# MaxMeanGT=60
# 
# #NonTransformed
# cat("Get Networks via lapply\n")
# Networks_Results_NonTransformed6 = lapply(1:MaxMeanGT, function(i){
#   Network=buildNetworks_NonTransformed(i)
#   return(Network)
# })
# cat("Save Network Results\n")
# save(Networks_Results_NonTransformed6, file = paste0("CPETransmissionChains/Network Results/",startDate, " to ", endDate, " Networks for meanGT 1 to ",MaxMeanGT, " with NonTransformed Distribution6.RData"))
# 
# #NonTransformed
# cat("Get Networks via lapply\n")
# Networks_Results_NonTransformed7 = lapply(1:MaxMeanGT, function(i){
#   Network=buildNetworks_NonTransformed(i)
#   return(Network)
# })
# cat("Save Network Results\n")
# save(Networks_Results_NonTransformed7, file = paste0("CPETransmissionChains/Network Results/",startDate, " to ", endDate, " Networks for meanGT 1 to ",MaxMeanGT, " with NonTransformed Distribution7.RData"))
# 
# #NonTransformed
# cat("Get Networks via lapply\n")
# Networks_Results_NonTransformed8 = lapply(1:MaxMeanGT, function(i){
#   Network=buildNetworks_NonTransformed(i)
#   return(Network)
# })
# cat("Save Network Results\n")
# save(Networks_Results_NonTransformed8, file = paste0("CPETransmissionChains/Network Results/",startDate, " to ", endDate, " Networks for meanGT 1 to ",MaxMeanGT, " with NonTransformed Distribution8.RData"))
# 
# #NonTransformed
# cat("Get Networks via lapply\n")
# Networks_Results_NonTransformed9 = lapply(1:MaxMeanGT, function(i){
#   Network=buildNetworks_NonTransformed(i)
#   return(Network)
# })
# cat("Save Network Results\n")
# save(Networks_Results_NonTransformed9, file = paste0("CPETransmissionChains/Network Results/",startDate, " to ", endDate, " Networks for meanGT 1 to ",MaxMeanGT, " with NonTransformed Distribution9.RData"))



######################
#### LOAD NETWORKS ####

# cat("Load Network Results for meanGT 1-60, Jan-June 2015\n")

#########################################
#### COMPONENTS ANALYSIS PER NETWORK ####

cat("Set min_support\n")
min_support=0.05

cat("Run Components Test\n")
Components_Results_Normal10=lapply(1:length(Networks_Results_Normal10), function(i){
  Network=Networks_Results_Normal10[[i]]
  Network_Plot=plot(Network, type="network", min_support = min_support)
  Network_Edgelist=cbind(Network_Plot$x$edges$from, Network_Plot$x$edges$to)
  Graph=graph_from_edgelist(Network_Edgelist)
  ConnectedGraph=Graph-V(Graph)[degree(Graph)==0]
  Components=components(ConnectedGraph)
  Result=list(as.integer(i), Components$no, length(Components$membership))
  return(Result)
})

Components_Results_Normal10=data.frame(matrix(unlist(Components_Results_Normal10), ncol=3, byrow=T))
colnames(Components_Results_Normal10)=c("meanGT", "Clusters", "Nodes")

cat("Save Components Results\n")
save(Components_Results_Normal10, file = paste0("CPETransmissionChains/Components Results/",startDate, " to ", endDate, " Components Results for meanGT 1 to ",MaxMeanGT, " with Normal10 Distribution and min_support=",min_support,".RData"))

###################################
#### GET NETWORKS VIA PARALLEL ####

# cat("Numer of cores to use\n")
# cores=5
# 
# cat("Maximum meanGT time to test\n")
# MaxMeanGT=5
# 
# cat("Make clusters for parallel\n")
# cl=makeCluster(cores)
# registerDoSNOW(cl)
# getDoParWorkers()
# 
# cat("RUN PARALLEL\n")
# Networks_Results <- foreach(i=1:MaxMeanGT, .verbose=T, .packages = c("foreach","outbreaker2", "igraph", "R0")) %dopar% buildNetworks(i)
# 
# cat("Stop parallel\n")
# stopCluster(cl)
# print("Cluster stopped")
# registerDoSEQ()

################################################################################
#### ANALYSIS OF THE NUMBER OF COMPONENTS (CHAINS) PER NETWORK VIA PARALLEL ####

# getComponents=function(i, Networks_Results, min_support){
#   Network=Networks_Results[[i]]
#   Network_Plot=plot(Network, type="network", min_support = min_support)
#   Network_Edgelist=cbind(Network_Plot$x$edges$from, Network_Plot$x$edges$to)
#   Graph=graph_from_edgelist(Network_Edgelist)
#   ConnectedGraph=Graph-V(Graph)[degree(Graph)==0]
#   Components=components(ConnectedGraph)
#   return(list(as.integer(i), Components$no, length(Components$membership)))
# }
# 
# cat("Numer of cores to use\n")
# cores=5
# 
# cat("Maximum GT time to test\n")
# maxGT=5
# 
# cat("Make clusters for parallel\n")
# cl=makeCluster(cores)
# registerDoSNOW(cl)
# getDoParWorkers()
# 
# cat("RUN PARALLEL\n")
# Components_Results <- foreach(i=1:length(Networks_Results), .combine='rbind', .verbose=T, .packages = c("foreach","igraph")) %dopar% getComponents(i, Networks_Results, 0.05)
# 
# cat("Stop parallel\n")
# stopCluster(cl)
# print("Cluster stopped")
# registerDoSEQ()
