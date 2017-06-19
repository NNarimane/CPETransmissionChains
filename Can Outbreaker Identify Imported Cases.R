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

#######################################
#### NUMBER OF NETWORKS TO COMPARE ####

cat("Set number of repetitions\n")
repetitions=10

#########################
#### MEAN GT TO TEST ####

cat("MeanGT time to test\n")
meanGT_toTest=30

cat("Set meanGT for each network to test\n")
meanGT=rep(meanGT_toTest, repetitions)

##################################################
#### GET/LOAD NETWORKS *WITH* ANCESTOR CONFIG ####

cat("Get Networks via lapply\n")
Networks_Results = lapply(meanGT, function(i){
  Network=buildNetworks_test(i)
  return(Network)
})

cat("Save Network Results\n")
save(Networks_Results, file = paste0("CPETransmissionChains/Network Results/",startDate, " to ", endDate, " ", repetitions," Networks for meanGT equals ",meanGT_toTest, " with Poisson Distribution with move_alpha.RData"))

# cat("Load Network Results for meanGT 1-60, Jan-June 2015\n")
# load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Network Results/2015-01-01 to 2015-02-28 Networks for meanGT equals 20 with Poisson Distribution.RData")

#####################################################
#### GET/LOAD NETWORKS *WITHOUT* ANCESTOR CONFIG ####

cat("Get Networks via lapply\n")
Networks_Results_NoAncestorConfig = lapply(meanGT, function(i){
  Network=buildNetworks_NoAncestorConfig(i)
  return(Network)
})

cat("Save Network Results\n")
save(Networks_Results_NoAncestorConfig, file = paste0("CPETransmissionChains/Network Results/",startDate, " to ", endDate, " ", repetitions," NonConfig Networks for meanGT equals ",meanGT_toTest, " with Poisson Distribution.RData"))

# cat("Load Network Results for meanGT 1-60, Jan-June 2015\n")
# load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Network Results/2015-01-01 to 2015-02-28 NonConfig Networks for meanGT equals 20 with Poisson Distribution.RData")

##############################
#### GET CONNECTED GRAPHS ####

cat("Set min_support to 5%\n")
min_support=0.05

# cat("Get Plots for Network w/Configuration\n")
# Network_Plots_Config=lapply(1:repetitions, function(i){
#   
#   cat("Get Plot\n")
#   Network_Plot <- plot(Networks_Results[[i]], type = "network", min_support = min_support)
#   
#   cat("Get iGraph of Plot\n")
#   Graph=graph.data.frame(Network_Plot$x$edges, vertices = Network_Plot$x$nodes)
#   
#   cat("Get Connected Graph\n")
#   Connected_Graph=Graph-V(Graph)[degree(Graph)==0]
#   
#   cat("Get Degree, InDegree, OutDegree\n")
#   V(Connected_Graph)$degree=degree(Connected_Graph, mode="all")
#   V(Connected_Graph)$indegree=degree(Connected_Graph, mode="in")
#   V(Connected_Graph)$outdegree=degree(Connected_Graph, mode="out")
#   
#   return(Connected_Graph)
# })
# 
# cat("Get Plots for Network *w/o* Configuration\n")
# Network_Plots_NoAncestorConfig=lapply(1:repetitions, function(i){
#   
#   cat("Get Plot\n")
#   Network_Plot <- plot(Networks_Results_NoAncestorConfig[[i]], type = "network", min_support = min_support)
#   
#   cat("Get iGraph of Plot\n")
#   Graph=graph.data.frame(Network_Plot$x$edges, vertices = Network_Plot$x$nodes)
#   
#   cat("Get Connected Graph\n")
#   Connected_Graph=Graph-V(Graph)[degree(Graph)==0]
#   
#   cat("Get Degree, InDegree, OutDegree\n")
#   V(Connected_Graph)$degree=degree(Connected_Graph, mode="all")
#   V(Connected_Graph)$indegree=degree(Connected_Graph, mode="in")
#   V(Connected_Graph)$outdegree=degree(Connected_Graph, mode="out")
#   
#   return(Connected_Graph)
# })

cat("Get Plots for Network w/Configuration\n")
Network_Plots_Config=lapply(1:repetitions, function(i){
  
  cat("Get Plot\n")
  tree=summary(Networks_Results[[i]])$tree
  final_tree=tree[which(complete.cases(tree) & tree$support >= min_support),]
  final_edgelist=cbind(as.character(final_tree$from), as.character(final_tree$to))
  
  cat("Get iGraph of Tree\n")
  Graph=graph_from_edgelist(as.matrix(final_edgelist))

  cat("Get Connected Graph\n")
  Connected_Graph=Graph-V(Graph)[degree(Graph)==0]
  
  cat("Get Degree, InDegree, OutDegree\n")
  V(Connected_Graph)$degree=degree(Connected_Graph, mode="all")
  V(Connected_Graph)$indegree=degree(Connected_Graph, mode="in")
  V(Connected_Graph)$outdegree=degree(Connected_Graph, mode="out")
  
  return(Connected_Graph)
})

cat("Get Plots for Network *w/o* Configuration\n")
Network_Plots_NoAncestorConfig=lapply(1:repetitions, function(i){
  
  cat("Get Plot\n")
  tree=summary(Networks_Results_NoAncestorConfig[[i]])$tree
  final_tree=tree[which(complete.cases(tree) & tree$support >= min_support),]
  final_edgelist=cbind(as.character(final_tree$from), as.character(final_tree$to))
  
  cat("Get iGraph of Tree\n")
  Graph=graph_from_edgelist(as.matrix(final_edgelist))
  
  cat("Get Connected Graph\n")
  Connected_Graph=Graph-V(Graph)[degree(Graph)==0]
  
  cat("Get Degree, InDegree, OutDegree\n")
  V(Connected_Graph)$degree=degree(Connected_Graph, mode="all")
  V(Connected_Graph)$indegree=degree(Connected_Graph, mode="in")
  V(Connected_Graph)$outdegree=degree(Connected_Graph, mode="out")
  
  return(Connected_Graph)
})

#############################
#### TRUE IMPORTED CASES ####

cat("Get Imported Cases Data\n")
myData=getCaseDataForPoissonTransformedDates(meanGT_toTest)
myImportedCases=rownames(myData[which(myData$Imported == "O"),])
print(length(myImportedCases))

################################
#### WHICH NETWORKS TO TEST ####

WithConfig=T

if(WithConfig){
  Plots=Network_Plots_Config
}else{
  Plots=Network_Plots_NoAncestorConfig
}

#######################
#### TRUE POSTIVES ####

cat("Identify Target Cases in Networks with Config (indegree > 0)\n")
Uniquely_Source_Cases=lapply(1:repetitions, function(i){
  Connected_Graph=Plots[[i]]
  Uniquely_Source_Cases=V(Connected_Graph)$name[V(Connected_Graph)$indegree == 0]
  return(Uniquely_Source_Cases)
})

cat("True Positives (Target Cases That Are Imported)\n")
True_Positives=lapply(1:repetitions, function(i){
  Uniquely_Source_Cases=Uniquely_Source_Cases[[i]]
  True_Positives=Uniquely_Source_Cases[which(Uniquely_Source_Cases %in% myImportedCases)]
  return(True_Positives)
})

cat("Counts of True Positives (Target Cases That Are Imported) per Network\n")
True_Positives_Counts=lapply(1:repetitions, function(i){
  True_Positives=True_Positives[[i]]
  True_Positives_Countes=length(True_Positives)
  return(True_Positives_Countes)
})

cat("Mean Number of True Positives (Target Cases That Are Imported)\n")
Mean_True_Positives=mean(unlist(True_Positives_Counts))


########################
#### FALSE POSTIVES ####

#Cannot calculate because do not know true number of non-imported cases that are not ancestors


#########################
#### FALSE NEAGTIVES ####

cat("Identify Target Cases in Networks with Config (indegree > 0)\n")
Target_Cases=lapply(1:repetitions, function(i){
  Connected_Graph=Plots[[i]]
  Target_Cases=V(Connected_Graph)$name[V(Connected_Graph)$indegree > 0]
  return(Target_Cases)
})

cat("True Positives (Target Cases That Are Imported)\n")
False_Positives=lapply(1:repetitions, function(i){
  Target_Cases=Target_Cases[[i]]
  False_Positives=Target_Cases[which(Target_Cases %in% myImportedCases)]
  return(False_Positives)
})

cat("Counts of True Positives (Target Cases That Are Imported) per Network\n")
False_Positives_Counts=lapply(1:repetitions, function(i){
  False_Positives=False_Positives[[i]]
  False_Positives_Countes=length(False_Positives)
  return(False_Positives_Countes)
})

cat("Mean Number of True Positives (Target Cases That Are Imported)\n")
Mean_False_Positives=mean(unlist(False_Positives_Counts))

########################
#### TRUE NEGATIVES ####

#Cannot calculate because do not know true number of non-imported cases that not ancestors


#####################
#### SENSITIVITY ####

Sensitivity=Mean_True_Positives/(Mean_True_Positives+Mean_False_Positives)*100
Sensitivity

#72% for 2 networks with configuration, meanGT 20
#0% for 2 networks without configuration, meanGT 20

#70% for 5 networks with configuration, meanGT 30
#0% for 5 networks without configuration, meanGT 30

#52% for 5 networks with configuration, meanGT 20
#0% for 5 networks without configuration, meanGT 20

#75.7% for 5 networks with configuration, meanGT 20, without move_alpha



###########################################################################################

# cat("Min_support to Test\n")
# min_support=0.05
# 
# cat("Calculate Number of Imported Cases that are Targets in Each Network, for Networks_Results\n")
# ImportedTargets_Networks_Results=lapply(1:length(Networks_Results), function(i){
#   cat("Get Graph for Networks_Results\n")
#   Network=Networks_Results[[i]]
#   Network_Plot=plot(Network, type="network", min_support = min_support)
#   Network_Edgelist=as.data.frame(cbind(as.character(Network_Plot$x$edges$from), as.character(Network_Plot$x$edges$to)), stringsAsFactors = F)
#   
#   cat("How many imported cases are targets?\n")
#   count(Network_Edgelist[,2] %in% myImportedCases)
#   
#   cat("Which imported cases are targets?\n")
#   Network_Edgelist_Imported=cbind(Network_Edgelist, Network_Edgelist[,2] %in% myImportedCases)
#   ImportedCases_AsTarget=unique(Network_Edgelist_Imported[which(Network_Edgelist_Imported[,3]==TRUE),2]) 
#   
#   ImportedTargets=length(ImportedCases_AsTarget)
#   return(ImportedTargets)
# })
# 
# cat("Mean Number of Imported Cases that are Targets in Each Network, for Networks_Results\n")
# mean(unlist(ImportedTargets_Networks_Results))
# 
# cat("Calculate Number of Imported Cases that are Targets in Each Network, for Networks_Results_NoAncestorConfig\n")
# ImportedTargets_Networks_Results_NoAncestorConfig=lapply(1:length(Networks_Results_NoAncestorConfig), function(i){
#   cat("Get Graph for Networks_Results_NoAncestorConfig\n")
#   Network_NoAncestorConfig=Networks_Results_NoAncestorConfig[[i]]
#   Network_Plot_NoAncestorConfig=plot(Network_NoAncestorConfig, type="network", min_support = min_support)
#   Network_Edgelist_NoAncestorConfig=as.data.frame(cbind(as.character(Network_Plot_NoAncestorConfig$x$edges$from), as.character(Network_Plot_NoAncestorConfig$x$edges$to)), stringsAsFactors = F)
#   
#   cat("How many imported cases are targets?\n")
#   count(Network_Edgelist_NoAncestorConfig[,2] %in% myImportedCases)
#   
#   cat("Which imported cases are targets?\n")
#   Network_Edgelist_Imported_NoAncestorConfig=cbind(Network_Edgelist_NoAncestorConfig, Network_Edgelist_NoAncestorConfig[,2] %in% myImportedCases)
#   ImportedCases_AsTarget_NoAncestorConfig=unique(Network_Edgelist_Imported_NoAncestorConfig[which(Network_Edgelist_Imported_NoAncestorConfig[,3]==TRUE),2]) #42 total cases
#   
#   ImportedTargets_NoAncestorConfig=length(ImportedCases_AsTarget_NoAncestorConfig)
#   return(ImportedTargets_NoAncestorConfig)
# })
# 
# cat("Mean Number of Imported Cases that are Targets in Each Network, for Networks_Results_NoAncestorConfig\n")
# mean(unlist(ImportedTargets_Networks_Results_NoAncestorConfig))

######################iGraph

# cat("Identify Number and Name of Imported Cases As Targets\n")
# Nodes_InDegreeMoreThan0=V(Graph_connected)$name[Graph_connected$indegree > 0]
# count(Nodes_InDegreeMoreThan0 %in% myImportedCases)
# Nodes_InDegreeMoreThan0[which(Nodes_InDegreeMoreThan0 %in% myImportedCases)]


#####################

# getAncestries_2=function(myData){
#   cat("Set imported ancestor as 'self' and non-imported as NA to estimate their ancestor\n")
#   myData=myData[order(myData$DateEpisode),]
#   rownumber=cbind(as.numeric(rownames(myData[myData$Imported == "N",])), as.numeric(rownames(myData[myData$Imported == "N",])))
#   myData$rownumber=as.numeric(rownames(myData))
#   myData=merge(myData, rownumber, by.x=("rownumber"), by.y="V1", all.x=T)
#   myData$moves=(myData$Imported == "N")
#   cat("Configure\n")
#   config = create_config(init_alpha = myData$V2, 
#                          # init_tree = myData$V2, 
#                          move_alpha = myData$moves)
#   return(config)
# }
# buildNetworks_2<-function(meanGT){
#   
#   cat(paste("Calculate generation time for meanGT =", meanGT, "\n"))
#   gentime=generation.time("gamma", c(meanGT, meanGT/2))
#   gentime=gentime$GT
#   
#   cat(paste("(Poisson) Transformation Function for meanGT =", meanGT, "\n"))
#   myData=getCaseDataForPoissonTransformedDates(meanGT)
#   
#   cat("Get Contact Network\n")
#   myContacts=getContactNetwork(myData)
#   
#   cat("Get Ancesteries\n")
#   myAncestries=getAncestries_2(myData)
#   
#   cat(paste("Get Outbreaker Data for meanGT =", meanGT, "\n"))
#   myOutbreakerData=outbreaker_data(dates=myData$Dates, 
#                                    dna=NULL,
#                                    ctd=myContacts,
#                                    w_dens=gentime)
#   
#   cat(paste("Run Outbreaker2  for meanGT =", meanGT, "\n"))
#   OutbreakerResult=outbreaker(data = myOutbreakerData, config=myAncestries)
#   
#   return(OutbreakerResult)
# }
# 
# Network_2=buildNetworks_2(14)
# 
# cat("Get Plot\n")
# Network_Plot <- plot(Network_2, type = "network", min_support = 0.05)
# class(Network_Plot)
# head(Network_Plot$x$edges)
# head(Network_Plot$x$nodes)
# 
# cat("Get iGraph of Plot\n")
# Graph=graph.data.frame(temp$x$edges, vertices = temp$x$nodes[1:4])
# 
# cat("Get Connected Graph\n")
# Graph_connected=Graph-V(Graph)[degree(Graph)==0]
# 
# cat("Plot Connected Graph\n")
# plot(Graph_connected, layout = layout.circle, vertex.size=2, 
#      # vertex.label=NA,
#      main = "Null model, posterior trees")
# 
# cat("Get InDegree\n")
# Graph_connected$indegree=degree(Graph_connected, mode="in")
# 
# cat("Identify Number and Name of Imported Cases As Targets\n")
# Nodes_InDegreeMoreThan0=V(Graph_connected)$name[Graph_connected$indegree > 0]
# count(Nodes_InDegreeMoreThan0 %in% myImportedCases)
# Nodes_InDegreeMoreThan0[which(Nodes_InDegreeMoreThan0 %in% myImportedCases)]
