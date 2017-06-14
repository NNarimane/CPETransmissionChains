###################################################
#####CPE Transmission Chains: Network Analysis#####
###################################################

#################################
#### INSTALL & LOAD PACKAGES ####

cat("Load Packages\n")
library("data.table")
library("igraph")
library("stringr")
library("visNetwork")
library("plyr")
library("foreach")
library("grid")
library("gridExtra")

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

####################
#### PARAMETERS ####

meanGT = 38
min_support = 0.05

#########################################
#### GET DATA ON OXA-48 CPE EPISODES ####

cat("Choose start date\n")
startDate="2015-01-01"

cat("Choose end date\n")
endDate="2015-06-30"

cat("Load data\n")
dsorted=getData(startDate, endDate)

cat(paste("(Poisson) Transformation Function for meanGT =", meanGT, "\n"))
myData=getCaseDataForPoissonTransformedDates(meanGT)

##############################
#### LOAD NETWORK RESULTS ####

cat("Load Network Results for meanGT 1-60, Jan-June 2015, Poisson, move_alpha\n")
load("CPETransmissionChains/Network Results/2015-01-01 to 2015-06-30 Networks for meanGT 1 to 60 with Poisson Distribution.RData")

##################################
#### SELECT CONNECTED NETWORK ####

cat("Get connected graph\n")
Network=Networks_Results_Poisson[[meanGT]]
Network_Plot=plot(Network, type="network", min_support = min_support)
Network_Edgelist=cbind(as.character(Network_Plot$x$edges$from), as.character(Network_Plot$x$edges$to))
Graph=graph_from_edgelist(Network_Edgelist, directed = T)
ConnectedGraph=Graph-V(Graph)[degree(Graph)==0]
plot(ConnectedGraph)

##########################################
#### PLOT NETWORK BY IN AND OUTDEGREE ####

cat("Set node attributes\n")
V(ConnectedGraph)$degree=degree(ConnectedGraph)
V(ConnectedGraph)$indegree=degree(ConnectedGraph, mode="in")
V(ConnectedGraph)$outdegree=degree(ConnectedGraph, mode="out")
V(ConnectedGraph)$betweenness=betweenness(ConnectedGraph)
V(ConnectedGraph)$closeness=closeness(ConnectedGraph)

outdegree=V(ConnectedGraph)$outdegree
indegree=V(ConnectedGraph)$indegree

cat("Set plot layout\n")
layout=layout.fruchterman.reingold(ConnectedGraph)

cat("Set color palette\n")
resolution=20
colors=colorRampPalette(c("white", "red"))(resolution)
graphColor_outdegree=colors[as.numeric(cut(outdegree,breaks = resolution))]
graphColor_indegree=colors[as.numeric(cut(indegree,breaks = resolution))]

cat("Plot outdegree\n")
plot_outdegree=plot(ConnectedGraph, edge.arrow.size=.5, 
     vertex.color=graphColor_outdegree, 
     # vertex.size=V(ConnectedGraph)$degree, 
     vertex.frame.color="black", vertex.label.color="black", 
     vertex.label.cex=0.8, vertex.label.dist=0, edge.curved=0.2, layout=layout,
     main="Transmission Chain Predictions, Episodes with Highest Outdegree") 

cat("Plot indegree\n")
plot_indegree=plot(ConnectedGraph, edge.arrow.size=.5, 
                    vertex.color=graphColor_indegree, 
                    # vertex.size=V(ConnectedGraph)$degree, 
                    vertex.frame.color="black", vertex.label.color="black", 
                    vertex.label.cex=0.8, vertex.label.dist=0, edge.curved=0.2, layout=layout,
                    main="Transmission Chain Predictions, Episodes with Highest Indegree") 


#####################################
#### IDENTIFY EVENTS OF INTEREST ####

cat("Get event info of connected graph\n") 
EventNodes=V(ConnectedGraph)$name
ConnectedEvents=foreach(i=1:length(EventNodes), .combine = 'rbind') %do% myData[which(myData$ID == EventNodes[i]),]
ConnectedEvents

cat("Highest outdegree event\n")
Superspreader=V(ConnectedGraph)$name[V(ConnectedGraph)$outdegree == max(V(ConnectedGraph)$outdegree)]
Superspreader_Info=foreach(i=1:length(Superspreader), .combine = 'rbind') %do% myData[which(myData$ID == Superspreader[i]),]
Superspreader_Info

cat("Highest indegree event\n")
Mostvulernable=V(ConnectedGraph)$name[V(ConnectedGraph)$indegree == max(V(ConnectedGraph)$indegree)]
Mostvulernable_Info=foreach(i=1:length(Mostvulernable), .combine = 'rbind') %do% myData[which(myData$ID == Mostvulernable[i]),]
Mostvulernable_Info


#################################
#### More Network Attributes ####

cat("Get network nodes and edges\n")
nodes=Network_Plot$x$nodes
edges=Network_Plot$x$edges

cat("Get nodes attributes for only connected nodes\n")
ConnectedEvents_Attributes=foreach(i=1:length(EventNodes), .combine = 'rbind') %do% nodes[which(nodes$id == EventNodes[i]),]

cat("Set attributes\n")
V(ConnectedGraph)$Value=ConnectedEvents_Attributes$value
V(ConnectedGraph)$color=ConnectedEvents_Attributes$color
E(ConnectedGraph)$Value=edges$value
E(ConnectedGraph)$color=edges$color

cat("plot\n")
plot(ConnectedGraph, vertex.size=V(ConnectedGraph)$Value*5)
