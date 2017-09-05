########################################################
#####CPE Transmission Chains: Network Plot Comparison###
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

# cat("Set Working Environment\n")
# envNN=T
# if(envNN){
#   currentwd=setwd("C:/Users/Narimane/Dropbox/CPE Transmission Chains")
# }else{
#   currentwd=setwd("/Users/pascalcrepey/Google Drive/1-EPC/stageNN/") 
# }

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("Generation Time Sensitivity Analysis Functions.R", 
       local = FALSE, verbose = getOption("verbose"))

###########################
#### LOAD DEPT NETWORK ####

cat("Upload Department Contact Network\n")
dbloc="/Users/pascalcrepey/Dropbox/"
#dbloc="..
load(paste0(dbloc,"Hospital_Network/HospitalNetwork/Data/Department Network.RData"))

#########################################
#### GET DATA ON OXA-48 CPE EPISODES ####

cat("Choose start date\n")
startDate="2015-01-01"

cat("Choose end date\n")
endDate="2015-01-15"

cat("Load data\n")
wd=getwd()
setwd(paste0(dbloc,"CPE Transmission Chains"))
dsorted=getData(startDate, endDate)
setwd(wd)
#########################
#### MEAN GT TO TEST ####

cat("MeanGT time\n")
meanGT=30

#########################
#### Run Outbreaker2 ####

cat(paste("(Poisson) Transformation Function for meanGT =", meanGT, "\n"))
myData=getCaseDataForPoissonTransformedDates(meanGT)

cat(paste("Calculate generation time for meanGT =", meanGT, "\n"))
gentime=generation.time("gamma", c(meanGT, meanGT/2), truncate = nrow(myData))
gentime=gentime$GT
gentime=gentime[gentime>0]

cat("Get Contact Network\n")
myContacts=getContactNetwork(myData)

cat("Configure Ancesteries\n")
myConfig = create_config(init_alpha = myData$initial_value, 
                         move_alpha = myData$move_alpha, 
                         init_tree = myData$initial_value)

cat(paste("Get Outbreaker Data for meanGT =", meanGT, "\n"))
myOutbreakerData=outbreaker_data(dates=myData$Dates, 
                                 dna=NULL,
                                 ctd=myContacts,
                                 w_dens=gentime)

cat(paste("Run Outbreaker2  for meanGT =", meanGT, "\n"))
OutbreakerResult=outbreaker(data = myOutbreakerData, config=myConfig)

###########################
#### GET NETWORK GRAPH ####

min_support=0.05

cat("Get Network Plot\n")
Network_Plot <- plot(OutbreakerResult, type = "network", min_support = min_support)
Network_Plot

###########################
#### GET CONSENSUS TREE ###

cat("Get Consensus Tree Plot\n")
tree=summary(OutbreakerResult)$tree
final_tree=tree[which(complete.cases(tree) & tree$support >= 0.05),]

final_edgelist=final_tree[,1:2]
Graph=graph_from_edgelist(as.matrix(final_edgelist))
ConnectedGraph=Graph-V(Graph)[degree(Graph)==0]

plot(ConnectedGraph)

cat("Set node attributes\n")
V(ConnectedGraph)$degree=degree(ConnectedGraph)
V(ConnectedGraph)$indegree=degree(ConnectedGraph, mode="in")
V(ConnectedGraph)$outdegree=degree(ConnectedGraph, mode="out")
V(ConnectedGraph)$betweenness=betweenness(ConnectedGraph)
V(ConnectedGraph)$closeness=closeness(ConnectedGraph)
# V(ConnectedGraph)$dept=

E(ConnectedGraph)$time=final_tree[,3]

outdegree=V(ConnectedGraph)$outdegree
indegree=V(ConnectedGraph)$indegree

cat("Set plot layout\n")
layout=layout_with_fr(ConnectedGraph)

cat("Set color palette\n")
resolution=20
colors=colorRampPalette(c("white", "red"))(resolution)
graphColor_outdegree=colors[as.numeric(cut(outdegree,breaks = resolution))]
graphColor_indegree=colors[as.numeric(cut(indegree,breaks = resolution))]

cat("Plot outdegree\n")
plot_outdegree=plot(ConnectedGraph, edge.arrow.size=.5, 
                    vertex.color=graphColor_outdegree, 
                    vertex.size=12, 
                    vertex.frame.color="black", vertex.label.color="black", 
                    vertex.label.cex=1, vertex.label.dist=0, edge.curved=0.2, layout=layout,
                    main="Transmission Chain Predictions, Episodes with Highest Outdegree") 

cat("Plot indegree\n")
plot_indegree=plot(ConnectedGraph, edge.arrow.size=.5, 
                   vertex.color=graphColor_indegree, 
                   vertex.size=V(ConnectedGraph)$degree, 
                   vertex.frame.color="black", vertex.label.color="black", 
                   vertex.label.cex=0.8, vertex.label.dist=0, edge.curved=0.2, layout=layout,
                   main="Transmission Chain Predictions, Episodes with Highest Indegree") 

##################################
#### GET TEMPORAL GRAPH OF TREE ##

g=ConnectedGraph

#time in the edges goes from 1 to 300. We kick off at time 3
ti <- -40
#remove edges which are not present
gt <- delete_edges(g,which(E(g)$time > ti))
#generate first layout using graphopt with normalized coordinates. This places the initially connected set of nodes in the middle. If you use fruchterman.reingold it will place that initial set in the outer ring.
layout.old <- norm_coords(layout.graphopt(gt), xmin = -1, xmax = 1, ymin = -1, ymax = 1)

#total time of the dynamics
total_time <- max(E(g)$time)
#This is the time interval for the animation. In this case is taken to be 1/10
#of the time (i.e. 10 snapshots) between adding two consecutive nodes
dt <- 1
#Output for each frame will be a png with HD size 1600x900 <img draggable="false" class="emoji" alt="🙂" src="https://s.w.org/images/core/emoji/2.3/svg/1f642.svg">
png(file="Consensus Tree Over Time.png", width=1600,height=900)
#Time loop starts
for(time in seq(3,total_time,dt)){
  #remove edges which are not present
  gt <- delete_edges(g,which(E(g)$time > time))
  #with the new graph, we update the layout a little bit
  layout.new <- layout_with_fr(gt,coords=layout.old,niter=10,start.temp=0.05,grid="nogrid")
  #plot the new graph
  plot(gt,layout=layout.new,vertex.label="",vertex.size=1+2*log(degree(gt)),vertex.frame.color=V(g)$color,edge.width=1.5,asp=9/16,margin=-0.15)
  #use the new layout in the next round
  layout.old <- layout.new
}
dev.off()
