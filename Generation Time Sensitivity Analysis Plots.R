###############################################################################
#####CPE Transmission Chains: Generation Time Sensitivity Analysis RESULTS#####
###############################################################################

cat("Set Working Environment\n")
envNN=T
if(envNN){
  currentwd=setwd("C:/Users/Narimane/Dropbox/CPE Transmission Chains")
}else{
  currentwd=setwd("/Users/pascalcrepey/Google Drive/1-EPC/stageNN/") 
}

# install.packages("smooth")
library("smooth")

##################################################################
##Mean of 5 Runs of Poisson Distribution (pre-move_alpha config)##

#### LOAD RESULTS ####

cat("Load Components Results1 for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results1 for meanGT 1 to 60 with Poisson Distribution and min_support=0.05.RData")

cat("Load Components Results2 for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results2 for meanGT 1 to 60 with Poisson Distribution and min_support=0.05.RData")

cat("Load Components Results3 for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results3 for meanGT 1 to 60 with Poisson Distribution and min_support=0.05.RData")

cat("Load Components Results4 for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results4 for meanGT 1 to 60 with Poisson Distribution and min_support=0.05.RData")

cat("Load Components Results5 for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results5 for meanGT 1 to 60 with Poisson Distribution and min_support=0.05.RData")

#### GET MEAN OF RESULTS ####

meanGT=Components_Results1$meanGT
meanClusters=rowMeans(cbind(Components_Results1$Clusters, Components_Results2$Clusters, Components_Results3$Clusters,
                            Components_Results4$Clusters, Components_Results5$Clusters))
meanNodes=rowMeans(cbind(Components_Results1$Nodes, Components_Results2$Nodes, Components_Results3$Nodes,
                         Components_Results4$Nodes, Components_Results5$Nodes))
Mean_Components_Results=as.data.frame(cbind(meanGT, meanClusters, meanNodes))

#### PLOT RESULTS ####

par(mfrow=c(2,1))
min_support="0.05,"
time=" Jan-June 2015"

####################
#Plot 5 Runs, Nodes#

plot(Components_Results1[,3], type="l", col="black", ylim=c(15,120), 
     main=paste0("#Nodes by MeanGT of 5 Runs, ", "min_support=", min_support, time),
     ylab="Number of Nodes", xlab="Mean Generation Time")
lines(Components_Results2[,3], col="red")
lines(Components_Results3[,3], col="blue")
lines(Components_Results4[,3], col="green")
lines(Components_Results5[,3], col="purple")
#Plot 5 Runs, Clusters
plot(Components_Results1[,2], type="l", col="black", ylim=c(0,22),
     main=paste0("#Clusters by MeanGT of 5 Runs, ", "min_support=", min_support, time),
     ylab="Number of Clusters", xlab="Mean Generation Time")
lines(Components_Results2[,2], col="red")
lines(Components_Results3[,2], col="blue")
lines(Components_Results4[,2], col="green")
lines(Components_Results5[,2], col="purple")

#################
#Plot Mean Nodes#

plot(Mean_Components_Results[,3], 
     ylab="Number of Nodes", xlab="Mean Generation Time", pch=16, col="red", type="l",
     main=paste0("Mean #Nodes by MeanGT of 5 Runs, ", "min_support=", min_support, time1))

#Plot Mean Clusters
plot(Mean_Components_Results[,2], 
     ylab="Number of Clusters", xlab="Mean Generation Time", pch=16, col="red", type="l",
     main=paste0("Mean #Clusters by MeanGT of 5 Runs, ", "min_support=", min_support, time1))

#####################
#Plot Moving Average#

cat("Calculate Moving Average\n")
plot_Nodes_JanJune2015=sma(Mean_Components_Results[,3])
plot_Clusters_JanJune2015=sma(Mean_Components_Results[,2])

#Plot Mean Number of Nodes of Moving Average
plot(plot_Nodes_JanJune2015$fitted, 
     ylab="Number of Nodes", xlab="Mean Generation Time", pch=16, col="red",
     main=paste0("Mean #Nodes by MeanGT of 5 Runs, ", "min_support=", min_support, time1))
grid(col="lightgray")

#Plot Mean Number of Clusters of Moving Average
plot(plot_Clusters_JanJune2015$fitted, 
     ylab="Number of Clusters", xlab="Mean Generation Time", pch=16, col="red",
     main=paste0("Mean #Clusters by MeanGT of 5 Runs, ", "min_support=", min_support, time1))
grid(col="lightgray")

#######################################
##Different Case Date Transformations##

#### LOAD RESULTS ####

cat("Load Components Results (Poisson) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results1 for meanGT 1 to 60 with Poisson Distribution and min_support=0.05.RData")

cat("Load Components Results (NonTransformed) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with NonTransformed Distribution and min_support=0.05.RData")

cat("Load Components Results (AddingRandomly) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with AddingRandomly Distribution and min_support=0.05.RData")

cat("Load Components Results (Normal) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Normal Distribution and min_support=0.05.RData")

####Moving Average####

plot_NonTransformed=sma(Components_Results_NonTransformed[,3])
plot_AddingRandomly=sma(Components_Results_AddingRandomly[,3])
plot_Normal=sma(Components_Results_Normal[,3])
plot_Poisson=sma(Components_Results_Poisson[,3])

####Paramaters####

min_support="0.05,"
time=" Jan-June 2015"

####Plot####

plot(plot_Poisson$fitted, 
     type="l",
     col="black", 
     # ylim=c(15,120), 
     main=paste0("#Nodes by MeanGT, Case Date Transformations, ", "min_support=", min_support, time),
     ylab="Number of Nodes (Moving Average)", xlab="Mean Generation Time")
# lines(plot_AddingRandomly$fitted, col="red")
lines(plot_Normal$fitted, col="blue")
lines(plot_NonTransformed$fitted, col="green")
# lines(plot_Nodes_JanJune2015$fitted, col="green")
grid(col="lightgray")
par(xpd=FALSE)
legend(14,33, 
       legend=c("Non-Transformed Dates (Episode Dates)",
                       "Adding Random Values 1 to meanGT", 
                       "Adding Normal Distribution to Dates (1 to meanGT)", 
                      "Adding Poisson Distribution to Dates (1 to meanGT) - Average of 5 Results"), 
       pch = c(16,16,16,16), 
       col = c("black","red","blue","green"),
       cex=0.7)



#######################################
##New Poisson Results with move_alpha##

#### LOAD RESULTS ####

cat("Load Components Results (Poisson) for meanGT 1-60, min_supp=0.05, Jan-June 2015, move_alpha\n")
load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Poisson Distribution and min_support=0.05.RData")

####Moving Average####

plot_Poisson=sma(Components_Results_Poisson[,3])

####Paramaters####

min_support="0.05,"
time=" Jan-June 2015"
par(mfrow=c(2,1))

####Plot####

plot(plot_Poisson$fitted, 
     type="l",
     col="red", 
     ylim=c(20,120),
     ylab="Number of Nodes", xlab="Mean Generation Time")
title(main=paste0("#Nodes by MeanGT, Poisson, ", "min_support=", min_support, time))
lines(Components_Results_Poisson[,3], col="gray")
grid(col="lightgray")
legend(40,110,
       legend = c("Real Values", "Moving Average"),
       lwd=c(2.5,2.5),
       col = c("gray", "red"))

plot(plot_Poisson$fitted, 
     type="l",
     col="red", 
     # ylim=c(20,120),
     main="Zoom on moving average results",
     ylab="Number of Nodes", xlab="Mean Generation Time")
grid(col="lightgray")
