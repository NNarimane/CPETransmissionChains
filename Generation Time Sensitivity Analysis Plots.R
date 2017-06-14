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
library("forecast")

####################################################
##NEW RESULTS WITH MOVE_ALPHA - MULTIPLE RUNS EACH##

#### LOAD RESULTS ####

load=T
if(load){
  #POISSON
  cat("Load Components Results (Poisson) for meanGT 1-60, min_supp=0.05, Jan-June 2015, move_alpha\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Poisson Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (Poisson2) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Poisson2 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (Poisson3) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Poisson3 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (Poisson4) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Poisson4 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (Poisson5) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Poisson5 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (Poisson6) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Poisson6 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (Poisson7) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Poisson7 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (Poisson8) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Poisson8 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (Poisson9) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Poisson9 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (Poisson10) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Poisson10 Distribution and min_support=0.05.RData")
  
  #NORMAL
  cat("Load Components Results (Normal) for meanGT 1-60, min_supp=0.05, Jan-June 2015, move_alpha\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Normal Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (Normal2) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Normal2 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (Normal3) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Normal3 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (Normal4) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Normal4 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (Normal5) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Normal5 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (Normal6) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Normal6 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (Normal7) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Normal7 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (Normal8) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Normal8 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (Normal9) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Normal9 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (Normal10) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with Normal10 Distribution and min_support=0.05.RData")
  
  #ADDINGRANDOM
  cat("Load Components Results (AddingRandomly) for meanGT 1-60, min_supp=0.05, Jan-June 2015, move_alpha\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with AddingRandomly Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (AddingRandomly2) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with AddingRandomly2 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (AddingRandomly3) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with AddingRandomly3 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (AddingRandomly4) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with AddingRandomly4 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (AddingRandomly5) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with AddingRandomly5 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (AddingRandomly6) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with AddingRandomly6 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (AddingRandomly7) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with AddingRandomly7 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (AddingRandomly8) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with AddingRandomly8 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (AddingRandomly9) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with AddingRandomly9 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (AddingRandomly10) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with AddingRandomly10 Distribution and min_support=0.05.RData")
  
  #NONTRANSFORMED
  cat("Load Components Results (NonTransformed) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with NonTransformed Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (NonTransformed2) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with NonTransformed2 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (NonTransformed3) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with NonTransformed3 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (NonTransformed4) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with NonTransformed4 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (NonTransformed5) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with NonTransformed5 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (NonTransformed6) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with NonTransformed6 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (NonTransformed7) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with NonTransformed7 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (NonTransformed8) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with NonTransformed8 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (NonTransformed9) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with NonTransformed9 Distribution and min_support=0.05.RData")
  
  cat("Load Components Results (NonTransformed10) for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
  load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-30 Components Results for meanGT 1 to 60 with NonTransformed10 Distribution and min_support=0.05.RData")
  
  }

#### GET MEAN OF RESULTS ####

means=T
if(means){
  meanGT=Components_Results_Poisson$meanGT
  
  #Poisson
  meanClusters_Poisson=rowMeans(cbind(Components_Results_Poisson$Clusters, Components_Results_Poisson2$Clusters, Components_Results_Poisson3$Clusters,
                                      Components_Results_Poisson4$Clusters, Components_Results_Poisson5$Clusters, Components_Results_Poisson6$Clusters, 
                                      Components_Results_Poisson7$Clusters, Components_Results_Poisson8$Clusters, Components_Results_Poisson9$Clusters,
                                      Components_Results_Poisson10$Clusters))
  meanNodes_Poisson=rowMeans(cbind(Components_Results_Poisson$Nodes, Components_Results_Poisson2$Nodes, Components_Results_Poisson3$Nodes,
                                   Components_Results_Poisson4$Nodes, Components_Results_Poisson5$Nodes, Components_Results_Poisson6$Nodes,
                                   Components_Results_Poisson7$Nodes, Components_Results_Poisson8$Nodes, Components_Results_Poisson9$Nodes,
                                   Components_Results_Poisson10$Nodes))
  Mean_Components_Results_Poisson=as.data.frame(cbind(meanGT, meanClusters_Poisson, meanNodes_Poisson))
  
  #Normal
  meanClusters_Normal=rowMeans(cbind(Components_Results_Normal$Clusters, Components_Results_Normal2$Clusters, Components_Results_Normal3$Clusters,
                                     Components_Results_Normal4$Clusters, Components_Results_Normal5$Clusters, Components_Results_Normal6$Clusters,
                                     Components_Results_Normal7$Clusters, Components_Results_Normal8$Clusters, Components_Results_Normal9$Clusters,
                                     Components_Results_Normal10$Clusters))
  meanNodes_Normal=rowMeans(cbind(Components_Results_Normal$Nodes, Components_Results_Normal2$Nodes, Components_Results_Normal3$Nodes,
                                  Components_Results_Normal4$Nodes, Components_Results_Normal5$Nodes, Components_Results_Normal6$Nodes,
                                  Components_Results_Normal7$Nodes, Components_Results_Normal8$Nodes, Components_Results_Normal9$Nodes,
                                  Components_Results_Normal10$Nodes))
  Mean_Components_Results_Normal=as.data.frame(cbind(meanGT, meanClusters_Normal, meanNodes_Normal))
  
  #AddingRandomly
  meanClusters_AddingRandomly=rowMeans(cbind(Components_Results_AddingRandomly$Clusters, Components_Results_AddingRandomly2$Clusters, Components_Results_AddingRandomly3$Clusters,
                                             Components_Results_AddingRandomly4$Clusters, Components_Results_AddingRandomly5$Clusters,
                                             Components_Results_AddingRandomly6$Clusters, Components_Results_AddingRandomly7$Clusters,
                                             Components_Results_AddingRandomly8$Clusters, Components_Results_AddingRandomly9$Clusters,
                                             Components_Results_AddingRandomly10$Clusters))
  meanNodes_AddingRandomly=rowMeans(cbind(Components_Results_AddingRandomly$Nodes, Components_Results_AddingRandomly2$Nodes, Components_Results_AddingRandomly3$Nodes,
                                          Components_Results_AddingRandomly4$Nodes, Components_Results_AddingRandomly5$Nodes, 
                                          Components_Results_AddingRandomly6$Nodes, Components_Results_AddingRandomly7$Nodes,
                                          Components_Results_AddingRandomly8$Nodes, Components_Results_AddingRandomly9$Nodes,
                                          Components_Results_AddingRandomly10$Nodes))
  Mean_Components_Results_AddingRandomly=as.data.frame(cbind(meanGT, meanClusters_AddingRandomly, meanNodes_AddingRandomly))
  
  #NonTransformed
  meanClusters_NonTransformed=rowMeans(cbind(Components_Results_NonTransformed$Clusters, Components_Results_NonTransformed2$Clusters, Components_Results_NonTransformed3$Clusters,
                                             Components_Results_NonTransformed4$Clusters, Components_Results_NonTransformed5$Clusters, Components_Results_NonTransformed6$Clusters,
                                             Components_Results_NonTransformed7$Clusters, Components_Results_NonTransformed8$Clusters, Components_Results_NonTransformed9$Clusters,
                                             Components_Results_NonTransformed10$Clusters))
  meanNodes_NonTransformed=rowMeans(cbind(Components_Results_NonTransformed$Nodes, Components_Results_NonTransformed2$Nodes, Components_Results_NonTransformed3$Nodes,
                                          Components_Results_NonTransformed4$Nodes, Components_Results_NonTransformed5$Nodes, Components_Results_NonTransformed6$Nodes,
                                          Components_Results_NonTransformed7$Nodes, Components_Results_NonTransformed8$Nodes, Components_Results_NonTransformed9$Nodes,
                                          Components_Results_NonTransformed10$Nodes))
  Mean_Components_Results_NonTransformed=as.data.frame(cbind(meanGT, meanClusters_NonTransformed, meanNodes_NonTransformed))
}

####Moving Average of Nodes (forecast package)####

order=5

plot_NonTransformed_ma=ma(Mean_Components_Results_NonTransformed[,3], order, centre=TRUE)
plot_AddingRandomly_ma=ma(Mean_Components_Results_AddingRandomly[,3], order, centre=TRUE)
plot_Normal_ma=ma(Mean_Components_Results_Normal[,3], order, centre=TRUE)
plot_Poisson_ma=ma(Mean_Components_Results_Poisson[,3], order, centre=TRUE)

####Simple Moving Average of Nodes (smooth package)####

plot_NonTransformed_sma=sma(Mean_Components_Results_NonTransformed[,3])
plot_AddingRandomly_sma=sma(Mean_Components_Results_AddingRandomly[,3])
plot_Normal_sma=sma(Mean_Components_Results_Normal[,3])
plot_Poisson_sma=sma(Mean_Components_Results_Poisson[,3])

####Paramaters####

par(mfrow=c(1,1))
min_support="0.05,"
time=" Jan-June 2015"

####Poisson Plots####

plot(Components_Results_Poisson[,3], 
     type="l",
     col="gray", 
     ylim=c(20,65),
     main=paste0("Poisson Distribution of Case Dates\nNumber of Connected Nodes by meanGT\nOutbreaker2 config with move_alpha, ", "min_support=", min_support,time),
     ylab="Number of Nodes", xlab="Mean Generation Time",
     cex.main=0.8)
lines(Components_Results_Poisson2[,3], col="lightgray")
lines(Components_Results_Poisson3[,3], col="lightgray")
lines(Components_Results_Poisson4[,3], col="lightgray")
lines(Components_Results_Poisson5[,3], col="lightgray")
lines(Components_Results_Poisson6[,3], col="lightgray")
lines(Components_Results_Poisson7[,3], col="lightgray")
lines(Components_Results_Poisson8[,3], col="lightgray")
lines(Components_Results_Poisson9[,3], col="lightgray")

lines(Mean_Components_Results_Poisson[,3], col="red", lwd=2)

lines(plot_Poisson_ma, col="blue", lwd=2)

lines(plot_Poisson_sma$fitted, col="limegreen", lwd=2)

legend(10,27, 
       legend=c("Values",
                "Mean", 
                "Moving average with order of 5 (forecast pckg) of mean", 
                "Simple moving average (smooth pckg) of mean"), 
       pch = c(16,16,16,16), 
       col = c("lightgray","red","blue","limegreen"),
       cex=0.7)

####MA Plot####

plot(plot_NonTransformed_ma, 
     type="l",
     col="black", 
     ylim=c(25,65),
     main=paste0("Number of Connected Nodes by meanGT, Outbreaker2 config with move_alpha, ", "min_support=", min_support,time),
     ylab="Number of Nodes (Moving Average)", xlab="Mean Generation Time",
     cex.main=0.8)
lines(plot_AddingRandomly_ma, col="red")
lines(plot_Normal_ma, col="blue")
lines(plot_Poisson_ma, col="green")
grid(col="lightgray")
par(xpd=FALSE)
legend(18,34, 
       legend=c("Non-Transformed (10 runs)",
                "Adding Random Values (10 runs)", 
                "Adding Normal Dist (10 runs)", 
                "Adding Poisson Dist (10 runs)"), 
       pch = c(16,16,16,16), 
       col = c("black","red","blue","green"),
       cex=0.7)

####SMA Plot####

plot(plot_NonTransformed_sma$fitted, 
     type="l",
     col="black", 
     ylim=c(25,65),
     main=paste0("Number of Connected Nodes by meanGT, Outbreaker2 config with move_alpha, ", "min_support=", min_support,time),
     ylab="Number of Nodes (Moving Average)", xlab="Mean Generation Time",
     cex.main=0.8, bty="n")
lines(plot_AddingRandomly_sma$fitted, col="red")
lines(plot_Normal_sma$fitted, col="blue")
lines(plot_Poisson_sma$fitted, col="green")
grid(col="gray")
par(xpd=FALSE)
legend(2,65, 
       legend=c("Non-Transformed Case Dates",
                "Adding Random Values to Case Dates", 
                "Adding Normal Distribution to Case Dates", 
                "Adding Poisson Distribution to Case Dates"), 
       pch = c(16,16,16,16), 
       col = c("black","red","blue","green"),
       cex=0.7)
