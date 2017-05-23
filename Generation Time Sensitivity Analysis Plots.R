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

######################
#### LOAD RESULTS ####

cat("Load Components Results for meanGT 1-60, min_supp=0.05, Jan-June 2015\n")
# load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-01 Components Results for meanGT 1 to 60 with Poisson Distribution and min_support=0.05.RData")

cat("Load Components Results for meanGT 1-60, min_supp=0.05, June-Dec 2015\n")
# load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-06-01 to 2015-12-01 Components Results for meanGT 1 to 60 with Poisson Distribution and min_support=0.05.RData")

cat("Load Components Results for meanGT 1-60, min_supp=0.02, Jan-June 2015\n")
# load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-01 Components Results for meanGT 1 to 60 with Poisson Distribution and min_support=0.02.RData")

cat("Load Components Results for meanGT 1-60, min_supp=0.02, June-Dec 2015\n")
# load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-06-01 to 2015-12-01 Components Results for meanGT 1 to 60 with Poisson Distribution and min_support=0.02.RData")

cat("Load Components Results for meanGT 1-60, min_supp=0.01, Jan-June 2015\n")
load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-01 Components Results for meanGT 1 to 60 with Poisson Distribution and min_support=0.01.RData")

cat("Load Components Results for meanGT 1-60, min_supp=0.01, June-Dec 2015\n")
load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-06-01 to 2015-12-01 Components Results for meanGT 1 to 60 with Poisson Distribution and min_support=0.01.RData")


######################
#### PLOT RESULTS ####

par(mfrow=c(2,2))
min_support="0.01,"
time1=" Jan-June 2015"
time2=" June-Dec 2015"

#Number of Nodes
plot(Components_Results[,1], Components_Results[,4],
     type="l",
     ylab="Number of Nodes", xlab="Mean Generation Time", pch=16, col="red",
     main=paste0("#Nodes by MeanGT, ", "min_support=", min_support, time2))

#Number of Clusters
plot(Components_Results[,1], Components_Results[,2],
     # ylim=c(1,14), xlim=c(0,20),
     type="l",
     ylab="Number of Clusters", xlab="Mean Generation Time", pch=16, col="red",
     main=paste0("#Clusters by MeanGT, ", "min_support=", min_support, time2))

###############################################################################

depth.class <- cut(Components_Results[,1], c(0,5,10,15,20,25,30,35,40,45,50,55,60), include.lowest = TRUE)
Components_Results$depth.class=depth.class

Components_Results_Aggregated=aggregate(Components_Results, by=list(Components_Results$depth.class), mean)

#Number of Nodes
plot(Components_Results_Aggregated[,2], Components_Results_Aggregated[,4],
     type="l",
     ylab="Mean Number of Nodes", xlab="Mean Generation Time", pch=16, col="red",
     main=paste0("Mean Nodes by MeanGT, ", "min_support=", min_support, time2))

#Number of Clusters
plot(Components_Results_Aggregated[,2], Components_Results_Aggregated[,3],
     # ylim=c(1,14), xlim=c(0,20),
     type="l",
     ylab="Number of Clusters", xlab="Mean Generation Time", pch=16, col="red",
     main=paste0("Mean Clusters by MeanGT, ", "min_support=", min_support, time2))


# #####For min_support=0.02
# #Number of Nodes
# plot(ComponentsOfAdding1to15DaysToCaseDates_002[,1], ComponentsOfAdding1to15DaysToCaseDates_002[,3],
#      ylim=c(150,270), xlim=c(0,20),
#      ylab="Number of Nodes", xlab="Mean Generation Time", pch=16, col="red",
#      main="Distribution of Nodes and Mean GT with min_support=0.02")
# points(ComponentsOfNormalDistOfCaseDates_002[,1], ComponentsOfNormalDistOfCaseDates_002[,3], 
#        pch=16, col="blue")
# points(ComponentsOfNonRandomDistOfCaseDates_002[,1], ComponentsOfNonRandomDistOfCaseDates_002[,3], 
#        pch=16, col="green")
# legend(14,265, legend=c("Adding Random 1 + 15 Days","Normal Distribution, SD=7.5", "Non-Random Dates"), pch = c(16,16,16), col = c("red","blue","green"))
# 
# #Number of Clusters
# plot(ComponentsOfAdding1to15DaysToCaseDates_002[,1], ComponentsOfAdding1to15DaysToCaseDates_002[,2],
#      ylim=c(1,14), xlim=c(0,20),
#      ylab="Number of Clusters", xlab="Mean Generation Time", pch=16, col="red",
#      main="Distribution of Clusters and Mean GT with min_support=0.02")
# points(ComponentsOfNormalDistOfCaseDates_002[,1], ComponentsOfNormalDistOfCaseDates_002[,2], 
#        pch=16, col="blue")
# points(ComponentsOfNonRandomDistOfCaseDates_002[,1], ComponentsOfNonRandomDistOfCaseDates_002[,2], 
#        pch=16, col="green")
# legend(5,14, legend=c("Adding Random 1 + 15 Days","Normal Distribution, SD=7.5", "Non-Random Dates"), pch = c(16,16,16), col = c("red","blue","green"))
# 
# #####For min_support=0.1
# #Number of Nodes
# plot(ComponentsOfAdding1to15DaysToCaseDates_01[,1], ComponentsOfAdding1to15DaysToCaseDates_01[,3],
#      ylim=c(0,14), xlim=c(0,20),
#      ylab="Number of Nodes", xlab="Mean Generation Time", pch=16, col="red",
#      main="Distribution of Nodes and Mean GT with min_support=0.1")
# points(ComponentsOfNormalDistOfCaseDates_01[,1], ComponentsOfNormalDistOfCaseDates_01[,3], 
#        pch=16, col="blue")
# points(ComponentsOfNonRandomDistOfCaseDates_01[,1], ComponentsOfNonRandomDistOfCaseDates_01[,3],
#        pch=16, col="green")
# legend(14,14, legend=c("Adding Random 1 + 15 Days","Normal Distribution, SD=7.5", "Non-Random Dates"), pch = c(16,16,16), col = c("red","blue","green"))
# 
# #Number of Clusters
# plot(ComponentsOfAdding1to15DaysToCaseDates_01[,1], ComponentsOfAdding1to15DaysToCaseDates_01[,2],
#      ylim=c(1,5), xlim=c(0,20),
#      ylab="Number of Clusters", xlab="Mean Generation Time", pch=16, col="red",
#      main="Distribution of Clusters and Mean GT with min_support=0.1")
# points(ComponentsOfNormalDistOfCaseDates_01[,1], ComponentsOfNormalDistOfCaseDates_01[,2], 
#        pch=16, col="blue")
# points(ComponentsOfNonRandomDistOfCaseDates_01[,1], ComponentsOfNonRandomDistOfCaseDates_01[,2],
#        pch=16, col="green")
# legend(14,5, legend=c("Adding Random 1 + 15 Days","Normal Distribution, SD=7.5", "Non-Random Dates"), pch = c(16,16,16), col = c("red","blue","green"))

###############################################################################

# cat("Load old results\n")
# load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/GT Results/Phase 2/2015-01-01 to 2015-06-01 GT Sensitivity Results (with myContacts & config & 0.02 support).RData")
# ComponentsOfAdding1to15DaysToCaseDates_002=nbComps_JanJun2015
# ComponentsOfAdding1to15DaysToCaseDates_002[,1]=1:15
# 
# load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/GT Results/Phase 2/2015-01-01 to 2015-06-01 GT Sensitivity Results (with myContacts & config & 0.02 support for Normalized Case Dates).RData")
# ComponentsOfNormalDistOfCaseDates_002=nbComps_JanJun2015_Normal002
# 
# load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/GT Results/Phase 2/2015-01-01 to 2015-06-01 GT Sensitivity Results (with myContacts & config & 0.02 support for Non-Randomized Case Dates).RData")
# ComponentsOfNonRandomDistOfCaseDates_002=nbComps_JanJun2015
# 
# load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/GT Results/Phase 2/2015-01-01 to 2015-06-01 GT Sensitivity Results (with myContacts & config & 0.1 support).RData")
# ComponentsOfAdding1to15DaysToCaseDates_01=nbComps_JanJun2015
# ComponentsOfAdding1to15DaysToCaseDates_01[,1]=1:20
# 
# load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/GT Results/Phase 2/2015-01-01 to 2015-06-01 GT Sensitivity Results (with myContacts & config & 0.1 support for Normalized Case Dates).RData")
# ComponentsOfNormalDistOfCaseDates_01=nbComps_JanJun2015_Normal01
# 
# load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/GT Results/Phase 2/2015-01-01 to 2015-06-01 GT Sensitivity Results (with myContacts & config & 0.1 support for Non-Randomized Case Dates).RData")
# ComponentsOfNonRandomDistOfCaseDates_01=nbComps_JanJun2015_NonRandom01

# load("C:/Users/Narimane/Dropbox/CPE Transmission Chains/CPETransmissionChains/Components Results/2015-01-01 to 2015-06-01 Components Results for meanGT 1 to 12 with Poisson Distribution and min_support=0.05.RData")
# nbComps_JanJun2015_Poisson_005_1to20=Components_Results
