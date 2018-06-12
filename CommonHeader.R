### Common header

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
# envNN=T
# if(envNN){
#   currentwd=setwd("C:/Users/Narimane/Dropbox/CPE Transmission Chains")
# }else{
#   currentwd=setwd("/Users/pascalcrepey/Google Drive/1-EPC/") 
# }

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("Generation Time Sensitivity Analysis Functions.R", 
       local = FALSE, verbose = getOption("verbose"))

###########################
#### LOAD DEPT NETWORK ####

cat("Upload Department Contact Network\n")
if(Sys.getenv("LOGNAME")=="pascalcrepey"){
  dbloc="/Users/pascalcrepey/Dropbox"
}else{
  dbloc="../.."
}

load(paste0(dbloc,"/Hospital_Network/HospitalNetwork/Data/Department Network.RData"))

#########################################
#### GET DATA ON OXA-48 CPE EPISODES ####

cat("Choose start date\n")
startDate="2015-01-01"

cat("Choose end date\n")
endDate="2015-06-30"

cat("Load data\n")
wd=getwd()
setwd(paste0(dbloc,"/CPE Transmission Chains"))
dsorted=getData(startDate, endDate)
setwd(wd)
