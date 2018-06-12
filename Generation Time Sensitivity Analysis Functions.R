#################################################################################
#####CPE Transmission Chains: Generation Time Sensitivity Analysis FUNCTIONS#####
#################################################################################

##################
#### GET DATA ####
getData=function(startDate, endDate){
  cat("Upload all data\n")
  data=read.csv(file=paste("Data/ALL_EPISODES (clean).csv", sep=""), header=T, sep=";", stringsAsFactors=FALSE)
  cat("Change dates to R dates\n")
  data$Department=as.character(data$Department)
  data$DateEpisode <- as.Date(data$DateEpisode, format = "%d/%m/%Y")
  data$DateOneCase <- as.Date(data$DateOneCase, format = "%d/%m/%Y")
  data$DateMoreOneCase <- as.Date(data$DateMoreOneCase, format = "%d/%m/%Y")
  data$DateImported <- as.Date(data$DateImported, format = "%d/%m/%Y")
  data$DateMoreFiveCases <- as.Date(data$DateMoreFiveCases, format = "%d/%m/%Y")
  cat("Select data by dates\n")
  data=data[which(data$DateEpisode > as.Date(startDate) & data$DateEpisode < as.Date(endDate)),]
  cat("Select dates OXA-48 only\n")
  OXA=data[!(data$OXA48 == ""),]
  OXA=OXA[,c("DateEpisode","Department", "Imported", "TotalCases")]
  cat("Order dates\n")
  dsorted=OXA[order(OXA[,1]),]
  cat("Set dates as numeric where t0 = first date\n")
  dsorted$Dates=as.numeric(dsorted[,1])-min(as.numeric(dsorted[,1]))
  cat("Set ID as rownumber\n")
  rownames(dsorted)=1:nrow(dsorted)
  dsorted$EpisodeID=as.numeric(rownames(dsorted))
  return(dsorted)
}

#####################################
#### TRANSFORM EPISODES TO CASES ####
getCaseDataForNonTransformedDates=function(){
  cat("Get Case Dates to Repeat\n")
  CaseDates=dsorted[which(dsorted$TotalCases > 1),]
  CasePerEpisode=CaseDates$TotalCases
  cat("Repeat Dates by N Case\n")
  RepeatedCaseDates=foreach(i=1:length(CaseDates$Dates)) %do% rep(CaseDates$Dates[i], CasePerEpisode[i])
  cat("Expand data by number of cases\n")
  Repeated_cases <- CaseDates[rep(row.names(CaseDates), CaseDates$TotalCases),]
  Repeated_cases$Dates=unlist(RepeatedCaseDates, use.names = FALSE)
  cat("Merge Data Back\n")
  Final_Case_Data=rbind(dsorted[which(!dsorted$TotalCases > 1),], Repeated_cases)
  cat("Reorder data by new dates and rename rownames\n")
  Final_Case_Data=Final_Case_Data[order(Final_Case_Data$Dates, Final_Case_Data$DateEpisode),]
  rownames(Final_Case_Data)=1:nrow(Final_Case_Data)
  
  cat("Reset ID to case ID\n")
  Final_Case_Data$ID=seq(from = 1, to = nrow(Final_Case_Data))
  
  cat("Set imported ancestor as 'self' and non-imported as NA to estimate their ancestor\n")
  Ancestery=cbind(as.numeric(rownames(Final_Case_Data[Final_Case_Data$Imported == "N",])), as.numeric(rownames(Final_Case_Data[Final_Case_Data$Imported == "N",])))
  Final_Case_Data=merge(Final_Case_Data, Ancestery, by.x=("ID"), by.y="V1", all.x=T)
  
  cat("Disable estimation of ancestors for imported cases\n")
  Final_Case_Data$move_alpha=(Final_Case_Data$Imported == "N")
  
  cat("Rename Columns\n")
  colnames(Final_Case_Data)=c("ID","DateEpisode","Department","Imported","TotalCases","Dates","EpisodeID","initial_value","move_alpha")
  
  return(Final_Case_Data)
}

############################################################
#### TRANSFORM EPISODES TO CASES: Randomly Add to Dates ####
getCaseDataForRandomlyTransformedDates=function(meanGT){
  cat("Get Case Dates to Repeat\n")
  CaseDates=dsorted[which(dsorted$TotalCases > 1),]
  CasePerEpisode=CaseDates$TotalCases
  cat("Repeat Dates by N Case\n")
  RepeatedCaseDates=foreach(i=1:length(CaseDates$Dates)) %do% rep(CaseDates$Dates[i], CasePerEpisode[i])
  
  cat("Random Sample of Dates: T0 + Random Sample of 0:meanGT\n")
  for(i in 1:length(RepeatedCaseDates)){
    RepeatedCaseDates[[i]][2:length(RepeatedCaseDates[[i]])] = round(sample(0:meanGT/3, length(RepeatedCaseDates[[i]])-1, replace=T)) + RepeatedCaseDates[[i]][1]
  }
  
  cat("Expand data by number of cases\n")
  Repeated_cases <- CaseDates[rep(row.names(CaseDates), CaseDates$TotalCases),]
  Repeated_cases$Dates=unlist(RepeatedCaseDates, use.names = FALSE)
  cat("Merge Data Back\n")
  Final_Case_Data=rbind(dsorted[which(!dsorted$TotalCases > 1),], Repeated_cases)
  cat("Reorder data by new dates and rename rownames\n")
  Final_Case_Data=Final_Case_Data[order(Final_Case_Data$Dates, Final_Case_Data$DateEpisode),]
  rownames(Final_Case_Data)=1:nrow(Final_Case_Data)
  
  cat("Reset ID to case ID\n")
  Final_Case_Data$ID=seq(from = 1, to = nrow(Final_Case_Data))
  
  cat("Set imported ancestor as 'self' and non-imported as NA to estimate their ancestor\n")
  Ancestery=cbind(as.numeric(rownames(Final_Case_Data[Final_Case_Data$Imported == "N",])), as.numeric(rownames(Final_Case_Data[Final_Case_Data$Imported == "N",])))
  Final_Case_Data=merge(Final_Case_Data, Ancestery, by.x=("ID"), by.y="V1", all.x=T)
  
  cat("Disable estimation of ancestors for imported cases\n")
  Final_Case_Data$move_alpha=(Final_Case_Data$Imported == "N")
  
  cat("Rename Columns\n")
  colnames(Final_Case_Data)=c("ID","DateEpisode","Department","Imported","TotalCases","Dates","EpisodeID","initial_value","move_alpha")
  
  return(Final_Case_Data)
}

##########################################################################
#### TRANSFORM EPISODES TO CASES: Normalize Dates Around Episode Date ####
getCaseDataForNormallyTransformedDates=function(meanGT){
  cat("Get Case Dates to Repeat\n")
  CaseDates=dsorted[which(dsorted$TotalCases > 1),]
  CasePerEpisode=CaseDates$TotalCases
  cat("Repeat Dates by N Case\n")
  RepeatedCaseDates=foreach(i=1:length(CaseDates$Dates)) %do% rep(CaseDates$Dates[i], CasePerEpisode[i])
  
  cat("Normal Distribution of Case Dates Around Episode Date: SD=15/2\n")
  for(i in 1:length(RepeatedCaseDates)){
    RepeatedCaseDates[[i]][1:length(RepeatedCaseDates[[i]])] = round(rnorm(length(RepeatedCaseDates[[i]]), mean=RepeatedCaseDates[[i]][1], sd=meanGT/2))
  }
  
  cat("Expand data by number of cases\n")
  Repeated_cases <- CaseDates[rep(row.names(CaseDates), CaseDates$TotalCases),]
  Repeated_cases$Dates=unlist(RepeatedCaseDates, use.names = FALSE)
  cat("Merge Data Back\n")
  Final_Case_Data=rbind(dsorted[which(!dsorted$TotalCases > 1),], Repeated_cases)
  cat("Account for 'negative' dates: set new T0\n")
  Final_Case_Data$Dates=Final_Case_Data$Dates-min(Final_Case_Data$Dates)
  cat("Reorder data by new dates and rename rownames\n")
  Final_Case_Data=Final_Case_Data[order(Final_Case_Data$Dates, Final_Case_Data$DateEpisode),]
  rownames(Final_Case_Data)=1:nrow(Final_Case_Data)
  
  cat("Reset ID to case ID\n")
  Final_Case_Data$ID=seq(from = 1, to = nrow(Final_Case_Data))
  
  cat("Set imported ancestor as 'self' and non-imported as NA to estimate their ancestor\n")
  Ancestery=cbind(as.numeric(rownames(Final_Case_Data[Final_Case_Data$Imported == "N",])), as.numeric(rownames(Final_Case_Data[Final_Case_Data$Imported == "N",])))
  Final_Case_Data=merge(Final_Case_Data, Ancestery, by.x=("ID"), by.y="V1", all.x=T)
  
  cat("Disable estimation of ancestors for imported cases\n")
  Final_Case_Data$move_alpha=(Final_Case_Data$Imported == "N")
  
  cat("Rename Columns\n")
  colnames(Final_Case_Data)=c("ID","DateEpisode","Department","Imported","TotalCases","Dates","EpisodeID","initial_value","move_alpha")
  
  return(Final_Case_Data)
}


getCaseDataForPoissonTransformedDatesPC<-function(meanGT){
  cat("Get Case Dates to Repeat\n")
  CaseDates=dsorted[which(dsorted$TotalCases > 1),]

  cat("Poisson Distribution of Case Dates After (Addition) Episode Date\n")
  RepeatedCaseDates=lapply(1:dim(CaseDates)[1], function(i){
    c(CaseDates$Dates[i],CaseDates$Dates[i]+rpois(n = CaseDates$TotalCases[i]-1,lambda=meanGT/3 ))
    })
  cat("Expand data by number of cases\n")
  Repeated_cases <- CaseDates[rep(row.names(CaseDates), CaseDates$TotalCases),]
  Repeated_cases$Dates=unlist(RepeatedCaseDates, use.names = FALSE)
  
  cat("Merge Data Back\n")
  Final_Case_Data=rbind(dsorted[which(dsorted$TotalCases == 1),], Repeated_cases)
  
  #cat("Account for 'negative' dates: set new T0\n")
  #Final_Case_Data$Dates=Final_Case_Data$Dates-min(Final_Case_Data$Dates)
  cat("Reorder data by new dates and rename rownames\n")
  Final_Case_Data=Final_Case_Data[order(Final_Case_Data$Dates, Final_Case_Data$DateEpisode),]
  rownames(Final_Case_Data)=1:nrow(Final_Case_Data)
  
  cat("Reset ID to case ID\n")
  Final_Case_Data$ID=seq(from = 1, to = nrow(Final_Case_Data))
  
  cat("Set imported ancestor as 'self' and non-imported as NA to estimate their ancestor\n")
  Ancestery=cbind(as.numeric(rownames(Final_Case_Data[Final_Case_Data$Imported == "N",])), as.numeric(rownames(Final_Case_Data[Final_Case_Data$Imported == "N",])))
  Final_Case_Data=merge(Final_Case_Data, Ancestery, by.x=("ID"), by.y="V1", all.x=T)
  
  cat("Disable estimation of ancestors for imported cases\n")
  Final_Case_Data$move_alpha=(Final_Case_Data$Imported == "N")
  
  cat("Rename Columns\n")
  colnames(Final_Case_Data)=c("ID","DateEpisode","Department","Imported","TotalCases","Dates","EpisodeID","initial_value","move_alpha")
  
  return(Final_Case_Data)
}
####################################################################################
#### TRANSFORM EPISODES TO CASES: Add Poisson Distributed Dates to Episode Date ####
getCaseDataForPoissonTransformedDates=function(meanGT){
  cat("Get Case Dates to Repeat\n")
  CaseDates=dsorted[which(dsorted$TotalCases > 1),]

  CasePerEpisode=CaseDates$TotalCases
  cat("Repeat Dates by N Case\n")
  RepeatedCaseDates=foreach(i=1:length(CaseDates$Dates)) %do% rep(CaseDates$Dates[i], CasePerEpisode[i])
  
  cat("Poisson Distribution of Case Dates After (Addition) Episode Date\n")
  for(i in 1:length(RepeatedCaseDates)){
    RepeatedCaseDates[[i]][2:length(RepeatedCaseDates[[i]])] = round(rpois(length(RepeatedCaseDates[[i]])-1, lambda=meanGT/3)) + RepeatedCaseDates[[i]][1]
  }
  
  cat("Poisson Distribution of Cases Dates After (Addition) Episode Date\n")
  # RepeatedCaseDates=lapply(1:dim(CaseDates)[1], function(i){
  #   c(CaseDates$Dates[i],CaseDates$Dates[i]+rpois(n = CaseDates$TotalCases[i]-1,lambda=meanGT/3 ))
  # })
  cat("Expand data by number of cases\n")
  Repeated_cases <- CaseDates[rep(row.names(CaseDates), CaseDates$TotalCases),]
  Repeated_cases$Dates=unlist(RepeatedCaseDates, use.names = FALSE)
  
  cat("Merge Data Back\n")
  Final_Case_Data=rbind(dsorted[which(dsorted$TotalCases == 1),], Repeated_cases)
  
  #cat("Account for 'negative' dates: set new T0\n")
  #Final_Case_Data$Dates=Final_Case_Data$Dates-min(Final_Case_Data$Dates)
  cat("Reorder data by new dates and rename rownames\n")
  Final_Case_Data=Final_Case_Data[order(Final_Case_Data$Dates, Final_Case_Data$DateEpisode),]
  rownames(Final_Case_Data)=1:nrow(Final_Case_Data)
  
  cat("Reset ID to case ID\n")
  Final_Case_Data$ID=seq(from = 1, to = nrow(Final_Case_Data))
  
  cat("Set imported ancestor as 'self' and non-imported as NA to estimate their ancestor\n")
  Ancestery=cbind(as.numeric(rownames(Final_Case_Data[Final_Case_Data$Imported == "N",])), as.numeric(rownames(Final_Case_Data[Final_Case_Data$Imported == "N",])))
  Final_Case_Data=merge(Final_Case_Data, Ancestery, by.x=("ID"), by.y="V1", all.x=T)
  
  cat("Disable estimation of ancestors for imported cases\n")
  Final_Case_Data$move_alpha=(Final_Case_Data$Imported == "N")
  
  cat("Rename Columns\n")
  colnames(Final_Case_Data)=c("ID","DateEpisode","Department","Imported","TotalCases","Dates","EpisodeID","initial_value","move_alpha")
  
  return(Final_Case_Data)
}

#############################
#### GET CONTACT NETWORK ####
getContactNetwork=function(myData){
  cat("Get network edges\n")
  edges=get.edgelist(directed.graph_Dept)
  
  cat("Subset Network by Relevant Edges\n")
  mydepts=unique(myData[,3])
  edges=as.data.frame(edges, stringsAsFactors = F)
  myEdges=edges[which(edges$V1 %in% mydepts),]
  myEdges=myEdges[which(myEdges$V2 %in% mydepts),]
  rownames(myEdges)=1:nrow(myEdges)
  
  cat("Rename Edges to Event ID (without repeated departments)\n")
  for(i in 1:nrow(myData)){
    myEdges$V1[which(myData[i,3]==myEdges$V1)]=row.names(myData[i,])
    myEdges$V2[which(myData[i,3]==myEdges$V2)]=row.names(myData[i,])
  }
  rownames(myEdges)=1:nrow(myEdges)
  colnames(myEdges)=c("Origin", "Target")
  myEdges$Origin=as.integer(myEdges$Origin)
  myEdges$Target=as.integer(myEdges$Target)
  return(myEdges)
}

########################
#### GET ANCESTRIES ####
# getAncestries=function(myData){
#   cat("Set imported ancestor as 'self' and non-imported as NA to estimate their ancestor\n")
#   myData=myData[order(myData$DateEpisode),]
#   rownumber=cbind(as.numeric(rownames(myData[myData$Imported == "N",])), as.numeric(rownames(myData[myData$Imported == "N",])))
#   myData$rownumber=as.numeric(rownames(myData))
#   myData=merge(myData, rownumber, by.x=("rownumber"), by.y="V1", all.x=T)
#   cat("Configure\n")
#   config = create_config(init_alpha = myData$V2, init_tree = myData$V2)
#   return(config)
# }

#################################
#### BUILD NETWORKS FUNCTION ####
buildNetworks<-function(meanGT){
  
  cat(paste("(Poisson) Transformation Function for meanGT =", meanGT, "\n"))
  myData=getCaseDataForPoissonTransformedDates(meanGT)
  
  cat(paste("Calculate generation time for meanGT =", meanGT, "\n"))
  gentime=generation.time("gamma", c(meanGT, meanGT/2), truncate = nrow(myData))
  gentime=gentime$GT
  
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
  
  return(OutbreakerResult)
}

buildNetworks_test<-function(meanGT){
  
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
                           move_kappa=FALSE,
                           init_tree = myData$initial_value)
  
  cat(paste("Get Outbreaker Data for meanGT =", meanGT, "\n"))
  myOutbreakerData=outbreaker_data(dates=myData$Dates, 
                                   dna=NULL,
                                   ctd=myContacts,
                                   w_dens=gentime)
  
  cat(paste("Run Outbreaker2  for meanGT =", meanGT, "\n"))
  OutbreakerResult=outbreaker(data = myOutbreakerData, config=myConfig)
  
  return(OutbreakerResult)
}
#########################################################
#### BUILD NETWORKS FUNCTION WITHOUT ANCESTOR CONFIG ####
buildNetworks_NoAncestorConfig<-function(meanGT){
  
  cat(paste("(Poisson) Transformation Function for meanGT =", meanGT, "\n"))
  myData=getCaseDataForPoissonTransformedDates(meanGT)
  
  cat(paste("Calculate generation time for meanGT =", meanGT, "\n"))
  gentime=generation.time("gamma", c(meanGT, meanGT/2), truncate = nrow(myData))
  gentime=gentime$GT
  
  cat("Get Contact Network\n")
  myContacts=getContactNetwork(myData)
  
  cat(paste("Get Outbreaker Data for meanGT =", meanGT, "\n"))
  myOutbreakerData=outbreaker_data(dates=myData$Dates, 
                                   dna=NULL,
                                   ctd=myContacts,
                                   w_dens=gentime)
  
  cat(paste("Run Outbreaker2  for meanGT =", meanGT, "\n"))
  OutbreakerResult=outbreaker(data = myOutbreakerData)
  
  return(OutbreakerResult)
}

##################################
#### BUILD NETWORKS FUNCTIONS ####
buildNetworks_Poisson<-function(meanGT){
  
  cat(paste("(Poisson) Transformation Function for meanGT =", meanGT, "\n"))
  myData=getCaseDataForPoissonTransformedDates(meanGT)
  
  cat(paste("Calculate generation time for meanGT =", meanGT, "\n"))
  gentime=generation.time("gamma", c(meanGT, meanGT/2), truncate = nrow(myData))
  gentime=gentime$GT
  
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
  
  return(OutbreakerResult)
}
buildNetworks_NonTransformed<-function(meanGT){
  
  cat(paste("(Non) Transformation Function for meanGT =", meanGT, "\n"))
  myData=getCaseDataForNonTransformedDates()
  
  cat(paste("Calculate generation time for meanGT =", meanGT, "\n"))
  gentime=generation.time("gamma", c(meanGT, meanGT/2), truncate = nrow(myData))
  gentime=gentime$GT
  
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
  
  return(OutbreakerResult)
}
buildNetworks_AddingRandomly<-function(meanGT){
  
  cat(paste("(Random) Transformation Function for meanGT =", meanGT, "\n"))
  myData=getCaseDataForRandomlyTransformedDates(meanGT)
  
  cat(paste("Calculate generation time for meanGT =", meanGT, "\n"))
  gentime=generation.time("gamma", c(meanGT, meanGT/2), truncate = nrow(myData))
  gentime=gentime$GT
  
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
  
  return(OutbreakerResult)
}
buildNetworks_Normal<-function(meanGT){
  
  cat(paste("(Normal) Transformation Function for meanGT =", meanGT, "\n"))
  myData=getCaseDataForNormallyTransformedDates(meanGT)
  
  cat(paste("Calculate generation time for meanGT =", meanGT, "\n"))
  gentime=generation.time("gamma", c(meanGT, meanGT/2), truncate = nrow(myData))
  gentime=gentime$GT
  
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
  
  return(OutbreakerResult)
}



#################################################################################

#### ORIGINAL BUILD NETWORK OF OUTBREAKER2 RESULTS ####

# buildNetwork_original<-function(meanGT, dates){
#   #Calculate generation time
#   gentime=generation.time("gamma", c(meanGT, meanGT/2))
#   gentime=gentime$GT
#   
#   #Convert to Outbreaker2 data
#   mydata=outbreaker_data(dates=dates, dna=NULL, 
#                          # ctd=myedges,
#                          w_dens=gentime)
#   #Run Outbreaker2
#   result=outbreaker(data = mydata)
#   
#   #Get graph
#   plot=plot(result, type="network")
#   edgelist=cbind(plot$x$edges$from, plot$x$edges$to)
#   graph=graph_from_edgelist(edgelist)
#   connectedGraph=graph-V(graph)[degree(graph)==0]
#   return(connectedGraph)
# }

#### ORIGINAL ANALYSIS OF THE NUMBER OF COMPONENTS (CHAINS) PER NETWORK ####

# nbComps_original=sapply(1:3, function(i){
#   net=buildNetwork(i, dates2)
#   comp=components(net)
#   print(i)
#   print(comp)
#   return(list(i,comp$no, length(comp$membership)))
# })
# 
# nbComps_original