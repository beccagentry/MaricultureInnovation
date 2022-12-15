

rm(list=ls())
setwd("/Users/beccagentry/Documents/Spread of Aquaculture Project/Analysis")
getwd()



library(nnet)
library(reshape2)
library(foreign)
library(DescTools)

library(FactoMineR)
library(factoextra)
library(ggplot2)
library(dplyr)
library(tidyr)


GovernanceData<-read.csv("FullGovtCascadeData.csv", stringsAsFactors = FALSE, na.strings = "NA")%>%select(-DoingBuisnessRank, -X)

ScoresGovData<-GovernanceData%>%
  select(WBGIAvg,DoingBuisnessScore, GlobalCompetitiveness)   #play around with weather to include GDP

SelectGovData<-GovernanceData%>%
  select( GE, RQ, StartingBuisnessScore,TradingScore,ICT_Adoption,Skills,BusinessDynamism,InnovationCapability)

ReleventGovData<-GovernanceData%>%
  select( GE, RQ, StartingBuisnessScore,TradingScore,ICT_Adoption,Skills,BusinessDynamism,InnovationCapability, PSNV, ROL,ConstructionScore,
         ElectricityScore,RegisteringPropScore,GettingCreditScore, ContractsScore,Institutions, Infrastructure,ProductMarket, LaborMarket,FinancialSystem,
         MarketSize)



###FactoMineR PCA, factoextra required to extract data

##for general scores
ScorePCA= PCA(ScoresGovData, scale.unit = TRUE, ncp = 2)  #running PCA (using FactoMineR), limited to 15 components, all NA REPLACED with column means.
barplot(ScorePCA$eig[,1],main="Eigenvalues",names.arg=1:nrow(ScorePCA$eig)) #plot of eigenvalues
ScorePCA_results = get_pca_ind(ScorePCA)  #extracting results 
ScorePCA_factorscores = ScorePCA_results$coord #extracting factor scores for each observation

ScorePCA$var
ScorePCA$eig


#For select data
SelectPCA= PCA(SelectGovData, scale.unit = TRUE, ncp = 4)  #running PCA (using FactoMineR), limited to 15 components, all NA REPLACED with column means.
barplot(SelectPCA$eig[,1],main="Eigenvalues",names.arg=1:nrow(SelectPCA$eig)) #plot of eigenvalues
SelectPCA_results = get_pca_ind(SelectPCA)  #extracting results 
SelectPCA_factorscores = SelectPCA_results$coord #extracting factor scores for each observation

SelectPCA$var
SelectPCA$eig

##make a datasheet for regressions
##first make a df with GDP, country name, and seq
GDPNameSeq<-GovernanceData%>%
  select(GDP, ISO3_Code)%>%
  mutate(seq=1:n())

SelectGovData<-as.data.frame(SelectPCA_factorscores)%>%
  mutate(seq=1:n())%>%
  left_join(GDPNameSeq, by="seq")%>%
  select(Dim.1, Dim.2, GDP, ISO3_Code)

write.csv(SelectGovData, "SelectGovDataForRegression.csv")

#For all relevent data
ReleventPCA= PCA(ReleventGovData, scale.unit = TRUE, ncp = 4)  #running PCA (using FactoMineR), limited to 15 components, all NA REPLACED with column means.
barplot(ReleventPCA$eig[,1],main="Eigenvalues",names.arg=1:nrow(ReleventPCA$eig)) #plot of eigenvalues
ReleventPCA_results = get_pca_ind(ReleventPCA)  #extracting results 
ReleventPCA_factorscores = ReleventPCA_results$coord #extracting factor scores for each observation

ReleventPCA$var
ReleventPCA$eig



