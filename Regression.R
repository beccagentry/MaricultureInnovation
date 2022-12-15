#######################
###Regression Models###
#######################
#######################
#######################
#packages
library(MuMIn) #dredge function
library(DescTools) #Tjur's R-squared
library(stargazer) #tables
library(dplyr)
library(Hmisc)
library(corrplot)
#####################################

innovdata <- read.csv("~/Dropbox/Aquaculture/Other Projects/Nodes of Innovation/PCAandNewVariables.csv")
adjdata <- read.csv("~/Dropbox/Aquaculture/Other Projects/Nodes of Innovation/PCAandNewVariables_NAadjusted.csv")

#removing NAs from innovdata
test <- subset(innovdata, select = c(3:5, 7:9, 11, 12))
test2 <- na.omit(test)

#selecting relevant columns for adjusted data
adjdata2 <- subset(adjdata, select = c(2:9, 11, 12, 15))
adjdata3 <- na.omit(adjdata2)

#
innovdata1 <- subset(innovdata, select = -c(6)) #removing mean elapsed years from dataset

dredgedata <- na.omit(innovdata) #Mean Elapsed Data for dredge models
dredgedata1 <- na.omit(innovdata1) #centrality, # countries, # connections for dredge models

#dredge data without SeafoodMarket and Consumption (inclusion drops a lot of observations)
innovdata2 <- subset(innovdata, select = -c(14, 15))
dredgedata2 <- na.omit(innovdata2) #ME for dredge

innovdata3 <- subset(innovdata1, select = -c(13, 14))
dredgedata3 <- na.omit(innovdata3) #dredge data for centrality, # countries, # connections

#####################################
#regress dependent against single variables to assess which scale of identical variables to use (e.g., gdp vs. gdp per capita)

#centrality#
##GDP
centralitygdp <- lm(Centrality ~ GDP, data = innovdata) #GDP, more significant (p = 0.082)
centralitygdppc <- lm(Centrality ~ GDPPC, data = innovdata) #GDP per capita (p = 0.65)

##collapsed fisheries
centralitynumber <- lm(Centrality ~ NCollapsedFisheries2016MA, data = innovdata) #number of collapsed fisheries (p = 0.41)
centralitypercent <- lm(Centrality ~ PercentCollapsed2016MA, data = innovdata) #percent collapsed fisheries (p = 0.91)

##consumption
centralitymarket <- lm(Centrality ~ SeafoodMarket, data = innovdata) #total seafood supply (p = 0.015)
centralityconsumption <- lm(Centrality ~ SeafoodConsumption, data = innovdata) #per capita seafood consumption (p = 0.75)

#number of countries#
##GDP
countriesconnectedgdp <- lm(CountriesConnected ~ GDP, data = innovdata) #GDP (p = 0.008)
countriesconnectedgdppc <- lm(CountriesConnected ~ GDPPC, data = innovdata) #GDP per capita (p = 0.0009)

##collapsed fisheries
ccnumber <- lm(CountriesConnected ~ NCollapsedFisheries2016MA, data = innovdata) #number of collapsed fisheries (p < 0.0001)
ccpercent <- lm(CountriesConnected ~ PercentCollapsed2016MA, data = innovdata) #percent collapsed fisheries (p = 0.025)

##consumption
ccmarket <- lm(CountriesConnected ~ SeafoodMarket, data = innovdata) #total seafood supply (p = 0.3411)
ccconsumption <- lm(CountriesConnected ~ SeafoodConsumption, data = innovdata) #per capita seafood consumption (p = 0.4785)

#total connections#
##GDP
connectionsgdp <- lm(TotalConnections ~ GDP, data = innovdata) #GDP (p = 0.0197)
connectionsgdppc <- lm(TotalConnections ~ GDPPC, data = innovdata) #GDP per capita (p = 0.0001)

##collapsed fisheries
connectionsnumber <- lm(TotalConnections ~ NCollapsedFisheries2016MA, data = innovdata) #number of collapsed fisheries (p < 0.0001)
connectionspercent <- lm(TotalConnections ~ PercentCollapsed2016MA, data = innovdata) #percent collapsed fisheries (p = 0.016)

##consumption
connectionsmarket <- lm(TotalConnections ~ SeafoodMarket, data = innovdata) #total seafood supply (p = 0.913)
connectionsconsumption <- lm(TotalConnections ~ SeafoodConsumption, data = innovdata) #per capita seafood consumption (p = 0.181)

#mean elapsed#
##GDP
MEgdp <- lm(MeanElapsedYears ~ GDP, data = innovdata) #GDP (p = 0.425)
MEgdppc <- lm(MeanElapsedYears ~ GDPPC, data = innovdata) #GDP per capita (p = 0.018)

##collapsed fisheries
MEnumbers <- lm(MeanElapsedYears ~ NCollapsedFisheries2016MA, data = innovdata) #number of collapse fisheries (p = 0.6998)
MEpercent <- lm(MeanElapsedYears ~ PercentCollapsed2016MA, data = innovdata) #percent collapsed fisheries (p = 0.1241)

##consumption
MEmarket <- lm(MeanElapsedYears ~ SeafoodMarket, data = innovdata) #total seafood supply (p = 0.8272)
MEconsumption <- lm(MeanElapsedYears ~ SeafoodConsumption, data = innovdata) #per capita seafood consumption (p = 0.446)

###########################################################
#REGRESSION MODELS#
##Centrality
centralitylm <- lm(Centrality ~ Dim.1 + Dim.2 + GDP + NCollapsedFisheries2016MA + SeafoodMarket + FWPRODUCTION, data = innovdata)
#GDP and N collapsed and Seafood Market
#Adj. R2: 0.002043
#P-value: 0.4248

centralitylm2 <- lm(Centrality ~ Dim.1 + Dim.2 + GDPPC + NCollapsedFisheries2016MA + SeafoodMarket + FWPRODUCTION, data = innovdata)
#GDPPC and N Collapsed and Seafood Market
#Adj. R2: 0.002036
#P-value: 0.4248

centralitylm3 <- lm(Centrality ~ Dim.1 + Dim.2 + GDP + NCollapsedFisheries2016MA + SeafoodConsumption + FWPRODUCTION, data = innovdata)
#GDP and N Collapsed and Seafood Consumption
#Adj. R2: 0.001865
#P-value: 0.4258

centralitylm4 <- lm(Centrality ~ Dim.1 + Dim.2 + GDP + PercentCollapsed2016MA + SeafoodMarket + FWPRODUCTION, data = innovdata)
#GDP and % Collapsed and Seafood Market
#Adj. R2: 0.01377
#P-value: 0.3618

centralitylm5 <- lm(Centrality ~ Dim.1 + Dim.2 + log(GDP) + PercentCollapsed2016MA + SeafoodConsumption + FWPRODUCTION, data = innovdata)
#GDP and % collapsed and Seafood Consumption
#Adj. R2: 0.01397
#P-value: 0.3608

centralitylm6 <- lm(Centrality ~ Dim.1 + Dim.2 + GDPPC + PercentCollapsed2016MA + SeafoodConsumption + FWPRODUCTION, data = innovdata)
#GDPPC and % collapsed and Seafood Consumption
#Adj. R2: 0.01518
#P-value: 0.3547

centralitylm7 <- lm(Centrality ~ Dim.1 + Dim.2 + log(GDP) + NCollapsedFisheries2016MA + FWPRODUCTION + SeafoodConsumption, data = innovdata)
#Adj. R2: 0.01824
#P-value: 0.2919

centralityadj <- lm(Centrality ~ Dim.1 + Dim.2 + log(GDP) + NCollapsedFisheries2016MA + FWPRODUCTION + SeafoodConsumption, data = adjdata2)
#Adj. R2: 0.04177
#P-value: 0.1171

centralitysimple <- lm(Centrality ~ Dim.1 + Dim.2 + FWPRODUCTION, data = innovdata) #PCA components + freshwater production (which is the only variable significant when all 4 add'l variables are thrown in with the PCA outputs)
#Adj. R2: 0.05305
#P-value: 0.06305

centralityfull <- lm(Centrality ~ Dim.1 + Dim.2 + GDP + GDPPC + NCollapsedFisheries2016MA + PercentCollapsed2016MA + FWPRODUCTION, data = dredgedata3, na.action = "na.fail")

centralitydredge <- dredge(centralityfull)

centralitydredgemodel <- lm(Centrality ~ Dim.1 + Dim.2 + FWPRODUCTION, data = innovdata) #dredge predominantly picked freshwater production so just adding to PCA dimensions

##Number of countries connected
cclm <- lm(CountriesConnected ~ Dim.1 + Dim.2 + GDP + NCollapsedFisheries2016MA + SeafoodMarket + FWPRODUCTION, data = innovdata)
#GDP and N collapsed and Seafood Market
#Adj. R2: 0.2651
#P-value: 0.0017

cclm2 <- lm(CountriesConnected ~ Dim.1 + Dim.2 + GDPPC + NCollapsedFisheries2016MA + SeafoodMarket + FWPRODUCTION, data = innovdata)
#GDPPC and N Collapsed and Seafood Market
#Adj. R2: 0.2827
#P-value: < 0.001

cclm3 <- lm(CountriesConnected ~ Dim.1 + Dim.2 + GDPPC + NCollapsedFisheries2016MA + SeafoodConsumption + FWPRODUCTION, data = innovdata)
#GDPPC and N Collapsed and Seafood Consumption
#Adj. R2: 0.2862
#P-value: < 0.001

cclm4 <- lm(CountriesConnected ~ Dim.1 + Dim.2 + log(GDP) + NCollapsedFisheries2016MA + FWPRODUCTION + SeafoodConsumption, data = innovdata)
#GDP and N Collapsed and Seafood Consumption
#Adj. R2: 0.2634
#P-value: 0.001735

cclm5 <- lm(CountriesConnected ~ Dim.1 + Dim.2 + log(GDP) + NCollapsedFisheries2016MA + FWPRODUCTION, data = innovdata)
#GDP and N Collapsed
#Adj. R2: 0.2224
#P-value: 0.0006

cclm6 <- lm(CountriesConnected ~ Dim.1 + Dim.2 + log(GDP) + PercentCollapsed2016MA + FWPRODUCTION, data = innovdata)

cclmadj <- lm(CountriesConnected ~ Dim.1 + Dim.2 + log(GDP) + NCollapsedFisheries2016MA + FWPRODUCTION + SeafoodConsumption, data = adjdata2)
#median filled data for dependent variables
#Adj. R2: 0.2664
#P-value: < 0.001

cclmadj2 <- lm(CountriesConnected ~ Dim.1 + Dim.2 + GDP + NCollapsedFisheries2016MA + FWPRODUCTION, data = adjdata2)
#removing seafood consumption
#Adj. R2: 0.2713
#P-value: < 0.001

cclmsimple <- lm(CountriesConnected ~ Dim.1 + Dim.2 + NCollapsedFisheries2016MA, data = innovdata) #PCA components + collapsed fisheries (which is the only variable significant when all 4 add'l variables are thrown in with the PCA outputs)
#Adj. R2: 0.2718
#P-value:  < 0.001

ccfull <- lm(CountriesConnected ~ Dim.1 + Dim.2 + GDP  + NCollapsedFisheries2016MA + PercentCollapsed2016MA + FWPRODUCTION, data = dredgedata3, na.action = "na.fail")

ccdredge <- dredge(ccfull)

##Total connections
connectionslm <- lm(TotalConnections ~ Dim.1 + Dim.2 + GDP + NCollapsedFisheries2016MA + SeafoodMarket + FWPRODUCTION, data = innovdata)
#GDP and N collapsed and Seafood Market
#Adj. R2: 0.4567
#P-value: < 0.001

connectionslm2 <- lm(TotalConnections ~ Dim.1 + Dim.2 + GDPPC + NCollapsedFisheries2016MA + SeafoodMarket + FWPRODUCTION, data = innovdata)
#GDPPC and N Collapsed and Seafood Market
#Adj. R2: 0.4827
#P-value: < 0.001

connectionslm3 <- lm(TotalConnections ~ Dim.1 + Dim.2 + GDPPC + NCollapsedFisheries2016MA + SeafoodConsumption + FWPRODUCTION, data = innovdata)
#GDPPC and N Collapsed and Seafood Consumption
#Adj. R2: 0.4829
#P-value: < 0.001

connectionslm4 <- lm(TotalConnections ~ Dim.1 + Dim.2 + GDPPC + PercentCollapsed2016MA + SeafoodConsumption + FWPRODUCTION, data = innovdata)
#GDPPC and % Collapsed and Seafood Consumption
#Adj. R2: 0.2403
#P-value: 0.003

connectionslm5 <-lm(TotalConnections ~ Dim.1 + Dim.2 + log(GDP) + NCollapsedFisheries2016MA + FWPRODUCTION + SeafoodConsumption, data = innovdata)
#Adj. R2: 0.4588
#P-value: < 0.001

connectionslmadj <- lm(TotalConnections ~ Dim.1 + Dim.2 + log(GDP) + NCollapsedFisheries2016MA + FWPRODUCTION + SeafoodConsumption, data = adjdata2)
#median adjusted independent variables
#Adj. R2: 0.3613
#P-value: < 0.001

connectionslmadj2 <- lm(TotalConnections ~ Dim.1 + Dim.2 + GDP + NCollapsedFisheries2016MA + FWPRODUCTION, data = adjdata2)
#median adjusted but dropping seafood consumption
#Adj. R2: 0.3669
#P-value: < 0.001

connectionslogadj <- lm(TotalConnections ~ Dim.1 + Dim.2 + log(GDP) + log(NCollapsedFisheries2016MA) + FWPRODUCTION, data = adjdata2)

connectionslmsimple <- lm(TotalConnections ~ Dim.1 + Dim.2 + NCollapsedFisheries2016MA, data = innovdata) #PCA components + collapsed fisheries (which is the only variable significant when all 4 add'l variables are thrown in with the PCA outputs)
#Adj. R2: 0.3822
#P-value: < 0.001

connectionsfull <- lm(TotalConnections ~ Dim.1 + Dim.2 + GDP + GDPPC + NCollapsedFisheries2016MA + PercentCollapsed2016MA + FWPRODUCTION, data = dredgedata3, na.action = "na.fail")

connectionsdredge <- dredge(connectionsfull)

##Mean Elapsed Years
MElm <- lm(MeanElapsedYears ~ Dim.1 + Dim.2 + GDP + NCollapsedFisheries2016MA + SeafoodMarket + FWPRODUCTION, data = innovdata)
#GDP and N Collapsed and Seafood Market
#Adj. R2: 0.05726
#P-value: 0.1835

MElm2 <- lm(MeanElapsedYears ~ Dim.1 + Dim.2 + GDPPC + NCollapsedFisheries2016MA + SeafoodMarket + FWPRODUCTION, data = innovdata)
#GDPPC and N Collapsed and Seafood Market
#Adj. R2: 0.06319
#P-value: 0.1657

MElm3 <- lm(MeanElapsedYears ~ Dim.1 + Dim.2 + GDPPC + PercentCollapsed2016MA + SeafoodMarket + FWPRODUCTION, data = innovdata)
#GDPPC and % collapsed and Seafood Market
#Adj. R2: 0.04905
#P-value: 0.2106

MElm4 <- lm(MeanElapsedYears ~ Dim.1 + Dim.2 + GDPPC + NCollapsedFisheries2016MA + SeafoodConsumption + FWPRODUCTION, data = innovdata)
#GDPPC and N collapsed and Seafood Consumption
#Adj. R2: 0.06238
#P-value: 0.1681

MElm5 <- lm(MeanElapsedYears ~ Dim.1 + Dim.2 + log(GDP) + NCollapsedFisheries2016MA + FWPRODUCTION + SeafoodConsumption, data = innovdata)
#no seafood consumption
#Adj. R2: 0.06535
#P-value: 0.1363

MElmadj <- lm(MeanElapsedYears ~ Dim.1 + Dim.2 + log(GDP) + NCollapsedFisheries2016MA + FWPRODUCTION + SeafoodConsumption, data = adjdata3)
#median adjusted independent variables
#Adj. R2: 0.0402
#P-value: 0.1937

MElmadj2 <- lm(MeanElapsedYears ~ Dim.1 + Dim.2 + GDP + NCollapsedFisheries2016MA + FWPRODUCTION, data = adjdata2)
#median adjusted minus seafood consumption
#Adj. R2: 0.05311
#P-value: 0.1257
  
MElmsimple <- lm(MeanElapsedYears ~ Dim.1 + Dim.2, data = innovdata)
#Adj. R2: 0.076
#P-value: 0.02339

MEfull <- lm(MeanElapsedYears ~ Dim.1 + Dim.2 + GDP + GDPPC + NCollapsedFisheries2016MA + PercentCollapsed2016MA + FWPRODUCTION, data = dredgedata2, na.action = "na.fail")

MEdredge <- dredge(MEfull)

##New Species
newlm <- lm(NewSpecies ~ Dim.1 + Dim.2 + GDP + NCollapsedFisheries2016MA + SeafoodMarket + FWPRODUCTION, data = innovdata)
#GDP and N collapsed and Seafood Market
#Adj. R2: 0.4167
#P-value: < 0.001

newlm2 <- lm(NewSpecies ~ Dim.1 + Dim.2 + GDPPC + NCollapsedFisheries2016MA + SeafoodMarket + FWPRODUCTION, data = innovdata)
#GDPPC and N Collapsed and Seafood Market
#Adj. R2: 0.403
#P-value: < 0.001

newlm3 <- lm(NewSpecies ~ Dim.1 + Dim.2 + GDPPC + NCollapsedFisheries2016MA + SeafoodConsumption + FWPRODUCTION, data = innovdata)
#GDPPC and N collapsed and Seafood Consumption
#Adj. R2: 0.482
#P-value: < 0.001

newlm4 <- lm(NewSpecies ~ Dim.1 + Dim.2 + GDP + NCollapsedFisheries2016MA + FWPRODUCTION, data = innovdata)
#no seafood consumption
#Adj. R2: 0.2894
#P-value: 0.0004255

newlm5 <- lm(NewSpecies ~ Dim.1 + Dim.2 + log(GDP) + NCollapsedFisheries2016MA + FWPRODUCTION + SeafoodConsumption, data = innovdata)
#with seafood consumption --> dimension 1 turns negative
#Adj. R2: 0.4605
#P-value: < 0.001

newlmadj <- lm(NewSpecies ~ Dim.1 + Dim.2 + log(GDP) + NCollapsedFisheries2016MA + FWPRODUCTION + SeafoodConsumption, data = adjdata3)
#median adjusted independent variables
#Adj. R2: 0.3635
#P-value: < 0.001

newlmadj2 <- lm(NewSpecies ~ Dim.1 + Dim.2 + GDP + NCollapsedFisheries2016MA + FWPRODUCTION, data = innovdata)
#no seafood consumption
#Adj. R2: 0.2894
#P-value: 0.0004255

newlmsimple <- lm(NewSpecies ~ Dim.1 + Dim.2 + NCollapsedFisheries2016MA + SeafoodConsumption, data = innovdata) #PCA components + collapsed fisheries and Seafood Consumption (which is the only variable significant when all 4 add'l variables are thrown in with the PCA outputs)
#Adj. R2: 0.4723
#P-value: < 0.001

#########################
#correlation test

innovdatacor <- innovdata[, c(7:15)]

cortable <- cor(innovdatacor, method = "pearson", use = "complete.obs")

res2 <- rcorr(as.matrix(cortable))
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank", tl.col = "black", tl.srt = 45)
##########################################################################################
##########################################################################################
#TABLE MODELS

##CENTRALITY
tbCentralbase <- lm(Centrality ~ Dim.1 + Dim.2, innovdata)
tbCentral1 <- lm(Centrality ~ Dim.1 + Dim.2 + log(GDP), innovdata)
tbCentral2 <- lm(Centrality ~ Dim.1 + Dim.2 + log(GDP) + FWPRODUCTION, innovdata)
tbCentral3 <- lm(Centrality ~ Dim.1 + Dim.2 + log(GDP) + NCollapsedFisheries2016MA, innovdata)
tbCentral4 <- lm(Centrality ~ Dim.1 + Dim.2 + log(GDP) + PercentCollapsed2016MA, innovdata)
tbCentral5 <- lm(Centrality ~ Dim.1 + Dim.2 + log(GDP) + SeafoodMarket, innovdata)
tbCentral6 <- lm(Centrality ~ Dim.1 + Dim.2 + log(GDP) + SeafoodConsumption, innovdata)
tbCentral7 <- lm(Centrality ~ Dim.1 + Dim.2 + log(GDP) + Fmsy, innovdata)
tbCentral8 <- lm(Centrality ~ Dim.1 + Dim.2 + log(GDP) + Bmsy, innovdata)

#############Individual variables################
tbCentralFW <- lm(Centrality ~ Dim.1 + Dim.2 + FWPRODUCTION, innovdata)
tbCentralCollapsed <- lm(Centrality ~ Dim.1 + Dim.2 + PercentCollapsed2016MA, innovdata)
tbCentralMarket <- lm(Centrality ~ Dim.1 + Dim.2 + SeafoodMarket, innovdata)
tbCentralConsumption <- lm(Centrality ~ Dim.1 + Dim.2 + SeafoodConsumption, innovdata)
tbCentralF <- lm(Centrality ~ Dim.1 + Dim.2 + Fmsy, innovdata)
tbCentralB <- lm(Centrality ~ Dim.1 + Dim.2 + Bmsy, innovdata)


##COUNTRIES CONNECTED
tbCCbase <- lm(CountriesConnected ~ Dim.1 + Dim.2, innovdata)
tbCC1 <- lm(CountriesConnected ~ Dim.1 + Dim.2 + log(GDP), innovdata)
tbCC2 <- lm(CountriesConnected ~ Dim.1 + Dim.2 + log(GDP) + FWPRODUCTION, innovdata)
tbCC3 <- lm(CountriesConnected ~ Dim.1 + Dim.2 + log(GDP) + NCollapsedFisheries2016MA, innovdata)
tbCC4 <- lm(CountriesConnected ~ Dim.1 + Dim.2 + log(GDP) + PercentCollapsed2016MA, innovdata)
tbCC5 <- lm(CountriesConnected ~ Dim.1 + Dim.2 + log(GDP) + SeafoodMarket, innovdata)
tbCC6 <- lm(CountriesConnected ~ Dim.1 + Dim.2 + log(GDP) + SeafoodConsumption, innovdata)
tbCC7 <- lm(CountriesConnected ~ Dim.1 + Dim.2 + log(GDP) + Fmsy, innovdata)
tbCC8 <- lm(CountriesConnected ~ Dim.1 + Dim.2 + log(GDP) + Bmsy, innovdata)

#############Individual variables################
tbCCFW <- lm(CountriesConnected ~ Dim.1 + Dim.2 + FWPRODUCTION, innovdata)
tbCCCollapsed <- lm(CountriesConnected ~ Dim.1 + Dim.2 + PercentCollapsed2016MA, innovdata)
tbCCMarket <- lm(CountriesConnected ~ Dim.1 + Dim.2 + SeafoodMarket, innovdata)
tbCCConsumption <- lm(CountriesConnected ~ Dim.1 + Dim.2 + SeafoodConsumption, innovdata)
tbCCF <- lm(CountriesConnected ~ Dim.1 + Dim.2 + Fmsy, innovdata)
tbCCB <- lm(CountriesConnected ~ Dim.1 + Dim.2 + Bmsy, innovdata)

##TOTAL CONNECTIONS
tbTotalCbase <- lm(TotalConnections ~ Dim.1 + Dim.2, innovdata)
tbTotalC1 <- lm(TotalConnections ~ Dim.1 + Dim.2 + log(GDP), innovdata)
tbTotalC2 <- lm(TotalConnections ~ Dim.1 + Dim.2 + log(GDP) + FWPRODUCTION, innovdata)
tbTotalC3 <- lm(TotalConnections ~ Dim.1 + Dim.2 + log(GDP) + NCollapsedFisheries2016MA, innovdata)
tbTotalC4 <- lm(TotalConnections ~ Dim.1 + Dim.2 + log(GDP) + PercentCollapsed2016MA, innovdata)
tbTotalC5 <- lm(TotalConnections ~ Dim.1 + Dim.2 + log(GDP) + SeafoodMarket, innovdata)
tbTotalC6 <- lm(TotalConnections ~ Dim.1 + Dim.2 + log(GDP) + SeafoodConsumption, innovdata)
tbTotalC7 <- lm(TotalConnections ~ Dim.1 + Dim.2 + log(GDP) + Fmsy, innovdata)
tbTotalC8 <- lm(TotalConnections ~ Dim.1 + Dim.2 + log(GDP) + Bmsy, innovdata)

#############Individual variables################
tbTotalFW <- lm(TotalConnections ~ Dim.1 + Dim.2 + FWPRODUCTION, innovdata)
tbTotalCollapsed <- lm(TotalConnections ~ Dim.1 + Dim.2 + PercentCollapsed2016MA, innovdata)
tbTotalMarket <- lm(TotalConnections ~ Dim.1 + Dim.2 + SeafoodMarket, innovdata)
tbTotalConsumption <- lm(TotalConnections ~ Dim.1 + Dim.2 + SeafoodConsumption, innovdata)
tbTotalF <- lm(TotalConnections ~ Dim.1 + Dim.2 + Fmsy, innovdata)
tbTotalB <- lm(TotalConnections ~ Dim.1 + Dim.2 + Bmsy, innovdata)

##NEW SPECIES
tbNewbase <- lm(NewSpecies ~ Dim.1 + Dim.2, innovdata)
tbNew1 <- lm(NewSpecies ~ Dim.1 + Dim.2 + log(GDP), innovdata)
tbNew2 <- lm(NewSpecies ~ Dim.1 + Dim.2 + log(GDP) + FWPRODUCTION, innovdata)
tbNew3 <- lm(NewSpecies ~ Dim.1 + Dim.2 + log(GDP) + NCollapsedFisheries2016MA, innovdata)
tbNew4 <- lm(NewSpecies ~ Dim.1 + Dim.2 + log(GDP) + PercentCollapsed2016MA, innovdata)
tbNew5 <- lm(NewSpecies ~ Dim.1 + Dim.2 + log(GDP) + SeafoodMarket, innovdata)
tbNew6 <- lm(NewSpecies ~ Dim.1 + Dim.2 + log(GDP) + SeafoodConsumption, innovdata)
tbNew7 <- lm(NewSpecies ~ Dim.1 + Dim.2 + log(GDP) + Fmsy, innovdata)
tbNew8 <- lm(NewSpecies ~ Dim.1 + Dim.2 + log(GDP) + Bmsy, innovdata)

#############Individual variables################
tbNewFW <- lm(NewSpecies ~ Dim.1 + Dim.2 + FWPRODUCTION, innovdata)
tbNewCollapsed <- lm(NewSpecies ~ Dim.1 + Dim.2 + PercentCollapsed2016MA, innovdata)
tbNewMarket <- lm(NewSpecies ~ Dim.1 + Dim.2 + SeafoodMarket, innovdata)
tbNewConsumption <- lm(NewSpecies ~ Dim.1 + Dim.2 + SeafoodConsumption, innovdata)
tbNewF <- lm(NewSpecies ~ Dim.1 + Dim.2 + Fmsy, innovdata)
tbNewB <- lm(NewSpecies ~ Dim.1 + Dim.2 + Bmsy, innovdata)

##MEAN ELAPSED YEARS
tbME1 <- lm(MeanElapsedYears ~ Dim.1 + Dim.2 + log(GDP), innovdata)
tbME2 <- lm(MeanElapsedYears ~ Dim.1 + Dim.2 + log(GDP) + FWPRODUCTION, innovdata)
tbME3 <- lm(MeanElapsedYears ~ Dim.1 + Dim.2 + log(GDP) + NCollapsedFisheries2016MA, innovdata)
tbME4 <- lm(MeanElapsedYears ~ Dim.1 + Dim.2 + log(GDP) + PercentCollapsed2016MA, innovdata)
tbME5 <- lm(MeanElapsedYears ~ Dim.1 + Dim.2 + log(GDP) + SeafoodMarket, innovdata)
tbME6 <- lm(MeanElapsedYears ~ Dim.1 + Dim.2 + log(GDP) + SeafoodConsumption, innovdata)
tbME7 <- lm(MeanElapsedYears ~ Dim.1 + Dim.2 + log(GDP) + Fmsy, innovdata)
tbME8 <- lm(MeanElapsedYears ~ Dim.1 + Dim.2 + log(GDP) + Bmsy, innovdata)