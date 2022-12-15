#creating data set to explore spread of species between countries

library(dplyr)
library(tidyr)
library(ggplot2)

AQData=read.csv("/Users/beccagentry/Documents/Spread of Aquaculture Project/Analysis/Inputs/FAO/TS_FI_AQUACULTURE.csv")
dim (AQData)
head(AQData)

str(AQData)

AQCountry=read.csv("/Users/beccagentry/Documents/Spread of Aquaculture Project/Analysis/Inputs/FAO/CL_FI_COUNTRY_GROUPS.csv")%>%
  select(UN_Code,Name_En,ISO3_Code, Continent_Group,EcoClass_Group,GeoRegion_Group)%>%
  rename(COUNTRY=UN_Code)

AQCountry$COUNTRY<-as.character(AQCountry$COUNTRY) ##ned to change to a character so it works wth the AQData which changed to a character due to gsub
head(AQCountry)

AQSpecies=read.csv("/Users/beccagentry/Documents/Spread of Aquaculture Project/Analysis/Inputs/FAO/CL_FI_SPECIES_GROUPS.csv")%>%
  select(X3Alpha_Code,Major_Group,Name_En)%>%
  rename(SPECIES=X3Alpha_Code)%>%
  rename(SpeciesName=Name_En)
head(AQSpecies)

###make some changes to the FAO dataset so that it later combines more easily with other datasets. See analysis notes document for explanation
## 1) change SCG (891) to MNE (499) for all species
## 2) change YUG (890) to SVN (705) for species BSS, MSM,and SBG  ; and YUG(890) to HRV (191) for OYF
## 3) change all SUN (810) to RUS (643)
## 4) change all EAZ (835) to TZA (834) 

AQData$COUNTRY <- gsub(891,499,AQData$COUNTRY)
AQData$COUNTRY <- gsub(810,643,AQData$COUNTRY)
AQData$COUNTRY <- gsub(835,834,AQData$COUNTRY)

AQData$COUNTRY<-ifelse(AQData$SPECIES=="OYF", gsub(890, 191, AQData$COUNTRY),AQData$COUNTRY)

AQData$COUNTRY<-ifelse(AQData$SPECIES=="BSS"|AQData$SPECIES=="MSM"|AQData$SPECIES=="SBG", gsub(890, 705, AQData$COUNTRY),AQData$COUNTRY)

AQDatatest=AQData%>% ##test
 filter(COUNTRY=="705")

##how many countries is each species produced in?
MaricultureSpeciesFrequency=AQData%>%
  filter(ENVIRONMENT==3)%>%##only marine data
  group_by(COUNTRY, SPECIES)%>%
  filter(QUANTITY>0)%>%
  summarize(FIRSTYEAR=min(YEAR))%>%
  ungroup()%>%
  count(SPECIES)%>%
  arrange(desc(n))%>%
  left_join(AQSpecies, by= "SPECIES")%>%
  select(-Major_Group)%>%
  rename("Num_Countries_Sp_Grown"="n")
##to figure out what species is being refered to in code, consult ASFIS_sp_2019 in FAO folder

MaricultureSpeciesFrequency


####find out total mariculture, AQ, and species group production by country by year


MaricultureByCountryByYear=AQData%>%
  filter(ENVIRONMENT==3)%>%  ##only marine data
  group_by(COUNTRY,YEAR)%>%
  summarize(MARPRODUCTION=sum(QUANTITY))%>%##this gives us the Mariculture production for each country in each year
  filter(YEAR==2017)%>%
  left_join(AQCountry, by="COUNTRY")

head(MaricultureByCountryByYear)

AqByCountryByYear=AQData%>%
  group_by(COUNTRY,YEAR)%>%
  summarize(AQPRODUCTION=sum(QUANTITY))##this gives us the Aquaculture (all) production for each country in each year
head(AqByCountryByYear)

PiscesByCountryByYear=AQData%>%
  left_join(AQSpecies, by="SPECIES")%>%
  filter(Major_Group=="PISCES")%>%
  group_by(COUNTRY,YEAR)%>%
  summarize(PISPRODUCTION=sum(QUANTITY))##this gives us the total production for fish (all aq) production for each country in each year
head(PiscesByCountryByYear)

MolByCountryByYear=AQData%>%
  left_join(AQSpecies, by="SPECIES")%>%
  filter(Major_Group=="MOLLUSCA")%>%
  group_by(COUNTRY,YEAR)%>%
  summarize(MOLPRODUCTION=sum(QUANTITY))##this gives us the total production for molluscs (all aq) production for each country in each year
head(MolByCountryByYear)

CrusByCountryByYear=AQData%>%
  left_join(AQSpecies, by="SPECIES")%>%
  filter(Major_Group=="CRUSTACEA")%>%
  group_by(COUNTRY,YEAR)%>%
  summarize(CRUSPRODUCTION=sum(QUANTITY))##this gives us the total production for crustacean (all aq) production for each country in each year
head(CrusByCountryByYear)

AlgByCountryByYear=AQData%>%
  left_join(AQSpecies, by="SPECIES")%>%
  filter(Major_Group=="PLANTAE_AQUATICAE")%>%
  group_by(COUNTRY,YEAR)%>%
  summarize(ALGPRODUCTION=sum(QUANTITY))##this gives us the total production for algae (all aq) production for each country in each year
head(AlgByCountryByYear)

InvByCountryByYear=AQData%>%
  left_join(AQSpecies, by="SPECIES")%>%
  filter(Major_Group=="INVERTEBRATA AQUATICA")%>%
  group_by(COUNTRY,YEAR)%>%
  summarize(INVPRODUCTION=sum(QUANTITY))##this gives us the total production for invertebrates (all aq) production for each country in each year
head(InvByCountryByYear)



##build species trajectory csv

##some species are being produced in multiple countries in 1950.  We have done research to figure out which one s first.
##in this case, we will change the "first" species to being produced in 1949

#japan ( 392) is first for LNJ - japanese kelp
AQData$YEAR<-ifelse(AQData$SPECIES=="LNJ"& AQData$COUNTRY=="392", gsub(1950,1949, AQData$YEAR),AQData$YEAR)

#France (250) if first for blue mussel -MUS -  according to https://www.cabi.org/isc/datasheet/73755
AQData$YEAR<-ifelse(AQData$SPECIES=="MUS"& AQData$COUNTRY=="250", gsub(1950,1949, AQData$YEAR),AQData$YEAR)


#OYF European flat oyster - to France very gradual transiion to wild to farmed in euroe since roman times.  france developed a new rearing tech in 1865.  that should count as first
AQData$YEAR<-ifelse(AQData$SPECIES=="OYF"& AQData$COUNTRY=="250", gsub(1950,1949, AQData$YEAR),AQData$YEAR)


#Japan is first for Pacific cupped oyster-OYG
AQData$YEAR<-ifelse(AQData$SPECIES=="OYG"& AQData$COUNTRY=="392", gsub(1950,1949, AQData$YEAR),AQData$YEAR)

#wakame -UDP toJapan.  China was the first to patent a cultivation technique in 1943; but commercial cultivation started first in Japan.  The two listed are japan and korea, so I think we can say japan
AQData$YEAR<-ifelse(AQData$SPECIES=="UDP"& AQData$COUNTRY=="392", gsub(1950,1949, AQData$YEAR),AQData$YEAR)

#blood cockle BLC is the only one with a four country trajectory that we dont know

AQData$YEAR<-as.numeric(AQData$YEAR)

SpeciesTrajectory=AQData%>%
  filter(ENVIRONMENT==3)%>%##only marine data
  left_join(AQCountry, by= "COUNTRY")%>%
  arrange(COUNTRY,SPECIES)%>%
  group_by(COUNTRY,SPECIES)%>%
  arrange(YEAR)%>%
  filter(QUANTITY>0)%>%
  slice(1)%>% #to select the year that each species was first cultivated in each country
  select(COUNTRY,Name_En,ISO3_Code,YEAR,SPECIES, QUANTITY,Continent_Group,GeoRegion_Group,EcoClass_Group)%>%
  ungroup()%>%
  arrange(SPECIES)%>%
  group_by(SPECIES)%>%
  arrange(SPECIES,YEAR)%>%
  mutate(order=row_number())%>%
  mutate(FIRSTYEAR=min(YEAR))%>%
  mutate(ElapsedYears=YEAR-FIRSTYEAR)%>%###figure out how many years have elapsed since the given species was first cultivated
  left_join(MaricultureSpeciesFrequency, by="SPECIES")%>%
  left_join(AQSpecies, by="SPECIES")%>%
  filter(!grepl("nei",SpeciesName))
  

head(SpeciesTrajectory)

count(SpeciesTrajectory,SPECIES)


#write.csv(SpeciesTrajectory, "speciestrajectoriesALL.csv")  ##version of this spread sheet from before changes to USSF, Yugolsavia, Tanzania is called"speciestrajectoriesALL_preCountrycoersion.csv

####Play with Species Trajectory Data Frame
SpeciesTrajectory=read.csv("speciestrajectoriesALL.csv")
head(SpeciesTrajectory)

## are there countries that commonly produce species first?
FirstCountry=SpeciesTrajectory%>%
  filter(ElapsedYears<=5) %>% #change to less than 5 to see countries that adopt in first 5 years
  ungroup()%>%
  count(Name_En)%>%
  arrange(desc(n))

head(FirstCountry)

