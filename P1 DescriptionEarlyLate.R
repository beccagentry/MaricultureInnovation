#looking at when a country first starts producing a species

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

###FAO DATA  
SpeciesCascades=read.csv("speciestrajectoriesALL.csv")

SpeciesCascades<-SpeciesCascades%>%
  select(-X) ##extra column from importing .csv

SpeciesCascadesGT4=SpeciesCascades%>%
  filter(Num_Countries_Sp_Grown>=4)
 
#how many species  #do for both cascades and cascadesGT4
numSpecies<-SpeciesCascades%>%
  group_by(SPECIES)%>%
  slice(1)

CountryNames<-SpeciesCascades%>%  #for use later on
  group_by(COUNTRY)%>%
  slice(1)%>%
  select(1:3,7)

#how many countries 
numCountries<-SpeciesCascades%>%
  group_by(COUNTRY)%>%
  slice(1)



SpeciesCascadesEarlyLate<-SpeciesCascadesGT4%>%
  mutate(First=if_else(ElapsedYears==0,1,0))%>%##get a 1 if the country is the first producer  ###remember that some species had more than 1 first
  mutate(Early=if_else(ElapsedYears<=5,1,0))%>%  ##get a 1 if the country has adopted within the first 5 years, a zero if more than 5 years
  mutate(Ones=if_else(ElapsedYears==0,1,1))  ##make a column of ones to make counting entries easier in later analysis

#how many first adopters are there? 
NumFirst<-count(SpeciesCascadesEarlyLate,First)

#how many early adopters are there?
count(SpeciesCascadesEarlyLate,Early)



## are there countries that commonly produce species first or early?

#First look only at species that are grown in atleast 4 countries
CountryFrequency<-SpeciesCascadesEarlyLate%>%
  group_by(COUNTRY)%>%
  summarize(CountFirst=sum(First), CountEarly=sum(Early), CountTotal=sum(Ones))%>%
  ungroup()%>%
  mutate(RatioFirst=CountFirst/CountTotal)#%>% ##ths is the ratio of first adoption events divided by the total number of adoption events.  a high ratio would mean that a given country is first more often than usual
  #mutate (RatioEarly=(CountEarly/CountTotal))##this is just the adopters that were early but NOT first - useful for graph, but not interpretation #dont subtract if the total early adopters is wanted
  #filter(CountTotal>=6)%>%###from here down is just to make the graph - looses some infomration
  #select(1, 5:6)%>%
  #gather(RatioType, Ratio, 2:3 )
  #left_join(CountryNames, by=("COUNTRY"))


##now look at all species
CountryFrequencyAll<-SpeciesCascades%>%
  mutate(First=if_else(ElapsedYears==0,1,0))%>%##get a 1 if the country is the first producer  ###remember that some species had more than 1 first
  mutate(Early=if_else(ElapsedYears<=5,1,0))%>%  ##get a 1 if the country has adopted within the first 5 years, a zero if more than 5 years
  mutate(Ones=if_else(ElapsedYears==0,1,1))%>%  ##make a column of ones to make counting entries easier in later analysis
  group_by(COUNTRY)%>%
  summarize(CountFirstALL=sum(First), CountEarlyALL=sum(Early), CountTotalALL=sum(Ones))%>%
  ungroup()%>%
  mutate(RatioFirstALL=ifelse(CountTotalALL<=2,"100",CountFirstALL/CountTotalALL))%>% ##ths is the ratio of first adoption events divided by the total number of adoption events.  a high ratio would mean that a given country is first more often than usual.  if a country has 3 or less species then I gave it a "NA"
  #mutate (RatioEarlyALL=(CountEarlyALL)/CountTotalALL)%>% ##counts both "early" and "first"
  #filter(CountTotalALL>=6)%>%###this lineis just to make the bargraph - looses some infomration
  left_join(CountryNames, by=("COUNTRY"))%>%
  mutate(CategoryFirstALL=ifelse(RatioFirstALL>1, "2 or fewer species",
                          ifelse(RatioFirstALL>=.75,"0.75-1.0",
                          ifelse(RatioFirstALL>=.5,"0.5-0.75",
                          ifelse(RatioFirstALL>=.25,"0.25-0.5","0-0.25")))))
                     
#write.csv(CountryFrequency, "CountryFrequency.csv") #or country frequency all
##make a map
library(rworldmap)

FrequencyMapJoin<-joinCountryData2Map(CountryFrequencyAll, joinCode="ISO3",nameJoinColumn="ISO3_Code",
                         verbose=TRUE)
#The following 6 countries wont map.  all small islands so not surprising
#[1,] "MYT"      
#[2,] "GLP"      
#[3,] "MTQ"      
#[4,] "BES"      
#[5,] "REU"      
#[6,] "CHI"

library(RColorBrewer)
#colourPalette <- RColorBrewer::brewer.pal(6,'RdPu')

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapDevice()
FrequencyMap<-mapCountryData(FrequencyMapJoin, 
                              nameColumnToPlot="CategoryFirstALL",
                              catMethod=c(0,.5,1),
                              #colourPalette =c("skyblue1", "skyblue2", "skyblue3", "skyblue4", "grey77"),
                              #colourPalette =c("#fee8c8","#fdbb84","#e34a33", "#d95f0e", "#bdbdbd"),
                             #colourPalette=c("#f0f9e8","#bae4bc", "#7bccc4","#2b8cbe", "#bdbdbd"),
                             #colourPalette=c("#f0f9e8","#bae4bc", "#7bccc4","#2b8cbe", "#bdbdbd"),
                             colourPalette=c("#bae4bc","#7bccc4","#43a2ca", "#0868ac", "#bdbdbd"),
                              mapTitle="",
                              addLegend = "FALSE")

do.call(addMapLegendBoxes, c(FrequencyMap,title="Proportion of Species"))

#quartz.save("FrequencyMap1.pdf", type="pdf")

#merge dataframes to explore some patterns  
CountryFrequencyMerged<-CountryFrequencyAll%>%
  left_join(CountryFrequency, by="COUNTRY")%>%
  mutate(success=CountFirst/CountFirstALL)%>%
  #filter(CountFirstALL>=3)%>%  #can filter by different things for exploring
  left_join(CountryNames, by=("COUNTRY"))

#write.csv(CountryFrequencyMerged, "CountryFrequency.csv") 
  
  
##Make Bar Graph (just for cascade>4 species)  #now just first adopters

PropFirstAdoptBar<-ggplot(CountryFrequency, aes(x=Name_En, y=RatioFirst, fill=RatioFirst))+
  geom_bar(stat="identity")+
  labs(x="Country", y="Proporton of first adoptions to total species adopted")+
  labs(fill="")+
  theme(axis.text.x=element_text(angle=90), 
        axis.title.x=element_blank())

PropFirstAdoptBar

#same thing but for all species grown 

PropFirstAdoptBarALL<-ggplot(CountryFrequencyAll, aes(x=Name_En, y=RatioFirstALL))+
  geom_bar(stat="identity", fill="blue")+
  labs(x="Country", y="Proporton of Total Species First Produced in Country")+
  labs(fill="")+
  #theme(axis.title.y=element_blank())+
  theme(axis.text.x=element_text(angle=90), 
        axis.title.x=element_blank())
  #coord_flip()

PropFirstAdoptBarALL


##what is the mean number of years for a country to adopt a species #only widely adopted  ##only those that have adopted atleast two species

MeanElapsed<-SpeciesCascadesGT4%>%
  filter(ElapsedYears>0)%>%  #not counting those that first produced the species
  group_by(COUNTRY)%>%
  summarise(MeanElapsedYears=mean(ElapsedYears),SDElapsedYears=sd(ElapsedYears))%>%#,se=sd(.)/sqrt(n())))%>%
  #summarise(mean(ElapsedYears))%>%
  left_join(CountryFrequency, by="COUNTRY")%>%
  left_join(CountryNames, by="COUNTRY")%>%
  filter(CountTotal-CountFirst>=2)%>%  #have to adopted atleast 2 species first produced elsewhere
  mutate(SEElapsedYears=SDElapsedYears / sqrt(CountTotal))%>%
  mutate(open="(")%>%
  mutate(close=")")%>%
  mutate(Name_En_Sp=paste(Name_En,open,CountTotal,close), sep= "-" )
 

MeanElapsed$Name_En_Sp=with(MeanElapsed, factor(Name_En_Sp, levels=Name_En_Sp[order(ave(MeanElapsedYears,Continent_Group, FUN=min),MeanElapsedYears)]))

MeanElapsed$Continent_Group<-factor(MeanElapsed$Continent_Group,levels=c("Asia","Oceania","Europe","Americas","Africa" ))

write.csv(MeanElapsed,"MeanElapsed.csv")
##make a bar chart

#MeanElapsedBar<-ggplot(MeanElapsed, aes(x=fct_reorder(Name_En,Continent_Group), y=MeanElapsedYears, fill=Continent_Group))+
  #geom_bar(stat="identity")

MeanElapsedBar<-ggplot(MeanElapsed, aes(x=Name_En_Sp, y=MeanElapsedYears, fill=Continent_Group))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=MeanElapsedYears-SEElapsedYears, ymax=MeanElapsedYears+SEElapsedYears), 
                width=.3, size=0.2)+
  labs(x="Country", y="Mean Elapsed Years")+
  labs(fill="")+
  theme(axis.title.y=element_blank())+
  scale_fill_brewer(palette="Set3")+
  scale_y_continuous(limits=c(0,65), expand=c(0,0))+
  coord_flip()

MeanElapsedBar

ggsave("MeanElapsedBar.pdf", width= 15, height=30, units="cm", dpi = 300)




####Look at mean elapsed time by species group

AQSpecies=read.csv("/Users/beccagentry/Documents/Spread of Aquaculture Project/Analysis/Inputs/FAO/CL_FI_SPECIES_GROUPS.csv")%>%
  #select(X3Alpha_Code,Major_Group, Name_En, Scientific_Name, ISSCAAP_Group)%>% #if we want more
  select(X3Alpha_Code,Major_Group, Name_En)%>%
  rename(SPECIES=X3Alpha_Code)
head(AQSpecies)

SpeciesList<-SpeciesCascadesGT4%>%  ##just to create a list of species for SI
  group_by(SPECIES)%>%
  left_join(AQSpecies, by="SPECIES")%>%
  slice(1)%>%
  select(SPECIES, SpeciesName, Major_Group)
  #select(SPECIES, SpeciesName, Major_Group,Scientific_Name, ISSCAAP_Group)
#write.csv(SpeciesList, "SpeciesList.csv")

  

MeanElapsedSp<-SpeciesCascadesGT4%>%
  filter(ElapsedYears>0)%>%  #not counting those that first produced the species
  filter(order<=4)%>% ##only use if we want to consider only the first four adoption events
  group_by(SPECIES)%>%
  summarise(MeanElapsed=mean(ElapsedYears),MinElapsed=min(ElapsedYears), SDMeanElapsed=sd(ElapsedYears),CountTotal=n())%>%
  #rename(MeanElapsedYears="mean(ElapsedYears)")#%>%
  left_join(AQSpecies, by="SPECIES")%>%
  filter(Major_Group!="INVERTEBRATA AQUATICA")%>%
  mutate(SEMeanElapsed=SDMeanElapsed / sqrt(CountTotal))

AnovaSpMean<-aov(MeanElapsed~Major_Group, MeanElapsedSp)
summary(AnovaSpMean)
TukeyHSD(AnovaSpMean, conf.level=0.99)

#first adoption event
AnovaSpMin<-aov(MinElapsed~Major_Group, MeanElapsedSp)
summary(AnovaSpMin)
TukeyHSD(AnovaSpMin, conf.level=0.99)


#make species bar graph
MeanElapsedSp$Name_En=with(MeanElapsedSp, factor(Name_En, levels=Name_En[order(ave(MeanElapsed,Major_Group, FUN=min),MeanElapsed)]))

MeanElapsedSpeciesBar<-ggplot(MeanElapsedSp, aes(x=Name_En, y=MeanElapsed, fill=Major_Group))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=MeanElapsed-SEMeanElapsed, ymax=MeanElapsed+SEElapsedYears))+
  labs(x="Species", y="Mean Elapsed Years")+
  labs(fill="")+
  theme(axis.title.y=element_blank())+
  coord_flip()

MeanElapsedSpeciesBar




MeanElapsedSpGrp<-SpeciesCascadesGT4%>%
  filter(ElapsedYears>0)%>%  #not counting those that first produced the species
  left_join(AQSpecies, by="SPECIES")%>%
  filter(order<=4)%>% ##only use if we want to consider only the first four adoption events
  group_by(Major_Group)%>%
  summarise(meanElapsed=mean(ElapsedYears),SDMeanElapsed=sd(ElapsedYears),CountTotal=n())%>%
  mutate(SEMeanElapsed=SDMeanElapsed / sqrt(CountTotal))%>%
  filter(Major_Group!="INVERTEBRATA AQUATICA")
 

MeanElapsedSpeciesGrpBar<-ggplot(MeanElapsedSpGrp, aes(x=Major_Group, y=meanElapsed, fill=Major_Group))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=meanElapsed-SEMeanElapsed, ymax=meanElapsed+SEMeanElapsed))+
  labs(x="Species Group", y="Mean Elapsed Years")+
  labs(fill="")+
  theme(axis.title.y=element_blank())#+
  coord_flip()

MeanElapsedSpeciesGrpBar



MinElapsedSpGrp<-MeanElapsedSp%>%
  group_by(Major_Group)%>%
  summarise(MeanMin=mean(MinElapsed))



