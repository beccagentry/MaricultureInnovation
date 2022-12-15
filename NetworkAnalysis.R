# network analysis


library(dplyr)
library(tidyr)
library(ggplot2)

###FAO DATA 
SpeciesCascades=read.csv("speciestrajectoriesALL.csv") ##filled in country code "CHI" for channel islands

CountryNames<-SpeciesCascades%>%
  group_by(COUNTRY)%>%
  slice(1)%>%
  select(2:4)


edges<-SpeciesCascades%>%
  filter(Num_Countries_Sp_Grown>=4)%>%  # we cold change this# 4 is just for widely adopted
  select(2:6, 11)%>% #all the columns we might need
  select(1,5)%>% #smallest number of columns we need
  group_by(SPECIES)%>%
  mutate(Country1=COUNTRY[1])%>%
  mutate(Country2=COUNTRY[2])%>%
  mutate(Country3=COUNTRY[3])%>%
  mutate(Country4=COUNTRY[4])%>%
  mutate(Country5=COUNTRY[5])%>%
  mutate(Country6=COUNTRY[6])%>%
  mutate(Country7=COUNTRY[7])%>%
  mutate(Country8=COUNTRY[8])%>%
  mutate(Country9=COUNTRY[9])%>%
  mutate(Country10=COUNTRY[10])%>%
  mutate(Country11=COUNTRY[11])%>%
  mutate(Country12=COUNTRY[12])%>%
  mutate(Country13=COUNTRY[13])%>%
  mutate(Country14=COUNTRY[14])%>%
  mutate(Country15=COUNTRY[15])%>%
  mutate(Country16=COUNTRY[16])%>%
  mutate(Country17=COUNTRY[17])%>%
  mutate(Country18=COUNTRY[18])%>%
  mutate(Country19=COUNTRY[19])%>%
  mutate(Country20=COUNTRY[20])%>%
  mutate(Country21=COUNTRY[21])%>%
  mutate(Country22=COUNTRY[22])%>%
  mutate(Country23=COUNTRY[23])%>%
  mutate(Country24=COUNTRY[24])%>%
  mutate(Country25=COUNTRY[25])%>%
  mutate(Country26=COUNTRY[26])%>%
  mutate(Country27=COUNTRY[27])%>%
  mutate(Country28=COUNTRY[28])%>%
  mutate(Country29=COUNTRY[29])%>%
  mutate(Country30=COUNTRY[30])%>%
  mutate(Country31=COUNTRY[31])%>%
  mutate(Country32=COUNTRY[32])%>%
  mutate(Country33=COUNTRY[33])%>%
  mutate(Country34=COUNTRY[34])%>%
  mutate(Country35=COUNTRY[35])%>%
  mutate(Country36=COUNTRY[36])%>%
  mutate(Country37=COUNTRY[37])%>%
  mutate(Country38=COUNTRY[38])%>%
  mutate(Country39=COUNTRY[39])%>%
  mutate(Country40=COUNTRY[40])%>%
  mutate(Country41=COUNTRY[41])%>%
  gather(order, COUNTRYNext, 3:43)%>%
  filter(COUNTRYNext>=1)%>%
  mutate(double=COUNTRY-COUNTRYNext)%>%
  filter(double>=1)%>%
  select(COUNTRY, COUNTRYNext)%>%
  ungroup()%>%
  group_by(COUNTRY, COUNTRYNext)%>%
  tally()%>%
  rename(from=COUNTRY)%>%
  rename(to=COUNTRYNext)%>%
  rename(weight=n)%>%
  filter(weight>=4) ##to restrict to those that have several connections for graph

nodes<-SpeciesCascades%>%
  filter(Num_Countries_Sp_Grown>=4)%>%
  group_by(COUNTRY)%>%
  slice(1)%>%
  select(2,4)%>%
  rename(id=COUNTRY)%>%
  rename(label=ISO3_Code)%>%
  left_join(edges, by=c("id"="from"))%>%
  left_join(edges, by=c("id"="to"))%>%
  mutate_all(~replace(., is.na(.), 0))%>% 
  mutate(weight=weight.x + weight.y)%>%
  filter(weight>=4)%>% ##use this line if we want to restrict network to only connections that share atleast 4 species
  group_by(id)%>%
  slice(1)%>%
  select(id,label)

nodes$label<-as.character(nodes$label)

##calculate the number of connections each country has, how many different countries it is connected with


#first calculate from the "from column"
SummaryEdgesFrom<-SpeciesCascades%>%
  filter(Num_Countries_Sp_Grown>=4)%>%
  group_by(COUNTRY)%>%
  slice(1)%>%
  select(2,4)%>%
  right_join(edges, by=c("COUNTRY"="from"))%>%
  group_by(COUNTRY)%>%
  add_count()%>%
  rename(CountriesConnectedFrom=n)%>%
  mutate(TotalConnectionsFrom=sum(weight))%>%
  slice(1)%>%
  select(1,5,6)


#now do the same thing for the "to" and then add them together
SummaryEdges<-SpeciesCascades%>%
  filter(Num_Countries_Sp_Grown>=4)%>%
  group_by(COUNTRY)%>%
  slice(1)%>%
  select(2,4)%>%
  right_join(edges, by=c("COUNTRY"="to"))%>%
  group_by(COUNTRY)%>%
  add_count()%>%
  rename(CountriesConnectedTo=n)%>%
  mutate(TotalConnectionsTo=sum(weight))%>%
  slice(1)%>%
  select(1,5,6)%>%
  full_join(SummaryEdgesFrom, by="COUNTRY")%>%
  replace_na(list(CountriesConnectedTo=0,CountriesConnectedFrom=0,TotalConnectionsTo=0, TotalConnectionsFrom=0))%>%#turn NAs to zeros so we can add
  mutate(CountriesConnected=sum(CountriesConnectedTo, CountriesConnectedFrom))%>%
  mutate(TotalConnections=sum(TotalConnectionsTo,TotalConnectionsFrom))%>%
  left_join(CountryNames, by="COUNTRY")

#write.csv(SummaryEdges, "SummaryEdges.csv")

###make figure

library(igraph)

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
routes_igraph

quartz()


#plot(routes_igraph,edge.color="black", vertex.color=colbar, vertex.size=sizebar)

#more spaced layout
plot(routes_igraph, layout = layout_with_graphopt, edge.color="black", vertex.color=colbar)

plot(routes_igraph, layout = layout_with_graphopt, edge.color="black", vertex.color=c("plum3","lightgoldenrod1","paleturquoise3", "plum3","paleturquoise3", "salmon",
                                                                                      "salmon","paleturquoise3", "paleturquoise3", "plum3", "plum3", "plum3","plum3",
                                                                                      "plum3","plum3", "paleturquoise3", "plum3", "plum3",  "plum3","paleturquoise3",
                                                                                      "paleturquoise3","paleturquoise3","paleturquoise3","plum3", "plum3",  "lightskyblue",
                                                                                      "lightskyblue", "plum3","paleturquoise3","plum3","paleturquoise3","paleturquoise3",
                                                                                      "plum3","lightskyblue", "plum3", "plum3","paleturquoise3","lightskyblue",  "plum3",
                                                                                      "plum3", "plum3","salmon"))
                                              
                                                                  
                                                                                     
                                                                                      

plot(routes_igraph, layout = layout_with_graphopt, edge.color="black", vertex.color=c("darkorchid3", "darkorange3",   "aquamarine3",   "darkorchid3",   "chartreuse3",   "deeppink2"  ,   "deeppink2"  ,   "aquamarine3" ,  "aquamarine3" ,  "darkorchid3"  , "darkorchid3" ,  "darkorchid3"  , "darkorchid3"  , "darkorchid3"  , "darkorchid3",  
                                                                                    "aquamarine3" ,  "darkorchid3"  , "darkorchid3" ,  "darkorchid3"  , "aquamarine3"  , "aquamarine3" ,  "aquamarine3"  , "chartreuse3"  , "darkorchid3"  , "darkorchid3" ,  "chartreuse3"  , "chartreuse3"  , "darkorchid3" ,  "aquamarine3"  , "darkorchid3"  ,
                                                                                    "chartreuse3"  , "aquamarine3"  , "darkorchid3"  , "chartreuse3"  , "darkorchid3"  , "darkorchid3"  , "chartreuse3"  , "chartreuse3"  , "darkorchid3"  , "darkorchid3"  , "darkorchid3"  , "deeppink2"  ))

#Color
##import csv with color by continent 
#E=Europe (yellow), M=middle east and africa (orange), A=Asia and oceania (lightblue), N=north america (pink), S=South America	(also pink)			
colors<-read.csv("colors4network4.csv")
colbar<-colors$color

colorvector<-as.vector(colors["color"])
colorlist<-as.list(colors["color"])

colorlist


library(RColorBrewer)
  
sizebar<-c(15,20,25)

quartz.save("networkmap.pdf", type="pdf")

##calculate centrality of nodes  # do this with the full data set- not the one with fewer countries for the figure
betweenness(routes_igraph)

#########################################





