#Synopsis
#Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health
#Across the United States, which types of events have the greatest economic consequences


#Data Processing
library(dplyr)
library(ggplot2)
library(gridExtra)

setwd("C:/Users/asus/Desktop/Reproducible Research")

#Downloading Files

if(!file.exists("repdata_data_StormData.csv.bz2")){
  
  fileUrl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  
  download.file(fileUrl,destfile="./repdata_data_StormData.csv.bz2")
}

storm<-read.csv("repdata_data_StormData.csv.bz2", header=TRUE, stringsAsFactors=FALSE) 

head(storm)

variables<-c("EVTYPE","FATALITIES","INJURIES","PROPDMG", "PROPDMGEXP","CROPDMG","CROPDMGEXP")
storm<-storm[variables]

head(storm)

unique(storm$PROPDMGEXP)

unique(storm$CROPDMGEXP)


storm_modified<-storm%>%
  mutate(PROPDMGEXP=replace(PROPDMGEXP,PROPDMGEXP=="K"|PROPDMGEXP==3,10^3))%>%
  mutate(PROPDMGEXP=replace(PROPDMGEXP,PROPDMGEXP=="M"|PROPDMGEXP=="m"|PROPDMGEXP==6,10^6))%>%
  mutate(PROPDMGEXP=replace(PROPDMGEXP,PROPDMGEXP==""|PROPDMGEXP=="+"|PROPDMGEXP==0|PROPDMGEXP=="?"|PROPDMGEXP=="-",10^0))%>%
  mutate(PROPDMGEXP=replace(PROPDMGEXP,PROPDMGEXP=="B",10^9))%>%mutate(PROPDMGEXP=replace(PROPDMGEXP,PROPDMGEXP==5,10^5))%>%
  mutate(PROPDMGEXP=replace(PROPDMGEXP,PROPDMGEXP==4,10^4))%>%
  mutate(PROPDMGEXP=replace(PROPDMGEXP,PROPDMGEXP=="h"|PROPDMGEXP=="H"|PROPDMGEXP==2,10^2))%>%
  mutate(PROPDMGEXP=replace(PROPDMGEXP,PROPDMGEXP==7,10^7))%>%mutate(PROPDMGEXP=replace(PROPDMGEXP,PROPDMGEXP==1,10^1))%>%
  mutate(PROPDMGEXP=replace(PROPDMGEXP,PROPDMGEXP==8,10^8))%>%
  mutate(CROPDMGEXP=replace(CROPDMGEXP,CROPDMGEXP==""|CROPDMGEXP==0|CROPDMGEXP=="?",10^0))%>%mutate(CROPDMGEXP=replace(CROPDMGEXP,CROPDMGEXP=="M"|CROPDMGEXP=="m",10^6))%>%
  mutate(CROPDMGEXP=replace(CROPDMGEXP,CROPDMGEXP=="k"|CROPDMGEXP=="K",10^3))%>%mutate(CROPDMGEXP=replace(CROPDMGEXP,CROPDMGEXP=="B",10^9))%>%
  mutate(CROPDMGEXP=replace(CROPDMGEXP,CROPDMGEXP==2,10^2))
  

storm_modified$PROPDMGEXP<-as.numeric(storm_modified$PROPDMGEXP)
storm_modified$CROPDMGEXP<-as.numeric(storm_modified$CROPDMGEXP)


storm_final<-storm_modified%>%
  mutate(PROPDMG_FINAL=PROPDMG*PROPDMGEXP)%>%
  mutate(CROPDMG_FINAL=CROPDMG*CROPDMGEXP)%>%
  select(EVTYPE,FATALITIES,INJURIES,PROPDMG_FINAL,CROPDMG_FINAL)
  

library(reshape2)

storm_fatalities_injuries<-melt(storm_final,id.vars="EVTYPE",measure.vars=c("FATALITIES","INJURIES"))

storm_fatalities_injuries<-storm_fatalities_injuries%>%
  group_by(EVTYPE,variable)%>%
  summarize(total=sum(value))%>%
  group_by(EVTYPE)%>%
  mutate(population_total=sum(total))%>%
  arrange(desc(population_total))

storm_fatalities_injuries<-head(storm_fatalities_injuries,14)

storm_fatalities_injuries

library(RColorBrewer)

p1<-ggplot(data=storm_fatalities_injuries,aes(x=(reorder(EVTYPE,population_total)),y=total,fill=variable))+
  geom_bar(stat="Identity",position=position_dodge(width = 0.9))+geom_text(aes(label=total), angle=0, size=3,check_overlap = TRUE)+
  scale_fill_brewer(palette="PRGn")+labs(x="Event Type",y="No of People Affected",title="Most Harmful Events with Respect to Population Health")+
  theme(axis.text.x = element_text(angle=90, hjust=1))

p1

storm_fatalities<-storm_final%>%
  group_by(EVTYPE)%>%
  summarize(TOTAL_FATALITIES=sum(FATALITIES))%>%
  arrange(desc(TOTAL_FATALITIES))%>%
  top_n(7)
  
p2<-ggplot(data=storm_fatalities,aes(x=(reorder(EVTYPE,TOTAL_FATALITIES)),y=TOTAL_FATALITIES,fill=EVTYPE))+geom_bar(stat="identity")+
  geom_text(aes(label=TOTAL_FATALITIES), angle=0, size=3,check_overlap = TRUE)+scale_fill_brewer(palette="Spectral")+
  theme(axis.text.x = element_text(angle=90, hjust=1))+labs(x="Event Type",y="No of Fatalities")
  
p2
storm_injuries<-storm_final%>%
  group_by(EVTYPE)%>%
  summarize(TOTAL_INJURIES=sum(INJURIES))%>%
  arrange(desc(TOTAL_INJURIES))%>%
  top_n(7)

p3<-ggplot(data=storm_injuries,aes(x=(reorder(EVTYPE,TOTAL_INJURIES)),y=TOTAL_INJURIES,fill=EVTYPE))+geom_bar(stat="identity")+
  geom_text(aes(label=TOTAL_INJURIES), angle=0, size=3,check_overlap = TRUE)+scale_fill_brewer(palette="Spectral")+
  theme(axis.text.x = element_text(angle=90, hjust=1))+labs(x="Event Type",y="No of Injuries")

p3


grid.arrange(arrangeGrob(p1,ncol=1,nrow=1),arrangeGrob(p2,p3,ncol=2,layout_matrix = matrix(c(1, 2),byrow = TRUE,ncol=2)),heights=c(2,1.5))




storm_prop_crop<-melt(storm_final,id.vars="EVTYPE",measure.vars=c("PROPDMG_FINAL","CROPDMG_FINAL"))

storm_prop_crop<-storm_prop_crop%>%
  group_by(EVTYPE,variable)%>%
  summarize(total=sum(value)/10^9)%>%
  group_by(EVTYPE)%>%
  mutate(prop_and_crop_total=sum(total))%>%
  arrange(desc(prop_and_crop_total))

storm_prop_crop<-head(storm_prop_crop,14)

storm_prop_crop

library(RColorBrewer)


storm_prop_dmg<-storm_final%>%
  group_by(EVTYPE)%>%
  summarize(TOTAL_PROP_DMG=round(sum(PROPDMG_FINAL)/10^9,1))%>%
  arrange(desc(TOTAL_PROP_DMG))%>%
  top_n(7)

p4<-ggplot(data=storm_prop_dmg,aes(x=(reorder(EVTYPE,TOTAL_PROP_DMG)),y=TOTAL_PROP_DMG,fill=EVTYPE))+geom_bar(stat="identity")+
  geom_text(aes(label=TOTAL_PROP_DMG), angle=0, size=3,check_overlap = TRUE)+scale_fill_brewer(palette="Purples")+
  theme(axis.text.x = element_text(angle=90, hjust=1))+labs(x="Event Type",y="Total Property Damage (billion $)",title="Most Harmful Events with Respect to Property Damage")

p4



storm_crop_dmg<-storm_final%>%
  group_by(EVTYPE)%>%
  summarize(TOTAL_CROP_DMG=round(sum(CROPDMG_FINAL)/10^9,2))%>%
  arrange(desc(TOTAL_CROP_DMG))%>%
  top_n(7)

p5<-ggplot(data=storm_crop_dmg,aes(x=(reorder(EVTYPE,TOTAL_CROP_DMG)),y=TOTAL_CROP_DMG,fill=EVTYPE))+geom_bar(stat="identity")+
  geom_text(aes(label=TOTAL_CROP_DMG), angle=0, size=3,check_overlap = TRUE)+scale_fill_brewer(palette="Greens")+
  theme(axis.text.x = element_text(angle=90, hjust=1))+labs(x="Event Type",y="Total Crop Damage (billion $)",title="Most Harmful Events with Respect to Crop Damage")

p5


grid.arrange(p4,p5,nrow=2)



