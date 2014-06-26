library(RCurl)

setwd("C://Users/Alliaria/Documents/Colautti 2012/ESA Invaders Different/Analysis")
EU<-read.csv("Europe.csv",header=T)
Arg<-read.csv("Argentina.csv",header=T)
SA<-read.csv("South_Africa.csv",header=T)

setwd("C://Users/Alliaria/Documents/Colautti 2014/ComparativeTransciptomesStudy")

traits<-read.csv("Trait Data/TRY_DATA.csv",header=T)

species<-read.csv("SpeciesList.csv",header=T,as.is=T)
species<-species[!duplicated(species),]
# Data correction
species[nrow(species),c("GENUS","SPECIES")]<-c("Zinnia","elegans")
species<-species[species$SPECIES!="5 spp",]


species$USDA<-paste("http://plants.usda.gov/core/profile?symbol=")
species$GrowthHabit<-NA
species$StateList<-NA
species$ProvList<-NA
species$NStates<-0
species$NProv<-0

for (i in 1:nrow(species)){
  symb<-paste0(toupper(substr(species$GENUS[i],1,2)),toupper(substr(species$SPECIES[i],1,2)))
  for (j in 1:20){
    content<-getURL(paste0(species$USDA[i],symb))
    # Find Genus + Species match
    if (grepl(species$GENUS[i],content) & grepl(species$SPECIES[i],content)){
      content<-unlist(strsplit(content,"[\n]"))
      species$USDA[i]<-paste0(species$USDA[i],symb)
      species$GrowthHabit[i]<-gsub("<.*","",gsub("\t","",content[grep("Growth",content)+5]))
      dist<-content[grep("<strong>USA</strong>",content)]
      USA<-gsub("USA</strong> \\((.*)\\),.*","\\1",dist)
      USA<-gsub(".*([A-Z]{2}).*","\\1",unlist(strsplit(USA,"<a href.*?\">")))
      USA<-USA[2:length(USA)]
      species$StateList[i]<-paste(USA,collapse=", ")
      species$NStates[i]<-length(USA)
      dist<-content[grep("CAN</strong>",content)]
      CAN<-unlist(strsplit(gsub(".* CAN</strong> \\((.*?)\\).*","\\1",dist),", "))
      
      species$ProvList[i]<-paste(CAN,collapse=", ")
      species$NProv[i]<-length(CAN)
      j<-20 # Exit for loop
    }
    if (!(grepl(species$GENUS[i],content) & grepl(species$SPECIES[i],content))){
      j<-j+1
      symb<-paste0(toupper(substr(species$GENUS[i],1,2)),toupper(substr(species$SPECIES[i],1,2)),j)
    }
  }
}

# Add EU distribution
species$bind<-paste(species$GENUS,species$SPECIES)
species<-merge(species,EU[,c("Species","Occurrence")],by.x="bind",by.y="Species",all.x=T)
names(species)[names(species)=="Occurrence"]<-"EU"
species$EU[is.na(species$EU)]<-0
# Add Argentina distribution
species<-merge(species,Arg[,c("Species","Occurrence")],by.x="bind",by.y="Species",all.x=T)
names(species)[names(species)=="Occurrence"]<-"Arg"
species$Arg[is.na(species$Arg)]<-0
# Add South Africa disribution info
species<-merge(species,SA[,c("Species","Occurrence")],by.x="bind",by.y="Species",all.x=T)
names(species)[names(species)=="Occurrence"]<-"SA"
species$SA[is.na(species$SA)]<-0
# Calculate weighted distribution trait
species$dist<-log((species$NStates+species$NProv)/mean(species$NStates+species$NProv,na.rm=T)+species$EU/mean(species$EU,na.rm=T))
hist(species$dist)
species$distbin<-"LOW"
species$distbin[species$dist>2]<-"HIGH"

# Add traits from TRY Database
traits$bind<-paste(traits$Genus,traits$SpeciesEpithet)
traitdata<-merge(species,traits,by="bind",all.x=T)

write.csv(traitdata,"SpeciesTraits.csv",row.names=F)

