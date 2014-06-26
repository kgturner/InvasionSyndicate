###traits


#update spplist and seqdata to only include 42 spp in RAxML tree
spplist <- scan("specieslist.txt", what="character",skip=0, sep="\t", quote='"')

seqdata <- read.csv("AssemblyList_Nov29 - AssemblyList_Nov29.csv.csv", header=T, sep=",", quote='"')
# head(seqdata)
seqdata <- subset(seqdata, INCLUDE==1,select=c(ASSEMBLY_ID, GENUS,SPECIES,NEWNAME,subspecies, type,read_tech))
# head(seqdata)
seqdata$name <- paste(seqdata$GENUS, seqdata$SPECIES)
# seqdata[seqdata$name=="Centaurea stoebe",]$name <- "Centaurea maculosa"
seqdata[seqdata$name=="Carthamus palastinus",]$name <- "Carthamus palaestinus"


traits <- read.csv("SpeciesTraits.csv")
traits[5,]
traits$bind <- as.character(traits$bind)
# traits[traits$bind=="Centaurea stoebe",]$bind <- "Centaurea maculosa"
traits[traits$bind=="Carthamus palastinus",]$bind <- "Carthamus palaestinus"
# traits <- rbind(traits, )
traits$bind <- as.factor(traits$bind)

setdiff(spplist, traits$bind)
setdiff(unique(seqdata$name), traits$bind)
seqdata <- subset(seqdata, name%in%traits$bind)
spplist <- subset(spplist,spplist%in%traits$bind)

spplist <- spplist[-41]
spplist <- c(spplist, "Centaurea maculosa")



write(spplist,"specieslist.txt")
write.table(seqdata, "AssemblyList_June25.txt", row.names=TRUE, col.names=TRUE, sep="\t", quote=F)


##getting info from Fryxell

# pageid <- eol_search(spplist)$pageid[1]
# (out <- eol_pages(taxonconceptID=pageid, synonyms=TRUE)$scinames)

synonyms(spplist, db="tropicos")
synonyms(spplist, db="itis")
