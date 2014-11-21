#Invasion Syndicate#
#11/1/13
#Categorizing species we have access to

library(devtools)
install_github("taxize_", "rcrossref","ropensci")
library(taxize)
library(XML)
library(rcrossref)

####citations####
citation(package = 'taxize')[1]
citation(package = 'taxize')[2]

taxize_cite(fxn="tnrs", what="citation")
taxize_cite(fxn="eol")

####make species list####
#lab inventory
#sunflower relatives
sunflrel <- read.table("Seq_info_sunflr_relatives.txt", header=T, sep="\t", quote='"')

sunflrel$sci_name <- paste(sunflrel$genus, sunflrel$species)
head(sunflrel$sci_name)

#sunflowers
sunfl <-  read.table("Seq_info_sunflr.txt", header=T, sep="\t", quote='"')
sunfl <- sunfl[sunfl$genus!="",]
sunfl$genus <- "Helianthus"
sunfl$sci_name <- paste(sunfl$genus, sunfl$species)
str(sunfl)

#lab inventory total
spplist <- c(unique(sunflrel$sci_name), unique(sunfl$sci_name))
spplist <- unique(spplist)

#some only have genus name, and not species??? fix according to crop paper
spplist <- spplist[-c(22:23,26,29)]

spplist <- c(spplist, "Iva annua", "Dahlia hybrida", "Leontodon taraxacoides")

##add species from CGP
#EST library
cgp_ESTlib <- read.delim("CGP EST LIBRARY INFO.txt", header=T, sep="\t") #
row.names(cgp_ESTlib) <- cgp_ESTlib[,1]
cgp_ESTlib <- cgp_ESTlib[,2:27]
str(cgp_ESTlib)
levels(cgp_ESTlib$GENOTYPE)

#EST assemblies
cgp_ESTassembly <- read.delim("CGP_Comp_EST_assembly_info.txt", header=T, sep="\t")#from NCBI?
row.names(cgp_ESTassembly) <- cgp_ESTassembly[,1]
cgp_ESTassembly <- cgp_ESTassembly[,2:19]
str(cgp_ESTassembly)
cgp_ESTassembly$sci_name <- paste(cgp_ESTassembly$GENUS,cgp_ESTassembly$EPITHET)
cgp_ESTassembly$sci_name <- as.factor(cgp_ESTassembly$sci_name)
levels(cgp_ESTassembly$sci_name)

#wd assembly page...
cgp_wd_assembly <- c("Ambrosia artemisiifolia", "Ambrosia trifida", "Carthamus oxyacanthus", "Centaurea diffusa", 
            "Centaurea solstitialis", "Cichorium endivia", "Cichorium intybus", "Cirsium arvense", "Cynara cardunculus",
             "Dahlia", "Echinacea angustifolia","Glebionis segetum", "Guizotia scabra", "Helianthus annuus",
             "Helianthus tuberosus", "Iva annua", "Leontodon", "Parthenium argentatum", "Smallanthus sonchifolius", 
             "Stevia rebaudiana")
             #cgp_wd_assemblies

#outgroup page
cgp_outgroup <- c("Acicarpha spathulata", "Scaevola aemula","Nymphoides peltata", "Platycodon grandiflorus")

#cgp tree
cgp_tree <- c("Barnadesia spinosa", "Chrysanthemum")  
# other things listed on tree, but not in cgp assembly tables

##combine in one list
spplist1 <- c(spplist, levels(cgp_ESTlib$GENOTYPE),levels(cgp_ESTassembly$sci_name),cgp_wd_assembly,cgp_outgroup,cgp_tree)
spplist2 <- unique(spplist1) #ditch duplicates
spplist2 <- spplist2[-c(40:59, 63:64,69,71,75:76,78)] #ditch empties, non-identical duplicates, genus only

write(spplist2,"specieslist.txt")

spplist2 <- scan("specieslist.txt", what="list",skip=0, sep="\t", quote='"')
####check status in databases####
###check status in GISD database (issg.org)

out <- gisd_isinvasive(spplist2)
out2 <- out
out2$located_in <- "cgp_tree"
out2[out2$species %in% as.character(levels(cgp_ESTlib$GENOTYPE)),]$located_in <- "cgp_ESTlib"
out2[out2$species %in% cgp_outgroup,]$located_in <- "cgp_outgroup"
out2[out2$species %in% cgp_wd_assembly,]$located_in <- "cgp_wd_assembly"
out2[out2$species %in% as.character(levels(cgp_ESTassembly$sci_name)),]$located_in <- "cgp_ESTassembly"
out2[out2$species %in% spplist,]$located_in <- "lab_seq_inventory"
out2$located_in <- as.factor(out2$located_in)

# str(cgp_ESTlib$GENOTYPE)
# str(unique(sunflrel$sci_name))
# str(as.character(levels(cgp_ESTlib$GENOTYPE)))
# str(cgp_outgroup)

out2[27:28,]$located_in <- "lab_seq_inventory"


write.table(out2, "GISDclassification_species.txt", row.names=TRUE, col.names=TRUE, sep="\t", quote=F)

gisdinfo<- read.table("GISDclassification_species.txt", header=T, sep="\t", quote='"')

######search other db?
library(devtools)
install_github("taxize_", "ropensci")

library(taxize)
library(XML)

?eol_invasive()
 
spplist <- scan("specieslist.txt", what="list",skip=0, sep="\t", quote='"')

gisdinfo <- gisd_isinvasive(spplist)

grisout <- eol_invasive(name=spplist2, dataset='gris')
grisout2 <- do.call(cbind,grisout)
head(grisout2)

iscout <- eol_invasive(name=spplist2, dataset='isc')
iscout2 <- do.call(cbind, iscout)

daisieout <- eol_invasive(name=spplist2, dataset='daisie')
daisieout2 <- do.call(cbind, daisieout)

i3nout <- eol_invasive(name=spplist2, dataset='i3n')
i3nout2 <- do.call(cbind, i3nout)

gisdout <- eol_invasive(name=spplist2, dataset='gisd')
gisdout2 <- do.call(cbind,gisdout)



write.table(grisout2, "eol_gris_species.txt", row.names=TRUE, col.names=TRUE, sep="\t", quote=F)
write.table(iscout2, "eol_isc_species.txt", row.names=TRUE, col.names=TRUE, sep="\t", quote=F)
write.table(daisieout2, "eol_daisie_species.txt", row.names=TRUE, col.names=TRUE, sep="\t", quote=F)
write.table(i3nout2, "eol_i3n_species.txt", row.names=TRUE, col.names=TRUE, sep="\t", quote=F)
write.table(gisdout2, "eol_gisd_species.txt", row.names=TRUE, col.names=TRUE, sep="\t", quote=F)

gris <- read.table("eol_gris_species.txt", header=T, sep="\t", quote='"')
isc <- read.table("eol_isc_species.txt", header=T, sep="\t", quote='"')
daisie <- read.table("eol_daisie_species.txt", header=T, sep="\t", quote='"')
i3n <- read.table("eol_i3n_species.txt", header=T, sep="\t", quote='"')
gisd <- read.table("eol_gisd_species.txt", header=T, sep="\t", quote='"')

##combine all the databases
# head(gisdinfo)
head(gisd)

colnames(daisie)[4] <- "in_daisie"
colnames(gisd)[4] <- "in_gisd"
# colnames(gris)[2] <- "gris_id"
colnames(i3n)[4] <- "in_i3n"
colnames(isc)[4] <- "in_isc"


# # setdiff(rownames(gisd), rownames(gris))
# setdiff(rownames(gisd), rownames(i3n))
# setdiff(rownames(gisd), rownames(isc))
# setdiff(rownames(gisd), rownames(daisie))
# 
# gisd$species <- rownames(gisd)
# # gris$species <- rownames(gris)
# i3n$species <- rownames(i3n)
# isc$species <- rownames(isc)
# daisie$species <- rownames(daisie)

gisd_isinvasive("Taraxacum officinale")

eol_invasive(name="Taraxacum officinale", dataset='gisd')


gisd <- subset(gisd, !is.na(eol_object_id))
gris <- gris[2:3]
i3n <- i3n[2:3]
isc <- isc[2:3]
daisie <- daisie[2:3]


# status <- cbind(gisd,gris$gris_id, i3n$i3n_id, isc$isc_id)
# status <- merge(gisd, gris, all=TRUE)
# setdiff(i3n$species, status$species)
status <- merge(gisd, i3n, all=TRUE)
status <- merge(status, isc, all=TRUE)

# status$species <- rownames(status)
status <- merge(status, daisie, all=TRUE)
colnames(gisdinfo)[2] <- "gisdinfo"

status <- merge(status, gisdinfo, all.x=TRUE, by.y="species", by.x="searched_name")

write.table(status, "classification_species.txt", row.names=TRUE, col.names=TRUE, sep="\t", quote=F)
write.csv(status, "classification_species.csv", row.names=TRUE, quote=F)

#scored by eye in excel

status<- read.table("classification_species.txt", header=T, sep="\t", quote='"')

#########make a tree############
library(taxize)

spptree <- spplist2[-c(17,38,36)] #didn't find H. winterii, Dahlia, Carthamus palastinus in Phylomatic
spptree <- c(spptree, "Dahlia")

statustree <- status[c(1,8:9)]
statustree$color <- "red"
statustree[statustree$status=="not weedy",]$color <- "green"

# statustree <- statustree[order(statustree$species)==order(tree$tip.lable),]

tree <- phylomatic_tree(taxa=spptree)
tree$tip.label <- taxize_capwords(tree$tip.label)
# tree$tip.label <- paste(taxize_capwords(tree$tip.label), status$status)
plot(tree, cex =0.75)

tiporder <- tree$tip.label
tiporder <- gsub("_", " ",tiporder)

statustree <- statustree[-c(6,35),] #H winterii, Carthamus palastinus

# library(plyr)
# statustree <- revalue(statustree$species, "Dahlia hybrida"="Dahlia")

levels(statustree$species)[levels(statustree$species)=="Dahlia hybrida"] <- "Dahlia"
statustree <- statustree[match(tiporder, statustree$species),]

pdf("InvSyn_speciestree.pdf", useDingbats=FALSE, height=10)

plot(tree, cex =0.75, tip.color=statustree$color, show.node.label=TRUE)
legend("bottomleft", legend=c("Weed", "Not a weed"), fill=c("red","green"))
title(main="Species with lab and CGP seq data")

dev.off()

#####recategorize###########

spplist <- scan("specieslist.txt", what="character",skip=0, sep="\t", quote='"')

sppres <- tnrs(query=spplist, source="iPlant_TNRS")

# spplist <- subset(spplist,!%in%c("Centaurea maculosa", "Carthamus palastinus"))
# spplist[[c("Centaurea maculosa", "Carthamus palastinus")]] <-NULL
# spplist <- spplist[-c(17,41)]
# spplist <- c(spplist, "Centaurea stoebe","Carthamus palaestinus") #"Taraxacum campylodes"
write(spplist,"specieslist.txt")

allout <- eol_invasive(name=spplist, dataset='all')
write.table(allout, "SpeciesSummary.txt", row.names=TRUE, col.names=TRUE, sep="\t", quote=F)

#checking synonyms - #shouldn't Taroff, Cenmac, Helarg, Leotar be red/weeds?
synlist <- c("Cryptocarpha spathulata","Ambrosia elatior","Ambrosia glandulosa",  "Ambrosia maritima","Ambrosia monophylla","Ambrosia paniculata",
             "Ambrosia aptera","Carthamus oxyacantha", "Acosta diffusa","Centaurea microcalathina", "Centaurea parviflora",
             "Centaurea stoebe ssp. micranthos","Centaurea stoebe", "Centaurea bieberisteinii","Leucantha solstitialis","Cichorium crispum", "Cichorium byzantinum",
             "Cichorium cicorea","Cichorium glabratum", "Cichorium glaucum","Cichorium perenne","Cichorium rigidum","Cichorium sylvestre",
             "Breea arvensis","Carduus arvensis", "Cephalonoplos arvense", "Cirsium incanum", "Cirsium lanatum","Cirsium ochrolepideum", "Cirsium setosum",
             "Cnicus arvensis","Cnicus lanatus", "Serratula arvensis",  "Serratula spinosa",   "Breea incana",
             "Lefrovia rhaponticoides","Cynara scolymus","Brauneria angustifolia",     "Echinacea pallida var. strigosa","Chrysanthemum segetum",
             "Chrysanthemum spatiosum", "Chrysanthemum umbrosum", "Matricaria segetum", "Pyrethrum segetum","Pyrethrum umbrosum", "Xantophtalmum segetum",
            "Helianthus aridus","Helianthus jaegeri","Helianthus lenticularis","Helianthus macrocarpus","Helianthus multiflorus","Helianthus ovatus",
             "Helianthus argophyllis","Helianthus integrifolius",   "Helianthus patens",       "Helianthus praecox",    "Helianthus canescens",    "Helianthus canus",
             "Helianthus esculentus","Helianthus tomentosus","Iva ciliata",   "Iva caudata","Ambrosia pitcheri", "Lactuca altaica","Lactuca scariola","Lactuca virosa",  
             "Crepis nudicaulis","Hyoseris taraxacoides", "Leontodon nudicaulis","Limnanthemum peltatum","Nymphoides nymphaeoides",
             "Campanula glauca","Campanula grandiflora","Platycodon autumalis","Platycodon autumnalis", "Platycodon chinensis", "Platycodon glaucus",
            "Platycodon sinensis",    "Platycodon grandiflorum","Polymnia edulis",       "Polymnia sonchifolia","Eupatorium rebaudianum",
             "Taraxacum brevicorniculatum","Leontodon taraxacum", "Leontodon vulgare", "Taraxacum dens-leonis","Taraxacum mexicanum ",
            "Taraxacum retroflexum", "Taraxacum subspathulatum","Taraxacum sylvanicum","Taraxacum taraxacum","Taraxacum tenejapense ",
            "Taraxacum vulgare",    "Taraxacum campylodes","Crassina elegans",    "Zinnia australis",      "Zinnia violacea"
)
synout <- eol_invasive(name=synlist, dataset='all')
write.table(synout, "SpeciesSummary_syn.txt", row.names=TRUE, col.names=TRUE, sep="\t", quote=F)
#yes, Cenmac is red/weed
gisd_isinvasive(spplist)
gisd_isinvasive(synlist)
gisd_isinvasive(c("Taraxacum officinale", "Helianthus argophyllus", "Leontodon taraxacoides"))
#taraxacum officinale should be red/weed

eol_invasive(c("Carthamus oxyacanthus", "Carthamus palaestinus", "Carthamus oxyacantha"), dataset='all')
gisd_isinvasive(c("Carthamus oxyacanthus", "Carthamus palaestinus", "Carthamus oxyacantha"))
#Caroxy probably should be a weed, but isn't turning up in the databases...


####new trees####
library(taxize)

spplist_tree <- spplist[-c(52)] #T. officinale and T. campylodes redundant

#refresh database searches
allout$db <- "red"
allout[is.na(allout$eol_object_id),]$db <- "green"
tail(allout)

# statustree <- statustree[order(statustree$species)==order(tree$tip.lable),]

tree_weed_db <- phylomatic_tree(taxa=spplist_tree)
tree_weed_db$tip.label <- taxize_capwords(tree_weed_db$tip.label)
# tree$tip.label <- paste(taxize_capwords(tree$tip.label), status$status)
plot(tree_weed_db, cex =0.75)

tiporder <- tree_weed_db$tip.label
tiporder <- gsub("_", " ",tiporder)

# statustree <- statustree[-c(6,35),] #H winterii, Carthamus palastinus

# library(plyr)
# statustree <- revalue(statustree$species, "Dahlia hybrida"="Dahlia")

# levels(allout$species)[levels(statustree$species)=="Dahlia hybrida"] <- "Dahlia"
allout <- allout[match(tiporder, allout$searched_name),]

# pdf("InvSyn_speciestree.pdf", useDingbats=FALSE, height=10)

plot(tree_weed_db, cex =0.75, tip.color=allout$weed_db, show.node.label=TRUE)
legend("bottomleft", legend=c("Weed", "Not a weed"), fill=c("red","green"))
title(main="Species with lab and CGP seq data")

# dev.off()

####platform tree####
seqdata <- read.csv("AssemblyList_Nov29 - AssemblyList_Nov29.csv.csv", header=T, sep=",", quote='"')
head(seqdata)

seqdata <- subset(seqdata, INCLUDE==1,select=c(ASSEMBLY_ID, GENUS,SPECIES,NEWNAME,subspecies, type,read_tech))
head(seqdata)
seqdata$name <- paste(seqdata$GENUS, seqdata$SPECIES)
seqdata[seqdata$name=="Centaurea maculosa",]$name <- "Centaurea stoebe"

# allout$platform <- "blue"
# allout[is.na(allout$eol_object_id),]$weed_db <- "green"
# tail(allout)
# 
# # statustree <- statustree[order(statustree$species)==order(tree$tip.lable),]
# 
# tree_weed_db <- phylomatic_tree(taxa=spplist_tree)
# tree_weed_db$tip.label <- taxize_capwords(tree_weed_db$tip.label)
# # tree$tip.label <- paste(taxize_capwords(tree$tip.label), status$status)
# plot(tree_weed_db, cex =0.75)
# 
# tiporder <- tree_weed_db$tip.label
# tiporder <- gsub("_", " ",tiporder)
# 
# # statustree <- statustree[-c(6,35),] #H winterii, Carthamus palastinus
# 
# # library(plyr)
# # statustree <- revalue(statustree$species, "Dahlia hybrida"="Dahlia")
# 
# # levels(allout$species)[levels(statustree$species)=="Dahlia hybrida"] <- "Dahlia"
# allout <- allout[match(tiporder, allout$searched_name),]
# 
# # pdf("InvSyn_speciestree.pdf", useDingbats=FALSE, height=10)
# 
# plot(tree_weed_db, cex =0.75, tip.color=allout$weed_db, show.node.label=TRUE)
# legend("bottomleft", legend=c("Weed", "Not a weed"), fill=c("red","green"))
# title(main="Species with lab and CGP seq data")
# 
# # dev.off()



#####how many asteraceae are invasives?####

Aster <- downstream("Asteraceae", db = c('col','itis'), downto = 'Species')

AsterL <- Aster[[1]]

AsterL2 <- as.vector(unique(AsterL$childtaxa_name))

Asterout <- eol_invasive(name=AsterL2, dataset='all') #count = TRUE,

summary(Asterout)

write.table(Asterout, "allAstersWeedStatus_20140909.txt", row.names=TRUE, col.names=TRUE, sep="\t", quote=F)

####how many us fed weeds are asteraceae?####
#from Sept 2014
USinv<- scan(file.choose(), what="list",skip=0, sep="\t", quote='"')#"USinvlist.txt"
write(USinv,"USinvlist.txt")

USinv <- unique(USinv)

USinvFam <- tax_name(query = USinv, get = "family", db = "both", verbose=TRUE)
USinvFam <- cbind(USinv, USinvFam)
USinvFam$family <- as.factor(USinvFam$family)
USinvFam$USinv <- as.factor(USinvFam$USinv)
USinvFam <- unique(USinvFam)
summary(USinvFam, verbose=TRUE)

write.table(USinvFam, "USinv_family.txt",row.names=TRUE, col.names=TRUE, sep="\t", quote=F)

####helianthus####
Sunfl <- downstream("Helianthus", db = c('col','itis'), downto = 'Species')

SunflL <- Sunfl[[1]]

SunflL2 <- as.vector(unique(SunflL$childtaxa_name))

Sunflout <- eol_invasive(name=SunflL2, dataset='all') #count = TRUE,

summary(Sunflout)
