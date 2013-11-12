#Invasion Syndicate#
#11/1/13

#Categorizing species we have access to
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

######add species from CGP
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



#########combine in one list


spplist1 <- c(spplist, levels(cgp_ESTlib$GENOTYPE),levels(cgp_ESTassembly$sci_name),cgp_wd_assembly,cgp_outgroup,cgp_tree)
spplist2 <- unique(spplist1) #ditch duplicates
spplist2 <- spplist2[-c(40:59, 63:64,69,71,75:76,78)] #ditch empties, non-identical duplicates, genus only

write(spplist2,"specieslist.txt")

spplist2 <- scan("specieslist.txt", what="list",skip=0, sep="\t", quote='"')
########check status in GISD database (issg.org)

library(devtools)
install_github("taxize_", "ropensci")

library(taxize)
library(XML)


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

#eol api key: 70268ca1d5fb6687295ae3623bccd8c9109e07d6

grisout <- eol_invasive(name=spplist2, dataset='gris',,key="70268ca1d5fb6687295ae3623bccd8c9109e07d6")
grisout2 <- do.call(rbind,grisout)

iscout <- eol_invasive(name=spplist2, dataset='isc',key="70268ca1d5fb6687295ae3623bccd8c9109e07d6")
iscout2 <- do.call(rbind, iscout)

daisieout <- eol_invasive(name=spplist2, dataset='daisie',key="70268ca1d5fb6687295ae3623bccd8c9109e07d6")
daisieout2 <- do.call(rbind, daisieout)

i3nout <- eol_invasive(name=spplist2, dataset='i3n',key="70268ca1d5fb6687295ae3623bccd8c9109e07d6")
i3nout2 <- do.call(rbind, i3nout)

gisdout <- eol_invasive(name=spplist2, dataset='gisd',key="70268ca1d5fb6687295ae3623bccd8c9109e07d6")
gisdout2 <- do.call(rbind,gisdout)

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

####combine all the databases###
head(gisdinfo)
head(gisd)

colnames(daisie)[2] <- "daisie_id"
colnames(gisd)[2] <- "gisd_id"
colnames(gris)[2] <- "gris_id"
colnames(i3n)[2] <- "i3n_id"
colnames(isc)[2] <- "isc_id"


setdiff(rownames(gisd), rownames(gris))
setdiff(rownames(gisd), rownames(i3n))
setdiff(rownames(gisd), rownames(isc))
setdiff(rownames(gisd), rownames(daisie))

gisd$species <- rownames(gisd)
gris$species <- rownames(gris)
i3n$species <- rownames(i3n)
isc$species <- rownames(isc)
daisie$species <- rownames(daisie)

gisd <- gisd[2:3]
gris <- gris[2:3]
i3n <- i3n[2:3]
isc <- isc[2:3]
daisie <- daisie[2:3]


# status <- cbind(gisd,gris$gris_id, i3n$i3n_id, isc$isc_id)
status <- merge(gisd, gris, all=TRUE)
setdiff(i3n$species, status$species)
status <- merge(status, i3n, all=TRUE)
status <- merge(status, isc, all=TRUE)

# status$species <- rownames(status)
status <- merge(status, daisie, all=TRUE)
status <- merge(status, gisdinfo, all=TRUE)

write.table(status, "classification_species.txt", row.names=TRUE, col.names=TRUE, sep="\t", quote=F)
write.csv(status, "classification_species.csv", row.names=TRUE, col.names=TRUE, sep="\t", quote=F)

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
