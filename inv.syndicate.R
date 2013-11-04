#Invasion Syndicate#
#11/1/13

#Categorizing species we have access to
#sunflower relatives
sunflrel <- read.table("Seq_info_sunflr_relatives.txt", header=T, sep="\t", quote='"')

sunflrel$sci_name <- paste(sunflrel$genus, sunflrel$species)
head(sunflrel$sci_name)

spplist <- unique(sunflrel$sci_name)
# #some only have genus name, and not species??? will be reported as Not in GISD...
# spplist <- spplist[c(-"",-"Iva",-"Dahlia",-"Leontodon")]

######add species from CGP
#EST library
cgpssp <- read.delim("CGP EST LIBRARY INFO.txt", header=T, sep="\t") #
row.names(cgpssp) <- cgpssp[,1]
cgpssp <- cgpssp[,2:27]

cgpass1 <- read.delim("CGP_Comp_EST_assembly_info.txt", header=T, sep="\t")#from NCBI?

cgpass2 <- c("Ambrosia artemisiifolia", "Ambrosia trifida", "Carthamus oxyacanthus", "Centaurea diffusa", 
            "Centaurea solstitialis", "Cichorium endivia", "Cichorium intybus", "Cirsium arvense", "Cynara cardunculus",
             #cgp_wd_assemblies
  
cgpass3 <- c("Acicarpha spathulata", "Scaevola aemula","Nymphoides peltata", "Platycodon grandiflorus", 
             "Barnadesia spinosa")  
#outgroups and other things listed on tree, but not in tables






spplist <- c(spplist, levels(cgpssp$GENOTYPE))


#check status in GISD database (issg.org)

library(devtools)
install_github("taxize_", "ropensci")

library(taxize)
library(XML)



gisd_isinvasive <- function(x, simplify = FALSE)
{
  species <- gsub(" ", "+", x) # reformat sp list
  # create urls to parse
  urls <- paste("http://www.issg.org/database/species/search.asp?sts=sss&st=sss&fr=1&x=13&y=9&sn=",
                species, "&rn=&hci=-1&ei=-1&lang=EN", sep = "")
  # create a data.frame to store the Output
  out <- data.frame(species = x, status = c(1:length(urls)))
  #loop through all species
  for(i in 1:length(urls)){
    #Parse url and extract table
    doc <- htmlTreeParse(urls[i], useInternalNodes = TRUE)
    if(length(getNodeSet(doc, "//span[@class='SearchTitle']")) > 0){
      out[i, 2] <- "Not in GISD"
    }
    else{
      if(simplify == FALSE){
        one <- getNodeSet(doc, "//span[@class='ListNote']", fun=xmlValue)[[1]]
        two <- paste(getNodeSet(doc, "//span[@class='Info']", fun=xmlValue), collapse="; ")
        out[i, 2] <- paste(one, two, sep="; ")
      } else {
        out[i, 2] <- "Invasive"
      }
    }
    message(paste("Checking species", i))        
  }
  message("Done")
  return(out)
}

out <- gisd_isinvasive(spplist)
cat(out[1,2])

write.table(out, "GISDclassification_lab_CGP.txt", row.names=TRUE, col.names=TRUE)
