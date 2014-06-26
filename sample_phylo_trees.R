####Using our tree to encode infos

library(ape)
library(geiger)

MyTree <- read.tree("RAxML_species_tree") #newick format - unrooted

plot.phylo(MyTree)
plot.phylo(MyTree, type="cladogram")
plot.phylo(MyTree, type="fan")
plot.phylo(MyTree, type="unrooted")
plot.phylo(MyTree, type="radial")
plot.phylo(MyTree, use.edge.length=FALSE)
