InvasionSyndicate
=================

Code contributing to Hodgins KA, Bock DG, Hahn MA et al. (2015) Comparative genomics in the Asteraceae reveals little evidence for parallel evolutionary change in invasive taxa. Molecular Ecology, 24, 2226–2240.

R code by KGT largely used for the purpose of classifying ~40 species included in comparative genomics dataset, using the taxize package in R. Additional work (not described in paper) addresses traits, synonyms, and phylogenies of those species. Main action included in inv.syndicate.R - producing species lists, querying databases, producing trees.

Other R files (not included in paper): 
  WeedScore.R - checking for correlation between "trait score" and distribution. 
  getting_traits.R - combining trait data with sequence data. 
  sample_phylo_trees.R - messing around with plot.phylo()
  Trait_Finder.R - scraping trait data from USDA PLANTS database (by R. Colautti).
  is.invasive.func.R - other functions for determining invasion status
  
