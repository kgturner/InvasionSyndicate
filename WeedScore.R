#Trait score and Dist score

scores <- read.table("TraitScore_DistScore.txt", header=T)
scores$name <- paste(scores$genus,scores$sp)
scores.na <- scores[!is.na(scores$dist),]


#correlation between trait score and distribution score?
cor(scores.na$TraitScore, scores.na$dist)
cor.test(scores.na$TraitScore, scores.na$dist,alternative="greater",method="pearson", exact=T)

# library("SuppDists")
# pKendall
# pSpearman