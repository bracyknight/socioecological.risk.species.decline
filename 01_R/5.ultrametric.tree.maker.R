
library(phytools)
library(ape)
library(stringr)

#setwd("~/Dropbox/01_Ch3_redux/Ch3")

tree <- read.nexus('02_data/10k.genbank.phylo.nex')
tips <- data.frame(tree$tip.label)
tree <- drop.tip(tree, tip = c(179, 32)) #removing extinct species not needed

plotTree(tree)
is.ultrametric(tree)
#Get the nodes of common ancestors of homo-pan, homo-pongo, Papio- Theropithecus, Cebus- Saimiri, Loris- Galago
node <- findMRCA(tree, c('Homo_sapiens', 'Pan_troglodytes_verus'))
#homo-pan = 425
node <- c(node, findMRCA(tree, c('Homo_sapiens', 'Pongo_pygmaeus')))
#homo-pongo = 421
node <- c(node, findMRCA(tree, c('Papio_anubis', 'Theropithecus_gelada')))
#Papio- Theropithecus = 341
node <- c(node, findMRCA(tree, c('Cebus_albifrons', 'Saimiri_ustus')))
#Cebus- Saimiri = 479
node <- c(node, findMRCA(tree, c('Loris_tardigradus', 'Galago_alleni')))
# Loris- Galago = 572
node <- c(node, findMRCA(tree, c('Cebus_albifrons', 'Papio_anubis')))
# Extant Catarrhini = 302

age.min <- c(5, 12.5, 3.5, 12.5, 38, 21)
age.max <- c(8, 18, 6.5, 15, 42, 30)
soft.bounds <- c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
mycalibration <- data.frame(node, age.min, age.max, soft.bounds) 

tree.c <- chronos(tree, lambda = 0.1, model = 'correlated', calibration = mycalibration)
is.ultrametric(tree.c)
plot(tree.c)
tree <- tree.c 

#Convert names to new designation
tree$tip.label[which(tree$tip.label == 'Cebus_apella')] <- 'Sapajus_apella'
tree$tip.label[which(tree$tip.label == 'Cebus_xanthosternos')] <- 'Sapajus_xanthosternos'
tree$tip.label <- tolower(tree$tip.label)
tips <- data.frame(tree$tip.label)

#Build the merge data.frame
tips <- tree$tip.label
tips <-data.frame(tips)
names(tips) <- "og"

tips$genus <- str_split_fixed(tips$og, "_", n = Inf)[,1]
tips$species <- str_split_fixed(tips$og, "_", n = Inf)[,2]
tips$binomial <- paste(tips$genus, tips$species, sep = " ")
tips$binomial <- tolower(tips$binomial)
tips$og <- NULL
tips$in.gb <- "Y"

tree$tip.label <- tips$binomial

dup.sp <- which(duplicated(tree$tip.label))
tree <- drop.tip(tree, tip = dup.sp)
tip_order <- factor(tree$tip.label)
write.tree(tree, '02_data/untrametric.genbank.tre')

##2 BRING IN THE HUMAN.DB AND CLEAN UP FOR GENUS-LEVEL CORRELATIONS
d <- human.db

#Genus Sapajus supercedes Cebus for several species:
d <- droplevels(d)
d$binomial[which(d$binomial == 'cebus apella')] <- 'sapajus apella'
d$binomial[which(d$binomial == 'cebus flavius')] <- 'sapajus flavius'
d$binomial[which(d$binomial == 'cebus libidinosus')] <- 'sapajus libidinosus'
d$binomial[which(d$binomial == 'cebus nigritus')] <- 'sapajus nigritus'
d$binomial[which(d$binomial == 'cebus xanthosternos')] <- 'sapajus xanthosternos'
d$binomial <- factor(d$binomial)

## 3 DO THE PHYLO-DATA MERGE - MAKE SURE TIPS LINE UP


#which species are in both DB's?
orphans <- setdiff(levels(tip_order), levels(d$binomial))
shared <- intersect(levels(tip_order), levels(d$binomial))
d <- subset(d, binomial %in% shared)
d <- droplevels(d)

tree <- drop.tip(tree, tip = orphans)
tip_order <- factor(tree$tip.label) 
levels(d$binomial)[as.numeric(tip_order)] == tree$tip.label
d$ordered_binomial <- factor(d$binomial, levels(d$binomial)[as.numeric(tip_order)])
all(levels(d$ordered_binomial) == tree$tip.label) # final check for agreement

# construct phylogenetic correlation matrix
cor_mat.s <- vcv(tree, corr=TRUE)
all(dimnames(cor_mat)[[1]] == levels(d$ordered_genus))
L_phylo <- t(chol(cor_mat))
image(cor_mat)


### GENUS TREE MAKER

#after working through to "write.tree" above
tree.g <- read.tree('02_data/untrametric.genbank.tre')

#for genus vs. species head to head, start genus section here
tips <- (tree.g$tip.label)
genera<-unique(sapply(strsplit(tips,"_"),function(x) x[1]))
ii<-sapply(genera,function(x,y) grep(x,y)[1],y=tips)
tree.g<-drop.tip(tree.g,setdiff(tree.g$tip.label,tips[ii]))
plot(tree.g)
tree.g$tip.label<-sapply(strsplit(tree.g$tip.label,"_"),function(x) x[1])
which(tree.g$tip.label == "ateles") #for unknown reason, even after culling, I end up with 2 ateles tips
tree.g <- drop.tip(tree.g, 34) #this removes one, but will not affect phylogeny
plotTree(tree.g,ftype="i")

#Genus Sapajus supercedes Cebus for several species:
d <- human.db

d$binomial[which(d$binomial == 'cebus apella')] <- 'sapajus apella'
d$binomial[which(d$binomial == 'cebus flavius')] <- 'sapajus flavius'
d$binomial[which(d$binomial == 'cebus libidinosus')] <- 'sapajus libidinosus'
d$binomial[which(d$binomial == 'cebus nigritus')] <- 'sapajus nigritus'
d$binomial[which(d$binomial == 'cebus xanthosternos')] <- 'sapajus xanthosternos'
d$binomial <- factor(d$binomial)
levels(d$binomial)


d$genus <- gsub( " .*$", "", d$binomial )
d$genus <- as.factor(d$genus)
#proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
#d$genus <- proper(d$genus)
#d$genus <- factor(d$genus)
levels(d$genus)


## 3 DO THE PHYLO-DATA MERGE - MAKE SURE TIPS LINE UP

tip_order <- factor(tree.g$tip.label) #getting order of tips in phylogey

#which genera are in both DB's?
orphans <- setdiff(levels(tip_order), levels(d$genus))
shared <- intersect(levels(tip_order), levels(d$genus))
d <- subset(d, genus %in% shared)
d <- droplevels(d)

tree.g <- drop.tip(tree.g, tip = orphans)
tip_order <- factor(tree.g$tip.label) 
levels(d$genus)[as.numeric(tip_order)] == tree.g$tip.label
d$ordered_genus <- factor(d$genus, levels(d$genus)[as.numeric(tip_order)])
all(levels(d$ordered_genus) == tree.g$tip.label) # final check for agreement

# construct phylogenetic correlation matrix
cor_mat.g <- vcv(tree.g, corr=TRUE)
all(dimnames(cor_mat.g)[[1]] == levels(d$ordered_genus))
L_phylo <- t(chol(cor_mat.g))
image(cor_mat.g)


