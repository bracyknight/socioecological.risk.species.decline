

library(ape)
library(phytools)
library(stringr)


setwd("~/Dropbox/01_Ch3_redux/Ch3")
#############################
#Species-level

## 1 BRING IN THE TREE, MAKE ULTRAMETRIC, & CONVERT TO GENUS-LEVEL
tree <- read.nexus('data/10k.genbank.chrono.nex')
tree.p <- read.nexus('data/10k.genbank.phylo.nex')

is.ultrametric(tree)
tree <- chronos(tree)
is.ultrametric(tree)

is.ultrametric(tree.p)
#tree.p <- chronos(tree.p)

plot(tree, cex = 0.5)
plot(tree.p, cex = 0.5)



tree.u <- chronopl(tree.p,
                   0.7, age.min = 1, age.max = NULL,
                   S = 1, tol = 1e-8,
                   CV = FALSE, eval.max = 500, iter.max = 500)
summary(tree)
#tree <- read.nexus('data/10k.genbank.nex')

#tree <- tree.u


#Convert names to new designation
tree$tip.label[which(tree$tip.label == 'Cebus_apella')] <- 'Sapajus_apella'
tree$tip.label[which(tree$tip.label == 'Cebus_xanthosternos')] <- 'Sapajus_xanthosternos'
tree$tip.label <- tolower(tree$tip.label)

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

#tree.u <- chronopl(tree,
                   0, age.min = 1, age.max = NULL,
                   S = 1, tol = 1e-8,
                   CV = FALSE, eval.max = 500, iter.max = 500)

#is.ultrametric(tree.u)
#tree <- tree.u

dup.sp <- which(duplicated(tree$tip.label))
tree <- drop.tip(tree, tip = dup.sp)
tip_order <- factor(tree$tip.label)
write.tree(tree, 'data/untrametric.genbank.tre')

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


#which genera are in both DB's?
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
cor_mat <- vcv(tree, corr=TRUE)
all(dimnames(cor_mat)[[1]] == levels(d$ordered_genus))
L_phylo <- t(chol(cor_mat))
image(cor_mat)


#############################
#Genus-level

## 1 BRING IN THE TREE, MAKE ULTRAMETRIC, & CONVERT TO GENUS-LEVEL
tree <- read.nexus('data/10k.genbank.phylo.nex')
#Convert names to new designation
tree$tip.label[which(tree$tip.label == 'Cebus_apella')] <- 'Sapajus_apella'
tree$tip.label[which(tree$tip.label == 'Cebus_xanthosternos')] <- 'Sapajus_xanthosternos'

tips <-tree$tip.label
tree.u <- chronopl(tree,
                   0, age.min = 1, age.max = NULL,
                   S = 1, tol = 1e-8,
                   CV = FALSE, eval.max = 500, iter.max = 500)

is.ultrametric(tree.u)
tree <- tree.u

#for genus vs. species head to head, start genus section here
tips <- (tree$tip.label)
genera<-unique(sapply(strsplit(tips,"_"),function(x) x[1]))
ii<-sapply(genera,function(x,y) grep(x,y)[1],y=tips)
tree<-drop.tip(tree,setdiff(tree$tip.label,tips[ii]))
plotTree(tree,ftype="i")
tree$tip.label<-sapply(strsplit(tree$tip.label,"_"),function(x) x[1])
which(tree$tip.label == "ateles") #for unknown reason, automatred genus drop-tip misses one
tree <- drop.tip(tree, 20) #so, this gets it
plotTree(tree,ftype="i") #check for only one ateles tip


##2 BRING IN THE HUMAN.DB AND CLEAN UP FOR GENUS-LEVEL CORRELATIONS
#d <- human.db #excise this line only if you are comparing genus and spp

#Genus Sapajus supercedes Cebus for several species:
d <- droplevels(d)
levels(d$binomial)
d$binomial[which(d$binomial == 'cebus apella')] <- 'sapajus apella'
d$binomial[which(d$binomial == 'cebus flavius')] <- 'sapajus flavius'
d$binomial[which(d$binomial == 'cebus libidinosus')] <- 'sapajus libidinosus'
d$binomial[which(d$binomial == 'cebus nigritus')] <- 'sapajus nigritus'
d$binomial[which(d$binomial == 'cebus xanthosternos')] <- 'sapajus xanthosternos'

d$genus <- gsub( " .*$", "", d$binomial )
d$genus <- as.factor(d$genus)
proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
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

tree <- drop.tip(tree, tip = orphans)
tip_order <- factor(tree$tip.label) 
levels(d$genus)[as.numeric(tip_order)] == tree$tip.label
d$ordered_genus <- factor(d$genus, levels(d$genus)[as.numeric(tip_order)])
all(levels(d$ordered_genus) == tree$tip.label) # final check for agreement



