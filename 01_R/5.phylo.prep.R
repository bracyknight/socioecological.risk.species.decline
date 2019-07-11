

#tree <- read.nexus('10k.wilsonreader.phylo.nex')
#tree <- drop.tip(tree, tip = c(176, 32))

tree <- read.nexus('02_Data/10k.genbank.phylo.nex')
tips <- data.frame(tree$tip.label)
tree <- drop.tip(tree, tip = c(179, 32))


plot(tree)
is.ultrametric(tree)
#Get the nodes of common ancestors of homo-pan, homo-pongo, Papio- Theropithecus, Cebus- Saimiri, Loris- Galago
findMRCA(tree, c('Homo_sapiens', 'Pan_troglodytes_verus'))
#homo-pan = 425
findMRCA(tree, c('Homo_sapiens', 'Pongo_pygmaeus'))
#homo-pongo = 421
findMRCA(tree, c('Papio_anubis', 'Theropithecus_gelada'))
#Papio- Theropithecus = 341
findMRCA(tree, c('Cebus_albifrons', 'Saimiri_ustus'))
#Cebus- Saimiri = 479
findMRCA(tree, c('Loris_tardigradus', 'Galago_alleni'))
# Loris- Galago = 572
findMRCA(tree, c('Cebus_albifrons', 'Papio_anubis'))
# Extant Catarrhini = 302


node <- c(425,421,341,479,572, 302)
age.min <- c(5, 12.5, 3.5, 12.5, 38, 21)
age.max <- c(8, 18, 6.5, 15, 42, 30)
soft.bounds <- c(
  FALSE, 
  FALSE, 
  FALSE, 
  FALSE,
  FALSE, 
  FALSE
)
mycalibration <- data.frame(node, age.min, age.max, soft.bounds) 

tree.c <- chronos(tree, lambda = 0.1, model = 'correlated', calibration = mycalibration)
is.ultrametric(tree.c)

plot.phylo(tree.c, type = 'fan', edge.color = "dark blue", tip.color = "dark blue", cex = 0.5)

tree <- tree.c









###################### STOP HERE



####WILSON & READER TREE
tree <- read.nexus('data/10k.wilsonreader.chrono.nex')
tips <- data.frame(tree$tip.label)
tree <- drop.tip(tree, tip = c(176, 32)) #Remove extinct species

#Get the nodes of common ancestors of homo-pan, homo-pongo, Papio- Theropithecus, Cebus- Saimiri, Loris- Galago
findMRCA(tree, c('Homo_sapiens', 'Pan_troglodytes_verus'))
#homo-pan = 418
findMRCA(tree, c('Homo_sapiens', 'Pongo_pygmaeus'))
#homo-pongo = 414
findMRCA(tree, c('Papio_anubis', 'Theropithecus_gelada'))
#Papio- Theropithecus = 336
findMRCA(tree, c('Cebus_albifrons', 'Saimiri_ustus'))
#Cebus- Saimiri = 471
findMRCA(tree, c('Loris_tardigradus', 'Galago_alleni'))
# Loris- Galago = 562
findMRCA(tree, c('Papio_anubis', 'Pongo_pygmaeus'))
# Catarrhini 298

node <- c(418,414,336,471,562, 298)
age.min <- c(5, 12.5, 3.5, 12.5, 38, 21)
age.max <- c(8, 18, 6.5, 20, 42, 30)
soft.bounds <- c(
  FALSE, 
  FALSE, 
  FALSE, 
  FALSE, 
  FALSE,
  FALSE
)
mycalibration <- data.frame(node, age.min, age.max, soft.bounds) 
tree.wr <- chronos(tree, lambda = 0.1, model = 'correlated', calibration = mycalibration)

# Publication quality graphs require 600dpi
dpi=600    #pixels per square inch
tiff("output.tif", width=6*dpi, height=5*dpi, res=dpi)
plot(tree.wr, cex = 0.1)
nodelabels("here", 298, adj = c(0.5, 0.5), frame = "rect",
           pch = 1, thermo = NULL, pie = NULL, piecol = NULL,
           col = "black", bg = "lightblue")
dev.off() 
