##Loop script to test models


#build texting function
send_text = function(message){
  system(paste('osascript -e \'tell application "Messages"\' -e \'send "',
               message, '" to buddy "303-999-6726" of (service 1 whose service type is iMessage)\' -e \'end tell\''))
}

send_text("finished")

##Spp Model
d$s.gdp <- scale(d$gdp)
d$s.rule_of_law <- scale(d$rule_of_law)
d$s.hdi <- scale(d$hdi)
d$s.deathrate <- scale(d$deathrate)
d$s.num.countries <- scale(d$num.countries)

#1 Get list of variables to test 

var.choose <- cbind(colnames(d),(1:length(colnames(d))));var.choose #create framework to choose variable

db <- d[c(1,2,5:18)]
#db <- d[c(1,2,5:13)]
#2 Setup db to save response
m.results <- matrix(nrow=64,ncol=10)
colnames(m.results) <- c("X1", 'X2','X3','X4','X5',
                         "Phylo","WAIC","WAIC.SE", 
                         "LOO","LOO.SE")

m.results <- data.frame(m.results)

# Derive possible combinations
names(db)
sixes <- data.frame(combn(c(names(db[c(12:16)]),"phylo"), 6))
names(sixes) <- "sixes1"
fives <- data.frame(combn(c(names(db[c(12:16)]),"phylo"), 5))
fives[6,] <- NA
names(fives) <- c('fives1', 'fives2','fives3','fives4','fives5','fives6')
fours <- data.frame(combn(c(names(db[c(12:16)]),"phylo"), 4))
fours[5,] <- NA; fours[6,] <- NA
names(fours) <- c('fours1', 'fours2','fours3','fours4','fours5','fours6',
                  'fours7','fours8','fours9','fours10','fours11','fours12',
                  'fours13','fours14','fours15'
)

threes <-data.frame(combn(c(names(db[c(12:16)]),"phylo"), 3))
names(threes) <- c('threes1', 'threes2','threes3','threes4','threes5','threes6',
                   'threes7','threes8','threes9','threes10','threes11','threes12',
                   'threes13','threes14','threes15','threes16','threes17',
                   'threes18','threes19','threes20'
)
threes[4,] <- NA; threes[5,] <- NA; threes[6,] <- NA

twos <- data.frame(combn(c(names(db[c(12:16)]),"phylo"), 2))
twos[3,] <- NA; twos[4,] <- NA; twos[5,] <- NA; twos[6,] <- NA
names(twos) <- c('twos1', 'twos2','twos3','twos4','twos5','twos6',
                 'twos7','twos8','twos9','twos10','twos11','twos12',
                 'twos13','twos14','twos15'
)
ones <- data.frame(combn(c(names(db[c(12:16)]),"phylo"), 1))
ones[2,] <- NA;ones[3,] <- NA; ones[4,] <- NA; ones[5,] <- NA; ones[6,] <- NA
names(ones) <- c('ones1', 'ones2','ones3','ones4','ones5','ones6')

#How many total models do I need to run?
length(sixes) + length(fives) + length(fours) +length(threes) +
  length(twos) +length(ones)

#Build a db with complete variable list

t1 <- cbind(sixes, fives, fours, threes, twos, ones)
View(t1)
length(t1)


phylo.model <- (rep(NA,63))

#Identify which models use a phylogeny
for(j in 1:length(t1)){
  phylo.model[j] <- 'phylo' %in% t1[,j]
}
phylo.model <- ifelse(phylo.model == TRUE, 'phylo', 'no.phylo')

for(i in 1:length(t1)){
  levels(t1[,i])[levels(t1[,i])=="phylo"] <- NA
}
t1[6,] <- NA
five <- seq(5,60, by = 5)
five <- c(1, five)

for(i in 1:length(t1)){
  #for(i in 1:5){ ## USE THIS FOR THE TESTING THE MODEL
  # Identify if model i has phylo
  ifelse(phylo.model[i] == 'phylo', source('01_R/p.model.R'), source('01_R/no.p.model.R'))
  if(i %in% five){
    send_text(paste("finishing model", i, "of 63", Sys.time(), sep = " "))
    } 
  if(i == 63){
    send_text("beginning last model")
    }
}

aggregate(WAIC ~ X1, data = m.results, FUN = sum)








