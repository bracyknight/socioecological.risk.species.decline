#Preparation of Human Predictors Dataset For Ch.3


#Bring in database
db <- read.csv("02_Data/WB_WGI.csv",header=TRUE, stringsAsFactors=FALSE)
#test <-  read.csv("WB_WGI.csv",header=TRUE, stringsAsFactors=FALSE)

#Clean Nomonclature
db$country[which(db$country == 'congo, dem. rep.')] <- 'democratic republic of the congo'
db$country[which(db$country == 'congo, rep.')] <- 'republic of congo'
db$country[which(db$country == 'venezuela, rb')] <- 'venezuela'
db$country[which(db$country == 'yemen, rep.')] <- 'yemen'
db$country[which(db$country == 'gambia, the')] <- 'gambia'
db$country[which(db$country == 'macedonia, fyr')] <- 'macedonia'
db$country[which(db$country == 'russian federation')] <- 'russia'
db$country[which(db$country == 'hong kong sar, china')] <- 'hong kong'
db$country[48] <- 'ivory coast'

#Bring in continents list 
countries <- read.csv("02_Data/country_continent.csv")
countries$Country <- tolower(countries$Country)
names(countries) <- tolower(names(countries))
countries$country[which(countries$country == 'burma')] <- 'myanmar'
countries$continent[which(countries$continent == 'Central America')] <- 'South America' #I do this to minimize mess of just a few South Amewrican Countries

#Associate continents with respective countries
db <- merge(db,countries, by = 'country', all = T)
dbsub <- db[which(db$country == 'mexico'|
                    db$country == 'trinidad and tobago'|
                    db$country == 'spain'|
                    db$continent == "Asia"|
                    db$continent == "Africa"|
                    db$continent == "South America"),]
dbsub <- na.omit(dbsub)


#Merge the crap out of the data
  #NOTE: this assumes you've run both the GDP and HDI scripts prior

source("01_R/HDI.R")
source("01_R/GDP.R")


db.t <- merge(dbsub, hdi_data, by = 'country', all = T) #merge with mean hdi variance
db.t <- merge(db.t, gdp, by = 'country', all = T) #merge with gdp and dgp variance

#grab only countries within study area
dbsub <- db.t[which(db.t$country == 'mexico'|
                      db.t$country == 'trinidad and tobago'|
                      db.t$country == 'gibraltar'|
                      db.t$country == 'spain'|
                      db.t$continent == "Asia"|
                      db.t$continent == "Africa"|
                      db.t$continent == "South America"),]


dbsub$country[which(dbsub$country == 'republic of the congo')] <- 'republic of congo'

#Final, cleaned human data before being associated with species 
  #These data are by-country, not by-species

dbsub <- na.omit(dbsub)


