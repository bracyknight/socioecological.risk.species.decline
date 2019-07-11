## Merging the GIS country file with the chosen predictors
  #NOTE: this code picks up after "merge.human.predictors.R"

library(rgdal)
library(raster)
library(sp)
library(parallel)
library(plyr)
library(rgeos)

setwd("~/Dropbox/01_Ch3/03_GIS")


#countries <- readOGR(dsn = ".", layer = "countries")
#plot(countries)
#shapefile('GIS/countries.shp')
#list.files('countries', pattern='\\.shp$')

spp.map <- readOGR(".", "primates.in.countries")
names(spp.map@data) <- tolower(names(spp.map@data))
spp.map$country <- tolower(spp.map$country)
spp.map$country[which(spp.map$country=="democratic republic of the congo")] <- 'republic of congo'
spp.map$country[which(spp.map$country=="macao")] <- "macau"
spp.map$country[which(spp.map$country=="cte d'ivoire")] <- "ivory coast"
spp.map$continent <- NULL
spp.map$unreg1 <- NULL
spp.map$unreg2 <- NULL
names(spp.map)

spp.map <- (spp.map[,c(1:2,13:17,22)])
#View(spp.map)

combinedData <- merge(spp.map, dbsub, by = 'country', type='left', match='all')
#View(combinedData@data)
#names(combinedData)

#Find out which names in the map don't match those in the db
orphans <- unique(combinedData@data$country[is.na(combinedData@data$population)])
#View(combinedData[is.na(combinedData$population),])
sort(orphans)
#sort(unique(dbsub$country))
#View(combinedData[which(combinedData$country == orphans),])

spp.map.w.data <- combinedData

geo <- "+proj=longlat +datum=WGS84"
crs(spp.map.w.data)<- geo
moll_crs<- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
sp.db <- spTransform(spp.map.w.data, CRS(moll_crs))
#plot(sp.db)

sp.db@data$area <- rgeos::gArea(sp.db,byid=TRUE)
#writeOGR(hrr.shp.2, dsn = getwd(), layer = "HRR_Bdry_NAD83", driver="ESRI Shapefile")

#calculate total range of species
sp.area <- aggregate(sp.db$area, by = list(sp.db$binomial), FUN = sum)
names(sp.area) <- c('binomial','geog.range') 

#calculate the amount of a spp range in each country (as a percent)
temp <-  merge(sp.db, sp.area, by = 'binomial', type = 'left', match = 'all')
sp.db <- temp; rm(temp)
names(sp.db)
sp.db$fr.in.country <- (sp.db$area/sp.db$geog.range)
#View(sp.db)
aggregate(sp.db$fr.in.country, by = list(sp.db$binomial), FUN = sum) #use this to check to make sure that area is right. If each spp range is much bigger or smaller than 1, you have a problem


names(sp.db)
db <- sp.db

#calculate the number of countries a sp is found in

test <- db@data %>% count(binomial, country)#counts the number of polys for a spp in each country
#View(test)

t1 <- aggregate(test$country, by = list(test$binomial), FUN = length) #gets the number of countries a species is found in
names(t1) <- c('binomial','num.countries')
#View(t1)
#unique(db$country[which(db$binomial == 'Alouatta palliata')])

db.t <-  merge(db, t1, by = 'binomial', type = 'left', match = 'all')
db <- db.t; rm(db.t)
#View(db)
db <- na.omit(db)
length(unique(db$binomial))


## which countries w/ primates are missing data:
missing <- unique(db$country[is.na(db$pol_stab)])

## To make things easy, grab only variables I intend to use
  #binomial, country, family, rlcategory, rule_of_law, continent, birthrate, hdi, 
  #gdp, geog.range, fr.in.country, num.countries
names(db)

db.f <- db@data[,c(1,2,4,5,12,14,17,19:22,23,24)]
names(db.f)
#View(db.f[,c(5,7:11)])
length(names(db.f))

#normalize the various indicators by the fraction in each country
db.f[,c(5,7:9)] <- (db.f[,c(5,7:9)])*db.f$fr.in.country

#View(db.f)
db.f <- na.exclude(db.f)

d1 <- aggregate(list(db.f$rule_of_law,
                     db.f$deathrate, 
                     db.f$hdi,
                     db.f$gdp,
                     db.f$fr.in.country),
                by = list(db.f$binomial), 
                FUN = sum)
names(d1) <- c('binomial','rule_of_law','deathrate', 'hdi', 'gdp','fr.in.co')

#View(d1)
d2 <- aggregate(db.f$num.countries, list(db.f$binomial), FUN = max)
names(d2) <- c('binomial','num.countries')
#View(d2)


d3 <- join(d1, db.f, by = 'binomial', type = 'left', match = 'first')
#using this join function, the columns on the left are the ones that are the weighted
  #averages for a species based on their countries
#View(d3)

#to make sure that number of countries is the correct number, I take this from d2���
d3$num.countries <- NULL

d4 <- join(d3,d2, by = 'binomial', type = 'left', match = 'all')
#View(d4)

#subset to clean up
names(d4)
d5 <- d4[,c(1,9,8,7,2:5,16,17,18)]
#View(d5)

#convert IUCN to numbers
d5$iucn.no <- NA
d5$iucn.no[which(d5$rlcategory == 'LC')] <- 1
d5$iucn.no[which(d5$rlcategory == 'NT' )] <- 2
d5$iucn.no[which(d5$rlcategory == 'VU')] <- 3
d5$iucn.no[which(d5$rlcategory == 'EN')] <- 4
d5$iucn.no[which(d5$rlcategory == 'CR')] <- 5
d5$rlcategory <- NULL
d5 <- na.exclude(d5)

#create final, ordered db
names(d5)
human.db <- d5[,c(1,11,2:8,9,10)]
human.db$family <- tolower(human.db$family)
human.db$binomial <- tolower(human.db$binomial)

#based on plots, I log transform this predictors
human.db$gdp <- log(human.db$gdp + 0.01)
human.db$deathrate <- log(human.db$deathrate + 0.01)
human.db$num.countries <- log(human.db$num.countries) #leave this non-log for num.country ~ geog.range

#View(human.db)
setwd("~/Dropbox/01_Ch3")
write.csv(human.db, "02_Data/human.db.csv")
rm(d1,d2,d3,d4,d5)
