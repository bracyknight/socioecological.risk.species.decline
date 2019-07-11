#Getting HDI data

#library(wbstats)


#setwd("~/Dropbox/01_Ch3_redux/Ch3/data")

#1 Bring in the continents data for subsetting
countries <- read.csv("02_Data/country_continent.csv",stringsAsFactors=FALSE)
names(countries) <- tolower(names(countries))
countries$country[146] <- "Myanmar"
countries$continent <- tolower(countries$continent)
countries$country <- tolower(countries$country)


#2 Human Development Index
  #For this, I previously queried World Bank, then took the mean of the last 10 years listed
  #This was saved as HDI.csv

hdi <- read.csv('02_Data/HDI.csv',stringsAsFactors=FALSE)
hdi <- droplevels(hdi)
hdi$Country[46] <- "Ivory Coast"
hdi$Country[184] <- "Venezuela"
hdi$Country[21] <- "bolivia"
hdi$Country[39] <- 'republic of the congo'
hdi$Country[40] <- 'democratic republic of the congo'
hdi$Country[166] <- 'Tanzania'
hdi$Country[25] <- 'brunei'
hdi$Country[93] <- 'Laos'
hdi$Country[90] <- 'South Korea'
hdi$Country[185] <- 'Vietnam'
hdi$Country[79] <- 'Iran'
hdi$Country[79] <- 'Syria'

hdi$Country[which(hdi$Country == ' Hong Kong, China (SAR)' )] <- 'hong kong'

hdi[189,1] <- 'Somalia'
hdi[189,2] <- 0.285
hdi[190,1] <- 'french guiana'
hdi[190,2] <- 0.862
hdi[191,1] <- 'macau'
hdi[191,2] <- 0.894
hdi[192,1] <- 'cape verde'
hdi[192,2] <- 0.646
hdi[193,1] <- 'north korea'
hdi[193,2] <- 0.766
hdi[194,1] <- 'iran'
hdi[194,2] <- 0.782
hdi[195,1] <- 'gibraltar'
hdi[195,2] <- 0.861
hdi[196,1] <- 'taiwan'
hdi[196,2] <- 0.882


names(hdi) <- tolower(names(hdi))
hdi$country <- tolower(hdi$country)
hdi$country <- as.factor(hdi$country)
library(stringr)
hdi$country <- str_trim(hdi$country, "left")
hdi$country
levels(hdi$country)

hdi <- merge(hdi, countries, by = 'country', all= T)
hdi_data <- hdi[which(hdi$country == "mexico"|
                        hdi$country == "spain"|
                        hdi$country == "gibraltar"|
                        hdi$country == "trinidad and tobago"|
                        hdi$continent == "asia"|
                        hdi$continent == "africa"|
                        hdi$continent == "south america"|
                        hdi$continent == "central america"),]

View(hdi_data[which(is.na(hdi_data$hdi)),])
hdi_data <- hdi_data[which(!is.na(hdi_data$hdi)),]
hdi_data <- hdi_data[,c(1,2)]
hdi_data$continent <- NULL
#View(hdi_data)
