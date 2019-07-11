#GDP data import

library(wbstats)

gdp.raw <- wb(indicator = c("NY.GDP.MKTP.CD"),mrv = 100, gapfill = F)
#View(gdp.raw)

gdp.raw$value <- as.numeric(gdp.raw$value)

gdp <- aggregate(value~country, data= gdp.raw, FUN = mean, na.rm = T)
names(gdp)
names(gdp) <- c('country','gdp')
gdp$country <- tolower(gdp$country)

gdp$country[which(gdp$country == 'egypt, arab rep.')] <- "egypt"
gdp$country[which(gdp$country == 'congo, dem. rep.')] <- 'democratic republic of the congo'
gdp$country[which(gdp$country == 'congo, rep.')] <- 'republic of the congo'
gdp$country[which(gdp$country == 'venezuela, rb')] <- 'venezuela'
gdp$country[which(gdp$country == 'yemen, rep.')] <- 'yemen'
gdp$country[which(gdp$country == 'gambia, the')] <- 'gambia'
gdp$country[which(gdp$country == 'iran, islamic rep.')] <- 'iran'
gdp$country[which(gdp$country == 'lao pdr')] <- 'laos'
gdp$country[which(gdp$country == 'macao sar, china')] <- 'macau'
gdp$country[which(gdp$country == 'korea, rep.')] <- 'south korea'
gdp$country[which(gdp$country == 'syrian arab republic')] <- 'syria'
gdp$country[which(gdp$country == 'brunei darussalam')] <- 'brunei'
gdp$country[which(gdp$country == 'brunei darussalam')] <- 'brunei'
gdp$country[which(gdp$country == 'hong kong sar, china')] <- 'hong kong'
gdp$country[which(gdp$country == 'cabo verde')] <- 'cape verde'
gdp$country[which(gdp$country == "cote d'ivoire")] <- 'ivory coast'


gdp[253,1] <- 'french guiana'
gdp[253,2] <- 4210000000
gdp[254,1] <- 'gibralter'
gdp[254,2] <- 2036650400
gdp[254,1] <- 'taiwan'
gdp[254,2] <- 523580000000


rm(gdp.raw) #no longer need the raw data
#View(gdp)



