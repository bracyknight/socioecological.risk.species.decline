#unemployment


unemploy <- wb(indicator = c("SL.UEM.TOTL.ZS"),mrv = 20, gapfill = T)
unemploy <- unemploy[which(unemploy$date == 2016),]
unemploy$country <- tolower(unemploy$country)
row.names(unemploy)<-seq(1:length(unemploy$country))


unemploy$country[which(unemploy$country == 'congo, dem. rep.')] <- 'democratic republic of the congo'
unemploy$country[which(unemploy$country == 'congo, rep.')] <- 'republic of the congo'
unemploy$country[which(unemploy$country == 'venezuela, rb')] <- 'venezuela'
unemploy$country[which(unemploy$country == 'yemen, rep.')] <- 'yemen'
unemploy[68,6] <- "brunei"
unemploy[93,6] <- "egypt"
unemploy[103,6] <- "gambia"
unemploy[85,6] <- "ivory coast"
unemploy[119,6] <- "iran"
unemploy[132,6] <- "kyrgyzstan"
unemploy[133,6] <- "kyrgyzstan"
unemploy[141,6] <- "macau"
unemploy[129,6] <- "north korea"
unemploy[130,6] <- "south korea"
unemploy[133,6] <- "laos"
unemploy[197,6] <- "syria"

row.names(unemploy)<-seq(1:length(unemploy$country))
unemploy <- unemploy[,c(1,6)]

unemploy.m <- merge(unemploy, countries, by = 'country', all= T)
unemploy_data <- unemploy.m[which(unemploy.m$country == "mexico"|unemploy.m$continent == "asia"|unemploy.m$continent == "africa"|unemploy.m$continent == "south america"|unemploy.m$continent == "central america"),]
rm(unemploy.m)
unemploy_data <- unemploy_data[,1:2]
names(unemploy_data) <- c('country','unemployment')
unemploy_data <- unemploy_data[which(!is.na(unemploy_data$unemployment)),]

