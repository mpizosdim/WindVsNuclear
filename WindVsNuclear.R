## Wind vs Nuclear :D
rm(list=ls())
library(XML)
library(maps)
library(ggplot2)
library(httr)
library(dplyr)

siteNuclear <- 'http://en.wikipedia.org/wiki/Nuclear_power_by_country'
siteNuclear <- GET(siteNuclear)
siteWind <- 'http://en.wikipedia.org/wiki/Wind_power_by_country'
siteWind <- GET(siteWind)

#Nuclear Data
tables<- readHTMLTable(rawToChar(siteNuclear$content))
NuclearData<-tables[[2]]
colnames(NuclearData) <- c('region','N_reactors','TotalCapacity_Nuclear_2014','Electricty_supplied','shareProduction')
keepers <- c('region','TotalCapacity_Nuclear_2014')
NuclearData <- NuclearData[,(names(NuclearData) %in% keepers)]
NuclearData$TotalCapacity_Nuclear_2014 <- as.numeric(as.character(NuclearData$TotalCapacity_Nuclear_2014))
#some changes

levels(NuclearData$region)[match("Russia",levels(NuclearData$region))] <- "USSR"
levels(NuclearData$region)[match("United States",levels(NuclearData$region))] <- "US"
levels(NuclearData$region)[match("United Kingdom",levels(NuclearData$region))] <- "UK"
levels(NuclearData$region)[match("Czech Republic",levels(NuclearData$region))] <- "Czechoslovakia"

#Wind Data
tables2<- readHTMLTable(rawToChar(siteWind$content))
WindData <- tables2[[4]]
colnames(WindData) <- c('#','region','2006','2007','2008','2009','2010','2011','2012','2013','TotalCapacity_Wind_2014')
keepers <- c('region','TotalCapacity_Wind_2014')
WindData <- WindData[,(names(WindData) %in% keepers)]
RegionTemp <- gsub('\\d','',WindData$region)
WindData$region <- gsub("[[:punct:]]",'',RegionTemp)
WindData$TotalCapacity_Wind_2014 <- gsub(",",'',WindData$TotalCapacity_Wind_2014)
WindData$TotalCapacity_Wind_2014 <- as.numeric(WindData$TotalCapacity_Wind_2014)
#Some changes 

levels(WindData$region)[match("Caribbean",levels(WindData$region))] <- "Bahamas"
levels(WindData$region)[match("Czech Republic",levels(WindData$region))] <- "Czechoslovakia"
levels(WindData$region)[match("Rest of Latin America  Caribbean",levels(WindData$region))] <- "Barbados"
levels(WindData$region)[match("United States",levels(WindData$region))] <- "USA"
levels(WindData$region)[match("United Kingdom",levels(WindData$region))] <- "UK"

#worldData

WorldData <- map_data('world')
WorldData %>% filter(region !='Antarctica') -> WorldData
#Total data

Wind_Nuclear_Data <- merge(WindData,NuclearData,by='region',all=TRUE)
Total <- merge(Wind_Nuclear_Data,WorldData,by='region',all=TRUE)


#plot
p <- ggplot()
p <- p + geom_map(data=WorldData, map=WorldData,
                  aes(x=long, y=lat, group=group, map_id=region),
                  fill="white", colour="#7f7f7f", size=0.5)
p <- p + geom_map(data=Total, map=WorldData,
                  aes(fill=TotalCapacity_Wind_2014, map_id=region),
                  colour="#7f7f7f", size=0.5)
#p <- p + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))
p <- p + scale_fill_continuous(low="thistle2", high="darkred", 
                               guide="colorbar")
p <- p + scale_y_continuous(c())
p <- p + scale_x_continuous(c())
p <- p + labs(fill="legend", title="Title", x="", y="")
#p <- p + theme_bw()
p <- p + theme(panel.border = element_blank())
p 

