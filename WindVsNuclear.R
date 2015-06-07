## Wind vs Nuclear :D
rm(list=ls())
library(XML)
library(maps)
library(ggplot2)
siteNuclear <- 'http://en.wikipedia.org/wiki/Nuclear_power_by_country'
siteWind <- 'http://en.wikipedia.org/wiki/Wind_power_by_country'

#Nuclear Data
tables<- readHTMLTable(siteNuclear)
NuclearData<-tables[[1]]
colnames(NuclearData) <- c('region','N_reactors','TotalCapacity_Nuclear_2014','Electricty_supplied','shareProduction')
keepers <- c('region','TotalCapacity_Nuclear_2014')
NuclearData <- NuclearData[,(names(NuclearData) %in% keepers)]
NuclearData$TotalCapacity_Nuclear_2014 <- as.numeric(as.character(NuclearData$TotalCapacity_Nuclear_2014))
#Wind Data
tables2<- readHTMLTable(siteWind)
WindData <- tables2[[4]]
colnames(WindData) <- c('#','region','2006','2007','2008','2009','2010','2011','2012','2013','TotalCapacity_Wind_2014')
keepers <- c('region','TotalCapacity_Wind_2014')
WindData <- WindData[,(names(WindData) %in% keepers)]
RegionTemp <- gsub('\\d','',WindData$region)
WindData$region <- gsub("[[:punct:]]",'',RegionTemp)
WindData$TotalCapacity_Wind_2014 <- gsub(",",'',WindData$TotalCapacity_Wind_2014)
WindData$TotalCapacity_Wind_2014 <- as.numeric(WindData$TotalCapacity_Wind_2014)
Wind_Nuclear_Data <- merge(WindData,NuclearData,by='region',all=TRUE)

#worldData
WorldData <- map_data('world')


Total <- merge(Wind_Nuclear_Data,WorldData,by='region')


p <- ggplot()
p <- p + geom_polygon(data=WorldData, aes(x=long, y=lat, group = group, fill=1),colour="white"
) + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")
P1 <- p + theme_bw()  + labs(fill = "Black to White Incarceration Rates \n Weighted by Relative Population" 
                             ,title = "State Incarceration Rates by Race, 2010", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())

