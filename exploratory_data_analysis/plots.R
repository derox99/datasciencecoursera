library(dplyr)
library(ggplot2)
balt<-NEI[NEI$fips=="24510",]
baltGrouped<-balt %>% group_by(year,type) %>% summarise(Emissions=sum(Emissions))
qplot(x = year, y=Emissions, data=baltGrouped, color=type, geom = "line")



indexComb<-grep("Comb",SCC$EI.Sector)
indexCoal<-grep("Coal",SCC$EI.Sector)
indexCoalComb<-intersect(indexCoal,indexComb)
sccCoalComb<-as.character(SCC[indexCoalComb,"SCC"])
NEICoal<-NEI[NEI$SCC %in% sccCoalComb,]
coalUSA<-NEICoal %>% group_by(year) %>% summarise(Emissions=sum(Emissions))
qplot(x = year, y=Emissions, data=coalUSA, geom = "line")


indexVehicle<-grep("Vehicle",SCC$EI.Sector)
sccVehicle<-as.character(SCC[indexVehicle,"SCC"])

balt<-NEI[NEI$fips=="24510",]
#merge data with SCC
baltSCC<-merge(x = balt, y = SCC, by.x = "SCC", by.y = "SCC")
#find vehicle in level two
baltVehicle<-baltSCC[grep("Vehicle",baltSCC$SCC.Level.Two),]
baltVehicleGrouped<-baltVehicle %>% group_by(year,SCC.Level.Two) %>% summarise(Emissions=sum(Emissions))
qplot(x = year, y=Emissions, data=baltVehicleGrouped, color=SCC.Level.Two, geom = "line")
