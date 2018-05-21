library(dplyr)
balt<-NEI[NEI$fips=="24510",]
baltGrouped<-balt %>% group_by(year,type) %>% summarise(Emissions=sum(Emissions))
qplot(x = year, y=Emissions, data=baltGrouped, color=type, geom = "line")