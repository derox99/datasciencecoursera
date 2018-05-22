sapply(split(NEI$Emissions,NEI$year),sum)
totEmissionPerYear<-with(NEI,tapply(Emissions, year, sum, na.rm=TRUE))
plot(x=c(1999,2002,2005,2008),y = totEmissionPerYear, xlab = "year", ylab = "Total Emissions")


totEmissionPerYearBal<-with(NEI[NEI$fips=="24510",],tapply(Emissions, year, sum, na.rm=TRUE))
plot(x=c(1999,2002,2005,2008),y = totEmissionPerYearBal, xlab = "year", ylab = "Total Emissions in Baltimore")

coalSect<-grep("Coal",x = SCC$EI.Sector)
combSect<-grep("Comb",x = SCC$EI.Sector)
intersect(coalSect,combSect)