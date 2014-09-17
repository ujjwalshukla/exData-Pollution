## loading data
NEI <- readRDS("summarySCC_PM25.rds")

## Load the packages used in the exploratory analysis
library(ggplot2)
library(plyr)

## Further Pre-processing of the data is done.
## Converting "year", "type", "Pollutant", "SCC", "fips" to factor
colToFactor <- c("year", "type", "Pollutant","SCC","fips")
NEI[,colToFactor] <- lapply(NEI[,colToFactor], factor)

## The levels have NA as "   NA", so converting that level back to NA
levels(NEI$fips)[1] = NA
NEIdata<-NEI[complete.cases(NEI),]

## Subset the data for fips == "24510" and then aggregate them by 
## summing the Emissions per years
NEIdataBaltimore<-subset(NEIdata, fips == "24510")
totalEmissionBaltimore <- aggregate(Emissions ~ year, NEIdataBaltimore, sum)

## Plotting the Total Emissions for baltimore over Years
par(ps = 12, cex = 1, cex.main = 1)
barplot(
      (totalEmissionBaltimore$Emissions)/10^6,
      names.arg=totalEmissionBaltimore$year,
      xlab="Year",
      ylab="PM2.5 Emissions (10^6 Tons)",
      main="Total PM2.5 Emissions From All Baltimore City Sources"
)

## saving the plot in the file
dev.copy(png, file="plot2.png")
dev.off()