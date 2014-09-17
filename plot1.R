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

## First aggregate the data in
totalEmission <- aggregate(Emissions ~ year, NEIdata, sum)

## Plotting the total Emissions over time using a base plotting
barplot(
      (totalEmission$Emissions)/10^6,
      names.arg=totalEmission$year,
      xlab="Year",
      ylab="PM2.5 Emissions (10^6 Tons)",
      main="Total PM2.5 Emissions From All US Sources"
)

## saving the plot in the file
dev.copy(png, file="plot1.png")
dev.off()