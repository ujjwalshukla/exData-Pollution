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

## Plotting year-Emissions with bar as a geom, stat as identity, 
g<-ggplot(aes(x = year, y = Emissions, fill=type), data=NEIdataBaltimore)
g+geom_bar(stat="identity")+
      facet_grid(.~type)+
      labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
      labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type"))+
      guides(fill=FALSE)+theme(axis.text.x = element_text(angle=90, vjust=1))

## saving the plot in the file
dev.copy(png, file="plot3.png")
dev.off()