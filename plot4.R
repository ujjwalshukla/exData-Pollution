## loading data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

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

## making the names in the SCC dataframe pretty by removing 
## \\. in all the names
names(SCC)<-gsub("\\.","", names(SCC))

## Note: The SCC levels go from generic to specific. 
## We assume that coal combustion related SCC records are those 
## where SCC.Level.One contains the substring 'comb' and SCC.Level.Four 
## contains the substring 'coal'.
SCCcombustion<-grepl(pattern = "comb", SCC$SCCLevelOne, ignore.case = TRUE)
SCCCoal<-grepl(pattern = "coal", SCC$SCCLevelFour, ignore.case = TRUE)

## extracting the SCC in rows with combustion and coal
## and subsetting the NEI data with same SCC
SCCCoalCombustionSCC<-SCC[SCCcombustion & SCCCoal,]$SCC
NIECoalCombustionValues<-NEIdata[NEIdata$SCC %in% SCCCoalCombustionSCC,]
NIECoalCombustionTotalEm<-aggregate(Emissions~year, NIECoalCombustionValues, sum)

## Plotting the subset of NIE data with SCC matched with coal and combustion.
g<-ggplot(aes(year, Emissions/10^5), data=NIECoalCombustionTotalEm)
g+geom_bar(stat="identity",fill="grey",width=0.75) +
    guides(fill=FALSE) +
    labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
    labs(title=expression("PM"[2.5]*" Coal Combustion Source Emissions Across US from 1999-2008"))

## saving the plot in the file
dev.copy(png, file="plot4.png")
dev.off()