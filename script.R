setwd('/Users/matt/Documents/workspace/R/class/ExploratoryDataAnalysis/Project2')

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

# 1. Have total emissions from PM2.5 decreased in the United States from 
# 1999 to 2008? Using the base plotting system, make a plot showing the 
# total PM2.5 emission from all sources for each of the years 1999, 2002,
# 2005, and 2008.
NEI.aggregate <- aggregate(Emissions ~ year, data = NEI, sum)
plot(NEI.aggregate, 
     type="b",
     main=expression("Total PM"[2.5] ~ "Emissions per Year, United States"),
     xlab="Year",
     ylab="Total Emissions",
     ylim=c(0,8000000),
     xaxt="n")
axis(side = 1, at = c(1999, 2002, 2005, 2008), labels = c(1999, 2002, 2005, 2008))
abline(lm(NEI.aggregate$Emissions ~ NEI.aggregate$year), col="blue")

# 2. Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510") from 1999 to 2008? Use the base plotting 
# system to make a plot answering this question.
bcdata = NEI[NEI$fips == "24510",]
NEI.BCaggregate <- aggregate(Emissions ~ year, data = bcdata, sum)
plot(NEI.BCaggregate, 
     type="b",
     main=expression("Total PM"[2.5] ~ "per Year: Baltimore City, MD"),
     xlab="Year",
     ylab="Total Emissions",
     ylim=c(0,4000),
     xaxt="n")
axis(side = 1, at = c(1999, 2002, 2005, 2008), labels = c(1999, 2002, 2005, 2008))
abline(lm(NEI.BCaggregate$Emissions ~ NEI.BCaggregate$year), col="blue")

# 3. Of the four types of sources indicated by the type (point, nonpoint, 
# onroad, nonroad) variable, which of these four sources have seen decreases
# in emissions from 1999–2008 for Baltimore City? Which have seen increases 
# in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot
# answer this question.
NEI.BCaggregate.byType <- aggregate(x=bcdata$Emissions, by=list(bcdata$year,bcdata$type), FUN=sum)
colnames(NEI.BCaggregate.byType) <- c("year","type","Emissions")
library(ggplot2)
ggplot(NEI.BCaggregate.byType, aes(year, Emissions, fill=type)) + 
    geom_bar(stat="identity",position="dodge") +
    xlab("Year") +
    ylab("Total Emissions") +
    ggtitle(expression("Total PM"[2.5] ~ "per Year, by Type: Baltimore City, MD")) +
    scale_fill_discrete(name="Type") +
    scale_x_continuous(breaks=c(1999,2002,2005,2008))

# 4. Across the United States, how have emissions from coal combustion-related
# sources changed from 1999–2008?
coaldata.SCC <- SCC[with(SCC, grepl("Comb.*Coal",Short.Name) & grepl("Coal",EI.Sector)),]$SCC
coaldata <- NEI[NEI$SCC %in% coaldata.SCC,]
NEI.coal.aggregate.by.year <- aggregate(x=coaldata$Emissions, by=list(coaldata$year), FUN=sum)
colnames(NEI.coal.aggregate.by.year) <- c("year", "Emissions")
ggplot(NEI.coal.aggregate.by.year, aes(year, Emissions)) + 
    geom_bar(stat="identity") +
    xlab("Year") +
    ylab("Total Coal Combustion-Related Emissions") +
    ggtitle(expression("Total PM"[2.5] ~ "per Year due to Coal Combustion")) +
    scale_x_continuous(breaks=c(1999,2002,2005,2008))

# 5. How have emissions from motor vehicle sources changed from 
# 1999–2008 in Baltimore City? 
vehdata.SCC <- SCC[with(SCC, grepl("Vehicle",SCC.Level.Two) & grepl("Veh",Short.Name)),]$SCC
vehdata <- bcdata[bcdata$SCC %in% vehdata.SCC,]
NEI.vehicle.aggregate.by.year <- aggregate(x=vehdata$Emissions, by=list(vehdata$year), FUN=sum)
colnames(NEI.vehicle.aggregate.by.year) <- c("year", "Emissions")
ggplot(NEI.vehicle.aggregate.by.year, aes(year, Emissions)) + 
    geom_bar(stat="identity") +
    xlab("Year") +
    ylab("Total Motor Vehicle-Related Emissions") +
    ggtitle(expression("Total PM"[2.5] ~ "per Year due to Motor Vehicles, Baltimore City")) +
    scale_x_continuous(breaks=c(1999,2002,2005,2008))

# 6. Compare emissions from motor vehicle sources in Baltimore City
# with emissions from motor vehicle sources in Los Angeles County, 
# California (fips == "06037"). Which city has seen greater changes 
# over time in motor vehicle emissions?
bc.or.lac.data <- NEI[NEI$fips == "24510" | NEI$fips == "06037",]
vehdata.SCC <- SCC[with(SCC, grepl("Vehicle",SCC.Level.Two) & grepl("Veh",Short.Name)),]$SCC
vehdata <- bc.or.lac.data[bc.or.lac.data$SCC %in% vehdata.SCC,]
NEI.vehicle.aggregate.by.year <- aggregate(x=vehdata$Emissions, by=list(vehdata$year,vehdata$fips), FUN=sum)
colnames(NEI.vehicle.aggregate.by.year) <- c("year", "fips", "Emissions")
ggplot(NEI.vehicle.aggregate.by.year, aes(year, Emissions, fill=fips)) + 
    geom_bar(stat="identity", position="dodge") +
    xlab("Year") +
    ylab("Total Motor Vehicle-Related Emissions") +
    scale_fill_discrete(name="County", labels=c("LA County", "Baltimore City")) +
    ggtitle(expression("Total PM"[2.5] ~ "per Year due to Motor Vehicles")) +
    scale_x_continuous(breaks=c(1999,2002,2005,2008))