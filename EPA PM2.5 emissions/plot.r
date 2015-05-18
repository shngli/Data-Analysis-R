# Author: Chisheng Li
####################################
# Plot 1
# 1.Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the baseplotting system, make a plot showing the total PM2.5 emission from all 
# sources for each of the years 1999, 2002, 2005, and 2008.

# Load the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Aggregate total emissions from PM2.5, convert from tons to kilotons
attach(NEI)
EmissionsPM2.5 <- tapply((Emissions)/1000,year,sum)

# Plot the bar chart
png("plot1a.png", height=480, width=480)
barplot(EmissionsPM2.5, xlab = "Year", ylab = expression("Emissions of PM"[2.5]* " (in kilotons)"), 
    main = expression("Total emissions from PM"[2.5]* " in US (1999-2008)"), col = c("red", "blue", "grey", "orange") )
dev.off()

###### Alternative line plot #######
# Aggregate total emissions from PM2.5, convert from tons to kilotons
EmissionsPM2.5 <- aggregate(NEI[c("Emissions")]/1000, list(year = NEI$year), sum)

# Create the line plot
png("plot1b.png", height = 480, width = 480)
plot(EmissionsPM2.5$year, EmissionsPM2.5$Emissions, type="b", xlab = "Year", 
    ylab = expression("Emissions of PM"[2.5]* " (in kilotons)"), 
    main = expression("Total emissions from PM"[2.5]* " in US (1999-2008)"), col = "Blue")
dev.off()
########################################################################################

# Plot 2
# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

# Load the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Aggregate total emissions in Baltimore, convert from tons to kilotons
Baltimore <- subset(NEI, fips == "24510")
BaltimorePM2.5 <- tapply((Baltimore$Emissions)/1000, Baltimore$year, sum)

# Plot the bar chart
png("plot2a.png", height = 480, width = 480)
barplot(BaltimorePM2.5, xlab = "Year", ylab = expression("Emissions of PM"[2.5]* " (in kilotons)"), 
    main = expression("Total emissions from PM"[2.5]* " in Baltimore City, MD (1999-2008)"), 
    col = c("red", "blue", "grey", "orange"))
dev.off()

###### Alternative line plot #######
# Aggregate total emissions in Baltimore, convert from tons to kilotons
Baltimore <- subset(NEI, fips == "24510")
BaltimorePM2.5 <- aggregate(Baltimore[c("Emissions")]/1000, list(year = Baltimore$year), sum)

# Create the line plot
png("plot2b.png", height = 480, width = 480)
plot(BaltimorePM2.5$year, BaltimorePM2.5$Emissions, type="b", xlab = "Year", 
    ylab = expression("Emissions of PM"[2.5]* " (in kilotons)"), 
    main = expression("Total emissions from PM"[2.5]* " in Baltimore City, MD (1999-2008)"), col = "Blue")
dev.off()
########################################################################################

#Plot 3
# 3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a 
# plot answer this question.

# Load the data
require(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI$year <- as.factor(NEI$year)	# remove this line for the line plot

Baltimore <- subset(NEI, fips == "24510")
BaltimorePM2.5 <- aggregate(Baltimore[c("Emissions")], list(type = Baltimore$type, year = Baltimore$year), sum)

# Plot bar graph
png("plot3a.png", height = 480, width = 480)
qplot(x = year, y = Emissions, fill = type, data = BaltimorePM2.5, geom = "bar", stat = "identity", 
    position = "dodge", xlab = "Year", ylab = expression("Emissions of PM"[2.5]* " (in tons)"), 
    main = expression("Total emissions from PM"[2.5]* " in Baltimore City, MD (1999-2008)"))
dev.off()

###### Alternative line plot ######
# Create line plot
png("plot3b.png", height = 480, width = 480)
qplot(year, Emissions, data = BaltimorePM2.5, color = type, geom = "path", xlab = "Year", 
    ylab = expression("Emissions of PM"[2.5]* " (in tons)"), 
    main = expression("Total emissions from PM"[2.5]* " in Baltimore City, MD (1999-2008)"))
dev.off()
################################################################################################

# Plot 4
# 4. Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999–2008?

# Load RDS
require(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Merge coal related SCC and NEI data set
SCC.coal <- SCC[grepl("coal", SCC$Short.Name, ignore.case=TRUE),]
CoalEmission <- merge(NEI, SCC.coal, by = "SCC")
# Aggregate total emissions from coal-related combustion, convert from tons to kilotons
CoalEmissionPM2.5 <- aggregate(CoalEmission[, "Emissions"]/1000, by = list(CoalEmission$year), sum)
colnames(CoalEmissionPM2.5) <- c("year", "Emissions")

# Create line plot
png("plot4.png", height = 480, width = 480)
ggplot(CoalEmissionPM2.5, aes(x = year, y = Emissions)) + 
  geom_line(aes(group = 1, col = Emissions)) + geom_point(aes(size = 1, col = Emissions)) + 
  ylab(expression("Emissions of PM"[2.5]* " (in kilotons)")) + xlab("Year") +
  ggtitle("Total emissions from Coal Combustion (1999-2008)")
dev.off()

################################################################################################

# Plot 5
# 5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# Load RDS
library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Aggregate total emissions from motor vehicles in Baltimore
Baltimore <- subset(NEI, fips == "24510" & type == "ON-ROAD")
Baltimore$year <- factor(Baltimore$year, levels = c('1999', '2002', '2005', '2008'))
BaltimorePM2.5 <- aggregate(Baltimore[, "Emissions"], by = list(Baltimore$year), sum)
colnames(BaltimorePM2.5) <- c("year", "Emissions")

# Create barplot
png("plot5a.png", height = 480, width = 480)
ggplot(BaltimorePM2.5, aes(x = year, y = Emissions)) + geom_bar(aes(fill=year), stat="identity") +  
  ylab(expression("Emissions of PM"[2.5]* " (in tons)")) + xlab("Year") + 
  ggtitle("Total Emissions of Motor Vehicles in Baltimore") +
  geom_text(aes(label = round(Emissions), size = 3, hjust = 1, vjust = 0))
dev.off()

##### Alternative line plot #####
png("plot5b.png", height = 480, width = 480)
ggplot(BaltimorePM2.5, aes(x = year, y = Emissions)) + 
    geom_line(aes(group = 1, col = Emissions)) + 
    ylab(expression("Emissions of PM"[2.5]* " (in tons)")) + xlab("Year") + 
    ggtitle("Total Emissions of Motor Vehicles in Baltimore")
dev.off()

################################################################################################

# Plot 6
# 6. Compare emissions from motor vehicle sources in Baltimore City with emissions from 
# motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

# Load RDS
library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Aggregate total emissions from motor vehicles in Baltimore
Baltimore <- subset(NEI, fips == "24510" & type == "ON-ROAD")
Baltimore$year <- factor(Baltimore$year, levels = c('1999', '2002', '2005', '2008'))
BaltimorePM2.5 <- cbind(aggregate(Baltimore[, "Emissions"], by = list(Baltimore$year), sum), City = "Baltimore")
colnames(BaltimorePM2.5) <- c("year", "Emissions", "City")

# Aggregate total emissions from motor vehicles in Los Angeles
LosAng <- subset(NEI, fips == "06037" & type == "ON-ROAD")
LosAng$year <- factor(LosAng$year, levels = c('1999', '2002', '2005', '2008'))
LosAngPM2.5 <- cbind(aggregate(LosAng[, "Emissions"], by = list(LosAng$year), sum), City = "Los Angeles")
colnames(LosAngPM2.5) <- c("year", "Emissions", "City")

BaltLA <- rbind.data.frame (BaltimorePM2.5, LosAngPM2.5)

# Plot bar chart for comparison 
png("plot6a.png", height = 480, width = 480)
ggplot(BaltLA, aes(year, Emissions)) + geom_bar(aes(fill = year), stat = "identity") + facet_grid(. ~ City) + 
    ggtitle("Total Emissions from motor vehicles in Baltimore and in Los Angeles") + 
    ylab(expression("Emissions of PM"[2.5]* " (in tons)")) + xlab("Year") + 
    geom_text(aes(label=round(Emissions), size = 2, hjust = 1, vjust = -1))
dev.off()

######## Alternative comparison in 3-year % change #########
### continue from ###
BaltLA <- rbind.data.frame (BaltimorePM2.5, LosAngPM2.5)

pairs <- c(1999, 2002, 2005, 2008)
percentchange <- round(data.frame(sapply(1:3, FUN = function(x) 
          (BaltLA$Emissions[BaltLA$year == pairs[x + 1]] - BaltLA$Emissions[BaltLA$year == pairs[x]]) / BaltLA$Emissions[BaltLA$year == pairs[x]])) * 100, 1)
percentchange <- rbind(t(percentchange[1, ]),t(percentchange[2, ]))
percentchange <- as.data.frame(cbind(rep(c("1999 to 2002", "2002 to 2005", "2005 to 2008"), 2), c(rep("Baltimore", 3), rep("Los Angeles", 3)), percentchange))
colnames(percentchange) <- c("Period", "City", "Change")
percentchange$Change <- as.numeric(as.character(percentchange$Change))

# Plot bar chart for percentage change comparison 
png(filename = "plot6b.png", width = 700, height = 480)
ggplot(percentchange, aes(Period, Change)) + geom_bar(aes(fill = City), stat = "identity") + facet_grid(. ~ City) + 
    ggtitle("% Change in Total Emissions from motor vehicles in Baltimore vs. L.A.") + 
    ylab(expression("3 Years % Change in PM"[2.5])) + xlab("Year") + 
    geom_text(aes(label=round(Change), size = 2, hjust = 1, vjust = -1))
dev.off()
