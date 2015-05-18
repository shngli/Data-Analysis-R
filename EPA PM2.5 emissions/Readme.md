Data is available [here](https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip).
The zip file contains two files:

1. PM2.5 Emissions Data (summarySCC_PM25.rds): This file contains a data frame with all of the PM2.5emissions data for 1999, 2002, 2005, and 2008. For each year, the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year.
2. Source Classification Code Table (Source_Classification_Code.rds): This table provides a mapping from the SCC digit strings int he Emissions table to the actual name of the PM2.5 source. The sources are categorized in a few different ways from more general to more specific.

Address the following questions and make a plot for each question in the exploratory analysis:

1)	Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Make a plot using the baseplotting system to show the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008. **Output**: 

![plot1a.png](https://github.com/shngli/R-data-analysis/blob/master/EPA%20PM2.5%20emissions/plot1a.png)

![plot1b.png](https://github.com/shngli/R-data-analysis/blob/master/EPA%20PM2.5%20emissions/plot1b.png)

2)	Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question. **Output**:

![plot2a.png](https://github.com/shngli/R-data-analysis/blob/master/EPA%20PM2.5%20emissions/plot2a.png)

![plot2b.png](https://github.com/shngli/R-data-analysis/blob/master/EPA%20PM2.5%20emissions/plot2b.png)

3)	Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question. **Output**: 

![plot3a.png](https://github.com/shngli/R-data-analysis/blob/master/EPA%20PM2.5%20emissions/plot3a.png)

![plot3b.png](https://github.com/shngli/R-data-analysis/blob/master/EPA%20PM2.5%20emissions/plot3b.png)

4)	Across the U.S., how have emissions from coal combustion-related sources changed from 1999–2008? **Output**:

![plot4.png](https://github.com/shngli/R-data-analysis/blob/master/EPA%20PM2.5%20emissions/plot4.png)

5)	How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City? **Output**: 

![plot5a.png](https://github.com/shngli/R-data-analysis/blob/master/EPA%20PM2.5%20emissions/plot5a.png) 

![plot5b.png](https://github.com/shngli/R-data-analysis/blob/master/EPA%20PM2.5%20emissions/plot5b.png)

6)	Compare emissions from motor vehicle sources in Baltimore  with emissions from motor vehicle sources in Los Angeles, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions? **Output**: 

![plot6a.png](https://github.com/shngli/R-data-analysis/blob/master/EPA%20PM2.5%20emissions/plot6a.png) 

![plot6b.png](https://github.com/shngli/R-data-analysis/blob/master/EPA%20PM2.5%20emissions/plot6b.png)
