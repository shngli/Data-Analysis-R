# Author: Chisheng Li
# Analysis and visualization of crime data trends in San Francisco Bay Area between June 1st 2014 and August 31st 2014
# Dataset from https://data.sfgov.org/Public-Safety/SFPD-Incidents-Previous-Three-Months/tmnf-yvry?

##Load data
crimeData <- read.table("/SFPD_Incidents_-_Previous_Three_Months.csv", fill=TRUE, sep=',', header=TRUE)

# Pre-transformed dataset
str(crimeData)
#'data.frame':   30760 obs. of  12 variables:
# $ IncidntNum: logi  NA NA NA NA NA NA ...
# $ Category  : Factor w/ 36 levels "ARSON","ASSAULT",..: 17 17 17 8 8 17 35 34 17 25 ...
# $ Descript  : Factor w/ 408 levels "ABANDONMENT OF CHILD",..: 199 197 197 299 286 197 407 360 269 319 ...
# $ DayOfWeek : Factor w/ 7 levels "Friday","Monday",..: 4 4 4 4 4 4 4 4 4 4 ...
# $ Date      : Factor w/ 92 levels "06/01/2014","06/02/2014",..: 92 92 92 92 92 92 92 92 92 92 ...
# $ Time      : Factor w/ 1408 levels "00:01","00:02",..: 1200 840 660 1039 1055 1 1101 600 960 1068 ...
# $ PdDistrict: Factor w/ 10 levels "BAYVIEW","CENTRAL",..: 2 2 2 4 5 2 7 3 2 2 ...
# $ Resolution: Factor w/ 16 levels "ARREST, BOOKED",..: 12 12 12 1 1 12 1 12 12 12 ...
# $ Address   : Factor w/ 8571 levels "0.0 Block of 10TH ST",..: 6847 6097 8245 2031 7133 6634 7317 2461 859 3761 ...
# $ X         : num  -122 -122 -122 -122 -122 ...
# $ Y         : num  37.8 37.8 37.8 37.8 37.8 ...
# $ Location  : Factor w/ 12597 levels "(37.7080829769597, -122.419241455854)",..: 10920 11550 10664 5751 9606 10699 6016 1109 11656 10266 ...

##################################################

# Part 1) Data Munging 
# variables to transform:
# a. $IncidntNum: change NAs to number 1 so that I can aggregate the number of incident counts later
# b. $Data: change from factor to Date format with as.Date()
# c. $Time: change from factor to numeric variable so that I can construct a time series with the time level later

# 1a) Replace all NA with 1
crimeData$IncidntNum[is.na(crimeData$IncidntNum)] <- 1
# Make sure the NAs are now 1s
unique(crimeData$IncidntNum)
#[1] 1
str(crimeData)


# 1b) Transform Date
crimeData$Date <- as.Date(crimeData$Date, format='%m/%d/%Y')
# Check the dates
unique(crimeData$Date)
str(crimeData)


# 1c) Transform Time into numeric variable Timenum
# http://www.statmethods.net/advstats/timeseries.html
# http://stat.ethz.ch/R-manual/R-patched/library/stats/html/ts.html

# Create Datetime variable with POSIX
crimeData$Datetime <- as.POSIXct(paste(crimeData$Date, as.character(crimeData$Time)))
summary(crimeData$Datetime)
#                 Min.               1st Qu.                Median                  Mean               3rd Qu.                  Max. 
#"2014-06-01 00:01:00" "2014-06-23 22:33:45" "2014-07-15 18:18:00" "2014-07-15 18:51:58" "2014-08-07 01:22:00" "2014-08-31 23:00:00" 
str(crimeData$Datetime)
# POSIXct[1:30760], format: "2014-08-31 20:30:00" "2014-08-31 14:30:00" "2014-08-31 11:30:00" "2014-08-31 17:49:00" "2014-08-31 18:05:00" ...

# Create Time numeric variable
crimeData$Timenum <- as.numeric(format(crimeData$Datetime, format="%H%M"))

str(crimeData)
#'data.frame':	30760 obs. of  14 variables:
# $ IncidntNum: num  1 1 1 1 1 1 1 1 1 1 ...
# $ Category  : Factor w/ 36 levels "ARSON","ASSAULT",..: 17 17 17 8 8 17 35 34 17 25 ...
# $ Descript  : Factor w/ 408 levels "ABANDONMENT OF CHILD",..: 199 197 197 299 286 197 407 360 269 319 ...
# $ DayOfWeek : Factor w/ 7 levels "Friday","Monday",..: 4 4 4 4 4 4 4 4 4 4 ...
# $ Date      : Date, format: "2014-08-31" "2014-08-31" "2014-08-31" "2014-08-31" ...
# $ Time      : Factor w/ 1408 levels "00:01","00:02",..: 1200 840 660 1039 1055 1 1101 600 960 1068 ...
# $ PdDistrict: Factor w/ 10 levels "BAYVIEW","CENTRAL",..: 2 2 2 4 5 2 7 3 2 2 ...
# $ Resolution: Factor w/ 16 levels "ARREST, BOOKED",..: 12 12 12 1 1 12 1 12 12 12 ...
# $ Address   : Factor w/ 8571 levels "0.0 Block of 10TH ST",..: 6847 6097 8245 2031 7133 6634 7317 2461 859 3761 ...
# $ X         : num  -122 -122 -122 -122 -122 ...
# $ Y         : num  37.8 37.8 37.8 37.8 37.8 ...
# $ Location  : Factor w/ 12597 levels "(37.7080829769597, -122.419241455854)",..: 10920 11550 10664 5751 9606 10699 6016 1109 11656 10266 ...
# $ Datetime  : POSIXct, format: "2014-08-31 20:30:00" "2014-08-31 14:30:00" "2014-08-31 11:30:00" "2014-08-31 17:49:00" ...
# $ Timenum   : num  2030 1430 1130 1749 1805 ...

##################################################

# Part 2) Now look at the number of counts each of the 36 categories of crime in the data set
summary(crimeData$Category)
#                      ARSON                     ASSAULT                  BAD CHECKS                     BRIBERY                    BURGLARY 
#                         60                        2518                           3                           1                        1257 
#         DISORDERLY CONDUCT DRIVING UNDER THE INFLUENCE               DRUG/NARCOTIC                 DRUNKENNESS                EMBEZZLEMENT 
#                         69                          93                        1292                         142                          19 
#                  EXTORTION             FAMILY OFFENSES      FORGERY/COUNTERFEITING                       FRAUD                    GAMBLING 
#                          7                           9                          88                         574                           1 
#                 KIDNAPPING               LARCENY/THEFT                 LIQUOR LAWS                   LOITERING              MISSING PERSON 
#                        128                        8205                          38                           5                        1135 
#               NON-CRIMINAL              OTHER OFFENSES     PORNOGRAPHY/OBSCENE MAT                PROSTITUTION                     ROBBERY 
#                       3653                        4004                           1                          44                         862 
#                    RUNAWAY      SEX OFFENSES, FORCIBLE  SEX OFFENSES, NON FORCIBLE             STOLEN PROPERTY                     SUICIDE 
#                         61                         123                           5                           7                          14 
#             SUSPICIOUS OCC                    TRESPASS                   VANDALISM               VEHICLE THEFT                    WARRANTS 
#                        682                         259                        1611                        1885                        1583 
#                WEAPON LAWS 
#                        322 

# Explore what are "OTHER OFFENSES" (4004) and "NON-CRIMINAL" (3653)
# 2a) "OTHER OFFENSES" description
otheroffense <- subset(crimeData, crimeData$Category == "OTHER OFFENSES")
otheroffense <- droplevels(otheroffense)
summary(otheroffense$Descript)
#             ACTS AGAINST PUBLIC TRANSIT                    AGGRESSIVE SOLICITING                  BEYOND PARENTAL CONTROL 
#                                       8                                        1                                        4 
#                              CONSPIRACY CONTRIBUTING TO THE DELINQUENCY OF MINOR                       CRUELTY TO ANIMALS 
#                                     121                                       10                                        9 
#          DANGER OF LEADING IMMORAL LIFE                   DEFRAUDING TAXI DRIVER    DRIVERS LICENSE, SUSPENDED OR REVOKED 
#                                       5                                       13                                     1304 
#                        ESCAPE FROM JAIL      FAILURE TO HEED RED LIGHT AND SIREN      FAILURE TO REGISTER AS SEX OFFENDER 
#                                       1                                        3                                       19 
#  FALSE EVIDENCE OF VEHICLE REGISTRATION                         FALSE FIRE ALARM                     FALSE REPORT OF BOMB 
#                                      27                                        5                                        5 
#                   FALSE REPORT OF CRIME                          HABITUAL TRUANT                    HARASSING PHONE CALLS 
#                                       2                                        1                                       64 
#                       INDECENT EXPOSURE  INDECENT EXPOSURE WITH PRIOR CONVICTION      INJURY TO TELEGRAPH/TELEPHONE LINES 
#                                      24                                        2                                       18 
#                    INTOXICATED JUVENILE            JUDGE/JUROR ACCEPTING A BRIBE                LOST/STOLEN LICENSE PLATE 
#                                       3                                        1                                      292 
#             MISCELLANEOUS INVESTIGATION               MISCELLANEOUS STATE FELONY          MISCELLANEOUS STATE MISDEMEANOR 
#                                     201                                       10                                       13 
#                  OBSCENE PHONE CALLS(S)        OBSTRUCTIONS ON STREETS/SIDEWALKS     OPEN CONTAINER OF ALCOHOL IN VEHICLE 
#                                      35                                        4                                        3 
#         OPERATING TAXI WITHOUT A PERMIT       OPERATING WITHOUT DANCEHALL PERMIT                         PAROLE VIOLATION 
#                                       3                                        1                                       88 
#              PEDDLING WITHOUT A LICENSE                              PEEPING TOM             POSSESSION OF BURGLARY TOOLS 
#                                       5                                        1                                      141 
#              POSSESSION OF FIRECRACKERS                      PROBATION VIOLATION                         RECKLESS DRIVING 
#                                       7                                      275                                       21 
#                        RESISTING ARREST      SELLING/DISCHARGING OF FIRECRACKERS                                 SPEEDING 
#                                     369                                        2                                        5 
#                    SPITTING ON SIDEWALK                 TAMPERING WITH A VEHICLE            THROWING SUBSTANCE AT VEHICLE 
#                                       2                                       47                                        4 
#                       TRAFFIC VIOLATION                 TRAFFIC VIOLATION ARREST        UNAUTHORIZED USE OF LOUD SPEAKERS 
#                                     295                                      325                                        1 
#            VIOLATION OF FEDERAL STATUTE                   VIOLATION OF FIRE CODE              VIOLATION OF MUNICIPAL CODE 
#                                       1                                        2                                      107 
#      VIOLATION OF MUNICIPAL POLICE CODE                   VIOLATION OF PARK CODE           VIOLATION OF RESTRAINING ORDER 
#                                      52                                       17                                       25 


# 2b) "NON-CRIMINAL" description
nonCriminal <- subset(crimeData, crimeData$Category == "NON-CRIMINAL")
nonCriminal <- droplevels(nonCriminal)
summary(nonCriminal$Descript)
#            ACCIDENTAL BURNS                   AIDED CASE         AIDED CASE, DOG BITE   AIDED CASE, INJURED PERSON 
#                           1                          236                           65                           33 
#AIDED CASE, MENTAL DISTURBED      AIDED CASE, SICK PERSON              COURTESY REPORT  DEATH REPORT, CAUSE UNKNOWN 
#                        1142                           12                           63                          148 
#DEATH REPORT, NATURAL CAUSES                  FIRE REPORT               FOUND PROPERTY            IMPOUNDED VEHICLE 
#                          18                           51                          719                            2 
#            LOCATED PROPERTY                LOST PROPERTY            MISPLACED VEHICLE  PROPERTY FOR IDENTIFICATION 
#                          19                         1038                            1                           37 
#                     SHELTER              TARASOFF REPORT             TRAFFIC ACCIDENT                TURNED IN GUN 
#                          11                           22                           12                           23 


# 2c) Create 3 classes of crime categories based on frequency of crime incidents (low, medium, high)
# 2ci) Create crimeLow for crimes that have total incidents < 50:
# BAD CHECKS; BRIBERY; EMBEZZLEMENT; EXTORTION; FAMILY OFFENSES; GAMBLING; LIQUOR LAWS; LOITERING; PORNOGRAPHY/OBSCENE MAT; PROSTITUTION;
# SEX OFFENSES, NON FORCIBLE; STOLEN PROPERTY; SUICIDE;
crimeLow <- subset(crimeData, crimeData$Category %in% c('BAD CHECKS', 'BRIBERY', 'EMBEZZLEMENT', 'EXTORTION', 'FAMILY OFFENSES', 'GAMBLING', 
					'LIQUOR LAWS', 'LOITERING', 'PORNOGRAPHY/OBSCENE MAT', 'PROSTITUTION', 'SEX OFFENSES, NON FORCIBLE', 'STOLEN PROPERTY',
					'SUICIDE'))
crimeLow <- droplevels(crimeLow)
nrow(crimeLow)
#[1] 154
summary(crimeLow)


# 2cii) Create crimeMed for crimes that have total incidents between 50 and 800:
# ARSON; DISORDERLY CONDUCT; DRIVING UNDER THE INFLUENCE; DRUNKENNESS; FORGERY/COUNTERFEITING; FRAUD; KIDNAPPING; RUNAWAY;
# SEX OFFENSES, FORCIBLE; SUSPICIOUS OCC; TRESPASS; WEAPON LAWS;
crimeMed <- subset(crimeData, crimeData$Category %in% c('ARSON', 'DISORDERLY CONDUCT', 'DRIVING UNDER THE INFLUENCE', 'DRUNKENNESS', 
					'FORGERY/COUNTERFEITING', 'FRAUD', 'KIDNAPPING', 'RUNAWAY', 'SEX OFFENSES, FORCIBLE', 'SUSPICIOUS OCC',
					'TRESPASS', 'WEAPON LAWS'))
crimeMed <- droplevels(crimeMed)
nrow(crimeMed)
#[1] 2601
summary(crimeMed)


# 2ciii) Create crimeHigh for crimes that have total incidents > 800:
# ASSAULT; BURGLARY; DRUG/NARCOTIC; LARCENY/THEFT; MISSING PERSON; NON-CRIMINAL; OTHER OFFENSES; ROBBERY; VANDALISM;
# VEHICLE THEFT; WARRANTS;
crimeHigh <- subset(crimeData, crimeData$Category %in% c('ASSAULT', 'BURGLARY', 'DRUG/NARCOTIC', 'LARCENY/THEFT', 'MISSING PERSON',
					'NON-CRIMINAL', 'OTHER OFFENSES', 'ROBBERY', 'VANDALISM', 'VEHICLE THEFT', 'WARRANTS'))
crimeHigh <- droplevels(crimeHigh)
nrow(crimeHigh)
#[1] 28005
summary(crimeHigh)

##################################################

# Part 3) Exploratory Analysis of Crime Trends
# Install the necessary packages
#install.packages('ggplot2')
#install.packages("plyr")
#install.packages('sqldf')
library(ggplot2)
library(plyr)

# 3a) Plot a histogram showing the frequency of each crime category

crimeCatCount <- cbind(aggregate(crimeData[, "IncidntNum"], by = list(crimeData$Category), sum))
colnames(crimeCatCount) <- c("Category", "Frequency")
#crimeCatCount <- crimeCatCount[order(crimeCatCount$Frequency,decreasing=TRUE),]

png("/crimecatHist.png", height=1200, width=1800)
ggplot(crimeCatCount, aes(Category, Frequency)) + geom_bar(stat = "identity") +  
    ggtitle("Frequency of each crime category in the SF Bay area") + 
    ylab("Frequency") + xlab("Crime category") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_text(aes(label=round(Frequency), size = 2, hjust = 1, vjust = -1))
dev.off()


# 3b) Frequency of each crime category distributed by date

categoryPerDate <- ddply(crimeData, c('Category','Date'), summarise, totalCategory = sum(IncidntNum, na.rm=T))
categoryPerDate

png("/crimecatDate.png", height=1500, width=2400)
ggplot(categoryPerDate, aes(x=Date, y=totalCategory, colour=Category)) + geom_line() +
	ggtitle("Distribution of frequency of each crime category by date") +
	ylab("Frequency") + xlab("Date")
dev.off()

# 3bi) Frequency of each low crime category distributed by date

categorylowPerDate <- ddply(crimeLow, c('Category','Date'), summarise, totalCategory = sum(IncidntNum, na.rm=T))
categorylowPerDate

png("/crimecatLowDate.png", height=1500, width=2400)
ggplot(categorylowPerDate, aes(x=Date, y=totalCategory, colour=Category)) + geom_line() +
	ggtitle("Distribution of frequency of each low crime category by date") +
	ylab("Frequency") + xlab("Date")
dev.off()

# 3bii) Frequency of each medium crime category distributed by date

categorymedPerDate <- ddply(crimeMed, c('Category','Date'), summarise, totalCategory = sum(IncidntNum, na.rm=T))
categorymedPerDate

png("/crimecatMedDate.png", height=1500, width=2400)
ggplot(categorymedPerDate, aes(x=Date, y=totalCategory, colour=Category)) + geom_line() +
	ggtitle("Distribution of frequency of each medium crime category by date") +
	ylab("Frequency") + xlab("Date")
dev.off()

# 3biii) Frequency of each high crime category distributed by date

categoryhighPerDate <- ddply(crimeHigh, c('Category','Date'), summarise, totalCategory = sum(IncidntNum, na.rm=T))
categoryhighPerDate

png("/crimecatHighDate.png", height=1500, width=2400)
ggplot(categoryhighPerDate, aes(x=Date, y=totalCategory, colour=Category)) + geom_line() +
	ggtitle("Distribution of frequency of each high crime category by date") +
	ylab("Frequency") + xlab("Date")
dev.off()


# 3c) Cumulative count of every crime category

categoryCumuDate <- ddply(categoryPerDate,.(Category),transform, cumCrime = cumsum(totalCategory)) 
categoryCumuDate

png("/crimecatCumu.png", height=1500, width=2400)
ggplot(categoryCumuDate, aes(x = Date, y = cumCrime, color = Category)) + geom_line() +
	ggtitle("Cumulative count of each crime category") + ylab("Cumulative Count") + xlab("Date")
dev.off()

# 3ci) Cumulative count of every low crime category

categorylowCumuDate <- ddply(categorylowPerDate,.(Category),transform, cumCrime = cumsum(totalCategory)) 
categorylowCumuDate 

png("/crimecatLowCumu.png", height=1500, width=2400)
ggplot(categorylowCumuDate, aes(x = Date, y = cumCrime, color = Category)) + geom_line() +
	ggtitle("Cumulative count of each low crime category") + ylab("Cumulative Count") + xlab("Date")
dev.off()

# 3cii) Cumulative count of every medium crime category

categorymedCumuDate <- ddply(categorymedPerDate,.(Category),transform, cumCrime = cumsum(totalCategory)) 
categorymedCumuDate 

png("/crimecatMedCumu.png", height=1500, width=2400)
ggplot(categorymedCumuDate, aes(x = Date, y = cumCrime, color = Category)) + geom_line() +
	ggtitle("Cumulative count of each medium crime category") + ylab("Cumulative Count") + xlab("Date")
dev.off()

# 3ciii) Cumulative count of every high crime category

categoryhighCumuDate <- ddply(categoryhighPerDate,.(Category),transform, cumCrime = cumsum(totalCategory)) 
categoryhighCumuDate 

png("/crimecatHighCumu.png", height=1500, width=2400)
ggplot(categoryhighCumuDate, aes(x = Date, y = cumCrime, color = Category)) + geom_line() +
	ggtitle("Cumulative count of each high crime category") + ylab("Cumulative Count") + xlab("Date")
dev.off()


# 3d) Distribution of all crimes by date

crimePerDate <- ddply(crimeData, c('Date'), summarise, totalCrimes = sum(IncidntNum, na.rm=T))
crimePerDate

png("/totcrimeDate.png", height=1500, width=2400)
ggplot(crimePerDate, aes(x = Date, y=totalCrimes)) + geom_line() +
	ggtitle("Frequency of Crimes by Date") + ylab("Crime Frequency") + xlab("Date") 
dev.off()


###############################  Question to ask: ##########################################

# Part 4) Where shouldn't you park your car?

# Subset vehicle theft to data frame 'carTheft'
carTheft <- subset(crimeData, crimeData$Category == "VEHICLE THEFT")
# 1885 vehicle theft incidents
nrow(carTheft)
#[1] 1885
# drop all the redundant levels from the data frame
carTheft <- droplevels(carTheft)
str(carTheft)

#What are the description of the each vehicle theft? 
summary(carTheft$Descript)
#    ATTEMPTED STOLEN VEHICLE STOLEN AND RECOVERED VEHICLE            STOLEN AUTOMOBILE                   STOLEN BUS 
#                          13                           89                         1307                            1 
#STOLEN MISCELLANEOUS VEHICLE            STOLEN MOTORCYCLE                 STOLEN TRUCK 
#                           2                          140                          333 

# Summation of the # of car theft on each date
cartheftPerDate <- ddply(carTheft, "Date", summarise, totalTheft = sum(IncidntNum, na.rm=T))

summary(cartheftPerDate)
#      Date              totalTheft   
# Min.   :2014-06-01   Min.   : 4.00  
# 1st Qu.:2014-06-23   1st Qu.:17.00  
# Median :2014-07-16   Median :20.00  
# Mean   :2014-07-16   Mean   :20.49  
# 3rd Qu.:2014-08-08   3rd Qu.:25.00  
# Max.   :2014-08-31   Max.   :38.00  

# The average number of car theft per day: 20.48913
mean(cartheftPerDate$totalTheft)
# The median number of car theft per day: 20
median(cartheftPerDate$totalTheft)

# Visualize the distribution of car theft incidents for each date
png("/cartheftperDate.png", height=800, width=1500)
ggplot(data=cartheftPerDate, aes(x=cartheftPerDate$Date, y=cartheftPerDate$totalTheft)) + geom_line() + 
    ggtitle("Distribution of Car Theft per Date") + xlab("Date") + 
    ylab("Total car theft per date")
dev.off()

# 2014-06-27 has the most number of car theft that day, with 38 thefts total on that day
cartheftPerDate[which.max(cartheftPerDate$totalTheft),]
#         Date totalTheft
#27 2014-06-27         38


# Summation of the number of car theft at each address
cartheftPerAddress <- ddply(carTheft, "Address", summarise, totalTheft = sum(IncidntNum, na.rm=T))
summary(cartheftPerAddress)
#                   Address       totalTheft   
# 0.0 Block of 12TH ST  :   1   Min.   :1.000  
# 0.0 Block of 9TH ST   :   1   1st Qu.:1.000  
# 0.0 Block of AGNON AV :   1   Median :1.000  
# 0.0 Block of AGUA WY  :   1   Mean   :1.152  
# 0.0 Block of ALBION ST:   1   3rd Qu.:1.000  
# 0.0 Block of ALPHA ST :   1   Max.   :5.000 

# The average number of car theft at each address: 1.1522
mean(cartheftPerAddress$totalTheft)
# The median number of car theft at each address: 1
median(cartheftPerAddress$totalTheft)

# The address that has the highest number of car theft occurred
cartheftPerAddress[which.max(cartheftPerAddress$totalTheft),]
#                    Address totalTheft
#859 400.0 Block of BEALE ST          5

# Addresses where there were more than 2 vehicle theft incidents: 31 addresses total
unique(cartheftPerAddress$Address[cartheftPerAddress$totalTheft > 2])
# [1] 0.0 Block of COLUMBIASQUARE ST    0.0 Block of LAFAYETTE ST         1200.0 Block of MINNESOTA ST      1300.0 Block of HYDE ST           1300.0 Block of SOUTH VAN NESS AV
# [6] 14TH ST / SOUTH VAN NESS AV       1600.0 Block of FELL ST           16TH ST / POTRERO AV              200.0 Block of BRUNSWICK ST       200.0 Block of ELLIS ST          
#[11] 200.0 Block of SHIPLEY ST         2300.0 Block of CESAR CHAVEZ ST   2300.0 Block of MARKET ST         2900.0 Block of 22ND ST           2900.0 Block of GEARY BL         
#[16] 300.0 Block of COLERIDGE ST       300.0 Block of WINDING WY         3200.0 Block of 20TH AV           400.0 Block of BEALE ST           400.0 Block of STOCKTON ST       
#[21] 4700.0 Block of MISSION ST        500.0 Block of 9TH ST             500.0 Block of JOHNMUIR DR        800.0 Block of EDDY ST            800.0 Block of MISSION ST        
#[26] 9TH ST / HARRISON ST              GROVE ST / OCTAVIA ST             LARKIN ST / BUSH ST               MINNA ST / 6TH ST                 POST ST / HYDE ST                
#[31] REVERE AV / HAWES ST             
#1636 Levels: 0.0 Block of 12TH ST 0.0 Block of 9TH ST 0.0 Block of AGNON AV 0.0 Block of AGUA WY 0.0 Block of ALBION ST 0.0 Block of ALPHA ST ... YUKON ST / 19TH ST


# Summation of the number of car theft at each location

cartheftPerLocation <- ddply(carTheft, c('X', 'Y', 'Location'), summarise, totalTheft = sum(IncidntNum, na.rm=T))
summary(cartheftPerLocation)
#       X                Y                                          Location      totalTheft   
# Min.   :-122.5   Min.   :37.71   (37.7080829769597, -122.419241455854):   1   Min.   :1.000  
# 1st Qu.:-122.4   1st Qu.:37.73   (37.7087317114534, -122.434482060354):   1   1st Qu.:1.000  
# Median :-122.4   Median :37.76   (37.7088054432761, -122.452664083403):   1   Median :1.000  
# Mean   :-122.4   Mean   :37.76   (37.7089619900721, -122.440941836311):   1   Mean   :1.125  
# 3rd Qu.:-122.4   3rd Qu.:37.78   (37.7091256836156, -122.405953128497):   1   3rd Qu.:1.000  
# Max.   :-122.4   Max.   :37.82   (37.7091523763418, -122.450139204429):   1   Max.   :4.000  
#                                  (Other)                              :1669          

# The average number of car theft at each location: 1.125373
mean(cartheftPerLocation$totalTheft)
# The median number of car theft at each location: 1
median(cartheftPerLocation$totalTheft)

# The location that has the highest number of car theft occurred
cartheftPerLocation[which.max(cartheftPerLocation$totalTheft),]
#                                 Location totalTheft
#266 (37.7274261358156, -122.382690587869)          4

# Locations where there were more than 2 vehicle theft incidents: 29 locations total
unique(cartheftPerLocation$Location[cartheftPerLocation$totalTheft > 2])
# [1] (37.7805345385575, -122.408162953857) (37.7911776792521, -122.400748631911) (37.7827993333406, -122.421828641798) (37.7746912745951, -122.432649310329)
# [5] (37.7820272296679, -122.450507717382) (37.7215153638924, -122.437244382447) (37.749041309393, -122.418137290501)  (37.777090953432, -122.394985875925) 
# [9] (37.7619674218399, -122.417180448635) (37.7874135388302, -122.391190144649) (37.8041461526497, -122.425110613231) (37.7607584311989, -122.437193152373)
#[13] (37.770848020393, -122.440359655894)  (37.7684043083371, -122.417803355796) (37.7889098399718, -122.418658294829) (37.7923429638602, -122.414295532568)
#[17] (37.7898710452058, -122.40700591265)  (37.7725264934557, -122.410078544842) (37.7731802966146, -122.444717526339) (37.7776021594287, -122.42481019069) 
#[21] (37.7822305870333, -122.41029351969)  (37.7647828166189, -122.424104315404) (37.7657827544572, -122.407538332319) (37.7838778192773, -122.4049702326)  
#[25] (37.7274261358156, -122.382690587869) (37.7494667413917, -122.400354051399) (37.7728293793314, -122.432282489277) (37.7650501214965, -122.419671780296)
#[29] (37.7872450378446, -122.416642370005)
#1675 Levels: (37.7080829769597, -122.419241455854) (37.7087317114534, -122.434482060354) (37.7088054432761, -122.452664083403) ... (37.8170463264521, -122.370740926948)

# to plot gis data,  check out http://www.r-bloggers.com/visualizing-gis-data-with-r-and-open-street-map/
# update this section later for visualization

###############################  Question to ask: ##########################################

# Part 5) What are the safest locations in SF? 

#Calculate the total crime per location
crimePerLocation <- ddply(crimeData, "Location", summarise, totalCrime = sum(IncidntNum, na.rm=T))

summary(crimePerLocation)
#                                  Location       totalCrime     
# (37.7080829769597, -122.419241455854):    1   Min.   :  1.000  
# (37.7081962685213, -122.443653772286):    1   1st Qu.:  1.000  
# (37.7082869675276, -122.445822610004):    1   Median :  1.000  
# (37.7083109744887, -122.420084075242):    1   Mean   :  2.442  
# (37.708463188697, -122.449101258208) :    1   3rd Qu.:  2.000  
# (37.7084755812092, -122.42053644771) :    1   Max.   :851.000  
# (Other)                              :12591 

# Average number of crime per location: 2.441851
mean(crimePerLocation$totalCrime)
# Median number of crime per location: 1
median(crimePerLocation$totalCrime)


# Location with the most number of crime ie. most dangerous (851 crimes total)
crimePerLocation[which.max(crimePerLocation$totalCrime),]
#                                  Location totalCrime
#7213 (37.7752316978411, -122.403742962696)        851

# Location with the least number of crime (1) ie. safest
# 7828 different locations reporting only 1 crime total
unique(crimePerLocation$Location[crimePerLocation$totalCrime == 1])

## Try address instead of location, compare how the answer differs
crimePerAddress <- ddply(crimeData, "Address", summarise, totalCrime = sum(IncidntNum, na.rm=T))

summary(crimePerAddress)
#                 Address       totalCrime     
# 0.0 Block of 10TH ST:   1   Min.   :  1.000  
# 0.0 Block of 11TH ST:   1   1st Qu.:  1.000  
# 0.0 Block of 12TH ST:   1   Median :  2.000  
# 0.0 Block of 13TH ST:   1   Mean   :  3.589  
# 0.0 Block of 14TH ST:   1   3rd Qu.:  3.000  
# 0.0 Block of 15TH ST:   1   Max.   :960.000  
# (Other)             :8565  

# Average number of crime per address: 3.588846
mean(crimePerAddress$totalCrime)
# Median number of crime per address: 2
median(crimePerAddress$totalCrime)

# Address with the most number of crimes ie. most dangerous (960 total)
crimePerAddress[which.max(crimePerAddress$totalCrime),]
#                      Address totalCrime
#5314 800.0 Block of BRYANT ST        960

# Location with the least number of crime (1) ie. safest
# 3875 addresses reporting only 1 crime
unique(crimePerAddress$Address[crimePerAddress$totalCrime == 1])


###############################  Question to ask: ##########################################

# Part 6) What days/times are especially dangerous?

# 6a. Distribution of crime category by Day instead of date
categoryPerDay <- ddply(crimeData, c('Category','DayOfWeek'), summarise, totalCategory = sum(IncidntNum, na.rm=T))
categoryPerDay

# Reorder the x-axis
categoryPerDay$DayOfWeek <- factor(categoryPerDay$DayOfWeek, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
categoryPerDay <- categoryPerDay[order(categoryPerDay$DayOfWeek), ]

# Refer to http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/
png("/crimecatDay.png", height=1500, width=2400)
ggplot(categoryPerDay, aes(x=DayOfWeek, y=totalCategory, group=Category,colour=Category)) + geom_line() + geom_point() +
	ggtitle("Distribution of frequency of each crime category by Day of Week") +
	ylab("Frequency") + xlab("Day of Week")
dev.off()

# 6ai) Distribution of low crime category by Day
categorylowPerDay <- ddply(crimeLow, c('Category','DayOfWeek'), summarise, totalCategory = sum(IncidntNum, na.rm=T))
categorylowPerDay

# Reorder the x-axis
categorylowPerDay$DayOfWeek <- factor(categorylowPerDay$DayOfWeek, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
categorylowPerDay <- categorylowPerDay[order(categorylowPerDay$DayOfWeek), ]

png("/crimecatLowDay.png", height=1500, width=2400)
ggplot(categorylowPerDay, aes(x=DayOfWeek, y=totalCategory, group=Category,colour=Category)) + geom_line() + geom_point() +
	ggtitle("Distribution of frequency of each low crime category by Day of Week") +
	ylab("Frequency") + xlab("Day of Week")
dev.off()

# 6aii) Distribution of medium crime category by Day
categorymedPerDay <- ddply(crimeMed, c('Category','DayOfWeek'), summarise, totalCategory = sum(IncidntNum, na.rm=T))
categorymedPerDay

# Reorder the x-axis
categorymedPerDay$DayOfWeek <- factor(categorymedPerDay$DayOfWeek, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
categorymedPerDay <- categorymedPerDay[order(categorymedPerDay$DayOfWeek), ]

png("/crimecatMedDay.png", height=1500, width=2400)
ggplot(categorymedPerDay, aes(x=DayOfWeek, y=totalCategory, group=Category,colour=Category)) + geom_line() + geom_point() +
	ggtitle("Distribution of frequency of each medium crime category by Day of Week") +
	ylab("Frequency") + xlab("Day of Week")
dev.off()

# 6aiii) Distribution of high crime category by Day
categoryhighPerDay <- ddply(crimeHigh, c('Category','DayOfWeek'), summarise, totalCategory = sum(IncidntNum, na.rm=T))
categoryhighPerDay

# Reorder the x-axis
categoryhighPerDay$DayOfWeek <- factor(categoryhighPerDay$DayOfWeek, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
categoryhighPerDay <- categoryhighPerDay[order(categoryhighPerDay$DayOfWeek), ]

png("/crimecatHighDay.png", height=1500, width=2400)
ggplot(categoryhighPerDay, aes(x=DayOfWeek, y=totalCategory, group=Category,colour=Category)) + geom_line() + geom_point() +
	ggtitle("Distribution of frequency of each high crime category by Day of Week") +
	ylab("Frequency") + xlab("Day of Week")
dev.off()



# 6b. Distribution of all crimes by Day instead of date
crimePerDay <- ddply(crimeData, c('DayOfWeek'), summarise, totalCrimes = sum(IncidntNum, na.rm=T))
crimePerDay

#Re-order the Day of the Week in the data set
crimePerDay$DayOfWeek <- factor(crimePerDay$DayOfWeek, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
crimePerDay <- crimePerDay[order(crimePerDay$DayOfWeek), ]

# Plot histogram
png("/totcrimeDay.png", height=800, width=800)
ggplot(crimePerDay, aes(x=DayOfWeek, y=totalCrimes)) + geom_bar(aes(fill = DayOfWeek), stat = "identity") +
	ggtitle("Frequency of Crimes by Day of Week") + ylab("Crime Frequency") + xlab("Day of Week") + 
    geom_text(aes(label=round(totalCrimes), size = 2, hjust = 1, vjust = -1))
dev.off()



# 6c. Distribution of each crime category by time of day

categoryPerTime <- ddply(crimeData, c('Category','Timenum'), summarise, totalCategory = sum(IncidntNum, na.rm=T))
categoryPerTime

png("/crimecatTime.png", height=1500, width=2400)
ggplot(categoryPerTime, aes(x = Timenum, y = totalCategory, color = Category)) + geom_line() +
	ggtitle("Distribution of Crime Categories by 24hour Interval") + ylab("Crime Frequency") + xlab("Interval (24-hours)")
dev.off()

# 6ci) Distribution of each low crime category by time of day
categorylowPerTime <- ddply(crimeLow, c('Category','Timenum'), summarise, totalCategory = sum(IncidntNum, na.rm=T))
categorylowPerTime

png("/crimecatLowTime.png", height=1500, width=2400)
ggplot(categorylowPerTime, aes(x = Timenum, y = totalCategory, color = Category)) + geom_line() +
	ggtitle("Distribution of Low Crime Categories by 24hour Interval") + ylab("Crime Frequency") + xlab("Interval (24-hours)")
dev.off()

# 6cii) Distribution of each medium crime category by time of day
categorymedPerTime <- ddply(crimeMed, c('Category','Timenum'), summarise, totalCategory = sum(IncidntNum, na.rm=T))
categorymedPerTime

png("/crimecatMedTime.png", height=1500, width=2400)
ggplot(categorymedPerTime, aes(x = Timenum, y = totalCategory, color = Category)) + geom_line() +
	ggtitle("Distribution of Medium Crime Categories by 24hour Interval") + ylab("Crime Frequency") + xlab("Interval (24-hours)")
dev.off()

# 6ciii) Distribution of each high crime category by time of day
categoryhighPerTime <- ddply(crimeHigh, c('Category','Timenum'), summarise, totalCategory = sum(IncidntNum, na.rm=T))
categoryhighPerTime

png("/crimecatHighTime.png", height=1500, width=2400)
ggplot(categoryhighPerTime, aes(x = Timenum, y = totalCategory, color = Category)) + geom_line() +
	ggtitle("Distribution of Medium Crime Categories by 24hour Interval") + ylab("Crime Frequency") + xlab("Interval (24-hours)")
dev.off()



# 6d. Distribution of all crimes by time of day

crimePerTime <- ddply(crimeData, "Timenum", summarise, totalCrimes = sum(IncidntNum, na.rm=T))
crimePerTime

png("/totcrimeTime.png", height=1500, width=2400)
ggplot(crimePerTime, aes(x = Timenum, y=totalCrimes)) + geom_line() +
	ggtitle("Frequency of Crimes by 24hour Interval") + ylab("Crime Frequency") + xlab("Interval (24-hours)") 
dev.off()


###############################  Question to ask: ##########################################

# Part 7) Are certain thefts more common in certain areas?

# Refer to SF Crime Heat Map 
# https://data.sfgov.org/Public-Safety/SF-Crime-Heat-Map-Previous-Three-Months/q6gg-sa2p

# Focus on EMBEZZLEMENT, LARCENY/THEFT, STOLEN PROPERTY, VEHICLE THEFT in the new data frame 
crimeTheft <- subset(crimeData, crimeData$Category %in% c('EMBEZZLEMENT', 'LARCENY/THEFT', 'STOLEN PROPERTY', 'VEHICLE THEFT'))
nrow(crimeTheft)
#[1] 10116
# drop all the redundant levels from the data frame
crimeTheft <- droplevels(crimeTheft)
str(crimeTheft)


#What are the description of the each theft crime? 
summary(crimeTheft$Descript)
#                 ATTEMPTED AUTO STRIP     ATTEMPTED GRAND THEFT FROM PERSON     ATTEMPTED PETTY THEFT OF PROPERTY                 ATTEMPTED SHOPLIFTING 
#                                    1                                     3                                     3                                     5 
#             ATTEMPTED STOLEN VEHICLE       ATTEMPTED THEFT FROM A BUILDING   ATTEMPTED THEFT FROM LOCKED VEHICLE ATTEMPTED THEFT FROM UNLOCKED VEHICLE 
#                                   13                                     6                                    57                                     2 
#         ATTEMPTED THEFT OF A BICYCLE                     EMBEZZLED VEHICLE             EMBEZZLEMENT, GRAND THEFT EMBEZZLEMENT, GRAND THEFT BY EMPLOYEE 
#                                    5                                     1                                     3                                     9 
#EMBEZZLEMENT, PETTY THEFT BY EMPLOYEE                GRAND THEFT AUTO STRIP                   GRAND THEFT BICYCLE     GRAND THEFT COIN OPERATED MACHINE 
#                                    6                                    22                                    88                                     1 
#          GRAND THEFT FROM A BUILDING          GRAND THEFT FROM LOCKED AUTO               GRAND THEFT FROM PERSON        GRAND THEFT FROM UNLOCKED AUTO 
#                                  248                                  3166                                   307                                   544 
#              GRAND THEFT OF PROPERTY                GRAND THEFT PICKPOCKET               GRAND THEFT PURSESNATCH               GRAND THEFT SHOPLIFTING 
#                                  384                                   212                                    10                                    60 
#           LOST PROPERTY, GRAND THEFT            LOST PROPERTY, PETTY THEFT                PETTY THEFT AUTO STRIP                   PETTY THEFT BICYCLE 
#                                   26                                    53                                     9                                    98 
#    PETTY THEFT COIN OPERATED MACHINE           PETTY THEFT FROM A BUILDING          PETTY THEFT FROM LOCKED AUTO        PETTY THEFT FROM UNLOCKED AUTO 
#                                    1                                   417                                   913                                   149 
#         PETTY THEFT MOTORCYCLE STRIP               PETTY THEFT OF PROPERTY               PETTY THEFT SHOPLIFTING                PETTY THEFT WITH PRIOR 
#                                    1                                   955                                   371                                    88 
#            RECEIVING STOLEN PROPERTY          STOLEN AND RECOVERED VEHICLE                     STOLEN AUTOMOBILE                            STOLEN BUS 
#                                    7                                    89                                  1307                                     1 
#         STOLEN MISCELLANEOUS VEHICLE                     STOLEN MOTORCYCLE                          STOLEN TRUCK 
#                                    2                                   140                                   333 


# Summation of the number theft crime at each location
crimeTheftPerLocation <- ddply(crimeTheft, c('Category', 'Descript', 'Address', 'PdDistrict', 'X', 'Y', 'Location'), summarise, totalTheft = sum(IncidntNum, na.rm=T))
summary(crimeTheftPerLocation)
#            Category                              Descript                          Address         PdDistrict         X                Y        
# EMBEZZLEMENT   :  17   GRAND THEFT FROM LOCKED AUTO  :2050   800.0 Block of MARKET ST  :  41   SOUTHERN :1442   Min.   :-122.5   Min.   :37.71  
# LARCENY/THEFT  :6115   STOLEN AUTOMOBILE             :1251   800.0 Block of BRYANT ST  :  26   CENTRAL  :1283   1st Qu.:-122.4   1st Qu.:37.76  
# STOLEN PROPERTY:   6   PETTY THEFT FROM LOCKED AUTO  : 763   2000.0 Block of MISSION ST:  19   NORTHERN : 885   Median :-122.4   Median :37.78  
# VEHICLE THEFT  :1819   PETTY THEFT OF PROPERTY       : 726   700.0 Block of MARKET ST  :  19   INGLESIDE: 815   Mean   :-122.4   Mean   :37.77  
#                        GRAND THEFT FROM UNLOCKED AUTO: 466   900.0 Block of MARKET ST  :  18   MISSION  : 812   3rd Qu.:-122.4   3rd Qu.:37.79  
#                        PETTY THEFT FROM A BUILDING   : 378   100.0 Block of OFARRELL ST:  17   BAYVIEW  : 637   Max.   :-122.4   Max.   :37.82  
#                        (Other)                       :2323   (Other)                   :7817   (Other)  :2083                                   
#                                  Location      totalTheft     
# (37.7752316978411, -122.403742962696):  19   Min.   :  1.000  
# (37.7787192628187, -122.414743835382):  15   1st Qu.:  1.000  
# (37.7822305870333, -122.41029351969) :  15   Median :  1.000  
# (37.7290910125835, -122.476011600296):  14   Mean   :  1.271  
# (37.7650501214965, -122.419671780296):  14   3rd Qu.:  1.000  
# (37.7847532835996, -122.407036790381):  14   Max.   :203.000  
# (Other)                              :7866                  

# The average number of theft crime at each location: 1.323391
mean(crimeTheftPerLocation$totalTheft)
# The median number of theft crime at each location: 1
median(crimeTheftPerLocation$totalTheft)

# The location that has the highest number of car theft occurred
crimeTheftPerLocation[which.max(crimeTheftPerLocation$totalTheft),]
#         Category                     Descript                  Address PdDistrict         X        Y                              Location totalTheft
#120 LARCENY/THEFT GRAND THEFT FROM LOCKED AUTO 800.0 Block of BRYANT ST   SOUTHERN -122.4037 37.77523 (37.7752316978411, -122.403742962696)        203
# Find the unique level of total theft crimes reported 
unique(crimeTheftPerLocation$totalTheft)
# [1]   2   1   8  24   9  13  30   3  23  26   4   6  21  10 203  40  18  16   5  17  49  32  14   7  28  12  19  35  11  15


## Find the locations and addresses where the total theft crimes are 10 or higher
## There are multiple repeated locations addresses
# Location and address  where there were 203 theft crimes: 1 location total
unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 203])
#[1] (37.7752316978411, -122.403742962696)
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 203])
#[1] 800.0 Block of BRYANT ST

# Location and address  where there were 49 theft crimes: 1 location total
unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 49])
#[1] (37.7752316978411, -122.403742962696)
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 49])
#[1] 800.0 Block of BRYANT ST

# Location and address  where there were 40 theft crimes: 1 location total
unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 40])
#[1] (37.7752316978411, -122.403742962696)
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 40])
#[1] 800.0 Block of BRYANT ST

# Location and address  where there were 35 theft crimes: 1 location total
unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 35])
#[1] (37.7846237669087, -122.403890223654)
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 35])
#[1] 700.0 Block of MISSION ST

unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 32])
#[1] (37.7898710452058, -122.40700591265)
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 32])
#[1] 400.0 Block of STOCKTON ST

unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 30])
#[1] (37.7724965522563, -122.465838183623)
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 30])
#[1] 500.0 Block of JOHNFKENNEDY DR

unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 28])
#[1] (37.7752316978411, -122.403742962696)
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 28])
#[1] 800.0 Block of BRYANT ST

unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 26])
#[1] (37.7843722316189, -122.39960084328)
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 26])
#[1] 200.0 Block of 3RD ST

unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 24])
#[1] (37.7865024401487, -122.407567826112)
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 24])
#[1] 100.0 Block of OFARRELL ST

unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 23])
#[1] (37.7752316978411, -122.403742962696)
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 23])
#[1] 800.0 Block of BRYANT ST

unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 19])
#[1] (37.7823643134787, -122.447204009307)
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 19])
#[1] 2600.0 Block of GEARY BL

unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 18])
#[1] (37.7290910125835, -122.476011600296) (37.8040873635258, -122.448223929422) (37.7772029583892, -122.512538760234)
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 18])
#[1] 3200.0 Block of 20TH AV       3600.0 Block of LYON ST       1000.0 Block of POINTLOBOS AV

unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 17])
#[1] (37.7850491022993, -122.406659517434) (37.7847532835996, -122.407036790381)
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 17])
#[1] 800.0 Block of MARKET ST

unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 16])
#[1] (37.7838778192773, -122.4049702326)   (37.7867246331993, -122.405795379675) (37.8170463264521, -122.370740926948)
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 16])
#[1] 800.0 Block of MISSION ST      0.0 Block of OFARRELL ST       200.0 Block of INTERSTATE80 HY

unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 15])
#[1] (37.7871027247862, -122.388007401911) (37.7709657259945, -122.412843427871)
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 15])
#[1] THE EMBARCADEROSOUTH ST / BRYANT ST 300.0 Block of 11TH ST             

unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 14])
#[1] (37.7804430348854, -122.412499579078) (37.771578576534, -122.411279189969) 
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 14])
#[1] 1100.0 Block of MARKET ST 10TH ST / HARRISON ST    

unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 13])
#[1] (37.7883971527809, -122.407309751547) (37.7865024401487, -122.407567826112) (37.7691519191763, -122.510692509436)
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 13])
#[1] 300.0 Block of POST ST     100.0 Block of OFARRELL ST 1000.0 Block of GREAT HY  

unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 12])
#[1] (37.771578576534, -122.411279189969)  (37.7895727625927, -122.388485562387) (37.7847532835996, -122.407036790381) (37.7290910125835, -122.476011600296)
#[5] (37.788837905642, -122.389432577161)  (37.7724965522563, -122.465838183623)
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 12])
#[1] HARRISON ST / 10TH ST                 THE EMBARCADEROSOUTH ST / HARRISON ST 800.0 Block of MARKET ST              3200.0 Block of 20TH AV              
#[5] HARRISON ST / SPEAR ST                500.0 Block of JOHNFKENNEDY DR       

unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 11])
#[1] (37.7807906550364, -122.399617558782) (37.7706305911072, -122.41248326348)  (37.8074092672378, -122.414821676232)
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 11])
#[1] HARRISON ST / 4TH ST    11TH ST / HARRISON ST   300.0 Block of BEACH ST

unique(crimeTheftPerLocation$Location[crimeTheftPerLocation$totalTheft == 10])
#[1] (37.7865414238751, -122.404887212032) (37.7710282062356, -122.456866495245) (37.7823643134787, -122.447204009307)
unique(crimeTheftPerLocation$Address[crimeTheftPerLocation$totalTheft == 10])
#[1] 700.0 Block of MARKET ST       100.0 Block of JOHNFKENNEDY DR 2600.0 Block of GEARY BL


## 27 unique addresses that report 10 or more total theft crimes
# Now see what are the common types of theft in each address:
# 7a. 800.0 Block of BRYANT ST  (37.7752316978411, -122.403742962696) 
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "800.0 Block of BRYANT ST"])
# [1] GRAND THEFT OF PROPERTY               PETTY THEFT FROM A BUILDING           GRAND THEFT FROM LOCKED AUTO          PETTY THEFT FROM LOCKED AUTO         
# [5] PETTY THEFT OF PROPERTY               LOST PROPERTY, PETTY THEFT            GRAND THEFT FROM UNLOCKED AUTO        EMBEZZLEMENT, PETTY THEFT BY EMPLOYEE
# [9] GRAND THEFT PICKPOCKET                ATTEMPTED THEFT FROM LOCKED VEHICLE   GRAND THEFT FROM PERSON               GRAND THEFT BICYCLE                  
#[13] PETTY THEFT BICYCLE                   GRAND THEFT FROM A BUILDING           PETTY THEFT WITH PRIOR                PETTY THEFT FROM UNLOCKED AUTO       
#[17] PETTY THEFT SHOPLIFTING               STOLEN AUTOMOBILE                     LOST PROPERTY, GRAND THEFT

# 7b. 700.0 Block of MISSION ST  (37.7846237669087, -122.403890223654)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "700.0 Block of MISSION ST"])
# [1] GRAND THEFT OF PROPERTY      PETTY THEFT WITH PRIOR       PETTY THEFT SHOPLIFTING      PETTY THEFT FROM A BUILDING  PETTY THEFT OF PROPERTY     
# [6] GRAND THEFT FROM A BUILDING  GRAND THEFT BICYCLE          GRAND THEFT FROM LOCKED AUTO GRAND THEFT FROM PERSON      PETTY THEFT BICYCLE         

# 7c. 400.0 Block of STOCKTON ST  (37.7898710452058, -122.40700591265)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "400.0 Block of STOCKTON ST"])
#[1] GRAND THEFT FROM LOCKED AUTO   STOLEN TRUCK                   GRAND THEFT FROM UNLOCKED AUTO STOLEN AUTOMOBILE              PETTY THEFT FROM LOCKED AUTO  
#[6] PETTY THEFT OF PROPERTY        GRAND THEFT OF PROPERTY       

# 7d. 500.0 Block of JOHNFKENNEDY DR   (37.7724965522563, -122.465838183623)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "500.0 Block of JOHNFKENNEDY DR"])
#[1] GRAND THEFT FROM LOCKED AUTO        PETTY THEFT BICYCLE                 PETTY THEFT OF PROPERTY             PETTY THEFT FROM LOCKED AUTO       
#[5] GRAND THEFT OF PROPERTY             GRAND THEFT FROM UNLOCKED AUTO      GRAND THEFT PICKPOCKET              ATTEMPTED THEFT FROM LOCKED VEHICLE

# 7e. 200.0 Block of 3RD ST  (37.7843722316189, -122.39960084328)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "200.0 Block of 3RD ST"])
#[1] GRAND THEFT FROM LOCKED AUTO   PETTY THEFT FROM LOCKED AUTO   PETTY THEFT OF PROPERTY        GRAND THEFT FROM UNLOCKED AUTO

# 7f. 100.0 Block of OFARRELL ST (37.7865024401487, -122.407567826112)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "100.0 Block of OFARRELL ST"])
# [1] PETTY THEFT SHOPLIFTING      PETTY THEFT WITH PRIOR       GRAND THEFT SHOPLIFTING      STOLEN MOTORCYCLE            GRAND THEFT FROM LOCKED AUTO
# [6] GRAND THEFT FROM A BUILDING  PETTY THEFT OF PROPERTY      PETTY THEFT FROM A BUILDING  STOLEN AUTOMOBILE            GRAND THEFT PICKPOCKET      
#[11] GRAND THEFT FROM PERSON     

# 7g. 2600.0 Block of GEARY BL  (37.7823643134787, -122.447204009307)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "2600.0 Block of GEARY BL"])
# [1] GRAND THEFT FROM LOCKED AUTO   PETTY THEFT FROM LOCKED AUTO   PETTY THEFT SHOPLIFTING        GRAND THEFT OF PROPERTY       
# [5] GRAND THEFT PICKPOCKET         GRAND THEFT FROM UNLOCKED AUTO PETTY THEFT WITH PRIOR         STOLEN AUTOMOBILE             
# [9] PETTY THEFT FROM A BUILDING    GRAND THEFT BICYCLE           

# 7h. 3200.0 Block of 20TH AV  (37.7290910125835, -122.476011600296)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "3200.0 Block of 20TH AV"])
# [1] PETTY THEFT SHOPLIFTING             PETTY THEFT FROM A BUILDING         GRAND THEFT FROM LOCKED AUTO        STOLEN TRUCK                       
# [5] PETTY THEFT OF PROPERTY             STOLEN AUTOMOBILE                   GRAND THEFT SHOPLIFTING             PETTY THEFT FROM LOCKED AUTO       
# [9] GRAND THEFT FROM PERSON             GRAND THEFT FROM UNLOCKED AUTO      LOST PROPERTY, PETTY THEFT          PETTY THEFT WITH PRIOR             
#[13] GRAND THEFT FROM A BUILDING         GRAND THEFT PICKPOCKET              ATTEMPTED THEFT FROM LOCKED VEHICLE

# 7i. 3600.0 Block of LYON ST   (37.8040873635258, -122.448223929422)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "3600.0 Block of LYON ST"])
#[1] GRAND THEFT FROM LOCKED AUTO PETTY THEFT FROM LOCKED AUTO STOLEN AUTOMOBILE           

# 7j. 1000.0 Block of POINTLOBOS AV   (37.7772029583892, -122.512538760234)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "1000.0 Block of POINTLOBOS AV"])
#[1] GRAND THEFT FROM LOCKED AUTO PETTY THEFT FROM LOCKED AUTO PETTY THEFT OF PROPERTY     

# 7k. 800.0 Block of MARKET ST  (37.7850491022993, -122.406659517434)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "800.0 Block of MARKET ST"])
# [1] GRAND THEFT OF PROPERTY               GRAND THEFT PICKPOCKET                PETTY THEFT SHOPLIFTING               PETTY THEFT OF PROPERTY              
# [5] PETTY THEFT FROM A BUILDING           GRAND THEFT FROM PERSON               GRAND THEFT SHOPLIFTING               GRAND THEFT FROM A BUILDING          
# [9] PETTY THEFT WITH PRIOR                PETTY THEFT FROM LOCKED AUTO          EMBEZZLEMENT, PETTY THEFT BY EMPLOYEE GRAND THEFT FROM LOCKED AUTO         
#[13] GRAND THEFT BICYCLE                   PETTY THEFT BICYCLE                   RECEIVING STOLEN PROPERTY             EMBEZZLEMENT, GRAND THEFT BY EMPLOYEE

# 7l. 800.0 Block of MISSION ST  (37.7838778192773, -122.4049702326)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "800.0 Block of MISSION ST"])
#[1] GRAND THEFT FROM LOCKED AUTO GRAND THEFT FROM PERSON      GRAND THEFT OF PROPERTY      PETTY THEFT OF PROPERTY      PETTY THEFT FROM LOCKED AUTO
#[6] STOLEN TRUCK                 STOLEN AUTOMOBILE           

# 7m. 0.0 Block of OFARRELL ST (37.7867246331993, -122.405795379675)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "0.0 Block of OFARRELL ST"])
#[1] PETTY THEFT SHOPLIFTING         ATTEMPTED THEFT FROM A BUILDING STOLEN TRUCK                    GRAND THEFT SHOPLIFTING        
#[5] PETTY THEFT WITH PRIOR          GRAND THEFT FROM LOCKED AUTO    PETTY THEFT FROM A BUILDING    

# 7n. 200.0 Block of INTERSTATE80 HY  (37.8170463264521, -122.370740926948)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "200.0 Block of INTERSTATE80 HY"])
#[1] GRAND THEFT OF PROPERTY        PETTY THEFT FROM A BUILDING    PETTY THEFT FROM LOCKED AUTO   GRAND THEFT FROM LOCKED AUTO   PETTY THEFT FROM UNLOCKED AUTO
#[6] STOLEN AUTOMOBILE              PETTY THEFT OF PROPERTY        GRAND THEFT FROM A BUILDING    STOLEN TRUCK                  

# 7o. THE EMBARCADEROSOUTH ST / BRYANT ST  (37.7871027247862, -122.388007401911)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "THE EMBARCADEROSOUTH ST / BRYANT ST"])
#[1] GRAND THEFT FROM LOCKED AUTO   GRAND THEFT FROM UNLOCKED AUTO

# 7p. 300.0 Block of 11TH ST  (37.7709657259945, -122.412843427871)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "300.0 Block of 11TH ST"])
#[1] GRAND THEFT FROM PERSON      GRAND THEFT PICKPOCKET       LOST PROPERTY, PETTY THEFT   PETTY THEFT OF PROPERTY      GRAND THEFT FROM LOCKED AUTO
#[6] GRAND THEFT OF PROPERTY     

# 7q. 1100.0 Block of MARKET ST  (37.7804430348854, -122.412499579078)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "1100.0 Block of MARKET ST"])
#[1] PETTY THEFT SHOPLIFTING      PETTY THEFT WITH PRIOR       GRAND THEFT FROM A BUILDING  GRAND THEFT FROM PERSON      PETTY THEFT BICYCLE         
#[6] GRAND THEFT PURSESNATCH      GRAND THEFT FROM LOCKED AUTO PETTY THEFT OF PROPERTY     

# 7r. 10TH ST / HARRISON ST  (37.771578576534, -122.411279189969)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "10TH ST / HARRISON ST"])
#[1] GRAND THEFT FROM UNLOCKED AUTO GRAND THEFT FROM LOCKED AUTO   PETTY THEFT FROM LOCKED AUTO  

# 7s. 300.0 Block of POST ST  (37.7883971527809, -122.407309751547) 
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "300.0 Block of POST ST"])
#[1] GRAND THEFT FROM LOCKED AUTO PETTY THEFT FROM LOCKED AUTO STOLEN TRUCK                 GRAND THEFT FROM PERSON      GRAND THEFT SHOPLIFTING     
#[6] PETTY THEFT SHOPLIFTING      LOST PROPERTY, GRAND THEFT  

# 7t. 1000.0 Block of GREAT HY (37.7691519191763, -122.510692509436) 
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "1000.0 Block of GREAT HY"])
#[1] PETTY THEFT FROM LOCKED AUTO GRAND THEFT FROM LOCKED AUTO PETTY THEFT BICYCLE          PETTY THEFT OF PROPERTY      GRAND THEFT FROM PERSON     
#[6] STOLEN AUTOMOBILE            GRAND THEFT OF PROPERTY     

# 7u. THE EMBARCADEROSOUTH ST / HARRISON ST  (37.7895727625927, -122.388485562387)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "THE EMBARCADEROSOUTH ST / HARRISON ST"])
#[1] GRAND THEFT FROM LOCKED AUTO

# 7v. HARRISON ST / SPEAR ST   (37.788837905642, -122.389432577161)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "HARRISON ST / SPEAR ST"])
#[1] GRAND THEFT FROM LOCKED AUTO   GRAND THEFT FROM UNLOCKED AUTO

# 7w. HARRISON ST / 4TH ST  (37.7807906550364, -122.399617558782)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "HARRISON ST / 4TH ST"])
#[1] GRAND THEFT FROM LOCKED AUTO   STOLEN AUTOMOBILE              GRAND THEFT FROM UNLOCKED AUTO PETTY THEFT FROM LOCKED AUTO  

# 7x. 11TH ST / HARRISON ST  (37.7706305911072, -122.41248326348)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "11TH ST / HARRISON ST"])
#[1] GRAND THEFT FROM LOCKED AUTO PETTY THEFT FROM LOCKED AUTO

# 7y. 300.0 Block of BEACH ST   (37.8074092672378, -122.414821676232)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "300.0 Block of BEACH ST"])
#[1] GRAND THEFT FROM LOCKED AUTO PETTY THEFT FROM LOCKED AUTO GRAND THEFT OF PROPERTY     

# 7z. 700.0 Block of MARKET ST  (37.7865414238751, -122.404887212032)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "700.0 Block of MARKET ST"])
#[1] PETTY THEFT FROM A BUILDING PETTY THEFT SHOPLIFTING     PETTY THEFT OF PROPERTY     GRAND THEFT PICKPOCKET      GRAND THEFT OF PROPERTY    
#[6] GRAND THEFT FROM PERSON     GRAND THEFT BICYCLE         PETTY THEFT WITH PRIOR     

# 7aa. 100.0 Block of JOHNFKENNEDY DR   (37.7710282062356, -122.456866495245)
unique(crimeTheftPerLocation$Descript[crimeTheftPerLocation$Address == "100.0 Block of JOHNFKENNEDY DR"])
#[1] GRAND THEFT FROM LOCKED AUTO LOST PROPERTY, PETTY THEFT   PETTY THEFT OF PROPERTY      PETTY THEFT FROM LOCKED AUTO
