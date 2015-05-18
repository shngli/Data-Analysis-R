# Author: Chisheng Li
#
# Your data analysis must address the following questions:
# 1. Across the United States, which types of events (as indicated in the EVTYPE variable) 
# are most harmful with respect to population health? (Injuries + Fatalities) 
# 2. Across the United States, which types of events have the greatest economic consequences?
# (Property damage + Crop damage)
stormData <- read.table("/repdata-data-StormData.csv", fill=TRUE, sep=',', header=TRUE)

# $EVTYPE Factor w/ 985 levels ie. there are 985 different events labels in stormData
str(stormData)

### Step 1: Data munging to consolidate the EVTYPE variable 
# The NOAA list 48 official storm data events (page 6) in this document
# http://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf
# According to NOAA website(http://www.ncdc.noaa.gov/stormevents/details.jsp):
# From 1950 through 1954, only tornado events were recorded.
# From 1955 through 1996, only tornado, thunderstorm wind and hail events were collected.
# All 48 event types were only collected beginning Jan 1996.
# For this reason, this data analysis only compares records with 'BGN_DATE' 
# after January 1, 1996.
stormData$BGN_DATE <- as.Date(as.character(stormData$BGN_DATE), format = "%m/%d/%Y")
stormDataNew <- subset(stormData, BGN_DATE > as.Date("1996-01-01"))
stormDataNew <- droplevels(stormDataNew)

# There are 515 EVTYPE variable labels in stormDataNew
# unique(grep("Thunder", stormDataNew$EVTYPE, value = TRUE))
# Many variable labels are duplicates eg. "Thunderstorm Wind", "THUNDERSTORM WIND", 
# "TSTM WIND", "Tstm Wind" 
unique(stormDataNew$EVTYPE)
summary(stormDataNew$EVTYPE)

# First round consolidation of the EVTYPE variable:
# 1. Remove extra white space, symbols and punctuations, or numbers
# 2. Remove redundant events eg. "Summary", "Other" and "OTHER"
# 3. Change abbreviations eg. "TSTM" (Thunderstorm) and "CSTL" (Coastal)

#install.packages('stringr')
library(stringr)
# Convert EVTYPE to character class, upper case, and remove all leading and trailing spaces
stormDataNew$EVTYPE <- str_trim(toupper(as.character(stormDataNew$EVTYPE))) 
# Replace "/" and "-" characters with spaces
stormDataNew$EVTYPE <- gsub("/|-", " ", stormDataNew$EVTYPE)
# Remove all non alphabetical characters
stormDataNew$EVTYPE <- gsub("& |\\.|\\(|\\)|,|[0-9]", "", stormDataNew$EVTYPE) 
# Remove all "SUMMARY" event types
stormDataNew <- stormDataNew[!grepl("SUMMARY", stormDataNew$EVTYPE), ] 
# Remove trailing "G" or duplicated white spaces
stormDataNew$EVTYPE <- str_trim(gsub(" G$|  ", "", stormDataNew$EVTYPE))

# Create a vector of the abbreviations used in the remaining event types
eventAbbrvs <- c("TSTM", "CSTL", "SML", "FLD", "FLDG", "WND")
# Create a vector of the long version of the abbreviation
eventLong <- c("THUNDERSTORM", "COASTAL", "SMALL", "FLOOD", "FLOOD", "WIND")

# Change all abbreviations into their proper terms
for (i in 1:length(eventAbbrvs)) {
	stormDataNew$EVTYPE <- gsub(eventAbbrvs[i], eventLong[i], stormDataNew$EVTYPE)
}

stormDataNew$EVTYPE <- as.factor(stormDataNew$EVTYPE)

# Second round consolidation of the EVTYPE variable:
# http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/
# Relabel the common variable names to fit the 48 official events
# Some variable names have high frequency (> 50 counts) but are ignored because they 
# do not fit into the official events eg. "LANDSLIDE", "DRY MICROBURST", "OTHER"
# There are also many variable names with only 1 or 2 counts that have been ignored as well
# due to time constraint, however this is insignificant because only about 500 out of 
# 65341 observations have been omitted
# Note: consider library(plyr) and revalue() if the pc allows install.packages('plyr')

levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="COASTAL FLOODING"] <- "COASTAL FLOOD"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="UNSEASONABLY COLD"] <- "COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="PROLONG COLD"] <- "COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="UNSEASONAL LOW TEMP"] <- "COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="COLD WEATHER"] <- "COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="UNSEASONABLY DRY"] <- "DROUGHT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="ABNORMALLY DRY"] <- "DROUGHT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RECORD WARMTH"] <- "EXCESSIVE HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="TEMPERATURE RECORD"] <- "EXCESSIVE HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RECORD HEAT"] <- "EXCESSIVE HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RECORD TEMPERATURE"] <- "EXCESSIVE HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="EXTREME COLD"] <- "EXTREME COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="EXTREME WINDCHILL"] <- "EXTREME COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="EXTREME WINDCHILL TEMPERATURES"] <- "EXTREME COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="URBAN SMALL STREAM FLOOD"] <- "FLOOD"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RIVER FLOOD"] <- "FLOOD"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="GLAZE"] <- "FROST FREEZE"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="BLACK ICE"] <- "FROST FREEZE"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="UNSEASONABLY WARM"] <- "HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="VERY WARM"] <- "HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HEAVY RAIN HIGH SURF"] <- "HEAVY RAIN"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HEAVY SURF HIGH SURF"] <- "HIGH SURF"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="ASTRONOMICAL HIGH TIDE"] <- "HIGH SURF"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HEAVY SURF"] <- "HIGH SURF"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="GUSTY WINDS"] <- "HIGH WIND"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HURRICANE TYPHOON"] <- "HURRICANE"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RIP CURRENTS"] <- "RIP CURRENT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="MIXED PRECIPITATION"] <- "SLEET"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="STORM SURGE"] <- "STORM SURGE TIDE"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="STRONG WINDS"] <- "STRONG WIND"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="THUNDERSTORM WIND HAIL"] <- "THUNDERSTORM WIND"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="WILD FOREST FIRE"] <- "WILDFIRE"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="BRUSH FIRE"] <- "WILDFIRE"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="WINTER WEATHER MIX"] <- "WINTER WEATHER"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="FREEZING RAIN"] <- "WINTER WEATHER"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="LIGHT SNOW"] <- "WINTER WEATHER"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="MODERATE SNOWFALL"] <- "WINTER WEATHER"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="WINTRY MIX"] <- "WINTER WEATHER"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="SMALL HAIL"] <- "WINTER WEATHER"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="LIGHT FREEZING RAIN"] <- "WINTER WEATHER"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="FREEZING DRIZZLE"] <- "WINTER WEATHER"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RECORD COLD"] <- "EXTREME COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="ICY ROADS"] <- "FROST FREEZE"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="EXCESSIVE SNOW"] <- "HEAVY SNOW"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="TIDAL FLOODING"] <- "COASTAL FLOOD"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="GUSTY WIND"] <- "HIGH WIND"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="UNSEASONABLY WET"] <- "HEAVY RAIN"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="SNOW SQUALLS"] <- "BLIZZARD"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="SLEET STORM"] <- "SLEET"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="UNSEASONABLY COOL"] <- "COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="UNSEASONABLY WARM AND DRY"] <- "HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RECORD RAINFALL"] <- "HEAVY RAIN"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="MIXED PRECIP"] <- "SLEET"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="UNSEASONABLY HOT"] <- "HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="WIND DAMAGE"] <- "STRONG WIND"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="UNUSUALLY COLD"] <- "COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="COASTAL FLOODING EROSION"] <- "COASTAL FLOOD"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="NON SEVERE HAIL"] <- "HAIL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RIVER FLOODING"] <- "FLOOD"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="DRY CONDITIONS"] <- "DROUGHT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="FREEZING RAIN SLEET"] <- "SLEET"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HARD FREEZE"] <- "FROST FREEZE"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HEAT WAVE"] <- "EXCESSIVE HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HYPOTHERMIA EXPOSURE"] <- "EXTREME COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="UNUSUAL WARMTH"] <- "HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="FUNNEL CLOUDS"] <- "FUNNEL CLOUD"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="GUSTY THUNDERSTORM WINDS"] <- "THUNDERSTORM WIND"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HIGH SURF ADVISORY"] <- "HIGH SURF"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RECORD COOL"] <- "EXTREME COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="SNOW AND SLEET"] <- "SLEET"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="DRY SPELL"] <- "DROUGHT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="DRY WEATHER"] <- "DROUGHT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HEAVY RAIN AND WIND"] <- "HEAVY RAIN"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HEAVY RAIN WIND"] <- "HEAVY RAIN"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="SNOW SLEET"] <- "SLEET"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="UNUSUALLY WARM"] <- "HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RECORD TEMPERATURES"] <- "EXCESSIVE HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="COLD WIND CHILL TEMPERATURES"] <- "COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="SNOW DROUGHT"] <- "DROUGHT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="DAMAGING FREEZE"] <- "FROST FREEZE"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="BITTER WIND CHILL TEMPERATURES"] <- "EXTREME COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="AGRICULTURAL FREEZE"] <- "FROST FREEZE"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="SNOW FREEZING RAIN"] <- "SLEET"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="PROLONGED RAIN"] <- "HEAVY RAIN"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RAIN SNOW"] <- "SLEET"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="PROLONG WARMTH"] <- "EXCESSIVE HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="ROUGH SURF"] <- "HIGH SURF"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="ABNORMAL WARMTH"] <- "EXCESSIVE HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="SNOW SHOWERS"] <- "HEAVY SNOW"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="SNOW SQUALL"] <- "HEAVY SNOW"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HIGH SEAS"] <- "HIGH SURF"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="COLD AND FROST"] <- "FROST FREEZE"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="PATCHY DENSE FOG"] <- "DENSE FOG"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="EXCESSIVE RAIN"] <- "HEAVY RAIN"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="TYPHOON"] <- "HURRICANE"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="STREET FLOODING"] <- "FLOOD"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="COLD TEMPERATURES"] <- "COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="URBAN FLOOD"] <- "FLOOD"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="GUSTY THUNDERSTORM WIND"] <- "THUNDERSTORM WIND"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RECORD WINTER SNOW"] <- "HEAVY SNOW"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="VOLCANIC ASHFALL"] <- "VOLCANIC ASH"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HIGH SURF ADVISORIES"] <- "HIGH SURF"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HIGH WATER"] <- "HIGH SURF"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="WARM WEATHER"] <- "HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RECORD PRECIPITATION"] <- "HEAVY RAIN"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="EXCESSIVE RAINFALL"] <- "HEAVY RAIN"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="EXTREME WIND CHILL"] <- "EXTREME COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HOT WEATHER"] <- "HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="WINTERY MIX"] <- "WINTER WEATHER"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HIGH WINDS"] <- "HIGH WIND"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HEAVY SNOW SQUALLS"] <- "HEAVY SNOW"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="COLD TEMPERATURE"] <- "COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="DRIEST MONTH"] <- "DROUGHT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="WINTER MIX"] <- "WINTER WEATHER"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="DRYNESS"] <- "DROUGHT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="UNUSUAL RECORD WARMTH"] <- "EXCESSIVE HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HAIL WIND"] <- "HAIL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HURRICANE EDOUARD"] <- "HURRICANE"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HEAVY SNOW SHOWER"] <- "HEAVY SNOW"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HEAVY PRECIPITATION"] <- "HEAVY RAIN"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RECORD WARM TEMPS"] <- "EXCESSIVE HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="VERY DRY"] <- "DROUGHT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RECORD SNOWFALL"] <- "HEAVY SNOW"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HYPERTHERMIA EXPOSURE"] <- "EXCESSIVE HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RECORD WARM"] <- "EXCESSIVE HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="BITTER WIND CHILL"] <- "EXTREME COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HAZARDOUS SURF"] <- "HIGH SURF"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="DUST DEVEL"] <- "DUST DEVIL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RED FLAG FIRE WX"] <- "WILDFIRE"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="FLASH FLOOD FLOOD"] <- "FLASH FLOOD"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="FLASH FLOODING"] <- "FLASH FLOOD"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HOT SPELL"] <- "HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="SNOW BLOWING SNOW"] <- "HEAVY SNOW"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="BLOWING SNOW"] <- "HEAVY SNOW"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="SNOW AND ICE"] <- "HEAVY SNOW"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="URBAN STREET FLOODING"] <- "FLOOD"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="SNOWMELT FLOODING"] <- "FLOOD"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HEAVY SURF AND WIND"] <- "HIGH SURF"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="STRONG WIND GUST"] <- "STRONG WIND"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="EARLY FROST"] <- "FROST FREEZE"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RECORDCOLD"] <- "EXTREME COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="WIND GUSTS"] <- "STRONG WIND"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="FLOOD FLASH FLOOD"] <- "FLASH FLOOD"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HEAVY RAINFALL"] <- "HEAVY RAIN"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="HOT AND DRY"] <- "HEAT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="ICE FOG"] <- "FREEZING FOG"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="ICE SNOW"] <- "HEAVY SNOW"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="EROSION COASTAL FLOOD"] <- "COASTAL FLOOD"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="EXTREMELY WET"] <- "HEAVY RAIN"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="EXTENDED COLD"] <- "COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="UNSEASONABLE COLD"] <- "EXTREME COLD WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="URBAN FLOODING"] <- "FLOOD"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RECORD MAY SNOW"] <- "HEAVY SNOW"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RECORD DRYNESS"] <- "DROUGHT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="EXCESSIVELY DRY"] <- "DROUGHT"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="GUSTY WIND HAIL"] <- "HAIL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RAIN DAMAGE"] <- "HEAVY RAIN"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="RAIN HEAVY"] <- "HEAVY RAIN"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="COLD"] <- "COLD/WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="WIND CHILL"] <- "COLD/WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="COLD WIND CHILL"] <- "COLD/WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="FREEZE"] <- "FROST/FREEZE"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="FROST"] <- "FROST/FREEZE"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="FROST FREEZE"] <- "FROST/FREEZE"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="EXTREME COLD WIND CHILL"] <- "EXTREME COLD/WIND CHILL"
levels(stormDataNew$EVTYPE)[levels(stormDataNew$EVTYPE)=="STORM SURGE TIDE"] <- "STORM SURGE/TIDE"

#summary(stormDataNew$EVTYPE)

### Step 2: Calculate the damage to population ((Injuries + Fatalities)) and the economy from each event (Property damage + Crop damage)
#install.packages('sqldf')
#install.packages('ggplot2')
library(sqldf)
library(ggplot2)
library(tcltk)

# Check the units of Property damage and Crop damage to calculate total damage expenses
unique(stormDataNew$PROPDMGEXP)
#[1] K   M B 0
#Levels:  0 B K M
unique(stormDataNew$CROPDMGEXP)
#[1] K   M B
#Levels:  B K M

# Replace the null, K, M, and B with numerical values for multiplication
levels(stormDataNew$PROPDMGEXP)[levels(stormDataNew$PROPDMGEXP)==""] <- "1"
levels(stormDataNew$PROPDMGEXP)[levels(stormDataNew$PROPDMGEXP)=="K"] <- "1000"
levels(stormDataNew$PROPDMGEXP)[levels(stormDataNew$PROPDMGEXP)=="M"] <- "1000000"
levels(stormDataNew$PROPDMGEXP)[levels(stormDataNew$PROPDMGEXP)=="B"] <- "1000000000"
stormDataNew$PROPDMGEXP <- as.numeric(as.character(stormDataNew$PROPDMGEXP))

levels(stormDataNew$CROPDMGEXP)[levels(stormDataNew$CROPDMGEXP)==""] <- "1"
levels(stormDataNew$CROPDMGEXP)[levels(stormDataNew$CROPDMGEXP)=="K"] <- "1000"
levels(stormDataNew$CROPDMGEXP)[levels(stormDataNew$CROPDMGEXP)=="M"] <- "1000000"
levels(stormDataNew$CROPDMGEXP)[levels(stormDataNew$CROPDMGEXP)=="B"] <- "1000000000"
stormDataNew$CROPDMGEXP <- as.numeric(as.character(stormDataNew$CROPDMGEXP))

# Convert Property damage and Crop damage into their full amount
stormDataNew$PROPDMGFULL <- stormDataNew$PROPDMG * stormDataNew$PROPDMGEXP 
stormDataNew$CROPDMGFULL <- stormDataNew$CROPDMG * stormDataNew$CROPDMGEXP 

sortStormData <- sqldf('select EVTYPE, sum(FATALITIES) fatalities, sum(INJURIES) injuries, sum(FATALITIES+INJURIES) population_harm,
                sum(PROPDMGFULL) property_damage, sum(CROPDMGFULL) crops_damage, sum(PROPDMGFULL+CROPDMGFULL) economic_cost
                from stormDataNew
                group by EVTYPE order by EVTYPE')
				
sortStates <- sqldf('select STATE, sum(FATALITIES) fatalities, sum(INJURIES) injuries, sum(FATALITIES+INJURIES) population_harm
                from stormDataNew
                group by STATE order by STATE')
				
# Select top 10 population harm and create a data frame for the plot  
popnHarm <- sqldf('select EVTYPE, fatalities, injuries, population_harm
                from sortStormData
                where population_harm > 0
                order by population_harm desc')

topHumDmg <- popnHarm[1:10,]
tolhumDmg <- sqldf('select EVTYPE evtype, population_harm, "Total" rtype from topHumDmg')
tolhumDmg <- rbind(tolhumDmg, sqldf('select EVTYPE evtype, fatalities population_harm, "Fatality" rtype from topHumDmg'))
tolhumDmg <- rbind(tolhumDmg, sqldf('select EVTYPE evtype, injuries population_harm, "Injury" rtype from topHumDmg'))

# Select top 10 economic cost and create a data frame for the plot  
econCost <- sqldf('select EVTYPE, property_damage, crops_damage, economic_cost
                from sortStormData
                where economic_cost > 0
                order by economic_cost desc')

topEconDmg <- econCost[1:10,]
toleconDmg <- sqldf('select EVTYPE evtype, economic_cost, "Total" rtype from topEconDmg')
toleconDmg <- rbind(toleconDmg, sqldf('select EVTYPE evtype, property_damage economic_cost, "Property Damage" rtype from topEconDmg'))
toleconDmg <- rbind(toleconDmg, sqldf('select EVTYPE evtype, crops_damage economic_cost, "Crop Damage" rtype from topEconDmg'))

# Select top 10 states that suffered the most population harm
statesHarm <- sqldf('select STATE, fatalities, injuries, population_harm
                from sortStates
                where population_harm > 0
                order by population_harm desc')

topSTEDmg <- statesHarm[1:10,]
tolsteDmg <- sqldf('select STATE state, population_harm, "Total" rtype from topSTEDmg')
tolsteDmg <- rbind(tolsteDmg, sqldf('select STATE state, fatalities population_harm, "Fatality" rtype from topSTEDmg'))
tolsteDmg <- rbind(tolsteDmg, sqldf('select STATE state, injuries population_harm, "Injury" rtype from topSTEDmg'))

### Step 3: Plot the top 10 population harm and economic cost by events, 
# and the top 10 states that suffered the most population damage
# The 10 events which are most harmful to population health are:  
topHumDmg

#              EVTYPE fatalities injuries population_harm
#1            TORNADO       1511    20667           22178
#2     EXCESSIVE HEAT       1800     6461            8261
#3              FLOOD        444     6838            7282
#4  THUNDERSTORM WIND        378     5128            5506
#5          LIGHTNING        650     4140            4790
#6        FLASH FLOOD        887     1674            2561
#7           WILDFIRE         87     1458            1545
#8       WINTER STORM        191     1292            1483
#9               HEAT        237     1241            1478
#10         HURRICANE        125     1328            1453

png("/populationharm.png", height=700, width=1050)
ggplot(data=tolhumDmg, aes(x=evtype, y=population_harm, fill=factor(rtype))) + 
    ggtitle('Top 10 Population Harm from Storm Events (1996-2011)') +
    geom_bar(position="dodge", stat="identity") + coord_flip() +
    scale_x_discrete(limits=rev(topHumDmg$EVTYPE), name="Event") +
    scale_y_continuous(name="Total Human Damages (per person)") + 
    theme(legend.title=element_blank(), legend.position=c(.75,.7))
dev.off()

# The 10 events which are most harmful to the economy are:  
topEconDmg

#              EVTYPE property_damage crops_damage economic_cost
#1              FLOOD    144129580200   5013161500  149142741700
#2          HURRICANE     81718889010   5350107800   87068996810
#3   STORM SURGE/TIDE     47834724000       855000   47835579000
#4            TORNADO     24616905710    283425010   24900330720
#5               HAIL     14595163420   2476029450   17071192870
#6        FLASH FLOOD     15222268910   1334901700   16557170610
#7            DROUGHT      1046101000  13367566000   14413667000
#8  THUNDERSTORM WIND      7913455880   1016942600    8930398480
#9     TROPICAL STORM      7642475550    677711000    8320186550
#10          WILDFIRE      7760449500    402255130    8162704630

png("/economiccost.png", height=700, width=1050)
ggplot(data=toleconDmg, aes(x=evtype, y=economic_cost, fill=factor(rtype))) + 
    ggtitle('Top 10 Economic Damages from Storm Events (1996-2011)') +
    geom_bar(position="dodge", stat="identity") + coord_flip() +
    scale_x_discrete(limits=rev(topEconDmg$EVTYPE), name="Event") +
    scale_y_continuous(name="Total Economic Cost ($)") +
    theme(legend.title=element_blank(), legend.position=c(.75,.7))
dev.off()

# The top 10 states that have the most injuries and fatalities are:
topSTEDmg

#   STATE fatalities injuries population_harm
#1     TX        756     9222            9978
#2     MO        533     5960            6493
#3     AL        449     3707            4156
#4     FL        544     2884            3428
#5     CA        498     2769            3267
#6     TN        327     2385            2712
#7     OK        219     2375            2594
#8     PA        492     1450            1942
#9     IL        586     1328            1914
#10    AR        228     1656            1884

png("/statedamage.png", height=700, width=1050)
ggplot(data=tolsteDmg, aes(x=state, y=population_harm, fill=factor(rtype))) + 
    ggtitle('Top 10 States Suffering Population Damages from Storm Events (1996-2011)') +
    geom_bar(position="dodge", stat="identity") + coord_flip() +
    scale_x_discrete(limits=rev(topSTEDmg$STATE), name="STATE") +
    scale_y_continuous(name="Total Human Damages (per person)") +
    theme(legend.title=element_blank(), legend.position=c(.75,.7))
dev.off()
