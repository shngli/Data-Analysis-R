####################
# Data Manipulation
####################

## Part A

# Bike Lanes Dataset: BikeBaltimore is the Department of Transportation's bike program. 
# https://data.baltimorecity.gov/Transportation/Bike-Lanes/xzfj-gyms
# 	Download as a CSV (like the Monuments dataset) in your current working directory


bike = read.csv(file = "/Desktop/Bike_Lanes.csv",as.is=TRUE,na.strings=" ")

# 1. How many bike "lanes" are currently in Baltimore?
nrow(bike)
#[1] 1631

# 2. How many (a) feet and (b) miles of bike "lanes" are currently in Baltimore?
sum(bike$length)
#[1] 439447.6
sum(bike$length)/5280
#[1] 83.22871
sum(bike$length/5280)
#[1] 83.22871

# 3. How many types of bike lanes are there? 
# Which type has (a) the most number of and (b) longest average bike lane length?
length(unique(bike$type))
#[1] 8
tab=table(bike$type)
tab[which.max(tab)]
#BIKE LANE 
#      621 

tab = tapply(bike$length, bike$type ,mean)
tab[which.max(tab)]
#SIDEPATH 
#665.8775 

# 4. How many different projects do the "bike" lanes fall into? 
# Which project category has the longest average bike lane? 
length(unique(bike$project))
#[1] 13
tab = tapply(bike$length,bike$project,mean,na.rm=TRUE)
tab[which.max(tab)]
#MAINTENANCE 
#   1942.152

## Part B

# Download the CSV: http://biostat.jhsph.edu/~ajaffe/files/indicatordeadkids35.csv
# Via: http://www.gapminder.org/data/
# Definition of indicator: How many children the average couple had that die before the age 35.

death = read.csv("http://biostat.jhsph.edu/~ajaffe/files/indicatordeadkids35.csv",
                  as.is=TRUE,row.names=1)
death2 = read.csv("http://biostat.jhsph.edu/~ajaffe/files/indicatordeadkids35.csv",
                 as.is=TRUE)
rownames(death2) = death2$X
death2$X=NULL
rownames(death2)

# 5. How many countries have data in any year?
dim(death)
#[1] 197 254
!is.na(death)[1:5,1:5]
#            X1760 X1761 X1762 X1763 X1764
#Afghanistan FALSE FALSE FALSE FALSE FALSE
#Albania     FALSE FALSE FALSE FALSE FALSE
#Algeria     FALSE FALSE FALSE FALSE FALSE
#Angola      FALSE FALSE FALSE FALSE FALSE
#Argentina   FALSE FALSE FALSE FALSE FALSE
table(rowSums(!is.na(death)))
# 60  64 131 208 214 254
#  3  55   1   1 135   2

# 6. When did measurements in the US start?
death["United States",]
# death[death$X=="United States",]
!is.na(death["United States",])
allIndex=  which(!is.na(death["United States",]))
allIndex[1]
#[1] 41
i= which(!is.na(death["United States",]))[1]
colnames(death)[i]
#[1] "X1800"

## one line version
colnames(death)[which(!is.na(death["United States",]))[1]]
#[1] "X1800"

# 7. How many countries, and which, had data the first year of measuring?
!is.na(death[,1])
which(!is.na(death[,1]))
#[1] 168 185
rownames(death)[which(!is.na(death[,1]))]
#[1] "Sweden"         "United Kingdom"
