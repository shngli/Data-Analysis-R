####################
# Data Summarization
####################

## Part A

# Bike Lanes Dataset: BikeBaltimore is the Department of Transportation's bike program.
# https://data.baltimorecity.gov/Transportation/Bike-Lanes/xzfj-gyms
# Download as a CSV (like the Monuments dataset) in your current working directory

bike = read.csv(file = "/Desktop/Bike_Lanes.csv",as.is=TRUE,na.strings=" ")

# 1. Using tapply():
# 	(a) Which project category has the longest average bike lane?
tab=tapply(bike$length,bike$project, mean,na.rm=TRUE)
tab[which.max(tab)]
#MAINTENANCE 
#   1942.152 

#	(b) What was the average bike lane length per year that they were installed?
tapply(bike$length,bike$dateInstalled,mean,na.rm=TRUE)
#        0      2006      2007      2008      2009      2010      2011      2012      2013 
# 214.9701 1469.3469  309.8903  249.4810  407.2947  245.6016  232.8128  271.4322  290.4264 

# 2. (a) Numerically [hint: `quantile()`] and (b) graphically [hint: `hist()` or `plot(density())`]
#	describe the distribution of bike "lane" lengths.
hist(bike$length)
hist(bike$length,breaks=100)

quantile(bike$length)
#       0%       25%       50%       75%      100% 
#   0.0000  124.0407  200.3027  341.0224 3749.3226 

# 3. Then describe as above, after stratifying by i) type then ii) number of lanes
boxplot(bike$length~bike$type)
levels(factor(bike$type)) # this is the order of boxes
#[1] "BIKE BOULEVARD"  "BIKE LANE"       "CONTRAFLOW"      "SHARED BUS BIKE" "SHARROW"        
#[6] "SIDEPATH"        "SIGNED ROUTE"   
boxplot(bike$length~bike$numLanes)

tapply(bike$length,bike$type, quantile,na.rm=TRUE)
tapply(bike$length,bike$numLanes, quantile,na.rm=TRUE)

## Part B

# Download the CSV: http://biostat.jhsph.edu/~ajaffe/files/indicatordeadkids35.csv
# Via: http://www.gapminder.org/data/
# Definition of indicator: How many children the average couple had that die before the age 35.

death = read.csv("http://biostat.jhsph.edu/~ajaffe/files/indicatordeadkids35.csv",
                  as.is=TRUE,row.names=1)

# 4. Plot the distribution of average country's count across all year.
rowMeans(death,na.rm=TRUE)
hist(rowMeans(death,na.rm=TRUE))

# 5.(a) How many entries are less than 1?
death < 1
sum(death < 1,na.rm=TRUE)
#[1] 8900
mean(death < 1,na.rm=TRUE)
#[1] 0.2661722

#	(b) Which array indices do they correspond to? [hint: `arr.ind` argument in `which()`]
head(which(death<1,arr.ind=FALSE))
#[1] 26851 27048 27157 27270 27354 27467
head(which(death<1,arr.ind=TRUE))
#        row col
#France   59 137
#France   59 138
#Sweden  168 138
#Ireland  84 139
#Sweden  168 139
#Ireland  84 140
ind =which(death<1,arr.ind=TRUE) 

# 6. Plot the count for each country across year in a line plot [hint: `matplot()`]
matplot(death,type="l")
