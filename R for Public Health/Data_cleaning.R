#################
# Data Cleaning and Plotting
##############

## Download the "Real Property Taxes" Data from OpenBaltimore:
## https://data.baltimorecity.gov/Property/Real-Property-Taxes/27w9-urtv
## as a .csv file


tax = read.csv(file = "/Desktop/Real_Property_Taxes.csv",as.is=TRUE,na.strings=" ")

# 1. Read the Property Tax data into R and call it the variable `tax`
tax = read.csv("Real_Property_Taxes.csv", as.is=TRUE)
tax = read.csv(file = "/Desktop/Real_Property_Taxes.csv",as.is=TRUE,na.strings=" ")


table(tax$resCode)
#NOT A PRINCIPAL RESIDENCE PRINCIPAL RESIDENCE       
#                   155652                    178440 
library(stringr)
tax$resCode = str_trim(tax$resCode)
table(tax$resCode)

# 2. How many addresses pay property taxes? 
dim(tax) # ? all homes in dataset
#[1] 334092     11
length(unique(tax$propertyAddress)) # those with unique address
#[1] 233955
length(unique(tax$propertyAddress[tax$cityTax != "$0.00"]))
#[1] 215516
# and those that have non-zero city
length(unique(tax$propertyAddress[tax$cityTax != "$0.00" & tax$stateTax != "$0.00"]))
#[1] 215516

# 3. What is the total city and state tax charged?
tax$cityTax = as.numeric(gsub("$","",tax$cityTax,fixed=TRUE))
tax$stateTax = as.numeric(gsub("$","",tax$stateTax,fixed=TRUE))
tax$amountDue = as.numeric(gsub("$","",tax$amountDue,fixed=TRUE))

colSums(tax[,c("cityTax","stateTax")])
#   cityTax   stateTax 
#1113077835   55745450 
sum(tax$cityTax)
#[1] 1113077835
sum(tax$stateTax)
#[1] 55745450
sum(colSums(tax[,c("cityTax","stateTax")]))
#[1] 1168823285
sum(tax$cityTax) + sum(tax$stateTax)
#[1] 1168823285
sum(tax$cityTax, tax$stateTax)
#[1] 1168823285

# 4. What is the 75th percentile of city and state tax paid by ward?
tapply(tax$cityTax, tax$ward, quantile, prob=0.75, na.rm=TRUE)
#       1        2        3        4        5        6        7        8        9 
#5703.453 5798.530 5443.200 6978.635 2732.550 3855.600  567.000 1573.600 2619.540 
#      10       11       12       13       14       15       16       17       18 
#1701.000 5783.400 4846.720 3597.050 2268.000 2431.300 1591.765 1816.670 1592.140 
#      19       20       21       22       23       24       25       26       27 
#1360.800 1798.520 3098.090 8498.185 6031.750 6594.775 2823.490 3075.410 3839.580 
#      28 
#3395.200 
tapply(tax$stateTax, tax$ward, quantile, prob=0.75, na.rm=TRUE)
#       1        2        3        4        5        6        7        8        9 
#281.6525 286.3500 268.8000 349.2425 134.9400 190.4000  28.0000  78.2900 129.3600 
#      10       11       12       13       14       15       16       17       18 
# 84.0000 285.6000 239.3400 177.6300 112.0000 120.0600  78.6200  89.7100  78.6200 
#      19       20       21       22       23       24       25       26       27 
# 67.2000  88.8200 153.2150 420.4750 298.4800 325.9950 140.0000 152.4300 190.4000 
#      28 
#168.3400 


# tapply(tax$cityTax, tax$ward, function(x) {
#   x = log(x + 500)
#   quantile(x, prob=0.75, na.rm=TRUE)
# })

# 5. Using `tapply()` and `table()`
#	a. how many observations are in each ward?
table(tax$ward)
#    1     2     3     4     5     6     7     8     9    10    11    12    13    14 
# 6618  3521  2484  1770   473  4382  5184 12648 11886  1705  3608  8979  9342  3108 
#   15    16    17    18    19    20    21    22    23    24    25    26    27    28 
#18718 11323  1453  2432  3846 11933  3219  1718  2576  6106 29046 48957 98162 18895 


#	b. what is the mean state tax per ward
tapply(tax$stateTax, tax$ward, mean)
#         1          2          3          4          5          6          7 
# 252.31866  268.09962  579.11967 1457.14076  234.17262  142.04565   53.24466 
#         8          9         10         11         12         13         14 
#  47.06865   79.18311   48.57355  260.23934  203.78882  163.08620  109.77235 
#        15         16         17         18         19         20         21 
#  81.94086   49.20244   72.47593   90.51167   54.56079   55.53162  180.35036 
#        22         23         24         25         26         27         28 
# 649.79503  248.67583  322.75092  146.19090  175.33461  186.44166  165.29778 

#	c. what is the maximum amount still due?
tapply(tax$amountDue, tax$ward, max)
#        1         2         3         4         5         6         7         8 
#  6951.93  14451.06  75157.19 108960.17  14743.73   5059.45  32484.58  10827.68 
#        9        10        11        12        13        14        15        16 
#  9854.02   8005.56  12772.90   8266.89   8062.78  14125.91  22222.81   8436.94 
#       17        18        19        20        21        22        23        24 
#342928.51  16109.32   8111.97  11559.39  15489.60   3022.20  29809.50   4944.63 
#       25        26        27        28 
#303239.99  40952.75 408130.30  52530.90


# 6. Make boxplots using a) default and b) ggplot2 graphics showing cityTax 
#	 	by whether the property	is a principal residence or not.

boxplot(cityTax ~ resCode, data=tax,ylab="city tax")
ct10 = log10(tax$cityTax+1) # try log10
boxplot(ct10 ~ tax$resCode,ylab="log10(city tax+1)")
boxplot(ct10 ~ tax$resCode,ylab="log10(city tax+1)",
        yaxt="n")
axis(2, at = 0:6, labels = paste0("$",10^(0:6)))

## ggplot2
library(ggplot2)
qplot(factor(resCode), cityTax, data = tax, geom = "boxplot")
qplot(factor(resCode), log10(cityTax+1),data = tax, geom = "boxplot")


# 7. Subset the data to only retain those houses that are principal residences. 
#	a) How many such houses are there?
tax2 = tax[tax$resCode=="PRINCIPAL RESIDENCE",]

#	b) Describe the distribution of property taxes on these residences.
hist(log10(tax2$cityTax+1))
quantile(tax2$cityTax)
#       0%       25%       50%       75%      100% 
#     0.00   2214.28   2881.94   3780.76 283500.00 
hist(log10(tax2$stateTax+1))
quantile(tax2$stateTax)
#      0%      25%      50%      75%     100% 
#    0.00   109.76   142.80   187.26 14000.00 

# 8. Convert the 'lotSize' variable to a numeric square feet variable. 
#	Tips: - Assume hyphens represent decimal places within measurements. 
#		  - 1 acre = 43560 square feet
# 		  - Don't spend more than 5-10 minutes on this; stop and move on
tax2$lotSize = str_trim(tax2$lotSize) # trim white space
lot = tax2$lotSize # i want to check later

# first lets take care of acres
aIndex= grep("ACRE.*", tax2$lotSize)
acre = tax2$lotSize[aIndex]

acre = gsub(" ACRE.*","",acre)
acre[is.na(as.numeric(acre))]

acre = gsub("-",".",acre,fixed=TRUE)
acre[is.na(as.numeric(acre))]

acre = gsub("ACRES","", acre, fixed=TRUE)
acre[is.na(as.numeric(acre))]

acre = gsub("O","0", acre, fixed=TRUE)
acre = gsub("Q","", acre, fixed=TRUE)
acre2 = as.numeric(acre)*43560

### now feet
fIndex = grep("X", tax2$lotSize)
ft = tax2$lotSize[fIndex]
ft = gsub("-",".",ft,fixed=TRUE)
width = as.numeric(sapply(strsplit(ft,"X"),"[", 1))
width = as.numeric(sapply(strsplit(ft,"X"),function(x) x[1]))

length = as.numeric(sapply(strsplit(ft,"X"),"[", 2))
sqft = width*length

# now add column
tax2$sqft = rep(NA)
tax2$sqft[aIndex] = acre2
tax2$sqft[fIndex] = sqft
mean(!is.na(tax2$sqft))
#[1] 0.9490697

# some are already sq ft
sIndex=c(grep("FT", tax2$lotSize), grep("S.*F.", tax2$lotSize))
sf = tax2$lotSize[sIndex]
sqft2 = as.numeric(sapply(strsplit(sf," "),function(x) x[1]))
tax2$sqft[sIndex] = sqft2
table(is.na(tax2$sqft)) # 62 (I don't understand the 62, I think it should be 82)
# FALSE   TRUE 
#178358     82 

## progress!
lot[is.na(tax2$sqft)]


# 9.a) Plot your numeric lotSize versus cityTax on principal residences. 
#	b) How many values of lot size were missing?

plot(log10(tax2$cityTax+1), log10(tax2$sqft))

#  b) How many values of lot size were missing?
sum(is.na(tax2$sqft)) # 62!!
#[1] 82


################################
## Read in the Salary FY2012 dataset

# 10. Make an object called health.sal using the salaries data set, 
#		with only agencies of those with "fire" (or any forms), if any, in the name


Sal = read.csv(file = "/Desktop/Baltimore_City_Employee_Salaries_FY2012.csv",
               as.is=TRUE)
Sal$AnnualSalary = as.numeric(gsub("$","",Sal$AnnualSalary,fixed=TRUE))
Sal$GrossPay = as.numeric(gsub("$","",Sal$GrossPay, fixed=TRUE))

health.sal = Sal[grep("fire", Sal$Agency, ignore.case=TRUE),]

# 11. Make a data set called trans which contains only agencies that contain "TRANS".
trans = Sal[grep("trans", Sal$Agency,ignore.case=TRUE),]

# 12. What is/are the profession(s) of people who have "abra" in their name for Baltimore's Salaries?
table(Sal$JobTitle[grep("abra", Sal$name)])
#POLICE OFFICER 
             1 

# 13. What is the distribution of annual salaries look like? What is the IQR?
hist(Sal$AnnualSalary)
quantile(Sal$AnnualSalary)
#    0%    25%    50%    75%   100% 
#   377  28668  42853  59914 238772 

# 14. Convert HireDate to the `Date` class - plot Annual Salary vs Hire Date
Sal$HireDate = as.Date(Sal$HireDate, "%m/%d/%Y")

# 15. Plot annual salary versus hire date. 
#		Hint: first convert to numeric and date respectively
plot(AnnualSalary ~ HireDate, data=Sal)
plot(Sal$HireDate, Sal$AnnualSalary)

# 16. Create a smaller dataset that only includes the
# 	Police Department,  Fire Department and Sheriff's Office.
Sal$Agency = str_trim(Sal$Agency)

Sal2 = Sal[Sal$Agency %in% c("Police Department",
                            "Fire Department", 
                             "Sheriff's Office"),]

#  a. How many employees are in this new dataset?
dim(Sal2)
#[1] 5387    7
nrow(Sal2)
#[1] 5387

# 17. Replot annual salary versus hire date, color by Agency using
#	i) regular plotting and ii) ggplot2
plot(AnnualSalary ~ HireDate, data=Sal2, 
      col = as.numeric(factor(Agency)))
legend("topleft", levels(factor(Sal2$Agency)),
       col = 1:3, pch = 15)

yl = quantile(Sal2$AnnualSalary, c(0,0.995))

qplot(x=HireDate, y=AnnualSalary, data=Sal2, geom=c("point", "smooth"), 
        color=Agency, ylim = yl)

ggplot(Sal2, aes(x=HireDate, y=AnnualSalary, color=Agency)) +
  geom_point(shape=19) +  ylim(yl) +
  geom_smooth(method=loess, se=TRUE, fullrange=TRUE,size=3,n=150)
