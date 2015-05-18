##############
## Data Input:
## Exploring 'Monument' Dataset
##############

# 1. Change the name of the "Location.1" column to "location"
mon = read.csv("/Desktop/Monuments.csv", header = TRUE, as.is = TRUE)
names(mon)
#[1] "name"            "zipCode"         "neighborhood"    "councilDistrict"
#[5] "policeDistrict"  "Location.1" 

names(mon)[6] = "location"
names(mon)
#[1] "name"            "zipCode"         "neighborhood"    "councilDistrict"
#[5] "policeDistrict"  "location"


# 2. How many monuments are in Baltimore (at least this collection...)?
nrow(mon)
#[1] 84


# 3. What are the (a) zip codes, (b) neighborhoods, (c) council districts,
#	and (d) police districts that contain monuments, and how many monuments are in each?

#a) zipcodes
table(mon$zipCode)
#21201 21202 21211 21213 21214 21217 21218 21223 21224 21225 21230 21231 21251 
#   11    16     8     4     1     9    14     4     8     1     3     4     1 

length(table(mon$zipCode))
#[1] 13

#b) neighborhoods
table(mon$neighborhood)
#                       Brooklyn                          Canton 
#                              1                               2 
#               Carrollton Ridge                    Clifton Park 
#                              2                               4 
#Coldstream Homestead Montebello                        Downtown 
#                              1                              11 
#                Druid Hill Park                 Dunbar-Broadway 
#                              4                               1 
#         Ednor Gardens-Lakeside                    Federal Hill 
#                              1                               1 
#                    Fells Point                        Guilford 
#                              1                               1 
#                    Harlem Park                Herring Run Park 
#                              1                               1 
#                Hopkins Bayview                    Inner Harbor 
#                              1                               4 
#         Johns Hopkins Homewood    Locust Point Industrial Area 
#                             17                               2 
#                   Madison Park                  McElderry Park 
#                              1                               1 
#             Mid-Town Belvedere    Middle Branch/Reedbird Parks 
#                              3                               1 
#        Morgan State University                    Mount Vernon 
#                              1                               7 
#      New Southwest/Mount Clare                  Patterson Park 
#                              1                               4 
#                      Remington                  Reservoir Hill 
#                              2                               1 
#                   Stadium Area                    Union Square 
#                              1                               1 
#                          Upton                 Washington Hill 
#                              2                               2 

length(table(mon$neighborhood))
#[1] 32

#c) council districts
table(mon$councilDistrict)
# 1  2  3  7  9 10 11 12 13 14 
#11  1  2  5  4  4 29  2  1 25 

length(table(mon$councilDistrict))
#[1] 10

#d) police districts
table(mon$policeDistrict)
#     CENTRAL      EASTERN NORTHEASTERN     NORTHERN SOUTHEASTERN     SOUTHERN 
#          27            1           10           20           14           11 
#     WESTERN 
#           1 

length(table(mon$policeDistrict))
#[1] 7

# 4. How many zip codes are in the (a) "Downtown" and (b) "Johns Hopkins Homewood" neighborhoods?
tab = table(mon$zipCode, mon$neighborhood)

#a) "Downtown"
tab[, "Downtown"]
#21201 21202 21211 21213 21214 21217 21218 21223 21224 21225 21230 21231 21251 
#    2     9     0     0     0     0     0     0     0     0     0     0     0
tab[, "Downtown"] != 0
#21201 21202 21211 21213 21214 21217 21218 21223 21224 21225 21230 21231 21251 
# TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
sum(tab[, "Downtown"] != 0)
#[1] 2

#b) "Johns Hopkins Homewood"
tab[, "Johns Hopkins Homewood"]
# 21202 21211 21213 21214 21217 21218 21223 21224 21225 21230 21231 21251 
#    0     0     6     0     0     0    11     0     0     0     0     0     0 
sum(tab[, "Johns Hopkins Homewood"] != 0)
#[1] 2


# 5. How many monuments (a) do and (b) do not have an exact location/address?
head(mon$location)
#[1] "408 CHARLES ST\nBaltimore, MD\n"  ""                                
#[3] ""                                 "100 HOLLIDAY ST\nBaltimore, MD\n"
#[5] "50 MARKET PL\nBaltimore, MD\n"    "100 CALVERT ST\nBaltimore, MD\n" 

table(mon$location != "") #TRUE = DO and FALSE = DO NOT
#FALSE  TRUE 
#   26    58

# 6. Which (a) zip code, (b) neighborhood, (c) council district,
#	and (d) police district contains the most number of monuments?

#a) zipcode
tabZ = table(mon$zipCode)
head(tabZ)
#21201 21202 21211 21213 21214 21217 
#   11    16     8     4     1     9
tabZ[which.max(tabZ)]
#21202 
#   16 

#b) neighborhood
tabN = table(mon$neighborhood)
head(tabN)
#                       Brooklyn                          Canton 
#                              1                               2 
#               Carrollton Ridge                    Clifton Park 
#                              2                               4 
#Coldstream Homestead Montebello                        Downtown 
#                              1                              11 
tabN[which.max(tabN)]
#Johns Hopkins Homewood 
#                    17

#c) council district
tabC = table(mon$councilDistrict)
head(tabC)
# 1  2  3  7  9 10 
#11  1  2  5  4  4 
tabC[which.max(tabC)]
#11 
#29 

#d) police district
tabP = table(mon$policeDistrict)
head(tabP)
#     CENTRAL      EASTERN NORTHEASTERN     NORTHERN SOUTHEASTERN     SOUTHERN 
#          27            1           10           20           14           11 
tabP[which.max(tabP)]
#CENTRAL 
#     27

# 7. Try reading in the tab-delimited `Monuments-tab.txt` file from:
# 		"http://biostat.jhsph.edu/~ajaffe/summerR/data/Monuments-tab.txt"
monTab = read.delim("http://biostat.jhsph.edu/~ajaffe/summerR/data/Monuments-tab.txt", header = TRUE, as.is = TRUE)
identical(mon$name, monTab$name)
#[1] TRUE
