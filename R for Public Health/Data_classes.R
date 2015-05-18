####################
# Data Classes
####################

## In this lab you can use the interactive console to explore but please record your commands here.  
# Remember anything you type here can be "sent" to the console with Cmd-Enter (OS-X) or Cntr-Enter (Windows/Linux).

# 1. Load in the `CO2` dataset (which is included like the `iris` dataset
data(CO2)

# 2. What class is `CO2`?
class(CO2)
#[1] "nfnGroupedData" "nfGroupedData"  "groupedData"    "data.frame" 

# 3. How many observations (rows) and variables (columns) are in the `CO2` dataset?
dim(CO2)
#[1] 84  5 #rows and column
nrow(CO2)
#[1] 84 #only rows
ncol(CO2)
#[1] 5 #only columns

# 4. How many different "plants" are in the data? (hint: `length` and `unique`)
unique(CO2$Plant)
# [1] Qn1 Qn2 Qn3 Qc1 Qc2 Qc3 Mn1 Mn2 Mn3 Mc1 Mc2 Mc3
#12 Levels: Qn1 < Qn2 < Qn3 < Qc1 < Qc3 < Qc2 < Mn3 < Mn2 < Mn1 < Mc2 < Mc3 < Mc1
length(unique(CO2$Plant))
#[1] 12 #12 unique plants
levels(CO2$Plant)
# [1] "Qn1" "Qn2" "Qn3" "Qc1" "Qc3" "Qc2" "Mn3" "Mn2" "Mn1" "Mc2"
#[11] "Mc3" "Mc1"   #listing out the plants
length(levels(CO2$Plant))
#[1] 12
length(table(CO2$Plant))
#[1] 12

# 5. How many different "types" are in the data?
length(unique(CO2$Type))
#[1] 2  #2 unique types

# 6. Tabulate "type" and "treatment" - what are the dimensions of the resulting table?
#		hint: you can assign tables to variables
tab = table(CO2$Type, CO2$Treatment)
dim(tab)
#[1] 2 2
tab
#              nonchilled chilled
#Quebec              21      21
#Mississippi         21      21

# 7. Create a new `data.frame` named `CO2.even` that contains the even rows of `CO2`
#		hint: subsetting and `seq` (note the `by` argument)
seq(2,nrow(CO2),2)
# [1]  2  4  6  8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40
#[21] 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80
#[41] 82 84
CO2.even = CO2[seq(2,nrow(CO2),by=2),]

# 8. How many observations are in `CO2.even`?
dim(CO2.even)
#[1] 42  5

# 9. What are the sums of the a) concentrations and b) uptake values in the `CO2.even` dataset?
#		And what are their means?
sum(CO2.even$conc)
#[1] 18270
sum(CO2.even$uptake)
#[1] 1151.5
mean(CO2.even$conc)
#[1] 435
mean(CO2.even$uptake)
#[1] 27.41667