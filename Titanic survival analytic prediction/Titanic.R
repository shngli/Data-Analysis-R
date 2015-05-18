# Author: Chisheng Li
# Analysis of RMS Titanic data for 1309 passengers and crews

## Load Data
titanic <- read.csv("/Titanic_data.csv",head=TRUE,sep=",")
# Pre-transformed dataset
str(titanic)
#'data.frame':	1309 obs. of  13 variables:
# $ Life_boat: Factor w/ 21 levels "","1","10","11",..: 1 1 1 1 1 20 1 7 6 20 ...
# $ survived : int  0 0 0 0 0 1 0 1 1 1 ...
# $ pclass   : int  1 1 1 1 3 3 3 3 3 3 ...
# $ name     : Factor w/ 1307 levels "Abbing, Mr. Anthony",..: 547 871 618 203 729 123 724 684 400 679 ...
# $ sex      : Factor w/ 2 levels "female","male": 2 2 2 2 2 2 2 2 2 2 ...
# $ age      : num  55 64 46 33 21 32 28 32 32 38 ...
# $ sibsp    : int  0 0 0 0 0 0 0 0 0 0 ...
# $ parch    : int  0 0 0 0 0 0 0 0 0 0 ...
# $ ticket   : Factor w/ 930 levels "110152","110413",..: 685 686 687 688 82 102 102 102 102 102 ...
# $ fare     : num  50 26 26 5 7.88 ...
# $ cabin    : Factor w/ 190 levels "","[D] O135_",..: 89 1 1 49 1 1 1 1 1 1 ...
# $ embarked : Factor w/ 4 levels "B","C","Q","S": 4 4 4 4 4 4 4 4 4 4 ...
# $ train    : int  0 1 0 1 0 1 1 1 1 1 ...

## Part 1: Output dataset using Google Vis onto web browser
#install.packages("googleVis")
library(googleVis)
titanicVis <- gvisTable(titanic, options = list(showRowNumber = T, height = 800,width=1200))
plot(titanicVis)

# View the 1st 5 rows of the dataset
titanic[1:5,]
#  Life_boat survived pclass                         name  sex age sibsp parch ticket    fare       cabin embarked train
#1                  0      1  Hipkins, Mr. William Edward male  55     0     0    680 50.0000         C39        S     0
#2                  0      1 Nicholson, Mr. Arthur Ernest male  64     0     0    693 26.0000                    S     1
#3                  0      1   Jones, Mr. Charles Cresson male  46     0     0    694 26.0000                    S     0
#4                  0      1     Carlsson, Mr. Frans Olof male  33     0     0    695  5.0000 B51 B53 B55        S     1
#5                  0      3   Lockyer, Mr. Edward Thomas male  21     0     0   1222  7.8792                    S     0


## Part 2: Data munging
# Transform passengers' name into character variables instead of factor variables
titanic$name <- as.character(titanic$name)
# Transform survived and passenger class into factor variables 
titanic$survived <- as.factor(titanic$survived)
titanic$pclass <- as.factor(titanic$pclass)

summary(titanic)
#   Life_boat   survived pclass      name               sex           age            sibsp            parch       
#        :837   0:808    1:324   Length:1309        female:466   Min.   : 0.17   Min.   :0.0000   Min.   :0.0000  
# C      : 41   1:501    2:276   Class :character   male  :843   1st Qu.:21.00   1st Qu.:0.0000   1st Qu.:0.0000  
# 13     : 36            3:709   Mode  :character                Median :28.00   Median :0.0000   Median :0.0000  
# 14     : 34                                                    Mean   :29.44   Mean   :0.4966   Mean   :0.3858  
# 15     : 34                                                    3rd Qu.:38.00   3rd Qu.:1.0000   3rd Qu.:0.0000  
# 4      : 31                                                    Max.   :74.00   Max.   :8.0000   Max.   :9.0000  
# (Other):296                                                    NA's   :2                                        
#      ticket          fare                     cabin      embarked     train       
# CA. 2343:  11   Min.   :  0.000                  :1010   B: 10    Min.   :0.0000  
# 1601    :   8   1st Qu.:  7.896   C23 C25 C27    :   6   C:267    1st Qu.:0.0000  
# CA 2144 :   8   Median : 14.454   B57 B59 B63 B66:   5   Q:123    Median :1.0000  
# 3101295 :   7   Mean   : 33.375   G6             :   5   S:909    Mean   :0.6807  
# 347077  :   7   3rd Qu.: 31.275   B96 B98        :   4            3rd Qu.:1.0000  
# 347082  :   7   Max.   :512.329   C22 C26        :   4            Max.   :1.0000  
# (Other) :1261   NA's   :1         (Other)        : 275                            

table(titanic$survived)
#  0   1 
#808 501 
table(titanic$pclass)
#  1   2   3 
#324 276 709 
table(titanic$sex)
#female   male 
#   466    843 


###############################################


# Part 3: Provide some summary of the data:
# 3a) a 3-way table for "survived", "pclass" and "sex"
titanicTab <- xtabs(~survived + pclass + sex, titanic)
ftable(titanicTab) 
#                sex female male
#survived pclass                
#0        1               5  118
#         2              12  146
#         3             110  417
#1        1             139   62
#         2              94   24
#         3             106   76

# For those who did not survive (survive=0)
titanicTab[1,,]
#      sex
#pclass female male
#     1      5  118
#     2     12  146
#     3    110  417

# For those who survived (survive=1)
titanicTab[2,,]
#      sex
#pclass female male
#     1    139   62
#     2     94   24
#     3    106   76


# 3b) 5-number-summaries and histogram plots for "age" and "fare".
fivenum(titanic$age, na.rm=T)
#[1]  0.17 21.00 28.00 38.00 74.00
fivenum(titanic$fare, na.rm=T)
#[1]   0.0000   7.8958  14.4542  31.2750 512.3292

#install.packages("ggplot2")
#install.packages("gridExtra")
library(ggplot2)
library(gridExtra)
plot1 <- qplot(titanic$age, xlab="Passenger Age (in years)")
plot2 <- qplot(titanic$fare, xlab="Passenger Fare (in pounds)")
png("/ageFare.png", height=1500, width=2400)
grid.arrange(plot1, plot2, ncol=2)
dev.off()

# 3c) frequency tables for "sibsp" and "embarked"
sibspTab <- table(titanic$sibsp)
sibspTab
#  0   1   2   3   4   5   8 
#887 324  45  18  20   6   9 
embarkedTab <- table(titanic$embarked)
embarkedTab
#  B   C   Q   S 
# 10 267 123 909


# 3d) Remember to inspect the data for some unusual patterns. 
# 3di) Can you travel with zero "fare"? 
summary(titanic$fare==0)
#   Mode   FALSE    TRUE    NA's 
#logical    1291      17       1 

# 17 passengers travelled on zero "fare"


# 3dii) Who paid the most expensive ticket? 
# The most expensive ticket cost 512.3292 pounds
expFare <- max(titanic$fare, na.rm=T)
expFare
#[1] 512.3292

# 4 passengers paid the most expensive fare
(1:1309)[!is.na(titanic$fare) & titanic$fare == expFare]
# [1] 1165 1166 1167 1168

# Names of the 4 passengers who paid the most expensive fare
a = 1;
expName <-array(0, dim=c(4,1))
for (i in 1 : 1309){
  if (!is.na(titanic$fare[i]) & titanic$fare[i] == expFare){
    expName[a] = titanic$name[i];
    a = a + 1;
  }
}
expName
#     [,1]                                                             
#[1,] "Ward, Miss. Anna"                                               
#[2,] "Cardeza, Mr. Thomas Drake Martinez"                             
#[3,] "Lesurer, Mr. Gustave J"                                         
#[4,] "Cardeza, Mrs. James Warburton Martinez (Charlotte Wardle Drake)"


# 3diii) There are several families (with more than 6 family members) on board; who are they? 
# Number of Siblings/Spouses Aboard
table(titanic$sibsp) 
#  0   1   2   3   4   5   8 
#887 324  45  18  20   6   9 

# Number of Parents/Children Aboard
table(titanic$parch) 
#   0    1    2    3    4    5    6    9 
#1000  175  108   10    6    6    2    2 

# 33 passengers have at least 6 other family members on board
length((1:1309)[titanic$sibsp + titanic$parch >= 6])
#[1] 33

# Names of the 33 passengers with at least 6 other family members on Titanic
bigFam <- array(0, dim=c(33,1))
a = 1;
for (i in 1 : 1309){
  if (titanic$sibsp[i] + titanic$parch[i] > 5){
    bigFam[a] = titanic$name[i];
    a = a + 1;
  }
}
bigFam
#      [,1]                                                       
# [1,] "Asplund, Mrs. Carl Oscar (Selma Augusta Emilia Johansson)"
# [2,] "Asplund, Master. Clarence Gustaf Hugo"                    
# [3,] "Asplund, Miss. Lillian Gertrud"                           
# [4,] "Asplund, Master. Edvin Rojj Felix"                        
# [5,] "Asplund, Master. Filip Oscar"                             
# [6,] "Asplund, Mr. Carl Oscar Vilhelm Gustafsson"               
# [7,] "Asplund, Master. Carl Edgar"                              
# [8,] "Andersson, Mr. Anders Johan"                              
# [9,] "Andersson, Miss. Ellis Anna Maria"                        
#[10,] "Andersson, Miss. Ingeborg Constanzia"                     
#[11,] "Andersson, Miss. Sigrid Elisabeth"                        
#[12,] "Andersson, Mrs. Anders Johan (Alfrida Konstantia Brogren)"
#[13,] "Andersson, Miss. Ebba Iris Alfrida"                       
#[14,] "Andersson, Master. Sigvard Harald Elias"                  
#[15,] "Goodwin, Master. William Frederick"                       
#[16,] "Goodwin, Miss. Lillian Amy"                               
#[17,] "Goodwin, Master. Sidney Leonard"                          
#[18,] "Goodwin, Master. Harold Victor"                           
#[19,] "Goodwin, Mrs. Frederick (Augusta Tyler)"                  
#[20,] "Goodwin, Mr. Charles Edward"                              
#[21,] "Goodwin, Mr. Charles Frederick"                           
#[22,] "Goodwin, Miss. Jessie Allis"                              
#[23,] "Sage, Master. Thomas Henry"                               
#[24,] "Sage, Miss. Constance Gladys"                             
#[25,] "Sage, Mr. Frederick"                                      
#[26,] "Sage, Mr. George John Jr"                                 
#[27,] "Sage, Miss. Stella Anne"                                  
#[28,] "Sage, Mr. Douglas Bullen"                                 
#[29,] "Sage, Miss. Dorothy Edith \"Dolly\""                      
#[30,] "Sage, Miss. Ada"                                          
#[31,] "Sage, Mr. John George"                                    
#[32,] "Master Anthony William Sage "                             
#[33,] "Sage, Mrs. John (Annie Bullen)" 


# 3div) You can find some big travel groups (i.e., more than 5 persons share the same ticket). Who are they?
# There were 930 unique tickets 
length(unique(titanic$ticket))
#[1] 930

# 12 tickets had at least 6 passengers sharing the same tickets
# 86 passengers total 
table(table(titanic$ticket))
#  1   2   3   4   5   6   7   8  11 
#715 131  49  16   7   4   5   2   1 

# Names of the 12 tickets that had at least 6 passengers sharing it
names(table(titanic$ticket))[table(titanic$ticket) > 5]
# [1] "113781"       "1601"         "19950"        "3101295"      "347077"       "347082"       "347088"      
# [8] "382652"       "CA 2144"      "CA. 2343"     "PC 17608"     "S.O.C. 14879"

# Names of the 86 passengers who shared the tickets
shareTix <- titanic[titanic$ticket %in% names(table(titanic$ticket))[table(titanic$ticket) > 5],]
shareTix[order(shareTix$ticket),]$name
# [1] "Allison, Miss. Helen Loraine"                              "Allison, Master. Hudson Trevor"                           
# [3] "Allison, Mrs. Hudson J C (Bessie Waldo Daniels)"           "Cleaver, Miss. Alice"                                     
# [5] "Daniels, Miss. Sarah"                                      "Allison, Mr. Hudson Joshua Creighton"                     
# [7] "Bing, Mr. Lee"                                             "Ling, Mr. Lee"                                            
# [9] "Lang, Mr. Fang"                                            "Foo, Mr. Choong"                                          
#[11] "Lam, Mr. Ali"                                              "Lam, Mr. Len"                                             
#[13] "Chip, Mr. Chang"                                           "Hee, Mr. Ling"                                            
#[15] "Fortune, Mr. Charles Alexander"                            "Fortune, Miss. Mabel Helen"                               
#[17] "Fortune, Miss. Alice Elizabeth"                            "Fortune, Mr. Mark"                                        
#[19] "Fortune, Miss. Ethel Flora"                                "Fortune, Mrs. Mark (Mary McDougald)"                      
#[21] "Panula, Master. Juha Niilo"                                "Panula, Master. Eino Viljami"                             
#[23] "Panula, Mr. Ernesti Arvid"                                 "Panula, Mrs. Juha (Maria Emilia Ojala)"                   
#[25] "Panula, Mr. Jaako Arnold"                                  "Panula, Master. Urho Abraham"                             
#[27] "Riihivouri, Miss. Susanna Juhantytar Sanni"                "Asplund, Mrs. Carl Oscar (Selma Augusta Emilia Johansson)"
#[29] "Asplund, Master. Clarence Gustaf Hugo"                     "Asplund, Miss. Lillian Gertrud"                           
#[31] "Asplund, Master. Edvin Rojj Felix"                         "Asplund, Master. Filip Oscar"                             
#[33] "Asplund, Mr. Carl Oscar Vilhelm Gustafsson"                "Asplund, Master. Carl Edgar"                              
#[35] "Andersson, Mr. Anders Johan"                               "Andersson, Miss. Ellis Anna Maria"                        
#[37] "Andersson, Miss. Ingeborg Constanzia"                      "Andersson, Miss. Sigrid Elisabeth"                        
#[39] "Andersson, Mrs. Anders Johan (Alfrida Konstantia Brogren)" "Andersson, Miss. Ebba Iris Alfrida"                       
#[41] "Andersson, Master. Sigvard Harald Elias"                   "Skoog, Master. Harald"                                    
#[43] "Skoog, Mrs. William (Anna Bernhardina Karlsson)"           "Skoog, Mr. Wilhelm"                                       
#[45] "Skoog, Miss. Mabel"                                        "Skoog, Miss. Margit Elizabeth"                            
#[47] "Skoog, Master. Karl Thorsten"                              "Rice, Master. Eugene"                                     
#[49] "Rice, Master. Arthur"                                      "Rice, Master. Eric"                                       
#[51] "Rice, Master. George Hugh"                                 "Rice, Mrs. William (Margaret Norton)"                     
#[53] "Rice, Master. Albert"                                      "Goodwin, Master. William Frederick"                       
#[55] "Goodwin, Miss. Lillian Amy"                                "Goodwin, Master. Sidney Leonard"                          
#[57] "Goodwin, Master. Harold Victor"                            "Goodwin, Mrs. Frederick (Augusta Tyler)"                  
#[59] "Goodwin, Mr. Charles Edward"                               "Goodwin, Mr. Charles Frederick"                           
#[61] "Goodwin, Miss. Jessie Allis"                               "Sage, Master. Thomas Henry"                               
#[63] "Sage, Miss. Constance Gladys"                              "Sage, Mr. Frederick"                                      
#[65] "Sage, Mr. George John Jr"                                  "Sage, Miss. Stella Anne"                                  
#[67] "Sage, Mr. Douglas Bullen"                                  "Sage, Miss. Dorothy Edith \"Dolly\""                      
#[69] "Sage, Miss. Ada"                                           "Sage, Mr. John George"                                    
#[71] "Master Anthony William Sage "                              "Sage, Mrs. John (Annie Bullen)"                           
#[73] "Ryerson, Miss. Emily Borie"                                "Ryerson, Miss. Susan Parker \"Suzette\""                  
#[75] "Ryerson, Mrs. Arthur Larned (Emily Maria Borie)"           "Chaudanson, Miss. Victorine"                              
#[77] "Ryerson, Master. John Borie"                               "Ryerson, Mr. Arthur Larned"                               
#[79] "Bowen, Miss. Grace Scott"                                  "Hood, Mr. Ambrose Jr"                                     
#[81] "Hickman, Mr. Stanley George"                               "Davies, Mr. Charles Henry"                                
#[83] "Hickman, Mr. Leonard Mark"                                 "Hickman, Mr. Lewis"                                       
#[85] "Deacon, Mr. Percy William"                                 "Dibden, Mr. William"  

# 3dv) There are still two missing values for variable "age". The two passengers with missing ages are both male with pclass=3. 
# What is the median age for all male passengers in the 3rd class? Replace the missing ages by this median.
# Identify the 2 passengers whose age are NA
titanic[is.na(titanic$age),]
#    Life_boat survived pclass                   name  sex age sibsp parch ticket   fare cabin embarked train
#784                  0      3    Kraeff, Mr. Theodor male  NA     0     0 349253 7.8958              C     1
#785                  0      3 Gheorgheff, Mr. Stanio male  NA     0     0 349254 7.8958              C     1

# There are 493 male passengers in the 3rd class
length((1:1309)[(titanic$pclass==3) & (titanic$sex=='male')]) 
#[1] 493

# Calculate the median age for all male passengers in the 3rd class
# The median age for all male passengers in the 3rd class is 24 years old
maleThird <- (1:1309)[(titanic$pclass==3) & (titanic$sex=='male')] 
maleThirdPass <- titanic[maleThird[order(titanic$name[maleThird])], ]
median(maleThirdPass$age,na.rm=T)
#[1] 24

# Assign the 2 male passengers to be 24 years old
titanic[784,]$age <- 24
titanic[785,]$age <- 24 

# Verify the age assignment
titanic[784,]
#    Life_boat survived pclass                name  sex age sibsp parch ticket   fare cabin embarked train
#784                  0      3 Kraeff, Mr. Theodor male  24     0     0 349253 7.8958              C     1
titanic[785,]
#    Life_boat survived pclass                   name  sex age sibsp parch ticket   fare cabin embarked train
#785                  0      3 Gheorgheff, Mr. Stanio male  24     0     0 349254 7.8958              C     1


# 3dvi) There is still one missing value for variable "fare". Replace it by the median fare of the corresponding class. 
# Identify the 1 fare that has NA value
titanic[is.na(titanic$fare),]
#    Life_boat survived pclass               name  sex age sibsp parch ticket fare cabin embarked train
#118                  0      3 Storey, Mr. Thomas male  51     0     0   3701   NA              S     0

# There are 709 passengers in the 3rd class
length((1:1309)[titanic$pclass==3])
#[1] 709


# The median fare of the 3rd class tickets was 8.05 pounds
thirdClassPass <- (1:1309)[titanic$pclass==3]
thirdClassTix <- titanic[thirdClassPass[order(titanic$name[thirdClassPass])], ]
median(thirdClassTix$fare,na.rm=T)
#[1] 8.05

# Assign the NA fare value to be 8.05
titanic[118,]$fare <- 8.05
# Verify the fare assignment
titanic[118,]
#    Life_boat survived pclass               name  sex age sibsp parch ticket fare cabin embarked train
#118                  0      3 Storey, Mr. Thomas male  51     0     0   3701 8.05              S     0


###############################################


# Part 4: II. Divide the data into training and test sets. Explore various logistic models.
trainSet <- titanic[titanic$train == 1, ]
testSet <- titanic[titanic$train == 0, ]

# 4a) Fit a logistic model survived ~ sex + pclass on the training data. How to interpret the coefficients? 
# What sorts of people were likely to survive? Summarize your prediction on the test data by a 2-by-2 table, 
# and report your prediction accuracy.

modelIIa <- glm(survived ~ sex + pclass, data = trainSet, family = binomial(link="logit"))
modelIIa
#Call:  glm(formula = survived ~ sex + pclass, family = binomial(link = "logit"), 
#    data = trainSet)
#
#Coefficients:
#(Intercept)      sexmale      pclass2      pclass3  
#      2.297       -2.642       -0.838       -1.905  
#
#Degrees of Freedom: 890 Total (i.e. Null);  887 Residual
#Null Deviance:      1187 
#Residual Deviance: 826.9        AIC: 834.9

drop1(modelIIa,test="Chi")
#Single term deletions
#
#Model:
#survived ~ sex + pclass
#       Df Deviance     AIC     LRT  Pr(>Chi)    
#<none>      826.89  834.89                      
#sex     1  1083.11 1089.11 256.220 < 2.2e-16 ***
#pclass  2   917.80  921.80  90.916 < 2.2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
 
summary(modelIIa)$coeff
#              Estimate Std. Error    z value     Pr(>|z|)
#(Intercept)  2.2971232  0.2189839  10.489917 9.611309e-26
#sexmale     -2.6418754  0.1840954 -14.350577 1.056551e-46
#pclass2     -0.8379523  0.2447436  -3.423797 6.175280e-04
#pclass3     -1.9054951  0.2141410  -8.898319 5.669887e-19
extractAIC(modelIIa)
#[1]   4.0000 834.8884
extractAIC(modelIIa, k=log(nrow(trainSet)))
#[1]   4.0000 854.0577

# Summarize the prediction on the test data by a 2-by-2 table
modelIIa.pred <- predict(modelIIa, newdata = testSet, type="response")
modelIIa.pred <- as.numeric(modelIIa.pred > 0.5)
table(modelIIa.pred, testSet[,2])           
#modelIIa.pred   0   1
#            0 213  53
#            1  46 106

# Report the prediction accuracy of model IIa
correctPred1 <- sum(modelIIa.pred == testSet[,2])
correctPred1
#[1] 319

# The prediction accuracy is 76.32%
accuracyPred1 <- correctPred1/dim(testSet)[1]
accuracyPred1
#[1] 0.7631579


# 4b) Fit a logistic model survived ~ sex*pclass on the training. Compare model II(a) versus model II(b). 
# How to interpret the coefficients? What sorts of people were likely to survive? 
# Summarize your prediction on the test data by a 2-by-2 table, and report your prediction accuracy. 

modelIIb <- glm(survived ~ sex * pclass, data = trainSet, family = binomial(link="logit"))
modelIIb
#Call:  glm(formula = survived ~ sex * pclass, family = binomial(link = "logit"), 
#    data = trainSet)
#
#Coefficients:
#    (Intercept)          sexmale          pclass2          pclass3  sexmale:pclass2  sexmale:pclass3  
#         3.4122          -3.9494          -0.9555          -3.4122          -0.1850           2.0958  
#
#Degrees of Freedom: 890 Total (i.e. Null);  885 Residual
#Null Deviance:      1187 
#Residual Deviance: 798.1        AIC: 810.1

drop1(modelIIb,test="Chi")
#Single term deletions
#
#Model:
#survived ~ sex * pclass
#           Df Deviance    AIC    LRT  Pr(>Chi)    
#<none>          798.10 810.10                     
#sex:pclass  2   826.89 834.89 28.791 5.598e-07 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
 
extractAIC(modelIIb)
#[1]   6.0000 810.0969
extractAIC(modelIIb, k=log(nrow(trainSet)))
#[1]   6.000 838.851
summary(modelIIb)$coeff
#                  Estimate Std. Error   z value     Pr(>|z|)
#(Intercept)      3.4122472  0.5867893  5.815115 6.059214e-09
#sexmale         -3.9493901  0.6160608 -6.410715 1.448386e-10
#pclass2         -0.9555114  0.7247579 -1.318387 1.873741e-01
#pclass3         -3.4122472  0.6099995 -5.593852 2.220860e-08
#sexmale:pclass2 -0.1849918  0.7939117 -0.233013 8.157513e-01
#sexmale:pclass3  2.0957553  0.6572051  3.188891 1.428199e-03

# Summarize the prediction on the test data by a 2-by-2 table
modelIIb.pred <- predict(modelIIb, newdata = testSet, type="response")
modelIIb.pred <- as.numeric(modelIIb.pred > 0.5)
table(modelIIb.pred, testSet[,2])             
#modelIIb.pred   0   1
#            0 251  87
#            1   8  72

# Report the prediction accuracy of model IIb
correctPred2 <- sum(modelIIb.pred == testSet[,2])
correctPred2
#[1] 323

# The prediction accuracy is 77.27%
accuracyPred2 <- correctPred2/dim(testSet)[1]
accuracyPred2
#[1] 0.7727273


# 4c) Add age to model II(b). Consider all interactions. Which sub-model would you select? 
# Summarize your prediction on the test data (using the selected model) by a 2-by-2 table, 
# and report your prediction accuracy. In the analysis above, you can either use the original age 
# or its log transformation; specify which form you use and explain why. 

# Model II(c)(1): survived ~ sex*pclass*age 
modelIIc1 <- glm(survived ~ sex*pclass*age, data = trainSet, family = "binomial")
modelIIc1
#Call:  glm(formula = survived ~ sex * pclass * age, family = "binomial", 
#    data = trainSet)
#
#Coefficients:
#        (Intercept)              sexmale              pclass2              pclass3                  age      sexmale:pclass2      sexmale:pclass3          sexmale:age  
#            1.43183             -0.08017              2.48904             -1.11655              0.06503             -2.97639             -1.19601             -0.11214  
#        pclass2:age          pclass3:age  sexmale:pclass2:age  sexmale:pclass3:age  
#           -0.11012             -0.07918              0.05476              0.09003  
#
#Degrees of Freedom: 890 Total (i.e. Null);  879 Residual
#Null Deviance:      1187 
#Residual Deviance: 756.5        AIC: 780.5

extractAIC(modelIIc1)
#[1]  12.0000 780.4776
extractAIC(modelIIc1, k=log(nrow(trainSet)))
#[1]  12.0000 837.9857
summary(modelIIc1)$coeff
#                       Estimate Std. Error     z value   Pr(>|z|)
#(Intercept)          1.43182561 1.47409912  0.97132248 0.33138772
#sexmale             -0.08017098 1.60652903 -0.04990323 0.96019951
#pclass2              2.48903542 1.95965675  1.27013847 0.20403531
#pclass3             -1.11655229 1.51520837 -0.73689686 0.46118508
#age                  0.06503179 0.05175012  1.25665011 0.20888036
#sexmale:pclass2     -2.97639193 2.16043109 -1.37768427 0.16830079
#sexmale:pclass3     -1.19600841 1.68915965 -0.70804936 0.47891460
#sexmale:age         -0.11214258 0.05402256 -2.07584741 0.03790807
#pclass2:age         -0.11011717 0.06209243 -1.77343970 0.07615588
#pclass3:age         -0.07918410 0.05356992 -1.47814465 0.13936906
#sexmale:pclass2:age  0.05476125 0.06940234  0.78904034 0.43008843
#sexmale:pclass3:age  0.09003184 0.05781943  1.55712070 0.11944183

# Summarize the prediction on the test data by a 2-by-2 table
modelIIc1.pred <- predict(modelIIc1, newdata = testSet, type="response")
modelIIc1.pred <- as.numeric(modelIIc1.pred > 0.5)
table(modelIIc1.pred, testSet[,2]) 
#modelIIc1.pred   0   1
#             0 226  63
#             1  33  96

# Report the prediction accuracy of model IIc1
correctPred3 <- sum(modelIIc1.pred == testSet[,2])
correctPred3
#[1] 322

# The prediction accuracy is 77.03%
accuracyPred3 <- correctPred3/dim(testSet)[1]
accuracyPred3
#[1] 0.7703349


# Model II(c)(2): survived ~ sex + pclass + age + sex:pclass + sex:age + pclass:age
modelIIc2 <- glm(survived ~ sex + pclass + age + sex:pclass + sex:age + pclass:age, data = trainSet, family="binomial")
modelIIc2
#Call:  glm(formula = survived ~ sex + pclass + age + sex:pclass + sex:age + 
#    pclass:age, family = "binomial", data = trainSet)
#
#Coefficients:
#    (Intercept)          sexmale          pclass2          pclass3              age  sexmale:pclass2  sexmale:pclass3      sexmale:age      pclass2:age      pclass3:age  
#       3.456216        -2.375584         0.887344        -3.306794        -0.001241        -1.248044         1.479656        -0.038977        -0.055531        -0.005464  
#
#Degrees of Freedom: 890 Total (i.e. Null);  881 Residual
#Null Deviance:      1187 
#Residual Deviance: 759.4        AIC: 779.4

extractAIC(modelIIc2)
#[1]  10.0000 779.3916
extractAIC(modelIIc2, k=log(nrow(trainSet)))
#[1]  10.000 827.315
summary(modelIIc2)$coeff
#                    Estimate Std. Error     z value     Pr(>|z|)
#(Intercept)      3.456216070 0.95510834  3.61866389 0.0002961279
#sexmale         -2.375583712 0.88299250 -2.69037814 0.0071371095
#pclass2          0.887344189 1.18376136  0.74959719 0.4534973433
#pclass3         -3.306793733 0.91277916 -3.62277524 0.0002914591
#age             -0.001240655 0.02120436 -0.05850941 0.9533428665
#sexmale:pclass2 -1.248043722 0.87648030 -1.42392673 0.1544676829
#sexmale:pclass3  1.479655930 0.70311571  2.10442736 0.0353411847
#sexmale:age     -0.038977399 0.01748386 -2.22933641 0.0257915295
#pclass2:age     -0.055531204 0.02565338 -2.16467396 0.0304126627
#pclass3:age     -0.005463808 0.01938407 -0.28187112 0.7780423332

# Summarize the prediction on the test data by a 2-by-2 table
modelIIc2.pred <- predict(modelIIc2, newdata = testSet, type="response")
modelIIc2.pred <- as.numeric(modelIIc2.pred > 0.5)
table(modelIIc2.pred, testSet[,2])               
#modelIIc2.pred   0   1
#             0 227  64
#             1  32  95

# Report the prediction accuracy of model IIc2
correctPred4 <- sum(modelIIc2.pred == testSet[,2])
correctPred4
#[1] 322

# The prediction accuracy is 77.03%
accuracyPred4 <- correctPred4/dim(testSet)[1]
accuracyPred4
#[1] 0.7703349


# Model II(c)(3): survived ~ sex*pclass*log(age)
modelIIc3 <- glm(survived ~ sex*pclass*log(age),data = trainSet, family = "binomial")
modelIIc3
#Call:  glm(formula = survived ~ sex * pclass * log(age), family = "binomial", 
#    data = trainSet)
#
#Coefficients:
#             (Intercept)                   sexmale                   pclass2                   pclass3                  log(age)           sexmale:pclass2  
#                  -2.319                     7.149                     9.546                     2.803                     1.780                   -10.568  
#         sexmale:pclass3          sexmale:log(age)          pclass2:log(age)          pclass3:log(age)  sexmale:pclass2:log(age)  sexmale:pclass3:log(age)  
#                  -7.656                    -3.258                    -3.183                    -1.950                     2.832                     2.825  
#
#Degrees of Freedom: 890 Total (i.e. Null);  879 Residual
#Null Deviance:      1187 
#Residual Deviance: 738.7        AIC: 762.7

extractAIC(modelIIc3)
#[1]  12.000 762.714
extractAIC(modelIIc3, k=log(nrow(trainSet)))
#[1]  12.0000 820.2221
summary(modelIIc3)$coeff
#                           Estimate Std. Error    z value    Pr(>|z|)
#(Intercept)               -2.318848  2.6092508 -0.8887026 0.374162914
#sexmale                    7.148656  3.3195145  2.1535246 0.031277476
#pclass2                    9.545768  4.9293049  1.9365344 0.052802288
#pclass3                    2.803137  2.6751499  1.0478428 0.294711035
#log(age)                   1.780420  0.8293079  2.1468747 0.031803262
#sexmale:pclass2          -10.568224  5.6036837 -1.8859422 0.059302744
#sexmale:pclass3           -7.655641  3.4252799 -2.2350411 0.025414651
#sexmale:log(age)          -3.258251  1.0012548 -3.2541677 0.001137251
#pclass2:log(age)          -3.183287  1.4432068 -2.2057039 0.027404736
#pclass3:log(age)          -1.949732  0.8525109 -2.2870462 0.022193127
#sexmale:pclass2:log(age)   2.832237  1.6381736  1.7288993 0.083827124
#sexmale:pclass3:log(age)   2.824546  1.0394344  2.7173877 0.006579947

# Summarize the prediction on the test data by a 2-by-2 table
modelIIc3.pred <- predict(modelIIc3, newdata = testSet, type="response")
modelIIc3.pred <- as.numeric(modelIIc3.pred > 0.5)
table(modelIIc3.pred, testSet[,2])                
#modelIIc3.pred   0   1
#             0 238  72
#             1  21  87

# Report the prediction accuracy of model IIc3
correctPred5 <- sum(modelIIc3.pred == testSet[,2])
correctPred5
#[1] 325

# The prediction accuracy is 77.75%
accuracyPred5 <- correctPred5/dim(testSet)[1]
accuracyPred5
#[1] 0.777512


# Model II(c)(4): survived ~ sex + pclass + log(age) + sex:pclass + sex:(log(age)) + pclass:(log(age))
modelIIc4 <- glm(survived ~ sex + pclass + log(age) + sex:pclass + sex:(log(age)) + pclass:(log(age)), data = trainSet, family="binomial")
modelIIc4
#Call:  glm(formula = survived ~ sex + pclass + log(age) + sex:pclass + 
#    sex:(log(age)) + pclass:(log(age)), family = "binomial", 
#    data = trainSet)
#
#Coefficients:
#     (Intercept)           sexmale           pclass2           pclass3          log(age)   sexmale:pclass2   sexmale:pclass3  sexmale:log(age)  pclass2:log(age)  
#          3.9640           -1.6321            2.6671           -3.7634           -0.1580           -1.0739            1.7052           -0.6334           -1.0752  
#pclass3:log(age)  
#          0.0878  
#
#Degrees of Freedom: 890 Total (i.e. Null);  881 Residual
#Null Deviance:      1187 
#Residual Deviance: 747.1        AIC: 767.1

extractAIC(modelIIc4)
#[1]  10.0000 767.0838
extractAIC(modelIIc4, k=log(nrow(trainSet)))
#[1]  10.0000 815.0073
summary(modelIIc4)$coeff
#                    Estimate Std. Error    z value   Pr(>|z|)
#(Intercept)       3.96397371  1.7982253  2.2043810 0.02749756
#sexmale          -1.63211079  1.1250946 -1.4506431 0.14687926
#pclass2           2.66707686  2.3902775  1.1158022 0.26450682
#pclass3          -3.76335863  1.7117410 -2.1985561 0.02790950
#log(age)         -0.15798319  0.4845865 -0.3260165 0.74441185
#sexmale:pclass2  -1.07392229  0.8502050 -1.2631333 0.20654128
#sexmale:pclass3   1.70523053  0.6817532  2.5012433 0.01237581
#sexmale:log(age) -0.63342819  0.2667986 -2.3741814 0.01758791
#pclass2:log(age) -1.07521615  0.6497485 -1.6548190 0.09796122
#pclass3:log(age)  0.08779824  0.4566718  0.1922568 0.84754109

# Summarize the prediction on the test data by a 2-by-2 table
modelIIc4.pred <- predict(modelIIc4, newdata = testSet, type="response")
modelIIc4.pred <- as.numeric(modelIIc4.pred > 0.5)
table(modelIIc4.pred, testSet[,2])  
#modelIIc4.pred   0   1
#             0 241  75
#             1  18  84               

# Report the prediction accuracy of model IIc4
correctPred6 <- sum(modelIIc4.pred == testSet[,2])
correctPred6
#[1] 325

# The prediction accuracy is 77.75%
accuracyPred6 <- correctPred6/dim(testSet)[1]
accuracyPred6
#[1] 0.777512


# 4d) Add fare, embarked, sibsp, and parch into the model II(c). Call the R function step to do model selection 
# using AIC and BIC. Summarize your prediction on the test data (using the selected model or models) by a 2-by-2 table, 
# and report your prediction accuracy. 

modelIId <- glm(survived ~ sex*pclass*log(age) + fare + embarked + sibsp + parch, data = trainSet, family="binomial")
modelIId
#Call:  glm(formula = survived ~ sex * pclass * log(age) + fare + embarked + 
#    sibsp + parch, family = "binomial", data = trainSet)
#
#Coefficients:
#             (Intercept)                   sexmale                   pclass2                   pclass3                  log(age)                      fare  
#              -16.015839                  6.879101                  9.654754                  3.487871                  1.604966                  0.002369  
#               embarkedC                 embarkedQ                 embarkedS                     sibsp                     parch           sexmale:pclass2  
#               14.709855                 14.769666                 14.351511                 -0.546037                 -0.163529                -10.149199  
#         sexmale:pclass3          sexmale:log(age)          pclass2:log(age)          pclass3:log(age)  sexmale:pclass2:log(age)  sexmale:pclass3:log(age)  
#               -6.787747                 -3.198238                 -3.133153                 -2.123169                  2.674810                  2.521739  
#
#Degrees of Freedom: 890 Total (i.e. Null);  873 Residual
#Null Deviance:      1187 
#Residual Deviance: 700.7        AIC: 736.7

extractAIC(modelIId)
#[1]  18.0000 736.6964
extractAIC(modelIId, k=log(nrow(trainSet)))
#[1]  18.0000 822.9586
summary(modelIId)$coeff
#                              Estimate   Std. Error     z value     Pr(>|z|)
#(Intercept)              -16.015839452 4.595020e+02 -0.03485478 9.721955e-01
#sexmale                    6.879101040 3.328940e+00  2.06645385 3.878565e-02
#pclass2                    9.654753606 5.042660e+00  1.91461532 5.554158e-02
#pclass3                    3.487871492 2.735215e+00  1.27517247 2.022482e-01
#log(age)                   1.604965722 8.312597e-01  1.93076333 5.351233e-02
#fare                       0.002368879 2.656184e-03  0.89183531 3.724812e-01
#embarkedC                 14.709855310 4.594945e+02  0.03201313 9.744616e-01
#embarkedQ                 14.769666078 4.594946e+02  0.03214329 9.743578e-01
#embarkedS                 14.351510884 4.594945e+02  0.03123326 9.750835e-01
#sibsp                     -0.546036509 1.400648e-01 -3.89845734 9.680745e-05
#parch                     -0.163529091 1.332613e-01 -1.22713154 2.197732e-01
#sexmale:pclass2          -10.149199471 5.733189e+00 -1.77025367 7.668489e-02
#sexmale:pclass3           -6.787747372 3.474605e+00 -1.95353054 5.075676e-02
#sexmale:log(age)          -3.198237909 1.004439e+00 -3.18410278 1.452034e-03
#pclass2:log(age)          -3.133153298 1.472433e+00 -2.12787485 3.334746e-02
#pclass3:log(age)          -2.123168778 8.656139e-01 -2.45278955 1.417532e-02
#sexmale:pclass2:log(age)   2.674809637 1.676033e+00  1.59591716 1.105073e-01
#sexmale:pclass3:log(age)   2.521739357 1.056657e+00  2.38652702 1.700836e-02


# Use AIC approach to generate regression model 
step(modelIId, k = 2)  
#Start:  AIC=736.7
#survived ~ sex * pclass * log(age) + fare + embarked + sibsp + 
#    parch
#
#                      Df Deviance    AIC
#- fare                 1   701.55 735.55
#- parch                1   702.26 736.26
#<none>                     700.70 736.70
#- embarked             3   708.84 738.84
#- sex:pclass:log(age)  2   707.31 739.31
#- sibsp                1   720.35 754.35
#
#Step:  AIC=735.55
#survived ~ sex + pclass + log(age) + embarked + sibsp + parch + 
#    sex:pclass + sex:log(age) + pclass:log(age) + sex:pclass:log(age)
#
#                      Df Deviance    AIC
#- parch                1   702.73 734.73
#<none>                     701.55 735.55
#- sex:pclass:log(age)  2   708.31 738.31
#- embarked             3   710.77 738.77
#- sibsp                1   720.36 752.36
#
#Step:  AIC=734.73
#survived ~ sex + pclass + log(age) + embarked + sibsp + sex:pclass + 
#    sex:log(age) + pclass:log(age) + sex:pclass:log(age)
#
#                      Df Deviance    AIC
#<none>                     702.73 734.73
#- sex:pclass:log(age)  2   709.87 737.87
#- embarked             3   712.23 738.23
#- sibsp                1   727.07 757.07
#
#Call:  glm(formula = survived ~ sex + pclass + log(age) + embarked + 
#    sibsp + sex:pclass + sex:log(age) + pclass:log(age) + sex:pclass:log(age), 
#    family = "binomial", data = trainSet)
#
#Coefficients:
#             (Intercept)                   sexmale                   pclass2                   pclass3                  log(age)                 embarkedC  
#                -16.0528                    7.1030                    9.6732                    3.3839                    1.6472                   14.8044  
#               embarkedQ                 embarkedS                     sibsp           sexmale:pclass2           sexmale:pclass3          sexmale:log(age)  
#                 14.8853                   14.4090                   -0.5639                  -10.4999                   -7.0651                   -3.2768  
#        pclass2:log(age)          pclass3:log(age)  sexmale:pclass2:log(age)  sexmale:pclass3:log(age)  
#                 -3.1977                   -2.1678                    2.8105                    2.6447  
#
#Degrees of Freedom: 890 Total (i.e. Null);  875 Residual
#Null Deviance:      1187 
#Residual Deviance: 702.7        AIC: 734.7

modelIId2 <- glm(survived ~ sex + pclass + log(age) + embarked + sibsp + sex:pclass + sex:log(age) + pclass:log(age) + sex:pclass:log(age), 
         data = trainSet, family="binomial")
modelIId2
#Call:  glm(formula = survived ~ sex + pclass + log(age) + embarked + 
#    sibsp + sex:pclass + sex:log(age) + pclass:log(age) + sex:pclass:log(age), 
#    family = "binomial", data = trainSet)
#
#Coefficients:
#             (Intercept)                   sexmale                   pclass2                   pclass3                  log(age)                 embarkedC  
#                -16.0528                    7.1030                    9.6732                    3.3839                    1.6472                   14.8044  
#               embarkedQ                 embarkedS                     sibsp           sexmale:pclass2           sexmale:pclass3          sexmale:log(age)  
#                 14.8853                   14.4090                   -0.5639                  -10.4999                   -7.0651                   -3.2768  
#        pclass2:log(age)          pclass3:log(age)  sexmale:pclass2:log(age)  sexmale:pclass3:log(age)  
#                 -3.1977                   -2.1678                    2.8105                    2.6447  
#
#Degrees of Freedom: 890 Total (i.e. Null);  875 Residual
#Null Deviance:      1187 
#Residual Deviance: 702.7        AIC: 734.7

extractAIC(modelIId2)
#[1]  16.0000 734.7276
extractAIC(modelIId2, k=log(nrow(trainSet)))
#[1]  16.0000 811.4051
summary(modelIId2)$coeff
#                            Estimate  Std. Error     z value     Pr(>|z|)
#(Intercept)              -16.0527975 457.8489137 -0.03506134 9.720308e-01
#sexmale                    7.1029565   3.3593612  2.11437711 3.448307e-02
#pclass2                    9.6731559   5.1181797  1.88996019 5.876328e-02
#pclass3                    3.3838558   2.6999731  1.25329243 2.100993e-01
#log(age)                   1.6471990   0.8309325  1.98234995 4.744009e-02
#embarkedC                 14.8044043 457.8415093  0.03233522 9.742047e-01
#embarkedQ                 14.8853447 457.8415723  0.03251200 9.740637e-01
#embarkedS                 14.4089662 457.8414761  0.03147152 9.748935e-01
#sibsp                     -0.5638624   0.1333038 -4.22990620 2.337888e-05
#sexmale:pclass2          -10.4999259   5.8353785 -1.79935644 7.196232e-02
#sexmale:pclass3           -7.0650949   3.5028061 -2.01698148 4.369745e-02
#sexmale:log(age)          -3.2767988   1.0123401 -3.23685561 1.208545e-03
#pclass2:log(age)          -3.1976836   1.4964552 -2.13683886 3.261110e-02
#pclass3:log(age)          -2.1677852   0.8640265 -2.50893372 1.210962e-02
#sexmale:pclass2:log(age)   2.8104965   1.7029364  1.65038254 9.886472e-02
#sexmale:pclass3:log(age)   2.6447315   1.0625443  2.48905535 1.280830e-02

# Summarize the prediction on the test data by a 2-by-2 table
modelIId2.pred <- predict(modelIId2, newdata = testSet, type="response")
modelIId2.pred <- as.numeric(modelIId2.pred > 0.5)
table(modelIId2.pred, testSet[,2])  
#modelIId2.pred   0   1
#             0 220  50
#             1  39 109    

# Report the prediction accuracy of model IId2
correctPred7 <- sum(modelIId2.pred == testSet[,2])
correctPred7
#[1] 329

# The prediction accuracy is 78.71%
accuracyPred7 <- correctPred7/dim(testSet)[1]
accuracyPred7
#[1] 0.7870813


# Use BIC approach to generate regression model 
step(modelIId, k = log(nrow(trainSet)))
#Start:  AIC=822.96
#survived ~ sex * pclass * log(age) + fare + embarked + sibsp + 
#    parch
#
#                      Df Deviance    AIC
#- embarked             3   708.84 810.72
#- sex:pclass:log(age)  2   707.31 815.99
#- fare                 1   701.55 817.02
#- parch                1   702.26 817.73
#<none>                     700.70 822.96
#- sibsp                1   720.35 835.82
#
#Step:  AIC=810.72
#survived ~ sex + pclass + log(age) + fare + sibsp + parch + sex:pclass + 
#    sex:log(age) + pclass:log(age) + sex:pclass:log(age)
#
#                      Df Deviance    AIC
#- sex:pclass:log(age)  2   715.73 804.03
#- fare                 1   710.77 805.87
#- parch                1   710.99 806.08
#<none>                     708.84 810.72
#- sibsp                1   730.33 825.42
#
#Step:  AIC=804.03
#survived ~ sex + pclass + log(age) + fare + sibsp + parch + sex:pclass + 
#    sex:log(age) + pclass:log(age)
#
#                  Df Deviance    AIC
#- pclass:log(age)  2   719.71 794.42
#- fare             1   717.95 799.45
#- parch            1   718.44 799.95
#<none>                 715.73 804.03
#- sex:log(age)     1   724.86 806.37
#- sex:pclass       2   740.82 815.54
#- sibsp            1   737.71 819.22
#
#Step:  AIC=794.42
#survived ~ sex + pclass + log(age) + fare + sibsp + parch + sex:pclass + 
#    sex:log(age)
#
#               Df Deviance    AIC
#- fare          1   721.53 789.45
#- parch         1   722.35 790.28
#<none>              719.71 794.42
#- sex:log(age)  1   731.94 799.87
#- sex:pclass    2   742.94 804.07
#- sibsp         1   745.11 813.03
#
#Step:  AIC=789.45
#survived ~ sex + pclass + log(age) + sibsp + parch + sex:pclass + 
#    sex:log(age)
#
#               Df Deviance    AIC
#- parch         1   723.41 784.54
#<none>              721.53 789.45
#- sex:log(age)  1   733.88 795.01
#- sex:pclass    2   745.37 799.71
#- sibsp         1   745.52 806.65
#
#Step:  AIC=784.54
#survived ~ sex + pclass + log(age) + sibsp + sex:pclass + sex:log(age)
#
#               Df Deviance    AIC
#<none>              723.41 784.54
#- sex:log(age)  1   734.74 789.08
#- sex:pclass    2   748.04 795.59
#- sibsp         1   754.99 809.33
#
#Call:  glm(formula = survived ~ sex + pclass + log(age) + sibsp + sex:pclass + 
#    sex:log(age), family = "binomial", data = trainSet)
#
#Coefficients:
#     (Intercept)           sexmale           pclass2           pclass3          log(age)             sibsp   sexmale:pclass2   sexmale:pclass3  sexmale:log(age)  
#          5.4756           -0.7115           -1.1085           -3.6815           -0.4681           -0.6099           -0.9492            1.6005           -0.9418  
#
#Degrees of Freedom: 890 Total (i.e. Null);  882 Residual
#Null Deviance:      1187 
#Residual Deviance: 723.4        AIC: 741.4

modelIId3 <- glm(survived ~ sex + pclass + log(age) + sibsp + sex:pclass + sex:log(age), data = trainSet, family="binomial")
modelIId3
#Call:  glm(formula = survived ~ sex + pclass + log(age) + sibsp + sex:pclass + 
#    sex:log(age), family = "binomial", data = trainSet)
#
#Coefficients:
#     (Intercept)           sexmale           pclass2           pclass3          log(age)             sibsp   sexmale:pclass2   sexmale:pclass3  sexmale:log(age)  
#          5.4756           -0.7115           -1.1085           -3.6815           -0.4681           -0.6099           -0.9492            1.6005           -0.9418  
#
#Degrees of Freedom: 890 Total (i.e. Null);  882 Residual
#Null Deviance:      1187 
#Residual Deviance: 723.4        AIC: 741.4

extractAIC(modelIId3)
#[1]   9.0000 741.4095
extractAIC(modelIId3, k=log(nrow(trainSet)))
#[1]   9.0000 784.5406
summary(modelIId3)$coeff
#                   Estimate Std. Error    z value     Pr(>|z|)
#(Intercept)       5.4755893  1.0001757  5.4746276 4.384327e-08
#sexmale          -0.7115053  1.1655687 -0.6104362 5.415729e-01
#pclass2          -1.1084735  0.7311133 -1.5161446 1.294828e-01
#pclass3          -3.6815102  0.6315813 -5.8290364 5.574834e-09
#log(age)         -0.4680834  0.2167141 -2.1599121 3.077947e-02
#sibsp            -0.6099112  0.1283943 -4.7502993 2.031158e-06
#sexmale:pclass2  -0.9491553  0.8288085 -1.1452045 2.521245e-01
#sexmale:pclass3   1.6005333  0.6887896  2.3236897 2.014213e-02
#sexmale:log(age) -0.9418224  0.2761850 -3.4101138 6.493579e-04

# Summarize the prediction on the test data by a 2-by-2 table
modelIId3.pred <- predict(modelIId3, newdata = testSet, type="response")
modelIId3.pred <- as.numeric(modelIId3.pred > 0.5)
table(modelIId3.pred, testSet[,2])  
#modelIId3.pred   0   1
#             0 220  55
#             1  39 104   

# Report the prediction accuracy of model IId3
correctPred8 <- sum(modelIId3.pred == testSet[,2])
correctPred8
#[1] 324

# The prediction accuracy is 77.51%
accuracyPred8 <- correctPred8/dim(testSet)[1]
accuracyPred8
#[1] 0.7751196


#4e) Summary your findings for logistic models. Report the AIC/BIC for model II(a), II(b), II(c), and II(d). 
# In terms of model selection, which model gives you the smallest AIC/BIC? Note that the model returned by II(d) 
# is not necessarily the model with the smallest AIC/BIC. So you still need to check the AIC/BIC of the other models 
# considered in (a)-(c). In terms of prediction accuracy, which model is the best? 

# Model modelIId2 gives the smallest AIC value at 734.7276 and the best prediction accuracy at 78.71%

AICSummary <- c(834.8884,810.0969,780.4776,779.3916,762.714,767.0838,736.6964,734.7276,741.4095)
sort(AICSummary)
#[1] 734.7276 736.6964 741.4095 762.7140 767.0838 779.3916 780.4776 810.0969 834.8884
BICSummary <- c(854.0577,838.851,837.9857,827.315,820.2221,815.0073,822.9586,811.4051,784.5406)
sort(BICSummary)
#[1] 784.5406 811.4051 815.0073 820.2221 822.9586 827.3150 837.9857 838.8510 854.0577

predAcc <- c(0.7631579,0.7727273,0.7703349,0.7703349,0.777512,0.777512,0.7870813,0.7751196)
sort(predAcc)
#[1] 0.7631579 0.7703349 0.7703349 0.7727273 0.7751196 0.7775120 0.7775120 0.7870813


##############################################################################################


# Part 5: III. Divide the data into training and test. Explore various tree models with all the variables except 
#Life_boat, name, ticket and cabin.

#install.packages("rpart")
library(rpart)
drops=c("Life_boat", "name", "ticket", "cabin")
trainSet = trainSet[, !names(trainSet) %in% drops]
testSet = testSet[, !names(testSet) %in% drops]

# 5a) Fit a tree model. Explain how you choose the tree size (or number of splits) -- the results may not be stable when 
# using CV errors to select the optimal tree (i.e., the optimal "nsplit" changes when you re-run the code); 
# you can run the code several times, and then pick the most frequent optimal "nsplit". 
# Plot the tree. Comment on the tree, e.g., do you think some of the splits make sense? Summarize your prediction on 
# the test data (using the seleceted tree) by a 2-by-2 table, and report your prediction accuracy.

# Run this code multiple times, 0.003898635 is the most common CP value
modelTree1 <- rpart(survived ~ ., data = trainSet, minsplit = 5, cp = 0.000001, maxdepth = 30, method = "class")
plotcp(modelTree1)
printcp(modelTree1)
#Classification tree:
#rpart(formula = survived ~ ., data = trainSet, method = "class", 
#    minsplit = 5, cp = 1e-06, maxdepth = 30)
#
#Variables actually used in tree construction:
#[1] age      embarked fare     parch    pclass   sex      sibsp   
#
#Root node error: 342/891 = 0.38384
#
#n= 891 
#
#           CP nsplit rel error  xerror     xstd
#1  0.44444444      0   1.00000 1.00000 0.042446
#2  0.03070175      1   0.55556 0.55556 0.035750
#3  0.02046784      5   0.43275 0.48538 0.033983
#4  0.00779727      6   0.41228 0.48246 0.033904
#5  0.00584795     10   0.37719 0.47953 0.033824
#6  0.00438596     12   0.36550 0.50000 0.034372
#7  0.00389864     14   0.35673 0.50585 0.034524
#8  0.00365497     17   0.34503 0.50585 0.034524
#9  0.00292398     28   0.28655 0.50585 0.034524
#10 0.00194932     40   0.25146 0.49415 0.034217
#11 0.00146199     43   0.24561 0.48830 0.034061
#12 0.00129955     53   0.23099 0.51170 0.034675
#13 0.00097466     65   0.21053 0.51754 0.034823
#14 0.00058480     68   0.20760 0.52924 0.035116
#15 0.00041771     73   0.20468 0.53509 0.035260
#16 0.00000100     80   0.20175 0.54678 0.035542 
modelTree1$cptable[which.min(modelTree1$cptable[,"xerror"]),"CP"]
#[1] 0.005847953

modelTree2 <- rpart(survived ~ ., data = trainSet, minsplit = 5, cp = 0.003898635, method = "class")
plotcp(modelTree2)
printcp(modelTree2)
#Classification tree:
#rpart(formula = survived ~ ., data = trainSet, method = "class", 
#    minsplit = 5, cp = 0.003898635)
#
#Variables actually used in tree construction:
#[1] age      embarked fare     pclass   sex      sibsp   
#
#Root node error: 342/891 = 0.38384
#
#n= 891 
#
#         CP nsplit rel error  xerror     xstd
#1 0.4444444      0   1.00000 1.00000 0.042446
#2 0.0307018      1   0.55556 0.55556 0.035750
#3 0.0204678      5   0.43275 0.50585 0.034524
#4 0.0077973      6   0.41228 0.47076 0.033582
#5 0.0058480     10   0.37719 0.45906 0.033253
#6 0.0043860     12   0.36550 0.44444 0.032831
#7 0.0038986     14   0.35673 0.44152 0.032745
#8 0.0038986     17   0.34503 0.43567 0.032571 
modelTree2$cptable[which.min(modelTree2$cptable[,"xerror"]),"CP"]

# The most common cp value = 0.003898635 where nsplit = 14

treePlot <- prune.rpart(modelTree2, 0.0039)
png("/treePlot.png", height=800, width=800)
plot(treePlot, compress = T, uniform = T, branch = 0.5, margin = 0.05)
text(treePlot, cex = 0.8, font = 2, use.n = T, all = T)
dev.off()

# Summarize the prediction on the test data by a 2-by-2 table
predTreePlot <- predict(treePlot, newdata = testSet,"class")
table(predTreePlot, testSet[,1])            
#predTreePlot   0   1
#           0 224  54
#           1  35 105

correctTreePlot = sum(predTreePlot == testSet[,1])
correctTreePlot
#[1] 329

# The accuracy of the tree model prediction is 78.7%
accuracyTreePlot = correctTreePlot/dim(testSet)[1]
accuracyTreePlot
#[1] 0.7870813


# 5b) Fit a random forest. Summarize your prediction on the test data by a 2-by-2 table, and report your prediction accuracy. 

#install.packages("randomForest")
library(randomForest)
modelForest <- randomForest(survived ~ ., data = trainSet, proximity = T, importance = TRUE, ntrees = 500, method = "class")

png("/forestPlot.png", height=800, width=800)
plot(modelForest)
dev.off()


predForest <- predict(modelForest, newdata = testSet,"class")
table(predForest, testSet$survived)
#predForest   0   1
#         0 223  57
#         1  36 102

correctForest = sum(predForest == testSet[,1])
correctForest
#[1] 325

# The accuracy of the random forest model prediction is 77.75%
accuracyForest = correctForest/dim(testSet)[1]
accuracyForest
#[1] 0.777512


# 5c) In terms of prediction accuracy, which one does better, the single tree or the forest?
# The tree model has a higher prediction accuracy at 78.7%