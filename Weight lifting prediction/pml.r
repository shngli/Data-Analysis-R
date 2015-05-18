# Author: Sheng Li
# To predict the manner ("classe") in which participants perform weightlifting exercises

install.packages('caret')
install.packages('RANN')
install.packages("randomForest")
install.packages("e1071",dep=T)

library(caret)
library(RANN)

# Load datasets
trainingSet <- read.csv("/pml-training.csv", header=TRUE, sep=",",stringsAsFactors=FALSE)
testingSet <- read.csv("/pml-testing.csv", header=TRUE, sep=",",stringsAsFactors=FALSE)


#########################################################


# Part 1: Partition the training set into 2 data sets using random selection without replacement, 
# 1 set for training purpose (70%) [trainSet] to achieve high accuracy and a smaller set for 
# cross validation (30%) [crossValSet] to check for sample error.

trainingPartition <- createDataPartition(trainingSet$classe, list = FALSE, p = 0.7)
trainSet <- trainingSet[trainingPartition,]
crossValSet <- trainingSet[-trainingPartition,]

# Remove indicators with near zero variance from the training set and the testing set
nearZero <- nearZeroVar(trainSet)
trainSet <- trainSet[-nearZero]
crossValSet <- crossValSet[-nearZero]
testingSet <- testingSet[-nearZero]

# Select numeric features and outcome for training purposes
numericTrain <- which(lapply(trainSet,class) %in% c('numeric'))

# Fill in the missing values in the training set using knnImpute
fillData <- preProcess(trainSet[,numericTrain], method = c('knnImpute'))
fillTrainSet <- cbind(trainSet$classe, predict(fillData, trainSet[,numericTrain]))
fillCrossValSet <- cbind(crossValSet$classe, predict(fillData, crossValSet[,numericTrain]))
predTestSet <- predict(fillData, testingSet[,numericTrain])

# Fix the label on classe
names(fillTrainSet)[1] <- 'classe'
names(fillCrossValSet)[1] <- 'classe'


##########################################################


# Part 2: Build a random forest model with the given numerical variables to predict the 20
# test cases in the testing set. 
library(randomForest)
library(e1071)
randForest <- randomForest(classe ~ ., fillTrainSet, ntree=500, mtry=32)
randForest
#Call:
# randomForest(formula = classe ~ ., data = fillTrainSet, ntree = 500,      mtry = 32) 
#               Type of random forest: classification
#                     Number of trees: 500
#No. of variables tried at each split: 32
#
#        OOB estimate of  error rate: 1.11%
#Confusion matrix:
#     A    B    C    D    E class.error
#A 3888    9    1    1    7 0.004608295
#B   23 2617   17    1    0 0.015425132
#C    0   16 2356   22    2 0.016694491
#D    3    3   22 2220    4 0.014209591
#E    2    3    7    9 2504 0.008316832


# Test the accuracy of the random forest model on both the training set and the cross validation
# set to verify whether biasness and overfitting exists in the model.

# The accuracy of the training set is 100% ie. there is no bias in the random forest model
predTrainSet <- predict(randForest, fillTrainSet)
print(confusionMatrix(predTrainSet, fillTrainSet$classe))
#Confusion Matrix and Statistics
#
#          Reference
#Prediction    A    B    C    D    E
#         A 3906    0    0    0    0
#         B    0 2658    0    0    0
#         C    0    0 2396    0    0
#         D    0    0    0 2252    0
#         E    0    0    0    0 2525
#
#Overall Statistics
#                                     
#               Accuracy : 1          
#                 95% CI : (0.9997, 1)
#    No Information Rate : 0.2843     
#    P-Value [Acc > NIR] : < 2.2e-16  
#                                     
#                  Kappa : 1          
# Mcnemar's Test P-Value : NA         
#
#Statistics by Class:
#
#                     Class: A Class: B Class: C Class: D Class: E
#Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
#Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
#Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
#Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
#Prevalence             0.2843   0.1935   0.1744   0.1639   0.1838
#Detection Rate         0.2843   0.1935   0.1744   0.1639   0.1838
#Detection Prevalence   0.2843   0.1935   0.1744   0.1639   0.1838
#Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000


# The accuracy of the cross validation set is 98.8%
predCrossValSet <- predict(randForest, fillCrossValSet)
print(confusionMatrix(predCrossValSet, fillCrossValSet$classe))
#Confusion Matrix and Statistics
#
#          Reference
#Prediction    A    B    C    D    E
#         A 1667   17    1    0    0
#         B    4 1114    8    1    3
#         C    2    8 1004    4    2
#         D    0    0   12  956    3
#         E    1    0    1    3 1074
#
#Overall Statistics
#                                         
#               Accuracy : 0.9881         
#                 95% CI : (0.985, 0.9907)
#    No Information Rate : 0.2845         
#    P-Value [Acc > NIR] : < 2.2e-16      
#                                         
#                  Kappa : 0.985          
# Mcnemar's Test P-Value : NA             
#
#Statistics by Class:
#
#                     Class: A Class: B Class: C Class: D Class: E
#Sensitivity            0.9958   0.9781   0.9786   0.9917   0.9926
#Specificity            0.9957   0.9966   0.9967   0.9970   0.9990
#Pos Pred Value         0.9893   0.9858   0.9843   0.9846   0.9954
#Neg Pred Value         0.9983   0.9947   0.9955   0.9984   0.9983
#Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
#Detection Rate         0.2833   0.1893   0.1706   0.1624   0.1825
#Detection Prevalence   0.2863   0.1920   0.1733   0.1650   0.1833
#Balanced Accuracy      0.9958   0.9873   0.9876   0.9943   0.9958


##########################################################


# Part 3: Predict the 20 test cases in the testing set
predTestCases <- predict(randForest, predTestSet)
predTestCases
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
# B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
#Levels: A B C D E