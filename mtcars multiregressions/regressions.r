# Author: Chisheng Li
# Regression model project
# Take the mtcars data set and answer 2 question using regression models 
# Q1. Is an automatic or manual transmission better for MPG
# Q2. Quantify the MPG difference between automatic and manual transmissions

## Step1: Data munging

data(mtcars)
str(mtcars)
#'data.frame':	32 obs. of  11 variables:
# $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
# $ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
# $ disp: num  160 160 108 258 360 ...
# $ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
# $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
# $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
# $ qsec: num  16.5 17 18.6 19.4 17 ...
# $ vs  : num  0 0 1 1 0 1 0 1 1 1 ...
# $ am  : num  1 1 1 0 0 0 0 0 0 0 ...
# $ gear: num  4 4 4 3 3 3 3 4 4 4 ...
# $ carb: num  4 4 1 1 2 1 4 2 2 4 ...

# cyl = Number of cylinders; vs = V/S; am = Transmission (0 = automatic, 1 = manual)
# gear = Number of forward gears; carb  Number of carburetors
# Convert "cyl", "vs", "gear", "carb" and "am" variables into factor variables
# Relabel "am" into "Automatic" and "Manual"

mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)

levels(mtcars$am) <- c("Automatic", "Manual")


## Step 2: Exploratory graphs
library(ggplot2)

# Box plot to compare mpg when am is "Automatic" or "Manual"
png("/boxplot.png", height = 600, width = 600)
ggplot(mtcars, aes(am, mpg)) + geom_boxplot(aes(fill = am)) + ggtitle("MPG by transmission type") + 
    ylab("Miles per gallon") + xlab("Transmission type")
dev.off()

# Pairs graph to view the relationships between every variable
png("/pairsgraph.png", height = 1300, width = 1300)
pairs(mtcars, pch = 21, bg = c("red", "blue")[mtcars$am], main = "Pairs graph for MTCars")
par(xpd = TRUE)
legend(0.01,0.995, as.vector(unique(mtcars$am)), fill=c("red", "blue"), cex = 0.8)
dev.off()


## Step 3: Hypothesis testing

# Compare the mpg means between automatic and manual transmissions
# H0 = There is no performance difference (MPG) between Automatic and Manual cars
# H1 = There exists a performance difference between Automatic and Manual cars

t.test(mpg ~ am, data = mtcars)

#	Welch Two Sample t-test
#
#data:  mpg by am
#t = -3.7671, df = 18.332, p-value = 0.001374
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
# -11.280194  -3.209684
#sample estimates:
#mean in group Automatic    mean in group Manual 
#               17.14737                24.39231 

# P-value (0.001374) < 0.05, We reject the null hypothesis

# Determine if there is a difference in the population means
wilcox.test(mpg ~ am, data = mtcars)

#	Wilcoxon rank sum test with continuity correction
#
#data:  mpg by am
#W = 42, p-value = 0.001871
#alternative hypothesis: true location shift is not equal to 0

# P-value (0.001871) < 0.05, we reject the null hypothesis that the mileage data of manual and automatic transmissions in mtcar are identical populations.


## Step 4: Regression model
# Generate a regression model using a stepwise method
model.all <- lm(mpg ~ ., data = mtcars)
n <- nrow(mtcars)
model1 <- step(model.all, direction = "backward", k = log(n))
summary(model1)

#Call:
#lm(formula = mpg ~ wt + qsec + am, data = mtcars)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-3.4811 -1.5555 -0.7257  1.4110  4.6610 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   9.6178     6.9596   1.382 0.177915    
#wt           -3.9165     0.7112  -5.507 6.95e-06 ***
#qsec          1.2259     0.2887   4.247 0.000216 ***
#amManual      2.9358     1.4109   2.081 0.046716 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 2.459 on 28 degrees of freedom
#Multiple R-squared:  0.8497,	Adjusted R-squared:  0.8336 
#F-statistic: 52.75 on 3 and 28 DF,  p-value: 1.21e-11

# The algorithm suggest the model lm(mpg ~ wt + qsec + am, data = mtcars)
# We add an interaction term between wt & am and qsec & am to the model 
model2 <- lm(mpg ~ wt:am + qsec:am, data = mtcars)
summary(model2)

#Call:
#lm(formula = mpg ~ wt:am + qsec:am, data = mtcars)

#Residuals:
#    Min      1Q  Median      3Q     Max 
#-3.9361 -1.4017 -0.1551  1.2695  3.8862 

#Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       13.9692     5.7756   2.419  0.02259 *  
#wt:amAutomatic    -3.1759     0.6362  -4.992 3.11e-05 ***
#wt:amManual       -6.0992     0.9685  -6.297 9.70e-07 ***
#amAutomatic:qsec   0.8338     0.2602   3.205  0.00346 ** 
#amManual:qsec      1.4464     0.2692   5.373 1.12e-05 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2.097 on 27 degrees of freedom
#Multiple R-squared:  0.8946,	Adjusted R-squared:  0.879 
#F-statistic: 57.28 on 4 and 27 DF,  p-value: 8.424e-13


# Adjusted r-squared = 0.879, 87.9% of the variation in mpg is explained by the variables in model2

# Anova test to compare the different models
#anova(lm(mpg ~ am, data = mtcars), lm(mpg ~ am + wt, data = mtcars), model1, model2)
#Analysis of Variance Table

#Model 1: mpg ~ am
#Model 2: mpg ~ am + wt
#Model 3: mpg ~ wt + qsec + am
#Model 4: mpg ~ wt:am + qsec:am
#  Res.Df    RSS Df Sum of Sq       F    Pr(>F)    
#1     30 720.90                                   
#2     29 278.32  1    442.58 100.671 1.321e-10 ***
#3     28 169.29  1    109.03  24.802 3.213e-05 ***
#4     27 118.70  1     50.59  11.507  0.002153 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

confint(model2)
#                      2.5 %    97.5 %
#(Intercept)       2.1186308 25.819783
#wt:amAutomatic   -4.4813221 -1.870450
#wt:amManual      -8.0864869 -4.111900
#amAutomatic:qsec  0.2999593  1.367612
#amManual:qsec     0.8939972  1.998754


## Step 5: Residuals
png("/residuals.png", height = 800, width = 800)
par(mfrow = c(2, 2))
plot(model2)
dev.off()
