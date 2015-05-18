# Author: Chisheng Li
# 
# Identify and quantify associations between the interest rate of the loan and the other variables 
# in the data set. Consider whether any of these variables have an important association with interest 
# rate after taking into account the applicant's FICO score. 

### Step 1: Data manipulation

loans <- read.csv("/loansData.csv",stringsAsFactors=FALSE)
str(loans)

# 1. Convert Interest Rate to numeric variable
loans$Interest.Rate<-as.numeric(gsub("%","",loans$Interest.Rate))

# 2. Convert Loan Length to numeric variable
loans$Loan.Length <- as.numeric(gsub("months","",loans$Loan.Length))
# 3. Factorize Loan Length
loans$Loan.Length.Fac <- as.factor(loans$Loan.Length)

# 4. Factorize Loan Purpose
loans$Loan.Purpose <- as.factor(loans$Loan.Purpose)
# 5. Convert Debt to Income Ratio to numeric variable
loans$Debt.To.Income.Ratio <- as.numeric(gsub("%","",loans$Debt.To.Income.Ratio))
# 6. Factorize State
loans$State <- as.factor(loans$State)
# 7. Factorize Home Ownership
loans$Home.Ownership <- as.factor(loans$Home.Ownership)

# 8. Factorize FICO Range
#loans$FICO.Range.Fac <- as.factor(loans$FICO.Range)
# 9. Create numeric variable for FICO score from the lower limit of each range
loans$FICO.Range <- as.numeric(substr(loans$FICO.Range,1,3))

# 10. Convert Employment Length to numeric
#loans$Employment.Length <- as.factor(loans$Employment.Length)
# Employment length that is listed as "< 1 year" or "n/a" are changed to 0 
loans$Employment.Length <- as.character(gsub("< 1", "0", loans$Employment.Length))
loans$Employment.Length <- as.character(gsub("n/a", "0", loans$Employment.Length))
# Remove "year" and "years" from the characters
loans$Employment.Length <- as.character(gsub("year|s$", "", loans$Employment.Length))
# Remove trailing white space 
loans$Employment.Length <- as.character(gsub(" ", "", loans$Employment.Length))
# Remove "+" and change "10+" to "10"
loans$Employment.Length <- as.character(gsub("\\+$", "", loans$Employment.Length))
# Change Employment.Length to numeric variable
loans$Employment.Length <- as.numeric(loans$Employment.Length)

# Drop NA values, 2500 observations --> 2498 observations 
# ie. remove 2 observations from this analysis
loans <- na.omit(loans)
str(loans)
summary(loans)


### Step 2: Exploratory Data Analysis and Plotting
# 3 plots to summarize the data set
# See http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
library(ggplot2)

# 1. Interest Rate vs Loan Length
png("/InterestRateLoanLength.png", height = 600, width = 600)
ggplot(loans, aes(Loan.Length.Fac, Interest.Rate)) + geom_boxplot(aes(fill = Loan.Length.Fac)) + 
    ggtitle("Interest Rate (%) by Loan Length (months)") + 
    ylab("Interest Rate (%)") + xlab("Loan Length (months)")
dev.off()

# 2. Interest Rate vs FICO Score, colored by Loan Length, showing 2 different lm lines for 36 months vs 60 months
png("/InterestRateFICOScore.png", height = 600, width = 600)
lm36 <- lm(loans$Interest.Rate[loans$Loan.Length.Fac == '36'] ~ loans$FICO.Range[loans$Loan.Length.Fac == '36'])
lm60 <- lm(loans$Interest.Rate[loans$Loan.Length.Fac == '60'] ~ loans$FICO.Range[loans$Loan.Length.Fac == '60'])
ggplot(loans, aes(x = FICO.Range, y = Interest.Rate, color = Loan.Length.Fac)) + geom_point() +  
    scale_colour_hue(l=50) + geom_smooth(method = lm, se=TRUE) + 
    labs(title = "Interest Rate (%) vs FICO Score (Loan Length colored)") + ylab("Interest Rate (%)") + xlab("FICO Score")
dev.off()

# 3. Interest Rate vs Amount Funded by Investors, colored by FICO Score
png("/InterestRateAmntFunded.png", height = 800, width = 1000)
ggplot(loans, aes(x = Amount.Funded.By.Investors, y = Interest.Rate, color = as.factor(FICO.Range))) + 
    geom_point() +  scale_colour_hue(l=50) + 
    labs(title = "Interest Rate (%) vs Amount Funded by Investors (FICO Score colored)") + 
    ylab("Interest Rate (%)") + xlab("Amount Funded ($)")
dev.off()


### Step 3: Hypothesis testing

# Compare the interest rate between the 36 months and 60 months individual
# H0 = There is no interest rate difference in interest rate between 36 and 60 months
# H1 = There exists an interest rate difference between 36 and 60 months
t.test(Interest.Rate ~ Loan.Length.Fac, data = loans)

#        Welch Two Sample t-test
#
#data:  Interest.Rate by Loan.Length.Fac
#t = -21.9172, df = 808.014, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
# -4.657765 -3.892044
#sample estimates:
#mean in group 36 mean in group 60 
#        12.13256         16.40746 

# P-value (< 2.2e-16) < 0.05, we reject the null hypothesis

# Determine if there is a difference in the sample mean
wilcox.test(Interest.Rate ~ Loan.Length.Fac, data = loans)

#        Wilcoxon rank sum test with continuity correction
#
#data:  Interest.Rate by Loan.Length.Fac
#W = 244522.5, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0

# P-value (< 2.2e-16) < 0.05, we reject the null hypothesis that the interest rate 
# of the 36 months and 60 months individuals are identical


### Step 4: Regression model
# Generate a regression model using a stepwise method
model.all <- lm(Interest.Rate ~ ., data = loans)
n <- nrow(loans)
model1 <- step(model.all, direction = "backward", k = log(n))
summary(model1)

#Call:
#lm(formula = Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + 
#    FICO.Range + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months, 
#    data = loans)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-9.5766 -1.3581 -0.1836  1.2047  9.8968 
#
#Coefficients:
#                                 Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                     6.723e+01  8.686e-01  77.393  < 2e-16 ***
#Amount.Funded.By.Investors      1.505e-04  5.960e-06  25.249  < 2e-16 ***
#Loan.Length                     1.341e-01  4.534e-03  29.577  < 2e-16 ***
#FICO.Range                     -8.693e-02  1.186e-03 -73.279  < 2e-16 ***
#Open.CREDIT.Lines              -4.853e-02  9.392e-03  -5.167 2.56e-07 ***
#Inquiries.in.the.Last.6.Months  3.891e-01  3.384e-02  11.496  < 2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 2.051 on 2492 degrees of freedom
#Multiple R-squared:  0.7595,    Adjusted R-squared:  0.759 
#F-statistic:  1574 on 5 and 2492 DF,  p-value: < 2.2e-16

# The algorithm suggested the model lm(formula = Interest.Rate ~ Amount.Funded.By.Investors 
# + Loan.Length + FICO.Range + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months, data = loans)

# We test several models to try achieve the best regression model

# Model 2: includes the log(loans$Revolving.CREDIT.Balance + 1).
# Also, shorter loan length tends to have smaller amount requested/funded,
# so add an interaction term between Amount.Funded.By.Investors and Loan.Length
# In addition, shorter loan length tends to have higher FICO score, so add
# an interaction term between FICO.Range and Loan.Length
model2 <- lm(formula = Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + 
    FICO.Range + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months +
	Amount.Funded.By.Investors:Loan.Length + FICO.Range:Loan.Length +
	log(loans$Revolving.CREDIT.Balance + 1), data = loans)
summary(model2)

#Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                              5.813e+01  3.583e+00  16.223  < 2e-16 ***
#Amount.Funded.By.Investors               8.700e-05  2.361e-05   3.685 0.000234 ***
#Loan.Length                              4.222e-01  8.479e-02   4.980 6.80e-07 ***
#FICO.Range                              -7.011e-02  5.019e-03 -13.970  < 2e-16 ***
#Open.CREDIT.Lines                       -2.654e-02  9.677e-03  -2.742 0.006146 ** 
#Inquiries.in.the.Last.6.Months           3.658e-01  3.339e-02  10.955  < 2e-16 ***
#log(loans$Revolving.CREDIT.Balance + 1) -2.169e-01  2.829e-02  -7.669 2.47e-14 ***
#Amount.Funded.By.Investors:Loan.Length   1.693e-06  5.308e-07   3.188 0.001448 ** 
#Loan.Length:FICO.Range                  -4.474e-04  1.189e-04  -3.765 0.000171 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 2.017 on 2489 degrees of freedom
#Multiple R-squared:  0.7676,    Adjusted R-squared:  0.7668 
#F-statistic:  1028 on 8 and 2489 DF,  p-value: < 2.2e-16

# Model 3: Consider an interaction between FICO score and Amount funded. 
# Also squared the Open credit line variable.
model3 <- lm(formula = Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + 
    FICO.Range + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months +
	Amount.Funded.By.Investors:Loan.Length + FICO.Range:Loan.Length +
	Amount.Funded.By.Investors:FICO.Range + I((Open.CREDIT.Lines)^2) +
	log(loans$Revolving.CREDIT.Balance + 1), data = loans)
summary(model3)

#Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                              5.931e+01  3.463e+00  17.130  < 2e-16 ***
#Amount.Funded.By.Investors               6.384e-04  1.097e-04   5.822 6.56e-09 ***
#Loan.Length                              2.804e-01  8.607e-02   3.258  0.00114 ** 
#FICO.Range                              -6.980e-02  4.846e-03 -14.403  < 2e-16 ***
#Open.CREDIT.Lines                       -3.982e-01  3.066e-02 -12.989  < 2e-16 ***
#Inquiries.in.the.Last.6.Months           3.695e-01  3.227e-02  11.450  < 2e-16 ***
#I((Open.CREDIT.Lines)^2)                 1.502e-02  1.189e-03  12.634  < 2e-16 ***
#log(loans$Revolving.CREDIT.Balance + 1) -1.712e-01  2.756e-02  -6.211 6.15e-10 ***
#Amount.Funded.By.Investors:Loan.Length   1.496e-06  5.137e-07   2.913  0.00361 ** 
#Loan.Length:FICO.Range                  -2.452e-04  1.208e-04  -2.030  0.04249 *  
#Amount.Funded.By.Investors:FICO.Range   -7.612e-07  1.486e-07  -5.122 3.25e-07 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.947 on 2487 degrees of freedom
#Multiple R-squared:  0.7838,    Adjusted R-squared:  0.7829 
#F-statistic: 901.5 on 10 and 2487 DF,  p-value: < 2.2e-16

# Model 4: Loan.Length:FICO.Range and Amount.Funded.By.Investors:Loan.Length
# appear to have low impact, consider dropping both coefficients.
model4 <- lm(formula = Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + 
    FICO.Range + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months +
	Amount.Funded.By.Investors:FICO.Range + I((Open.CREDIT.Lines)^2) +
	log(loans$Revolving.CREDIT.Balance + 1), data = loans)
summary(model4)

#Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                              6.469e+01  1.468e+00  44.076  < 2e-16 ***
#Amount.Funded.By.Investors               7.933e-04  1.002e-04   7.917 3.63e-15 ***
#Loan.Length                              1.301e-01  4.338e-03  29.980  < 2e-16 ***
#FICO.Range                              -7.868e-02  2.013e-03 -39.090  < 2e-16 ***
#Open.CREDIT.Lines                       -3.987e-01  3.073e-02 -12.975  < 2e-16 ***
#Inquiries.in.the.Last.6.Months           3.728e-01  3.232e-02  11.535  < 2e-16 ***
#I((Open.CREDIT.Lines)^2)                 1.503e-02  1.191e-03  12.615  < 2e-16 ***
#log(loans$Revolving.CREDIT.Balance + 1) -1.756e-01  2.758e-02  -6.366 2.30e-10 ***
#Amount.Funded.By.Investors:FICO.Range   -8.874e-07  1.409e-07  -6.296 3.60e-10 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.951 on 2489 degrees of freedom
#Multiple R-squared:  0.7827,    Adjusted R-squared:  0.782 
#F-statistic:  1121 on 8 and 2489 DF,  p-value: < 2.2e-16

# Anova test to compare the different models
anova(lm(Interest.Rate ~ FICO.Range, data = loans), lm(Interest.Rate ~ FICO.Range + Loan.Length, data = loans), lm(Interest.Rate ~ FICO.Range + Amount.Funded.By.Investors + Loan.Length, data = loans), model1, model2, model3, model4)

#Analysis of Variance Table
#
#Model 1: Interest.Rate ~ FICO.Range
#Model 2: Interest.Rate ~ FICO.Range + Loan.Length
#Model 3: Interest.Rate ~ FICO.Range + Amount.Funded.By.Investors + Loan.Length
#Model 4: Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + FICO.Range + 
#    Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months
#Model 5: Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + FICO.Range + 
#    Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months + Amount.Funded.By.Investors:Loan.Length + 
#    FICO.Range:Loan.Length + log(loans$Revolving.CREDIT.Balance + 
#    1)
#Model 6: Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + FICO.Range + 
#    Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months + Amount.Funded.By.Investors:Loan.Length + 
#    FICO.Range:Loan.Length + Amount.Funded.By.Investors:FICO.Range + 
#    I((Open.CREDIT.Lines)^2) + log(loans$Revolving.CREDIT.Balance + 
#    1)
#Model 7: Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + FICO.Range + 
#    Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months + Amount.Funded.By.Investors:FICO.Range + 
#    I((Open.CREDIT.Lines)^2) + log(loans$Revolving.CREDIT.Balance + 
#    1)
#  Res.Df     RSS Df Sum of Sq         F    Pr(>F)    
#1   2496 21663.3                                     
#2   2495 13508.2  1    8155.1 2151.9392 < 2.2e-16 ***
#3   2494 11099.4  1    2408.8  635.6254 < 2.2e-16 ***
#4   2492 10482.3  2     617.1   81.4155 < 2.2e-16 ***
#5   2489 10130.3  3     352.0   30.9572 < 2.2e-16 ***
#6   2487  9424.9  2     705.5   93.0789 < 2.2e-16 ***
#7   2489  9472.4 -2     -47.5    6.2655  0.001931 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# My final model is lm(formula = Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + 
#    FICO.Range + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months + Amount.Funded.By.Investors:FICO.Range + 
#    I((Open.CREDIT.Lines)^2) + log(loans$Revolving.CREDIT.Balance + 1), data = loans)


### Step 5: Residuals
png("/residualsmodel4.png", height = 800, width = 800)
par(mfrow = c(2, 2))
plot(model4)
dev.off()
