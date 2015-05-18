# Author: Chisheng Li
# 
# If 2 people have the same FICO score, can the other variables explain a difference 
# in interest rate between them?

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
## Univariate plots of each variable
# 1) Histogram of Amount requested
png("/AmountRequest.png", height=600, width=600)
hist(loans$Amount.Requested, main = "Histogram of Amount Requested", xlab = "Amount Requested")
summary(loans$Amount.Requested)
dev.off()

# 2) Histogram of Amount funded
png("/AmountFunded.png", height=600, width=600)
hist(loans$Amount.Requested, main = "Histogram of Amount Funded by Investors", xlab = "Amount Funded")
summary(loans$Amount.Funded.By.Investors)
dev.off()

# 3) Histogram of Interest rate
png("/InterestRate.png", height=600, width=600)
hist(loans$Interest.Rate, main = "Histogram of Interest Rate", xlab = "Interest Rate (%)")
summary(loans$Interest.Rate)
dev.off()

# 4) Histogram of Loan length
png("/LoanLength.png", height=600, width=600)
hist(loans$Loan.Length, main = "Histogram of Loan Length", xlab = "Months")
summary(loans$Loan.Length)
dev.off()

# 5) Histogram of Debt to income ratio
png("/DebttoIncomeRatio.png", height=600, width=600)
hist(loans$Debt.To.Income.Ratio, main = "Histogram of Debt to Income Ratio", xlab = "Debt to Income Ratio (%)")
summary(loans$Debt.To.Income.Ratio)
dev.off()

# 6) Histogram of log Monthly income
png("/logMonthlyIncome.png", height=600, width=600)
hist(log(loans$Monthly.Income), main = "Histogram of log(Monthly Income)", xlab = "Monthly Income (log($))")
summary(log(loans$Monthly.Income))
dev.off()

# 7) Histogram of FICO Score 
png("/FICOScore.png", height=600, width=600)
hist(loans$FICO.Range, main = "Histogram of FICO Score", xlab = "FICO Score")
summary(loans$Open.CREDIT.Lines)
dev.off()

# 8) Histogram of Open credit lines
png("/OpenCreditLines.png", height=600, width=600)
hist(loans$Open.CREDIT.Lines, main = "Histogram of Open Credit Lines", xlab = "Number of Open Credit Lines")
summary(loans$Open.CREDIT.Lines)
dev.off()

# 9) Histogram of log Revolving credit balance
png("/logRevolvingCreditBalance.png", height=600, width=600)
hist(log(loans$Revolving.CREDIT.Balance), main = "Histogram of log(Revolving Credit Balance)", xlab = "Revolving Credit Balance (log($)")
summary(log(loans$Revolving.CREDIT.Balance))
dev.off()

# 10) Histogram of Inquiries in the last 6 months
png("/Inquiries6Months.png", height=600, width=600)
hist(loans$Inquiries.in.the.Last.6.Months, main = "Histogram of Number Inquiries in the last 6 months", xlab = "Number of Inquiries in the last 6 months")
summary(loans$Inquiries.in.the.Last.6.Months)
dev.off()

# 11) Histogram of Employment Length
png("/EmploymentLength.png", height=600, width=600)
hist(loans$Employment.Length, main = "Histogram of Employment Length", xlab = "Years")
summary(loans$Employment.Length)
dev.off()

## Bivariate plots
#Red = 60 months, Black = 36 months

# 1) Interest rate vs Amount Requested, colored by factorized Loan Length
# p-value: < 2.2e-16, higher amount requested correlated to higher interest rate
# Adjusted R-squared:  0.1098
# 60months group towards upper right corner, 36months towards lower left corner
png("/InterestRateAmountRequest.png", height=600, width=600)
plot(loans$Amount.Requested, loans$Interest.Rate, col = loans$Loan.Length.Fac)
summary(lm(Interest.Rate ~ Amount.Requested, data = loans)) 
dev.off()

#Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      1.086e+01  1.482e-01   73.28   <2e-16 ***
#Amount.Requested 1.777e-04  1.011e-05   17.57   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 3.942 on 2496 degrees of freedom
#Multiple R-squared:  0.1101,    Adjusted R-squared:  0.1098 
#F-statistic: 308.8 on 1 and 2496 DF,  p-value: < 2.2e-16

# 2) Interest rate vs Amount Funded, colored by factorized Loan Length
# p-value: < 2.2e-16, higher amount funded correlated to higher interest rate
# Adjusted R-squared:  0.1131
# 60months group towards upper right corner, 36months towards lower left corner
png("/InterestRateAmountFunded.png", height=600, width=600)
plot(loans$Amount.Funded.By.Investors, loans$Interest.Rate, col = loans$Loan.Length.Fac)
summary(lm(Interest.Rate ~ Amount.Funded.By.Investors, data = loans)) 
dev.off()

#Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                1.089e+01  1.453e-01   74.94   <2e-16 ***
#Amount.Funded.By.Investors 1.817e-04  1.017e-05   17.87   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 3.935 on 2496 degrees of freedom
#Multiple R-squared:  0.1135,    Adjusted R-squared:  0.1131 
#F-statistic: 319.4 on 1 and 2496 DF,  p-value: < 2.2e-16

# 3) Interest rate vs numeric Loan Length
png("/InterestRateLoanLength.png", height=600, width=600)
plot(loans$Loan.Length, loans$Interest.Rate)
summary(lm(Interest.Rate ~ Loan.Length, data = loans)) 
dev.off()

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 5.720202   0.323684   17.67   <2e-16 ***
#Loan.Length 0.178121   0.007626   23.36   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 3.786 on 2496 degrees of freedom
#Multiple R-squared:  0.1794,    Adjusted R-squared:  0.179 
#F-statistic: 545.5 on 1 and 2496 DF,  p-value: < 2.2e-16

# 4) Interest rate vs Loan purpose
# Adjusted R-squared: 0.03236, not a significant relationship
png("/InterestRateLoanPurpose.png", height=600, width=600)
plot(loans$Loan.Purpose, loans$Interest.Rate)
summary(lm(Interest.Rate ~ Loan.Purpose, data = loans)) 
dev.off()

#Residual standard error: 4.11 on 2484 degrees of freedom
#Multiple R-squared:  0.0374,    Adjusted R-squared:  0.03236 
#F-statistic: 7.424 on 13 and 2484 DF,  p-value: 1.608e-14

# 5) Interest rate vs Debt To Income Ratio, colored by factorized Loan Length
# p-value: < 2.2e-16, Adjusted R-squared:  0.02927
# Doesn't seem to have significant correlation from the plot
png("/InterestRateDebtToIncomeRatio.png", height=600, width=600)
plot(loans$Debt.To.Income.Ratio, loans$Interest.Rate, col = loans$Loan.Length.Fac)
summary(lm(Interest.Rate ~ Debt.To.Income.Ratio, data = loans)) 
dev.off()

#Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          11.59480    0.18796  61.688   <2e-16 ***
#Debt.To.Income.Ratio  0.09591    0.01098   8.734   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 4.116 on 2496 degrees of freedom
#Multiple R-squared:  0.02965,   Adjusted R-squared:  0.02927 
#F-statistic: 76.28 on 1 and 2496 DF,  p-value: < 2.2e-16

# 6) Interest Rate vs State
# No significant relationship
png("/InterestRateState.png", height=600, width=600)
plot(loans$State, loans$Interest.Rate)
summary(lm(Interest.Rate ~ State, data = loans)) 
dev.off()

#Residual standard error: 4.17 on 2452 degrees of freedom
#Multiple R-squared:  0.02164,   Adjusted R-squared:  0.003688 
#F-statistic: 1.205 on 45 and 2452 DF,  p-value: 0.1654

# 7) Interest Rate vs Home Ownership
# No significant relationship
png("/InterestRateHomeOwnership.png", height=600, width=600)
plot(loans$Home.Ownership, loans$Interest.Rate)
summary(lm(Interest.Rate ~ Home.Ownership, data = loans)) 
dev.off()

#Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          12.7411     0.1229 103.634  < 2e-16 ***
#Home.OwnershipOTHER   3.2969     1.8670   1.766 0.077530 .  
#Home.OwnershipOWN     0.1649     0.3192   0.517 0.605431    
#Home.OwnershipRENT    0.6752     0.1740   3.881 0.000107 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 4.166 on 2494 degrees of freedom
#Multiple R-squared:  0.007133,  Adjusted R-squared:  0.005939 
#F-statistic: 5.972 on 3 and 2494 DF,  p-value: 0.0004708

# 8) Interest rate vs log Monthly Income, colored by factorized Loan Length
# No significant relationship
png("/InterestRateMonthlyIncome.png", height=600, width=600)
plot(log(loans$Monthly.Income), loans$Interest.Rate, col = loans$Loan.Length.Fac)
summary(lm(Interest.Rate ~ log(Monthly.Income), data = loans)) 
dev.off()

#Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          10.1167     1.3616   7.430 1.48e-13 ***
#log(Monthly.Income)   0.3474     0.1599   2.173   0.0298 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 4.175 on 2496 degrees of freedom
#Multiple R-squared:  0.001889,  Adjusted R-squared:  0.001489 
#F-statistic: 4.724 on 1 and 2496 DF,  p-value: 0.02984

# 9) Interest rate vs numeric FICO Score, colored by factorized Loan Length
# Higher FICO score is correlated with lower interest rate. 
# The 60 months individuals generally have lower FICO score and higher 
# interest rate than the 36 months individuals
# Adjusted R-squared:  0.5028, p-value: < 2.2e-16
png("/InterestRateFICORange.png", height=600, width=600)
plot(loans$FICO.Range, loans$Interest.Rate, col = loans$Loan.Length.Fac)
summary(lm(Interest.Rate ~ FICO.Range, data = loans)) 
dev.off()

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 72.838757   1.190658   61.17   <2e-16 ***
#FICO.Range  -0.084675   0.001685  -50.26   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 2.946 on 2496 degrees of freedom
#Multiple R-squared:  0.503,     Adjusted R-squared:  0.5028 
#F-statistic:  2526 on 1 and 2496 DF,  p-value: < 2.2e-16

# 10) Interest rate vs Open Credit Line, colored by factorized Loan Length
# Generally, more open credit line = higher interest rate, very mild relationship
png("/InterestRateOpenCREDIT.png", height=600, width=600)
plot(loans$Open.CREDIT.Lines, loans$Interest.Rate, col = loans$Loan.Length.Fac)
summary(lm(Interest.Rate ~ Open.CREDIT.Lines, data = loans)) 
dev.off()

#Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       12.22719    0.20390   59.97  < 2e-16 ***
#Open.CREDIT.Lines  0.08368    0.01847    4.53 6.17e-06 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 4.162 on 2496 degrees of freedom
#Multiple R-squared:  0.008155,  Adjusted R-squared:  0.007758 
#F-statistic: 20.52 on 1 and 2496 DF,  p-value: 6.169e-06

# 11) Interest rate vs log Revolving Credit Balance, colored by factorized Loan Length
# Note: Use log( + 1) to offset the error from log(0)
# Mild relationship
png("/InterestRateRevolvingCREDIT.png", height=600, width=600)
plot(log(loans$Revolving.CREDIT.Balance + 1), loans$Interest.Rate, col = loans$Loan.Length.Fac)
summary(lm(Interest.Rate ~ log(Revolving.CREDIT.Balance + 1), data = loans)) 
dev.off()

#Coefficients:
#                                  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                        9.80841    0.47629  20.593  < 2e-16 ***
#log(Revolving.CREDIT.Balance + 1)  0.36142    0.05197   6.955  4.5e-12 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 4.139 on 2496 degrees of freedom
#Multiple R-squared:  0.01901,   Adjusted R-squared:  0.01862 
#F-statistic: 48.37 on 1 and 2496 DF,  p-value: 4.498e-12

# 12) Interest rate vs Inquiries in the last 6 months, colored by factorized Loan Length
# Mild relationship, more inquiries somewhat = higher interest rate
png("/InterestRateInquiries.png", height=600, width=600)
plot(loans$Inquiries.in.the.Last.6.Months, loans$Interest.Rate, col = loans$Loan.Length.Fac)
summary(lm(Interest.Rate ~ Inquiries.in.the.Last.6.Months, data = loans)) 
dev.off()

#Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                    12.56392    0.10242  122.67   <2e-16 ***
#Inquiries.in.the.Last.6.Months  0.55879    0.06701    8.34   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 4.122 on 2496 degrees of freedom
#Multiple R-squared:  0.02711,   Adjusted R-squared:  0.02672 
#F-statistic: 69.55 on 1 and 2496 DF,  p-value: < 2.2e-16

# 13) Interest rate vs Employment Length, colored by factorized Loan Length
# No significant relationship
png("/InterestRateEmploymentLength.png", height=600, width=600)
plot(loans$Employment.Length, loans$Interest.Rate, col = loans$Loan.Length.Fac)
summary(lm(Interest.Rate ~ Employment.Length, data = loans)) 
dev.off()

#Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       12.76640    0.14585  87.529   <2e-16 ***
#Employment.Length  0.05815    0.02288   2.542   0.0111 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 4.173 on 2496 degrees of freedom
#Multiple R-squared:  0.002582,  Adjusted R-squared:  0.002182 
#F-statistic: 6.461 on 1 and 2496 DF,  p-value: 0.01109

# 14) Interest rate vs factorized Loan length
# Longer loan length = higher interest rate
png("/InterestRateLoanLengthFac.png", height=600, width=600)
plot(loans$Loan.Length.Fac, loans$Interest.Rate)
summary(lm(Interest.Rate ~ Loan.Length.Fac, data = loans))
dev.off()

#Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       12.13256    0.08573  141.53   <2e-16 ***
#Loan.Length.Fac60  4.27490    0.18303   23.36   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 3.786 on 2496 degrees of freedom
#Multiple R-squared:  0.1794,    Adjusted R-squared:  0.179 
#F-statistic: 545.5 on 1 and 2496 DF,  p-value: < 2.2e-16


# Factors that appear to be significant for further investigation:
# 1) Amount.Requested, 2) Amount.Funded.By.Investors, 3) Loan.Length/Loan.Length.Fac
# 4) FICO.Range, 5) Open.CREDIT.Lines, 6) log(loans$Revolving.CREDIT.Balance + 1) 
# 7) Inquiries.in.the.Last.6.Months


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

# Model 2: includes the log(loans$Revolving.CREDIT.Balance + 1)
model2 <- lm(formula = Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + 
    FICO.Range + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months +
	log(loans$Revolving.CREDIT.Balance + 1), data = loans)
summary(model2)

#Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                              7.029e+01  9.438e-01  74.483  < 2e-16 ***
#Amount.Funded.By.Investors               1.615e-04  6.054e-06  26.669  < 2e-16 ***
#Loan.Length                              1.323e-01  4.487e-03  29.481  < 2e-16 ***
#FICO.Range                              -8.881e-02  1.197e-03 -74.213  < 2e-16 ***
#Open.CREDIT.Lines                       -2.596e-02  9.720e-03  -2.670  0.00762 ** 
#Inquiries.in.the.Last.6.Months           3.691e-01  3.354e-02  11.006  < 2e-16 ***
#log(loans$Revolving.CREDIT.Balance + 1) -2.219e-01  2.838e-02  -7.819  7.8e-15 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 2.027 on 2491 degrees of freedom
#Multiple R-squared:  0.7653,    Adjusted R-squared:  0.7647 
#F-statistic:  1354 on 6 and 2491 DF,  p-value: < 2.2e-16

confint(model2)
#                                                2.5 %        97.5 %
#(Intercept)                             68.4424736323 72.1437124724
#Amount.Funded.By.Investors               0.0001495858  0.0001733292
#Loan.Length                              0.1234765856  0.1410732356
#FICO.Range                              -0.0911608969 -0.0864674425
#Open.CREDIT.Lines                       -0.0450158392 -0.0068962359
#Inquiries.in.the.Last.6.Months           0.3033671170  0.4348982626
#log(loans$Revolving.CREDIT.Balance + 1) -0.2775627362 -0.1662561467

# Model 3: Shorter loan length tends to have smaller amount requested/funded
# Add an interaction term between Amount.Funded.By.Investors and Loan.Length
# No significant increase in adjusted R-squared
model3 <- lm(formula = Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + 
    FICO.Range + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months +
	Amount.Funded.By.Investors:Loan.Length +
	log(loans$Revolving.CREDIT.Balance + 1), data = loans)
summary(model3)

#Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                              7.111e+01  9.750e-01  72.936  < 2e-16 ***
#Amount.Funded.By.Investors               8.711e-05  2.367e-05   3.680 0.000239 ***
#Loan.Length                              1.050e-01  9.504e-03  11.053  < 2e-16 ***
#FICO.Range                              -8.846e-02  1.199e-03 -73.758  < 2e-16 ***
#Open.CREDIT.Lines                       -2.595e-02  9.701e-03  -2.675 0.007524 ** 
#Inquiries.in.the.Last.6.Months           3.685e-01  3.347e-02  11.008  < 2e-16 ***
#log(loans$Revolving.CREDIT.Balance + 1) -2.174e-01  2.836e-02  -7.664 2.57e-14 ***
#Amount.Funded.By.Investors:Loan.Length   1.729e-06  5.322e-07   3.248 0.001176 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 2.023 on 2490 degrees of freedom
#Multiple R-squared:  0.7663,    Adjusted R-squared:  0.7656 
#F-statistic:  1166 on 7 and 2490 DF,  p-value: < 2.2e-16

# Model 4: Shorter loan length tends to have higher FICO score
# Add an interaction term between FICO.Range and Loan.Length
model4 <- lm(formula = Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + 
    FICO.Range + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months +
	Amount.Funded.By.Investors:Loan.Length + FICO.Range:Loan.Length +
	log(loans$Revolving.CREDIT.Balance + 1), data = loans)
summary(model4)

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

# Model 5: How about interaction between FICO score and Amount funded?
model5 <- lm(formula = Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + 
    FICO.Range + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months +
	Amount.Funded.By.Investors:Loan.Length + FICO.Range:Loan.Length +
	Amount.Funded.By.Investors:FICO.Range +
	log(loans$Revolving.CREDIT.Balance + 1), data = loans)
summary(model5)

#Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                              5.749e+01  3.568e+00  16.112  < 2e-16 ***
#Amount.Funded.By.Investors               6.396e-04  1.131e-04   5.655 1.73e-08 ***
#Loan.Length                              2.846e-01  8.877e-02   3.205  0.00137 ** 
#FICO.Range                              -6.922e-02  4.998e-03 -13.850  < 2e-16 ***
#Open.CREDIT.Lines                       -2.935e-02  9.647e-03  -3.042  0.00237 ** 
#Inquiries.in.the.Last.6.Months           3.746e-01  3.328e-02  11.256  < 2e-16 ***
#log(loans$Revolving.CREDIT.Balance + 1) -2.191e-01  2.816e-02  -7.780 1.05e-14 ***
#Amount.Funded.By.Investors:Loan.Length   1.495e-06  5.298e-07   2.822  0.00482 ** 
#Loan.Length:FICO.Range                  -2.516e-04  1.246e-04  -2.019  0.04360 *  
#Amount.Funded.By.Investors:FICO.Range   -7.656e-07  1.533e-07  -4.995 6.28e-07 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 2.008 on 2488 degrees of freedom
#Multiple R-squared:  0.7699,    Adjusted R-squared:  0.7691 
#F-statistic: 924.9 on 9 and 2488 DF,  p-value: < 2.2e-16

# Model 6: Because of certain non-linearity in the relations among the interest rate and other variables, 
# we squared those variables to the regression.
model6 <- lm(formula = Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + 
    FICO.Range + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months +
	Amount.Funded.By.Investors:Loan.Length + FICO.Range:Loan.Length +
	Amount.Funded.By.Investors:FICO.Range + I((Open.CREDIT.Lines)^2) +
	log(loans$Revolving.CREDIT.Balance + 1), data = loans)
summary(model6)

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

# Model 7: Loan.Length:FICO.Range appears to be insignificant coefficient
# What happen if I drop Amount.Funded.By.Investors:Loan.Length and Loan.Length:FICO.Range?
model7 <- lm(formula = Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + 
    FICO.Range + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months +
	Amount.Funded.By.Investors:FICO.Range + I((Open.CREDIT.Lines)^2) + 
	log(loans$Revolving.CREDIT.Balance + 1), data = loans)
summary(model7)

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
anova(lm(Interest.Rate ~ FICO.Range, data = loans), lm(Interest.Rate ~ FICO.Range + Loan.Length, data = loans), model1, model2, model3, model4, model5, model6, model7)
#Analysis of Variance Table
#
#Model 1: Interest.Rate ~ FICO.Range
#Model 2: Interest.Rate ~ FICO.Range + Loan.Length
#Model 3: Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + FICO.Range + 
#    Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months
#Model 4: Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + FICO.Range + 
#    Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months + log(loans$Revolving.CREDIT.Balance + 
#    1)
#Model 5: Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + FICO.Range + 
#    Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months + Amount.Funded.By.Investors:Loan.Length + 
#    log(loans$Revolving.CREDIT.Balance + 1)
#Model 6: Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + FICO.Range + 
#    Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months + Amount.Funded.By.Investors:Loan.Length + 
#    FICO.Range:Loan.Length + log(loans$Revolving.CREDIT.Balance + 
#    1)
#Model 7: Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + FICO.Range + 
#    Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months + Amount.Funded.By.Investors:Loan.Length + 
#    FICO.Range:Loan.Length + Amount.Funded.By.Investors:FICO.Range + 
#    log(loans$Revolving.CREDIT.Balance + 1)
#Model 8: Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + FICO.Range + 
#    Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months + Amount.Funded.By.Investors:Loan.Length + 
#    FICO.Range:Loan.Length + Amount.Funded.By.Investors:FICO.Range + 
#    I((Open.CREDIT.Lines)^2) + log(loans$Revolving.CREDIT.Balance + 
#    1)
#Model 9: Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + FICO.Range + 
#    Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months + Amount.Funded.By.Investors:FICO.Range + 
#    I((Open.CREDIT.Lines)^2) + log(loans$Revolving.CREDIT.Balance + 
#    1)
#  Res.Df     RSS Df Sum of Sq         F    Pr(>F)    
#1   2496 21663.3                                     
#2   2495 13508.2  1    8155.1 2151.9392 < 2.2e-16 ***
#3   2492 10482.3  3    3025.9  266.1521 < 2.2e-16 ***
#4   2491 10231.2  1     251.1   66.2585 6.182e-16 ***
#5   2490 10188.0  1      43.2   11.3930 0.0007485 ***
#6   2489 10130.3  1      57.7   15.2203 9.821e-05 ***
#7   2488 10029.8  1     100.6   26.5431 2.779e-07 ***
#8   2487  9424.9  1     604.9  159.6147 < 2.2e-16 ***
#9   2489  9472.4 -2     -47.5    6.2655 0.0019309 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# My final model is lm(formula = Interest.Rate ~ Amount.Funded.By.Investors + Loan.Length + 
#    FICO.Range + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months + Amount.Funded.By.Investors:FICO.Range + 
#    I((Open.CREDIT.Lines)^2) + log(loans$Revolving.CREDIT.Balance + 1), data = loans)

### Step 5: Residuals
png("/residualsmodel1.png", height = 800, width = 800)
par(mfrow = c(2, 2))
plot(model1)
dev.off()

png("/residualsmodel2.png", height = 800, width = 800)
par(mfrow = c(2, 2))
plot(model2)
dev.off()

png("/residualsmodel3.png", height = 800, width = 800)
par(mfrow = c(2, 2))
plot(model3)
dev.off()

png("/residualsmodel4.png", height = 800, width = 800)
par(mfrow = c(2, 2))
plot(model4)
dev.off()

png("/residualsmodel5.png", height = 800, width = 800)
par(mfrow = c(2, 2))
plot(model5)
dev.off()

png("/residualsmodel6.png", height = 800, width = 800)
par(mfrow = c(2, 2))
plot(model6)
dev.off()

png("/residualsmodel7.png", height = 800, width = 800)
par(mfrow = c(2, 2))
plot(model7)
dev.off()
