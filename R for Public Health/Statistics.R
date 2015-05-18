########
# Statistics
# Property Tax data
############

# 1. Using your numeric lot size variable created from module 10,
#	compute the Spearman correlation between lot size and cityTax
#	among those houses that are primary residences
# 	(pick up the data where you left off)
cor(tax2$sqft, tax2$cityTax, method="spearman",use="complete")
### note spearman is rank-based/non-parametric
cor(log10(tax2$sqft), log10(tax2$cityTax), method="spearman",use="complete")
cor(log10(tax2$sqft), tax2$cityTax, method="spearman",use="complete")

# example of using with()
with(tax2, cor(sqft, cityTax, method="spearman",use="complete"))

# 2. Fit a regression model with cityTax as the outcome - what
#	is the coefficient for lot size, as well as its t-statistic
#	and p-value? How much do these metrics change if you 
#	transform the variables (e.g. log10)
f = lm(cityTax ~ sqft, data=tax2)
summary(f)$coef[2,c(1,3,4)]

fl = lm(log10(cityTax+1) ~ log10(sqft+1), data=tax2)
summary(fl)$coef[2,c(1,3,4)]

plot(log10(tax2$sqft+1),log10(tax2$cityTax+1))

###########
#	Download: 'http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/cdystonia.sav'
#	And then place in your working directory

#	load("cdystonia.sav")
#	codebook: http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/Ccdystonia.html

# Randomized to placebo (N=36), 5000 units of BotB (N=36), 
#	10,000 units of BotB (N=37)
# Response variable: total score on Toronto Western Spasmodic Torticollis Rating 
#	Scale (TWSTRS), measuring severity, pain, and disability of cervical dystonia 
#	(high scores mean more	impairment)
# TWSTRS measured at baseline (week 0) and weeks 2, 4, 8, 12, 16 
#	after treatment began

# 3. Visualize data using lattice or ggplot2
library(lattice)
xyplot(twstrs ~ week | site, data=cdystonia,type="l")
xyplot(twstrs ~ week | id, data=cdystonia,type="l")
