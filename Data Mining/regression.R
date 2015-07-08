# Example of small regression coefficients
# Simulation with n = 50 and p = 30. The entries of of the predictor matrix X ∈ R^(50x30) are all iid N(0,1),
# so overall the variables have low correlation
set.seed(0)
n = 50
p = 30
x = matrix(rnorm(n*p),nrow=n)

bstar = c(runif(10,0.5,1),runif(20,0,0.3))
bstar
## [1] 0.76569446 0.90117477 0.73974759 0.58870078 0.69856667 0.90711350 0.97067854 0.58667497 0.67747040 0.85252002
##[11] 0.28777228 0.12834042 0.04077460 0.24412237 0.24205349 0.27243311 0.21108057 0.13763896 0.02448732 0.21922319
##[21] 0.27980445 0.01078449 0.03211183 0.11891668 0.04568512 0.03393367 0.08584566 0.17430292 0.04248852 0.14832520

mu = as.numeric(x%*%bstar)
mu
## [1] -1.31528284 -0.51718897  5.77417229  2.74977948  1.48187258 -3.90293573 -1.87635013  3.75006840 -3.25567322
##[10]  1.30130233  1.68397650  1.23708070 -3.24193495  2.95460554 -0.06592703  6.76077095 -3.45190257 -0.75407118
##[19] -0.54238041 -2.94195294 -0.24707029  0.67846251 -3.81386753 -4.49622617  2.14063304  5.84375617  1.67928622
##[28]  1.08463546 -7.24875104 -3.40866819 -2.54661736 -0.94318992 -0.68134865  1.05901970  1.62400831  0.91592834
##[37]  3.62379032 -0.94495632 -2.33445442 -1.76434725  7.56625750  1.72500422  0.18474592 -2.41036754 -3.03533440
##[46]  0.34688310 -3.99332347 -1.24567011  1.33595544 -2.20657722


# Plot the histogram of the true regression coefficients beta* ∈ R^30
# Here 10 coefficients are large (between 0.5 and 1) and 20 coefficients are small (between 0 and 0.3)
par(mar=c(4.5,4.5,0.5,0.5))
hist(bstar,breaks=30,col="gray",main="",
xlab="True coefficients")


# We repeat the following 100 times:
# - Generate a response vector y
# - Compute the linear regression fit X*beta_hat
# - Generate a new response y'
# - Record the error
set.seed(1)
R = 100

fit = matrix(0,R,n)
err = numeric(R)

for (i in 1:R) {
	cat(c(i,", "))
	y = mu + rnorm(n)
	ynew = mu + rnorm(n)
	
	a = lm(y~x+0)
	bls = coef(a)
	fit[i,] = x%*%bls
	err[i] = mean((ynew-fit[i,])^2)
}

# We average this observed error over the 100 repetitions to get an estimate of the prediction error
prederr = mean(err)

# We also estimate the squared bias and variance of the fits X*beta_hat over the 100 repetitions
# Recall that it should be true that prediction error = 1 + squared bias + variance
bias = sum((colMeans(fit)-mu)^2)/n
var = sum(apply(fit,2,var))/n

bias
##[1] 0.00647163
var
##[1] 0.6273129
p/n
##[1] 0.6
1 + bias + var
##[1] 1.633785
prederr
##[1] 1.644363