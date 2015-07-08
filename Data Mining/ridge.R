# Ridge regression. Example: subset of small  coefficients
# We have n=50, p=30, and sigma^2 = 1. The true model is linear with 10 large
# coefficients (between 0.5 and 1) and 20 small ones (between 0 and 0.3).
set.seed(0)
n = 50
p = 30
x = matrix(rnorm(n*p),nrow=n)

bstar = c(runif(10,0.5,1),runif(20,0,0.3))

mu = as.numeric(x%*%bstar)

par(mar=c(4.5,4.5,0.5,0.5))
hist(bstar,breaks=30,col="gray",main="", xlab="True coefficients")

# We can do better by shrinking the coefficients to reduce variance

# Comparing linear regression to ridge regression
# 1) Linear regression:
# - squared bias = 0.006
# - variance = 0.627
# - prediction error = 1 + 0.006 + 0.627 = 1.633

# 2) Ridge regression, at its best:
# - squared bias = 0.077
# - variance = 0.403
# - prediction error = 1 + 0.077 + 0.403 = 1.48

library(MASS)

set.seed(1)
R = 100
nlam = 60
lam = seq(0,25,length=nlam)

fit.ls = matrix(0,R,n)
fit.rid  = array(0,dim=c(R,nlam,n))
err.ls = numeric(R)
err.rid = matrix(0,R,nlam)

for (i in 1:R) {
	cat(c(i,", "))
	y = mu + rnorm(n)
	ynew = mu + rnorm(n)
	
	a = lm(y~x+0)
	bls = coef(a)
	fit.ls[i,] = x%*%bls
	err.ls[i] = mean((ynew-fit.ls[i,])^2)
	
	aa = lm.ridge(y~x+0,lambda=lam)
	brid = coef(aa)
	fit.rid[i,,] = brid%*%t(x)
	err.rid[i,] = rowMeans(scale(fit.rid[i,,],center=ynew,scale=F)^2)
}

aveerr.ls = mean(err.ls)

aveerr.rid = colMeans(err.rid)

bias.ls = sum((colMeans(fit.ls)-mu)^2)/n
var.ls = sum(apply(fit.ls,2,var))/n

bias.rid = rowSums(scale(apply(fit.rid,2:3,mean),center=mu,scale=F)^2)/n

var.rid = rowSums(apply(fit.rid,2:3,var))/n


mse.ls = bias.ls + var.ls
mse.ls
##[1] 0.6337845
mse.rid = bias.rid + var.rid

prederr.ls = mse.ls + 1
##[1] 1.633785
prederr.rid = mse.rid + 1
##[1] 1.633785 1.602999 1.578955 1.559486 1.543416 1.530024 1.518832 1.509501 1.501776 1.495461 1.490400 1.486465
##[13] 1.483550 1.481562 1.480426 1.480074 1.480447 1.481491 1.483162 1.485417 1.488218 1.491532 1.495327 1.499574
##[25] 1.504249 1.509326 1.514783 1.520601 1.526760 1.533241 1.540029 1.547108 1.554464 1.562082 1.569951 1.578057
##[37] 1.586391 1.594940 1.603694 1.612645 1.621783 1.631099 1.640586 1.650235 1.660039 1.669990 1.680083 1.690311
##[49] 1.700668 1.711147 1.721744 1.732453 1.743268 1.754186 1.765200 1.776308 1.787504 1.798784 1.810145 1.821582

bias.ls
##[1] 0.00647163
var.ls
##[1] 0.6273129
p/n
##[1] 0.6

prederr.ls
##[1] 1.633785
aveerr.ls
##[1] 1.644363

cbind(prederr.rid,aveerr.rid)

par(mar=c(4.5,4.5,0.5,0.5))
plot(lam,prederr.rid,type="l",
xlab="Amount of shrinkage",ylab="Prediction error")
abline(h=prederr.ls,lty=2)
text(c(1,24),c(1.48,1.48),c("Low","High"))
legend("topleft",lty=c(2,1),
legend=c("Linear regression","Ridge regression"))

# Plot the mean squared error 
par(mar=c(4.5,4.5,0.5,0.5))
plot(lam,mse.rid,type="l",ylim=c(0,max(mse.rid)),
xlab=expression(paste(lambda)),ylab="")
lines(lam,bias.rid,col="red")
lines(lam,var.rid,col="blue")
abline(h=mse.ls,lty=2)
legend("bottomright",lty=c(2,1,1,1),
legend=c("Linear MSE","Ridge MSE","Ridge Bias^2","Ridge Var"),
col=c("black","black","red","blue"))

