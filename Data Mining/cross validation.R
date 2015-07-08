# Example: cross-validation for smoothing splines
# Create folds that respect ordering: sort the inputs x1...xn, and for K-fold
# cross-validation, take:
# Fold 1: points 1, K+1, 2K+1,...
# Fold 2: points 2, K+2, 2K+2,...
# ...
# Fold K: points K, 2K, 3K,...

# We perform a 5-fold cross-validation in ths structured way
# The usual rule selects a model with 27 degrees of freedom
# The one standard error rule selects a model with 9 degrees of freedom

load(file="splines.Rdata")
n = 100
x = 1:n/n

K = 5
folds = vector(mode="list",length=K)
for (k in 1:K) {
	folds[[k]] = seq(k,n,by=K)
}

dfs = 2:30
ndfs = length(dfs)
errs = matrix(0,n,ndfs)
for (k in 1:K) {
	i.tr = unlist(folds[-k])
	i.val = folds[[k]]
	x.tr = x[i.tr]
	y.tr = y[i.tr]
	x.val = x[i.val]
	y.val = y[i.val]
	
	for (j in 1:ndfs) {
		a = smooth.spline(x.tr,y.tr,df=dfs[j])
		yhat = predict(a,x.val)$y
		errs[i.val,j] = (yhat-y.val)^2
	}  
}

cv = colMeans(errs)
cv
## [1] 8.5285698 6.8308987 4.4138757 2.4282283 1.4659507 1.1098875 0.9851361 0.9391298 0.9201703 0.9105233 0.9040889
##[12] 0.8988355 0.8942735 0.8903558 0.8871009 0.8845227 0.8824659 0.8807949 0.8793550 0.8780394 0.8767672 0.8755580
##[23] 0.8744607 0.8735228 0.8729028 0.8726866 0.8730241 0.8739908 0.8756542

errs0 = matrix(0,K,ndfs)
for (k in 1:K) {
	errs0[k,] = colMeans(errs[folds[[k]],])
}
se = apply(errs0,2,sd)/sqrt(K)
se
## [1] 0.48102909 0.48137653 0.41160514 0.28958598 0.18034612 0.11169188 0.07338789 0.05190497 0.03989368 0.03337658
##[11] 0.03047356 0.02992038 0.03089292 0.03290027 0.03560431 0.03876920 0.04222770 0.04582773 0.04945657 0.05300947
##[21] 0.05648101 0.05977284 0.06287960 0.06581927 0.06852624 0.07106618 0.07340025 0.07553753 0.07746662

# Usual rule
i1 = which.min(cv)
i1
##[1] 26

# One standard error rule---note the min!
i2 = min(which(cv<=cv[i1]+se[i1]))
i2
##[1] 8

plot(dfs,cv,type="l",ylim=range(c(cv-se,cv+se)))
points(dfs,cv,pch=20)
lines(dfs,cv-se,lty=3)
lines(dfs,cv+se,lty=3)
abline(v=dfs[i1],col="red",lty=2)
abline(v=dfs[i2],col="blue",lty=2)

yhat1 = smooth.spline(x,y,df=dfs[i1])$y
yhat2 = smooth.spline(x,y,df=dfs[i2])$y

plot(x,y)
lines(x,yhat1,col="red",lwd=2)
lines(x,yhat2,col="blue",lwd=2)
