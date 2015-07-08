library(MASS)
library(RColorBrewer)
library(class)

mycols <- brewer.pal(8, "Dark2")[c(3,2)]

#sink("classification-out.txt")

#########
# Plots showing decision boundaries
s <- sqrt(1/5)
set.seed(30)

makeX <- function(M, n=100, sigma=diag(2)*s) {
  z <- sample(1:nrow(M), n, replace=TRUE)
  m <- M[z,]
  return(t(apply(m,1,function(mu) mvrnorm(1,mu,sigma))))
}

M0 <- mvrnorm(10, c(1,0), diag(2)) # generate 10 means

# generate 100 values
x0 <- makeX(M0) ## the final values for y0=blue

# generate 10 means
M1 <- mvrnorm(10, c(0,1), diag(2))

x1 <- makeX(M1)

# row bind x0 and x1 to 200 rows
x <- rbind(x0, x1)
# create 100 0's and 100 1's
y <- c(rep(0,100), rep(1,100))
cols <- mycols[y+1]

GS <- 75 # put data in a Gs x Gs grid
XLIM <- range(x[,1])
##[1] -3.926215  4.020635

# generate 75 values
tmpx <- seq(XLIM[1], XLIM[2], len=GS)

YLIM <- range(x[,2])
##[1] -2.665980  3.762865

# generate 75 values
tmpy <- seq(YLIM[1], YLIM[2], len=GS)

# generate 5625 (75*75) Var1 and Var2 values
newx <- expand.grid(tmpx, tmpy)


# linear regression

# these are the covariates
X1 <- x[,1]
X2 <- x[,2]
linear.fit <- lm(y~X1+X2)
##Call:
##lm(formula = y ~ X1 + X2)
##
##Coefficients:
##(Intercept)           X1           X2  
##    0.63042     -0.15790      0.06369 

m <- -linear.fit$coef[2]/linear.fit$coef[3]
##      X1 
##2.479296
b <- (0.5 - linear.fit$coef[1])/linear.fit$coef[3]
##(Intercept) 
##  -2.047804 

# prediction on grid
yhat <- predict(linear.fit, newdata=data.frame(X1=newx[,1],X2=newx[,2])); yhat <- as.numeric(yhat>0.5)
colshat <- mycols[as.numeric(yhat>0.5)+1]


#### MAKE PNG HERE
png("classification-plots-01.png", width=864, height=864, pointsize=12)
par(mfrow=c(2,2), mgp=c(1.25,.5,0), mar=c(2.25, 2.1, 1,1))

plot(x,col=cols,xlab="X1",ylab="X2",xlim=XLIM,ylim=YLIM,type="n")
abline(b,m)
points(newx,col=colshat,pch=".")
points(x,col=cols)
title("Linear regression")

# KNN (1)
yhat <- knn(x, newx, y, k=1)
colshat <- mycols[as.numeric(yhat)]

plot(x, xlab="X1", ylab="X2", xlim=XLIM, ylim=YLIM, type="n")
points(newx, col=colshat, pch=".")
contour(tmpx, tmpy, matrix(as.numeric(yhat),GS,GS), levels=c(1,2), add=TRUE, drawlabels=FALSE)
points(x, col=cols)
title("KNN (1)")

# KNN (15)
yhat <- knn(x, newx, y, k=15)
colshat <- mycols[as.numeric(yhat)]

plot(x, xlab="X1", ylab="X2", xlim=XLIM, ylim=YLIM, type="n")
points(newx, col=colshat, pch=".")
contour(tmpx, tmpy, matrix(as.numeric(yhat),GS,GS), levels=c(1,2), add=TRUE, drawlabels=FALSE)
points(x, col=cols)
title("KNN (15)")

# Bayes classifier
# probability of Y given X
p <- function(x) {
  p0 <- mean(dnorm(x[1], M0[,1], s) * dnorm(x[2], M0[,2], s))
  p1 <- mean(dnorm(x[1], M1[,1], s) * dnorm(x[2], M1[,2], s))
  p1/(p0+p1)
}

bayesrule <- apply(newx, 1, p)
colshat <- mycols[as.numeric(bayesrule>0.5)+1]

plot(x, xlab="X1", ylab="X2", xlim=XLIM, ylim=YLIM, type="n")
points(newx, col=colshat, pch=".")
contour(tmpx, tmpy, matrix(round(bayesrule),GS,GS), levels=c(1,2), add=TRUE, drawlabels=FALSE)
points(x, col=cols)
title("Bayes rule")
dev.off()

##################
# Plots showing linear regression boundaries
s <- sqrt(1/500)
s1 <- sqrt(1/350)
mycols <- brewer.pal(8, "Dark2")[c(6,3,2)]

set.seed(30)
GS <- 300
N <- 333

M1 <- mvrnorm(10, c(-1/2,0), matrix(c(3*s1,0,0,s1),2,2))
M1[1,] <- c(0,1.5)
M1[2,] <- c(0,.5)
M1[3,] <- c(-.5,.2)
x1 <- makeX(M1, n=N, sigma=diag(2)*s)

M2 <- mvrnorm(10, c(-1,1), diag(2)*s1)
x2 <- makeX(M2, n=N, sigma=diag(2)*s)

M3 <- mvrnorm(10, c(1,.5), matrix(c(s1/10, 0,0, s1*4),2,2))
M3[1,] <- c(0.5,-0.25)
M3[2,] <- c(0.4,1.75)
x3 <- makeX(M3, n=N, sigma=diag(2)*s)

X <- rbind(x1,x2,x3)
Index <- rep(1:3, c(N,N,N))
X1 <- X[,1]
X2 <- X[,2]

XLIM <- range(X1)
tmpx <- seq(XLIM[1], XLIM[2], len=GS)
YLIM <- range(X2)
tmpy <- seq(YLIM[1], YLIM[2], len=GS)
newx <- as.matrix(expand.grid(tmpx,tmpy))

Y <- t(sapply(Index, function(x) as.numeric((1:3)%in% x)))

# linear regression on indicator matrix
B <- apply(Y, 2, function(y) lm(y~X1+X2)$coef)
GridY <- cbind(1,newx) %*% B
GridG <- apply(GridY, 1, which.max)

png("classification-plots-02.png", width=864, height=432, pointsize=12)
par(mfrow=c(1,2), mgp=c(1.25,.5,0), mar=c(2.25, 2.1, 1,1))
plot(X1, X2, col=mycols[Index], pch=as.character(Index))
contour(tmpx, tmpy, matrix(GridG, GS, GS), levels=c(1,2,3), add=TRUE, drawlabels=FALSE)

# linear regression with quadratic terms
X3 <- X1^2
X4 <- X2^2
X5 <- X1*X2

B <- apply(Y, 2, function(y) lm(y~X1+X2+X3+X4+X5)$coef)
GridY <- cbind(1,newx,newx^2,newx[,1]*newx[,2]) %*% B
GridG <- apply(GridY, 1, which.max)

plot(X1, X2, col=mycols[Index], pch=as.character(Index))
contour(tmpx, tmpy, matrix(GridG,GS,GS), levels=c(1,2), add=TRUE, drawlabels=FALSE)
dev.off()

###
# plots showing lda and qda decision boundaries
Y <- as.factor(Index)

# lda
fit1 <- lda(Y~X1+X2)
Ghat <- as.numeric(predict(fit1, newdata=data.frame(X1=newx[,1], X2=newx[,2]))$class)

png("classification-plots-07.png", width=864, height=864, pointsize=12)
par(mfrow=c(2,2), mgp=c(1.25,.5,0), mar=c(2.25, 2.1, 1,1))

plot(X1,X2, col=mycols[Index], pch=as.character(Index),cex=1.25)
contour(tmpx, tmpy, matrix(Ghat,GS,GS), levels=c(1,2), add=TRUE, drawlabels=FALSE)
title("LDA")
      
# lda with quadratic terms
fit2 <- lda(Y~X1+X2+X3+X4+X5)
Ghat <- as.numeric(predict(fit2, newdata=data.frame(X1=newx[,1],X2=newx[,2],X3=newx[,1]^2,X4=newx[,2]^2,X5=newx[,1]*newx[,2]))$class)

plot(X1,X2, col=mycols[Index], pch=as.character(Index), cex=1.25)
contour(tmpx, tmpy, matrix(Ghat,GS,GS), levels=c(1,2), add=TRUE, drawlabels=FALSE)
title("LDA with quadratic terms")

# qda
fit3 <- qda(Y~X1+X2)
Ghat <- as.numeric(predict(fit3, newdata=data.frame(X1=newx[,1],X2=newx[,2]))$class)

plot(X1, X2, col=mycols[Index], pch=as.character(Index), cex=1.25)
contour(tmpx, tmpy, matrix(Ghat, GS, GS), levels=c(1,2), add=TRUE, drawlabels=FALSE)
title("QDA")
dev.off()

######
# Figure showing problem with indicator matrix regression
N <- 300
Gs <- 300

set.seed(3)
sigma <- 0.10
rho <- -0.15
Index <- sample(1:3, N, replace=TRUE)
mx <- c(-1,0,1)[Index]
X <- t(sapply(mx, function(mu)
              mvrnorm(1, c(mu,mu), Sigma=sigma*matrix(c(1,rho,rho,1),2,2))))

X1 <- X[,1]
X2 <- X[,2]
Y <- t(sapply(Index, function(x) as.numeric(1:3 %in% x)))

XLIM <- range(X1)
tmpx <- seq(XLIM[1], XLIM[2], len=GS)

YLIM <- range(X2)
tmpy <- seq(YLIM[1], YLIM[2], len=GS)

newx <- as.matrix(expand.grid(tmpx, tmpy))

B <- apply(Y, 2, function(y) lm(y~X1+X2)$coef)
Yhat1 <- cbind(1,X) %*% B

GridY <- cbind(1,newx) %*% B
GridG <- apply(GridY, 1, which.max)

png("classification-plots-03.png", width=864, height=432, pointsize=12)
par(mfrow=c(1,2), mgp=c(1.25,.5,0), mar=c(2.25, 2.1, 1,1))

plot(X1, X2, col=mycols[Index], pch=as.character(Index))
contour(tmpx, tmpy, matrix(GridG, GS, GS), levels=c(1,2,3), add=TRUE, drawlabels=FALSE)

X3 <- X[,1]^2
X4 <- X[,2]^2
X5 <- X[,1] * X[,2]

B <- apply(Y, 2, function(y) lm(y~X1+X2+X3+X4+X5)$coef)
Yhat2 <- cbind(1,X,X^2,X[,1]*X[,2]) %*% B

GridY <- cbind(1,newx,newx^2,newx[,1]*newx[,2]) %*% B
GridG <- apply(GridY, 1, which.max)

plot(X1, X2, col=mycols[Index], pch=as.character(Index))
contour(tmpx, tmpy, matrix(GridG, GS, GS), levels=c(1,2,3), add=TRUE, drawlabels=FALSE)
dev.off()

png("classification-plots-04.png", width=864, height=432, pointsize=12)
par(mfrow=c(1,2), mgp=c(1.25,.5,0), mar=c(2.25, 2.1, 1,1))

U1 <- X %*% c(1/sqrt(2), 1/sqrt(2))
matplot(U1, Yhat1, col=mycols)
for (i in 1:3)
  rug(U1[Index==i], col=mycols[i])

matplot(U1, Yhat2, col=mycols)
for (i in 1:3)
  rug(U1[Index==i], col=mycols[i])
dev.off()

#####################
# vowel data

library(ElemStatLearn)
data(vowel.train)

mycols2 <- brewer.pal(11, "Paired")

png("classification-plots-05.png", width=432, height=432, pointsize=12)
par(mfrow=c(1,1), mgp=c(1.25,.5,0), mar=c(2.25, 2.1, 1,1))

plot(vowel.train$x.1, vowel.train$x.2, type="n", xlab="First Coordinate", ylab="Second Coordinate")
text(vowel.train$x.1, vowel.train$x.2, as.character(vowel.train$y), col=mycols2[vowel.train$y], cex=1.2)
means <- apply(vowel.train[c("x.1","x.2")], 2, function(x) tapply(x, vowel.train$y, mean,simplify=TRUE))
points(means, col=mycols2, pch=16, cex=2)
dev.off()

########################
# lda plots

sigma <- .15
rho <- 0.5
Sigma <- matrix(c(1,rho,rho,1),2,2)*sigma
mu <- matrix(c(-1/sqrt(2),0,1/sqrt(2),0,1,0),3,2)

XLIM <- c(-2,2)
YLIM <- c(-2,2)

tmpx <- seq(XLIM[1], XLIM[2], len=GS)
tmpy <- seq(YLIM[1], YLIM[2], len=GS)
newx <- as.matrix(expand.grid(tmpx, tmpy))

iSigma <- solve(Sigma)
dkx <- newx %*% iSigma %*% t(mu)
tmp <- diag(0.5 * mu %*% iSigma %*% t(mu)) + log(1/3)
Yhat <- sweep(dkx, 2, tmp)
Ghat <- apply(Yhat, 1, which.max)

library(ellipse)
png("classification-plots-06.png", width=864, height=432, pointsize=12)
par(mfrow=c(1,2), mgp=c(1.25,.5,0), mar=c(2.25, 2.1, 1,1))

plot(0,0, type="n", ylim=YLIM, xlim=XLIM, xlab="", ylab="")
for (i in 1:3) {
  lines(ellipse(Sigma, centre=mu[i,]), col=mycols[i], lwd=2)
}
points(mu, pch=3)
contour(tmpx, tmpy, matrix(Ghat, GS,GS), levels=c(1,2,3), add=TRUE, drawlabels=FALSE)

K <- 3
X <- t(apply(mu[Index,], 1, function(mu) mvrnorm(1, c(mu[1],mu[2]), Sigma=Sigma)))

muhat <- t(sapply(1:3, function(i) colMeans(X[Index==i,])))
Sigmahat <- sapply(1:3, function(i) {
  X <- sweep(X[Index==i,],2,muhat[i,])
  cov(X)
})
Sigmahat <- matrix(rowMeans(Sigmahat), 2,2)

plot(X, col=mycols[Index], pch=as.character(Index), xlab="", ylab="")
contour(tmpx, tmpy, matrix(Ghat, GS,GS), levels=c(1,2,3), add=TRUE, drawlabels=FALSE, lty=2)

iSigma <- solve(Sigmahat)
dkx <- newx %*% iSigma %*% t(muhat)
tmp <- diag(0.5 * muhat %*% iSigma %*% t(muhat)) + log(1/3)
Yhat <- sweep(dkx, 2, tmp)
Ghat <- apply(Yhat, 1, which.max)

contour(tmpx, tmpy, matrix(Ghat, GS,GS), levels=c(1,2,3), add=TRUE, drawlabels=FALSE)
dev.off()

######
# separating hyperplanes

s <- sqrt(1/5)
set.seed(23)

M0 <- mvrnorm(3, c(1,1), diag(2))
x0 <- makeX(M0, n=10, sigma=diag(2)*s)

M1 <- mvrnorm(3, c(-1,-1), diag(2))
x1 <- makeX(M1, n=10, sigma=diag(2)*s)

mycols <- brewer.pal(8, "Dark2")[c(3,2)]

x <- rbind(x0, x1)
y <- c(rep(-1,10), rep(1,10))
cols <- mycols[y/2+1.5]

source("rosenblatt.R")
beta1 <- rosen(cbind(1,x), y, beta0=1)
beta2 <- rosen(cbind(1,x), y, beta0=2)

png("classification-plots-08.png", width=432, height=432, pointsize=12)
par(mgp=c(1.25,.5,0), mar=c(2.25, 2.1, 1,1))

plot(x, col=cols, pch=19)

m <- -beta1[2]/beta1[3]
b <- -beta1[1]/beta1[3]
abline(b,m)

m <- -beta2[2]/beta2[3]
b <- -beta2[1]/beta2[3]
abline(b,m)

X1 <- x[,1]
X2 <- x[,2]
y <- ifelse(y==-1,0,1)

lmfit <- lm(y~X1+X2)
beta3 <- lmfit$coef
m <- -beta3[2]/beta3[3]
b <- (0.5 - beta3[1])/beta3[3]
abline(b,m,col="orange")
dev.off()
