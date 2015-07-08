set.seed(1)

CD4.data <- read.table("cd4.data", col.names=c("Time", "CD4", "Age", "Packs", "Drugs", "Sex", "Cesd", "ID"))

#sink("regression-out.txt")
attach(CD4.data[c("CD4", "Time")])
fit <- loess(CD4~Time)
f <- function(x) predict(fit, newdata=data.frame(Time=x))

x <- seq(-2, 4, len=500)

png("regression-plots-01.png", width=432, height=432, pointsize=12)
plot(Time, CD4, xlim=c(-2,4), ylim=c(0,3000), main="CD4 counts vs. Time", xlab="Time since zero conversion", ylab="CD4", cex=.6)
lines(x, f(x), col="blue", lwd=2)
dev.off()

make.x <- function(n)
  {
    runif(n, min=-2, max=4)
  }

train.xx <- make.x(500)
test.xx <- make.x(100)

train.y <- 3*f(train.xx) + rnorm(length(train.xx),sd=250)
test.y <- 3*f(test.xx) + rnorm(length(test.xx), sd=250)

train.df <- data.frame(x=train.xx, y=train.y)
test.df <- data.frame(x=test.xx, y=test.y)

png("regression-plots-02.png", width=432, height=432, pointsize=12)
plot(train.xx, train.y, cex=.6, main="CD4 counts vs. Time", xlab="Time since zero conversion", ylab="CD4")
lines(x,3*f(x), col="blue", lwd=2)
dev.off()


lmfit <- lm(y~x, data=train.df)
png("regression-plots-03.png", width=432, height=432, pointsize=12)
plot(train.xx, train.y, cex=.6, main="CD4 counts vs. Time", xlab="Time since zero conversion", ylab="CD4")
abline(lmfit, col="red", lwd=2)
dev.off()


source("knn.R")
knny15 <- knnReg(train.df$x, x, train.df$y, k=15)

png("regression-plots-04.png", width=432, height=432, pointsize=12)
plot(train.xx, train.y, cex=.6, main="CD4 counts vs. Time", xlab="Time since zero conversion", ylab="CD4")
abline(lmfit, col="red", lwd=2)
lines(x, knny15, col="orange", lwd=2)
dev.off()

knny1 <- knnReg(train.df$x, x, train.df$y, k=1)

png("regression-plots-05.png", width=432, height=432, pointsize=12)
plot(train.xx, train.y, cex=.6, main="CD4 counts vs. Time", xlab="Time since zero conversion", ylab="CD4")
abline(lmfit, col="red", lwd=2)
lines(x, knny1, col="purple", lwd=2, lty=2)
dev.off()


png("regression-plots-06.png", width=432, height=432, pointsize=12)
plot(test.xx, test.y, main="CD4 counts vs. Time", xlab="Time since zero conversion", ylab="CD4")
abline(lmfit, col="red", lwd=2)
lines(x, knny15, col="orange", lwd=2)
lines(x, knny1, col="purple", lwd=2 ,lty=2)
dev.off()

rss <- function(f,y) mean((f-y)^2)

lm.train.rss <- rss(predict(lmfit), train.df$y)
knn15.train.rss <- rss(knnReg(train.df$x, train.df$x, train.df$y, k=15), train.df$y)
knn1.train.rss <- rss(knnReg(train.df$x, train.df$x, train.df$y, k=1), train.df$y)

lm.test.rss <- rss(predict(lmfit, newdata=test.df), test.df$y)
knn15.test.rss <- rss(knnReg(train.df$x, test.df$x, train.df$y, k=15), test.df$y)
knn1.test.rss <- rss(knnReg(train.df$x, test.df$x, train.df$y, k=1), test.df$y)

res <- cbind(c("LM", "KNN 15", "KNN 1"),
             sprintf("%.2f\n", c(lm.train.rss, knn15.train.rss, knn1.train.rss)/1e3),
             sprintf("%.2f\n", c(lm.test.rss, knn15.test.rss, knn1.test.rss)/1e3))
print(res)

ks <- ceiling(seq(1,50,len=10))
knnrss <- matrix(0.0, nr=length(ks), nc=2)
for (i in seq_along(ks)) {
  cat(ks[i])
  knnrss[i,1] <- rss(knnReg(train.df$x, train.df$x, train.df$y, k=ks[i]), train.df$y)
  knnrss[i,2] <- rss(knnReg(train.df$x, test.df$x, train.df$y, k=ks[i]), test.df$y)
}

knnrss <- knnrss/1e3

png("regression-plots-07.png", width=432, height=432, pointsize=12)
plot(ks,knnrss[,1], type="b", col="blue", ylim=range(knnrss), xlab="k", ylab="rss")
points(ks, knnrss[,2], type="b", col="red")
legend("topright", c("train rss", "test rss"), lty=1, col=c("blue","red"))
dev.off()

nsim <- 500
ntrain <- 100

x <- replicate(nsim, make.x(ntrain))
true.f <- apply(x, 2, f)
y <- true.f + matrix(rnorm(nsim*ntrain, sd=250), nc=nsim)

simres <- matrix(0.0, nr=nsim, nc=3)

for (j in 1:nsim) {
  df <- data.frame(x=x[,j], y=y[,j])
  lmfit <- lm(y~x, data=df)
  simres[j,1] <- predict(lmfit, newdata=data.frame(x=1))
  simres[j,2] <- knnReg(x[,j], 1, y[,j], k=15)
  simres[j,3] <- knnReg(x[,j], 1, y[,j], k=1)
}

colnames(simres) <- c("linear", "k=15", "k=1")

png("regression-plots-08.png", width=432, height=432, pointsize=12)
boxplot(simres-f(1), ylab="prediction - truth")
dev.off()
