library(RColorBrewer)

### olive

dat=read.table("olive.txt",sep=",")
Y=c("Southern Italy","   Sardinia","Northern Italy")
Y2=c("Southern Italy","Sardinia","Northern Italy")
dat=cbind(dat,region=Y[dat$Region])
dat[,4:11]=dat[,4:11]/100


mycol=brewer.pal(8,"Dark2")
attach(dat)

# plot the olive data
png("tree-plots-02.png", width=432, height=432, pointsize=12)
par(mgp=c(1.25,.5,0), mar=c(2.25, 2.1, 1,1))
par(las=1)
plot(eicosenoic,linoleic,type="n")
points(eicosenoic,linoleic,col=mycol[Region],cex=0.7,pch=15)
legend("topright", Y2,col=mycol,pch=rep(15,3))
dev.off()

# plot olive data with partition
png("tree-plots-03.png", width=432, height=432, pointsize=12)
par(mgp=c(1.25,.5,0), mar=c(2.25, 2.1, 1,1))
par(las=1)
plot(eicosenoic,linoleic,type="n")
points(eicosenoic,linoleic,col=mycol[Region],cex=0.7,pch=15)
abline(v=0.07)
lines(c(-0.10,0.07),rep(10.535,2))
legend("topright",Y2,col=mycol,pch=rep(15,3))
dev.off()

detach(dat)

library(rpart)
rpart1=rpart(region~linoleic+eicosenoic,data=dat)

# plot olive decision tree
png("tree-plots-04.png", width=432, height=432, pointsize=12)
par(mgp=c(1.25,.5,0), mar=c(2.25, 2.1, 1,1))
plot(rpart1,uniform=T,branch=1,compress=T,margin=0.1)
text(rpart1,all=F,use.n=T,fancy=F)
dev.off()

# iris
data(iris)
nms <- levels(iris$Species)

# plot iris data
png("tree-plots-05.png", width=432, height=432, pointsize=12)
par(las=1,xaxs="i",yaxs="i")
par(mgp=c(1.25,.5,0), mar=c(2.25, 2.1, 1,1))
plot(c(0.9,7),c(0,2.6),xlab="petal length",ylab="petal width",type="n")
points(iris[,3:4], col=mycol[iris$Species], cex=0.9, pch=15)
legend("topleft",nms,col=mycol,pch=rep(15,3))
dev.off()


# plot iris data partition
png("tree-plots-06.png", width=432, height=432, pointsize=12)
par(las=1,xaxs="i",yaxs="i")
par(mgp=c(1.25,.5,0), mar=c(2.25, 2.1, 1,1))
plot(c(0.9,7),c(0,2.6),xlab="petal length",ylab="petal width",type="n")
abline(v=2.45)
lines(c(2.45,8),rep(1.75,2))
lines(rep(4.95,2),c(0,1.75))
polygon(c(0.9,2.45,2.45,0.9),c(0,0,2.6,2.6),col=rgb(t(col2rgb(mycol[1])), alpha=60, maxColorValue=255))
polygon(c(2.45,4.95,4.95,2.45),c(0,0,1.75,1.75),col=rgb(t(col2rgb(mycol[2])), alpha=60, maxColorValue=255))
polygon(c(4.95,7,7,4.95),c(0,0,1.75,1.75),col=rgb(t(col2rgb(mycol[3])), alpha=60, maxColorValue=255))
polygon(c(2.45,7,7,2.45),c(1.75,1.75,2.6,2.6),col=rgb(t(col2rgb(mycol[3])), alpha=60, maxColorValue=255))
points(iris[,3:4], col=mycol[iris$Species], cex=0.7, pch=15)
points(iris[,3:4], col=mycol[iris$Species], cex=0.9, pch=0)
legend("topleft",nms,col=mycol,pch=rep(15,3),bg="white")
dev.off()

temp <- rpart.control(cp=0,minsplit=8)
rpart.iris <- rpart(Species~Petal.Length+Petal.Width,data=iris,control=temp)

# plot iris tree
png("tree-plots-07.png", width=432, height=432, pointsize=12)
plot(rpart.iris,uniform=T,branch=1,compress=T,margin=0.1)
text(rpart.iris,all=F,use.n=T,fancy=F)
dev.off()


# trees vs. linear regression comparison
set.seed(1)
x1 <- runif(50)
x2 <- runif(50)

# simulate from linear model
y1 <- 5 + x1 + 1.5*x2 + 0.1 * rnorm(50)

# esimtate with linear model
mydat1 <- data.frame(y1=y1, x1=x1, x2=x2)
lm1 <- lm(y1~x1+x2, data=mydat1)

x <- seq(0,1,0.05)
y <- seq(0,1,0.05)
xy <- expand.grid(x,y)
newdat <- data.frame(y1=0, x1=xy[,1], x2=xy[,2])

# predict with linear model
z1 <- matrix(predict(lm1, newdat), ncol=21)
z1b <- predict(lm1)


library(tree)
# estimate tree
tree1 <- tree(y1~x1+x2,data=mydat1)
tree2 <- prune.tree(tree1,best=3)

# predict with tree
z2 <- matrix(predict(tree2,newdat),ncol=21)
z2b <- predict(tree2)

#simulate from tree model
y2 <- rep(5, 50) + 0.25*rnorm(50)
wh1 <- (1:50)[x1<0.5]
wh2 <- (1:50)[(x1>=0.5) & (x2<0.5)]
y2[wh1] <- y2[wh1] + 2
y2[wh2] <- y2[wh2] - 2

mydat2 <- data.frame(y2=y2, x1=x1, x2=x2)
lm2 <- lm(y2~x1+x2, data=mydat2)

newdat2 <- data.frame(y2=0, x1=xy[,1], x2=xy[,2])

# estimate and predict with linear model
z3 <- matrix(predict(lm2, newdat2), ncol=21)
z3b <- predict(lm2)


# estimate tree
tree3 <- tree(y2~x1+x2, data=mydat2)
tree4 <- prune.tree(tree3, best=3)

# predict from tree
z4 <- matrix(predict(tree4, newdat2), ncol=21)
z4b <- predict(tree4)

# now plot
png("tree-plots-10.png", width=432, height=432, pointsize=12)
par(mfrow=c(2,2), mar=rep(2,4))
par(mgp=c(1.25,.5,0))
# linear data, linear predictor
theplot <- persp(x,y,z1, theta=30, phi=30, expand=0.8, col="lightblue", xlab="x", ylab="y", zlab="z", zlim=range(c(mydat1$y1, z1, z1b)))
aa <- trans3d(mydat1[,2], mydat1[,3], mydat1[,1], theplot)
bb <- trans3d(mydat1[,2], mydat1[,3], z1b, theplot)
for (j in 1:50) lines(c(aa$x[j], bb$x[j]), c(aa$y[j], bb$y[j]), lwd=2)
points(aa, col="red", pch=16)
points(bb, col="green", pch=16)

# linear data, tree predictor
theplot <- persp(x, y, z2, theta=30, phi=30, expand=0.8, col="lightblue", xlab="x", ylab="y", zlab="z", zlim=range(c(mydat1$y1, z1, z1b, z2, z2b)))
aa <- trans3d(mydat1[,2], mydat1[,3], mydat1[,1], theplot)
bb <- trans3d(mydat1[,2], mydat1[,3], z2b, theplot)
for (j in 1:50) lines(c(aa$x[j], bb$x[j]), c(aa$y[j], bb$y[j]), lwd=2)
points(aa, col="red", pch=16)
points(bb, col="green", pch=16)

# tree data, linear predictor
theplot <- persp(x, y, z3, theta=30, phi=30, expand=0.8, col="lightblue", xlab="x", ylab="y", zlab="z", zlim=range(c(mydat2$y2, z3, z3b)))
aa <- trans3d(mydat2[,2], mydat2[,3], mydat2[,1], theplot)
bb <- trans3d(mydat2[,2], mydat2[,3], z3b, theplot)
for (j in 1:50) lines(c(aa$x[j], bb$x[j]), c(aa$y[j], bb$y[j]), lwd=2)
points(aa, col="red", pch=16)
points(bb, col="green", pch=16)

# tree data, tree predictor
theplot <- persp(x, y, z4, theta=30, phi=30, expand=0.8, col="lightblue", xlab="x", ylab="y", zlab="z", zlim=range(c(mydat2$y2, z4, z4b)))
aa <- trans3d(mydat2[,2], mydat2[,3], mydat2[,1], theplot)
bb <- trans3d(mydat2[,2], mydat2[,3], z4b, theplot)
for (j in 1:50) lines(c(aa$x[j], bb$x[j]), c(aa$y[j], bb$y[j]), lwd=2)
points(aa, col="red", pch=16)
points(bb, col="green", pch=16)
dev.off()

