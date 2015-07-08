plot.image = function(b,nrow=50) {
m = matrix(b,nrow=nrow)
m[m<0] = 0
m[m>1] = 1
image(m,col=gray.colors(30),zlim=c(0,1))
}

plot.cv = function(cv,se,lam,i1,i2,...) {
plot(log(lam),cv,type="l",ylim=range(c(cv-se,cv+se)),
xlab=expression(paste(log(lambda))),ylab="CV error",...)
points(log(lam),cv,pch=20,cex=0.7)
segments(log(lam),cv-se,log(lam),cv+se)
abline(v=log(lam[i1]),lty=2)
abline(v=log(lam[i2]),lty=3)
legend("topleft",lty=c(2,3),
legend=c(sprintf("Usual rule: %.3f",lam[i1]),
sprintf("One SE rule: %.3f",lam[i2])))
}


