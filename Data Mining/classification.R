# Linear discriminant analysis procedure summary:
# 1. Compute the sample estimates pi_j, mu_j, sigma
# 2. Make 2 transformation: 1st, sphere the data points based on factoring sigma;
# 2nd, project down to the affine subspace spanned by the sphered centroids.
# 3. Given any point x, transform x and classify according to the class j = 1,...K
# to the nearest centroid in the transformed space

# Example: olive oil data

library(classifly)
data(olives)

y = as.numeric(olives[,1])
x = as.matrix(olives[,3:10])

n = nrow(x)
##572
p = ncol(x)
##8

# Create a 1x3 vector
pi = numeric(3)
# Create a 3x8 matrix
mu = matrix(0,3,8)

for (j in 1:3) {
	pi[j] = sum(y==j)/n
	mu[j,] = colMeans(x[y==j,])
}

# Create a 8x8 matrix
Sigma = matrix(0,p,p)
for (j in 1:3) {
	A = scale(x[y==j,],center=T,scale=F)
	Sigma = Sigma + t(A)%*%A
}
Sigma = Sigma/(n-3)
Sigma
##               palmitic palmitoleic     stearic       oleic    linoleic  linolenic  arachidic eicosenoic
##palmitic     15311.4209   3634.7384 -1039.30964 -28819.2688  11167.3648 -118.41745  131.22925 -251.53367
##palmitoleic   3634.7384   1672.9936  -420.38457  -9583.1683   5329.9183 -174.82956 -137.08275 -104.37080
##stearic      -1039.3096   -420.3846  1352.69813   1421.4234  -1538.3061   12.34048  -15.52241   74.19826
##oleic       -28819.2688  -9583.1683  1421.42337  78005.3120 -43704.7659  844.13764  467.53724  613.95495
##linoleic     11167.3648   5329.9183 -1538.30610 -43704.7659  32476.9455 -868.24813 -959.88373 -455.36823
##linolenic     -118.4174   -174.8296    12.34048    844.1376   -868.2481  116.08616  117.67382   17.08601
##arachidic      131.2293   -137.0828   -15.52241    467.5372   -959.8837  117.67382  321.82857   30.18824
##eicosenoic    -251.5337   -104.3708    74.19826    613.9550   -455.3682   17.08601   30.18824   40.08443

Sigmainv = solve(Sigma)
Sigmainv
##               palmitic palmitoleic     stearic       oleic    linoleic    linolenic     arachidic    eicosenoic
##palmitic    0.002296343 0.001499165 0.002112504 0.002010322 0.001916856  0.002038265  0.0015404745  0.0033586793
##palmitoleic 0.001499165 0.003745381 0.001982654 0.001733359 0.001475833  0.003534244  0.0013742005  0.0031648544
##stearic     0.002112504 0.001982654 0.002967828 0.001989830 0.001912954  0.002532201  0.0018555196  0.0017024497
##oleic       0.002010322 0.001733359 0.001989830 0.001886356 0.001803068  0.002190251  0.0015777985  0.0029139094
##linoleic    0.001916856 0.001475833 0.001912954 0.001803068 0.001794789  0.002261986  0.0015685223  0.0029572015
##linolenic   0.002038265 0.003534244 0.002532201 0.002190251 0.002261986  0.019281297 -0.0030181181  0.0035093784
##arachidic   0.001540474 0.001374201 0.001855520 0.001577798 0.001568522 -0.003018118  0.0066692836 -0.0002738482
##eicosenoic  0.003358679 0.003164854 0.001702450 0.002913909 0.002957201  0.003509378 -0.0002738482  0.0387864485


lda.classify = function(x0, pi, mu, Sigmainv) {
	K = length(pi)
	delta = numeric(K)
	for (j in 1:K) {
		delta[j] = t(x0) %*% Sigmainv %*% mu[j,] - 
		0.5 * t(mu[j,]) %*% Sigmainv %*% mu[j,] + log(pi[j])
	}
	return(which.max(delta))
}

# Create a 1x572 vector that maps the region (1-3)
yhat = numeric(n)
for (i in 1:n) {
	yhat[i] = lda.classify(x[i,],pi,mu,Sigmainv)
}

# Training errors
sum(yhat!=y)
##[1] 5


library(MASS)
a = lda(x,y)

# Visualize the LDA classification rule, ie. the decision boundaries here
# In terms of lecture notation, this is A^T 
at = a$scaling
z = x %*% at

cols = c("red","darkgreen","blue")
plot(z,col=cols[y])
legend("bottomleft",pch=21,col=cols,
       legend=c("Region 1","Region 2","Region 3"))

# We've reduced the problem to only looking at 2, rather than 8 dimensions! Plus, now the decision 
# boundaries are pretty easy to draw, because it's essentially nearest centroid classification

mu = a$means %*% at
mu
##         LD1      LD2
##1 -14.906391 57.28609
##2  -9.057370 54.41873
##3  -9.034536 59.13423

pi = a$prior
pi
##        1         2         3 
##0.5646853 0.1713287 0.2639860 

# Plot the centroid points in the 3 clusters
points(mu,col=cols,pch=19,cex=2)
points(mu,pch=21,cex=2)

getab = function(j,k,mu,pi) {
	b = (mu[k,1]-mu[j,1])/(mu[j,2]-mu[k,2])
	normj = sum(mu[j,]^2)
	normk = sum(mu[k,]^2)
	a = (log(pi[k]/pi[j]) + 0.5*(normj-normk))/(mu[j,2]-mu[k,2])
	return(list(a=a,b=b))
}

# Plot the 3 decision boundaries, using the formula that we derived
# 1 and 2
ab12 = getab(1,2,mu,pi)
abline(a=ab12$a,b=ab12$b,lty=1)

# 1 and 3
ab13 = getab(1,3,mu,pi)
abline(a=ab13$a,b=ab13$b,lty=2)

# 2 and 3
ab23 = getab(2,3,mu,pi)
abline(a=ab23$a,b=ab23$b,lty=3)

## Find points of intersection
zx = (ab12$a-ab13$a)/(ab13$b-ab12$b)
zy = ab12$a + ab12$b*zx

# Now redraw with the appropriate boundaries
plot(z,col=cols[y])
legend("bottomleft",pch=21,col=cols,
       legend=c("Region 1","Region 2","Region 3"))
points(mu,col=cols,pch=19,cex=2)
points(mu,pch=21,cex=2)

segments(zx,zy,-15,ab12$a+ab12$b*(-15))
segments(zx,zy,-7,ab23$a+ab23$b*(-7))
segments(zx,zy,-15,ab13$a+ab13$b*(-15))
