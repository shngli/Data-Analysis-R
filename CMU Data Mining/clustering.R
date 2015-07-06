# K-Means clustering example

# Load kclust.R to compute K-means clustering or K-medoids clustering
source("kclust.R")
set.seed(0)

# Generate 300 random points
x = rbind(matrix(rnorm(2*100,sd=0.2),ncol=2),
scale(matrix(rnorm(2*100,sd=0.3),ncol=2),cent=-c(1,1),scal=F),
scale(matrix(rnorm(2*100,sd=0.2),ncol=2),cent=-c(0,1),scal=F))

# Set 3 initial center points at (0.5,1), (1,0) and (0,0.5)
k = 3
cent.init = rbind(c(0.5,1),c(1,0),c(0,0.5))

# Plot the 300 points and the 3 initial center points
cols = c("red","darkgreen","blue")
plot(x)
points(cent.init,pch=19,cex=2,col=cols)

km1 = kclust(x,centers=cent.init,alg="kmeans")
km2 = kmeans(x,centers=cent.init,alg="Lloyd")
sum(km1$cluster!=km2$cluster)
#[1] 0

# Update the plot and the 3 center points
plot(x,col=cols[km1$cluster])
points(km1$centers,pch=19,cex=2,col=cols)

# Assign cent.old as the initial coordinates
cent.old = cent.init
##     [,1] [,2]
##[1,]  0.5  1.0
##[2,]  1.0  0.0
##[3,]  0.0  0.5

# Plot the 300 points again and the 3 initial center points
plot(x)
points(cent.old,pch=19,cex=2,col=cols)

par(ask=TRUE)

# Update the kmeans in 9 iterations
for (i in 1:km1$iter) {
  # Plot the new clusters
  plot(x,col=cols[km1$cluster.history[i,]],main=paste("Iteration",i))
  points(cent.old,pch=19,cex=2,col=cols) 

  # Plot the new centers
  plot(x,col=cols[km1$cluster.history[i,]],main=paste("Iteration",i))
  cent.new = centfromclust(x,km1$cluster.history[i,],k,alg="kmeans")
  points(cent.new,pch=19,cex=2,col=cols)

  cent.old = cent.new
}


######################################################################################


# Example of chaining and crowding:
# 1. Single linkage (nearest neighbor linkage): the dissimlarity between G,H is the smallest dissimilarity 
# between 2 points in opposite groups
# Single linkage suffers from chaining; in order to merge 2 groups, only need 1 pair of points to be close 
# irrespective of all others, therefore clusters can be too spread out and not compact enough

# 2. Complete linkage (furthest neighbor linkage): the dissimilarity between G,H is the largest dissimilarity
# bwtween 2 points in opposite groups
# Cmplete linkage suffers from crowding; because its score is based on the worst-case dissimilarity between pairs,
# a point can be closer to points in other clusters than to poinsts in its own cluster ie. clusters are compact
# but not far enough apart

# 3. Average linkage: the dissimilarity between G,H is the average dissimilarity over all points in opposite groups
# Average linkages tries to strike a balance; it uses average pairwise dissimilarity so clusters tend to be 
# relatively compact while sufficiently far apart

set.seed(0)
# Generate 60 random points
x = rbind(scale(matrix(rnorm(2*20),ncol=2),cent=c(1,1),scale=F),
scale(matrix(rnorm(2*30),ncol=2),cent=-c(1,1),scale=F))
x = rbind(x,matrix(runif(2*10,min(x),max(x)),ncol=2))
d = dist(x)

plot(x)

tree.sing = hclust(d,method="single")
tree.comp = hclust(d,method="complete")
tree.avg = hclust(d,method="average")

par(mfrow=c(2,3))
plot(tree.sing,labels=F,hang=-1e-10)
plot(tree.comp,labels=F,hang=-1e-10)
plot(tree.avg,labels=F,hang=-1e-10)

labs.sing = cutree(tree.sing,k=3)
labs.comp = cutree(tree.comp,k=3)
labs.avg = cutree(tree.avg,k=3)

cols = c("red","darkgreen","blue")
plot(x,col=cols[labs.sing])
plot(x,col=cols[labs.comp])
plot(x,col=cols[labs.avg])


######################################################################################


# Compare clustering with 3 and 4 groups
cols = c("red","blue","darkgreen","purple")
labs.avg2 = cutree(tree.avg,k=4)

par(mfrow=c(1,2))
plot(x,col=cols[labs.avg],main="3 clusters")
plot(x,col=cols[labs.avg2],main="4 clusters")

table(labs.avg,labs.avg2)
##        labs.avg2
##labs.avg  1  2  3  4
##       1 23  0  0  0
##       2  0  4 27  0
##       3  0  0  0  6


######################################################################################


# Example of centroid linkage and minmax linkage

set.seed(0)
x = rbind(matrix(rnorm(2*20,mean=-1),ncol=2),
matrix(rnorm(2*30,mean=1),ncol=2))
x = rbind(x,matrix(runif(2*10,min(x),max(x)),ncol=2))
d = dist(x)

# Centroid linkage: the distance between the group centroids (ie. group averages)
tree.cent = hclust(d,method="centroid")
labs.cent = cutree(tree.cent,k=3)

# Minimax linkage: the smallest radius encompassing all points in group G and H
#install.packages("protoclust")
library(protoclust)
tree.mm = protoclust(d)
a = protocut(tree.mm,h=2.5)
labs.mm = a$cl
protos = a$protos

par(mfrow=c(2,2))

plot(tree.cent,labels=FALSE,hang=-1e-10)
abline(h=1.25,lty=2)
plot(tree.mm,labels=rep("",60),hang=-1e-10)
abline(h=2.5,lty=2)

plot(x,col=labs.cent+1)
plot(x,col=labs.mm+1)
points(x[protos,],pch=19,cex=2,col=2:4)

