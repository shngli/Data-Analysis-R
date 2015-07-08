# Compute either K-means clustering or K-medoids clustering
#
# Arguments:
# x: data matrix, n observations (rows) by p features (cols).
# centers: a vector giving the starting centers. Defaults
#   to NULL in which case we choose k centers at random.
# k: number of clusters. If the centers argument is specified,
#   then this doesn't need to be specified.
# alg: algorithm. Can be either "kmeans" or "kmedoids". 
#   Defaults to "kmeans".
# maxiter: Maximum number of iterations before we quit. 
#   Defaults to 100.
#
# Returns:
# centers: a vector of length k, giving the final centers.
# cluster: a vector of length n, giving the final clustering
#   assignments.
# iter: number of iterations performed.
# cluster.history: a matrix of dimension iter by k, each row
#   giving the cluster assignments at the corresponding iteration.
# 
kclust = function(x, centers=NULL, k=NULL, alg="kmeans", maxiter=100) {
  n = nrow(x)
  p = ncol(x)
  if (is.null(centers)) {
    if (is.null(k)) stop("Either centers or k must be specified.")
    if (alg=="kmeans") centers = matrix(runif(k*p,min(x),max(x)),nrow=k)
    else centers = x[sample(n,k),]
  }
  k = nrow(centers)
  cluster = matrix(0,nrow=0,ncol=n)

  for (iter in 1:maxiter) {
    cluster.new = clustfromcent(x,centers,k)
    centers.new = centfromclust(x,cluster.new,k,alg)

    cluster = rbind(cluster,cluster.new)
    j = is.na(centers.new[,1])
    if (sum(j)>0) centers.new[j,] = centers[j,]
    centers = centers.new

    if (iter>1 & sum(cluster[iter,]!=cluster[iter-1,])==0) break
  }
  return(list(centers=centers,cluster=cluster[iter,],iter=iter,cluster.history=cluster))
}

# Compute clustering assignments, given centers
clustfromcent = function(x, centers, k) {
  n = nrow(x)
  dist = matrix(0,n,k)
  for (i in 1:k) {
    dist[,i] = colSums((t(x)-centers[i,])^2)
  }
  return(max.col(-dist,ties="first"))
}

# Compute centers, given clustering assignments
centfromclust = function(x, cluster, k, alg) {
  if (alg=="kmeans") return(avgfromclust(x,cluster,k))
  else return(medfromclust(x,cluster,k))
}

avgfromclust = function(x, cluster, k) {
  p = ncol(x)
  centers = matrix(0,k,p)
  for (i in 1:k) {
    j = cluster==i
    if (sum(j)==0) centers[i,] = rep(NA,p)
    else centers[i,] = colMeans(x[j,,drop=FALSE])
  }
  return(centers)
}

medfromclust = function(x, cluster, k) {
  p = ncol(x)
  centers = matrix(0,k,p)
  for (i in 1:k) {
    j = cluster==i
    if (sum(j)==0) centers[i,] = rep(NA,p)
    else {
      d = as.matrix(dist(x[j,],diag=T,upper=T)^2)
      ii = which(j)[which.min(colSums(d))]
      centers[i,] = x[ii,]
    }
  }
  return(centers)
}

# Compute within-cluster variation
wcv = function(x, cluster, centers) {
  k = nrow(centers)
  wcv = 0
  for (i in 1:k) {
    j = cluster==i
    nj = sum(j)
    if (nj>0) {
      wcv = wcv + sum((t(x[j,])-centers[i,])^2)
    }
  }
  return(wcv)
}
