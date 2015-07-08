knnReg <- function(train, test, y, k=3)
  {
    ntrain <- length(train)
    if (length(y) != ntrain)
      stop("train and y must be of the same length")

    if (ntrain < k)
      stop("not enough training points")
    
    ntest <- length(test)
    
    o1 <- order(train)
    x1 <- train[o1]
    
    o2 <- order(test)
    x2 <- test[o2]

    y <- y[o1]

    # find where the test points lie within the training points
    inds <- findInterval(x2, x1)

    # grab 2*k+1 training points around each testing point
    left <- pmax(1, inds-k)
    right <- pmin(ntrain, left+2*k)

    d <- right-left+1
    d <- 2*k+1 - d
    left <- pmax(1,left-d)

    ii <- mapply(FUN=function(l,r) l:r, left, right)
    xx <- mapply(FUN=function(l,r) x1[l:r], left, right)

    # compute distance between each test point and the corresponding 2*k+1 training points
    d <- sweep(xx, 2, x2)
    d <- sqrt(d^2)

    # get the k nearest y values
    ymat <- matrix(0.0, nrow=ntest, ncol=k)
    for (j in 1:ntest) {
      dd <- order(d[,j])
      iij <- ii[dd[1:k], j]

      if (length(iij) != k)
        stop("Didn't get k points")

      ymat[j,] <- y[iij]
    }

    yhat <- rowMeans(ymat)
    yhat[order(o2)]
  }

test.knnReg <- function()
  {
    train <- runif(20, max=10)
    test <- runif(5, max=10)
    y <- train + rnorm(20)

    plot(train, y)
    y1 <- knnReg(train, train, y, k=1)
    y5 <- knnReg(train, train, y, k=10)

    points(train, y1, col=2, pch=19)
    points(train, y5, col=3, pch=19)
    abline(a=0,b=1)

    rss <- function(f,y) mean(sum((f-y)^2))
    
    print(rss(y1,y))
    print(rss(y5,y))
  }
