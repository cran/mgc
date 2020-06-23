## ---- message=FALSE-----------------------------------------------------------
library(mgc)
library(reshape2)
library(ggplot2)

plot_mtx <- function(Dx, main.title="Distance Matrix", xlab.title="Sample Sorted by Source", ylab.title="Sample Sorted by Source") {
  data <- melt(Dx)
  ggplot(data, aes(x=Var1, y=Var2, fill=value)) +
    geom_tile() +
    scale_fill_gradientn(name="dist(x, y)",
                         colours=c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3"),
                         limits=c(min(Dx), max(Dx))) +
    xlab(xlab.title) +
    ylab(ylab.title) +
    theme_bw() +
    ggtitle(main.title)
}

## -----------------------------------------------------------------------------
nsrc <- 5
nobs <- 10
d <- 20
set.seed(12345)
src_id <- array(1:nsrc)
Y <- sample(rep(src_id, nobs))
X <- t(sapply(Y, function(lab) rnorm(2, mean=lab, sd=1/2)))

## ---- fig.height=4, fig.width=5-----------------------------------------------
X.dat <- data.frame(x1=X[,1], x2=X[,2], Individual=factor(Y), Dataset="First Dataset")
  ggplot(X.dat, aes(x=x1, y=x2, color=Individual)) +
    geom_point() +
    xlab("First Dimension") +
    ylab("Second Dimension") +
    ggtitle("Plot of Simulated Data") +
    theme_bw()

## -----------------------------------------------------------------------------
discr.stat(X, Y)$discr  # expect high discriminability since measurements taken at a source have the same mean and sd of only 1

## ---- fig.width=5, fig.height=4-----------------------------------------------
Dx <- as.matrix(dist(X[sort(Y, index=TRUE)$ix,]), method='euclidian')
plot_mtx(Dx)

## -----------------------------------------------------------------------------
discr.stat(Dx, sort(Y), is.dist=TRUE)$discr

## -----------------------------------------------------------------------------
# two norm between pairs of points
dist.fxn <- function(X) {
  n <- nrow(X)
  D <- array(0, dim=c(n, n))
  for (i in 1:(n - 1)) {
    for (j in i:n) {
      D[i,j] <- sum(abs(X[i,] - X[j,])^2)
    }
  }
  D <- D + t(D)
  return(D)
}

discr.stat(X, Y, dist.xfm=dist.fxn, dist.params=NULL, dist.return=NULL)$discr

## -----------------------------------------------------------------------------
# two norm between pairs of points
dist.fxn <- function(X, method="2") {
  if (method == "2") {
    n <- nrow(X)
    D <- array(0, dim=c(n, n))
    for (i in 1:(n - 1)) {
      for (j in i:n) {
        D[i,j] <- sum(abs(X[i,] - X[j,])^2)
      }
    }
    D <- D + t(D)
  } else {
    stop("Mistakes were made.")
  }
  return(list(Distance=D))
}

discr.stat(X, Y, dist.xfm=dist.fxn, dist.params=list(method="2"), dist.return="Distance")$discr

## -----------------------------------------------------------------------------
discr.test.one_sample(X, Y)$p.value

## ---- fig.height=3, fig.width=6-----------------------------------------------
X2 <- t(sapply(Y, function(lab) rnorm(2, mean=lab, sd=2)))
X.dat.both <- rbind(X.dat, data.frame(x1=X2[,1], x2=X2[,2], Individual=factor(Y), Dataset="Second Dataset"))
ggplot(X.dat.both, aes(x=x1, y=x2, color=Individual)) +
  geom_point() +
  xlab("First Dimension") +
  ylab("Second Dimension") +
  ggtitle("Plot of Simulated Data") +
  theme_bw() +
  facet_grid(. ~ Dataset)

## -----------------------------------------------------------------------------
discr.stat(X2, Y)$discr

## -----------------------------------------------------------------------------
discr.test.two_sample(X, X2, Y, alt="greater")$p.value

## ---- fig.width=5, fig.height=4-----------------------------------------------
Dx <- as.matrix(dist(iris[sort(as.vector(iris$Species), index=TRUE)$ix,c(1,2,3,4)]))

plot_mtx(Dx)

## -----------------------------------------------------------------------------
discr.stat(iris[,c(1,2,3,4)], as.vector(iris$Species))$discr

