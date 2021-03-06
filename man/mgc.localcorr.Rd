% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MGCLocalCorr.R
\name{mgc.localcorr}
\alias{mgc.localcorr}
\title{MGC Local Correlations}
\usage{
mgc.localcorr(
  X,
  Y,
  is.dist.X = FALSE,
  dist.xfm.X = mgc.distance,
  dist.params.X = list(method = "euclidean"),
  dist.return.X = NULL,
  is.dist.Y = FALSE,
  dist.xfm.Y = mgc.distance,
  dist.params.Y = list(method = "euclidean"),
  dist.return.Y = NULL,
  option = "mgc"
)
}
\arguments{
\item{X}{is interpreted as:
\describe{
   \item{a \code{[n x d]} data matrix}{X is a data matrix with \code{n} samples in \code{d} dimensions, if flag \code{is.dist.X=FALSE}.}
   \item{a \code{[n x n]} distance matrix}{X is a distance matrix. Use flag \code{is.dist.X=TRUE}.}
}}

\item{Y}{is interpreted as:
\describe{
   \item{a \code{[n x d]} data matrix}{Y is a data matrix with \code{n} samples in \code{d} dimensions, if flag \code{is.dist.Y=FALSE}.}
   \item{a \code{[n x n]} distance matrix}{Y is a distance matrix. Use flag \code{is.dist.Y=TRUE}.}
}}

\item{is.dist.X}{a boolean indicating whether your \code{X} input is a distance matrix or not. Defaults to \code{FALSE}.}

\item{dist.xfm.X}{if \code{is.dist == FALSE}, a distance function to transform \code{X}. If a distance function is passed,
it should accept an \code{[n x d]} matrix of \code{n} samples in \code{d} dimensions and return a \code{[n x n]} distance matrix
as the \code{$D} return argument. See \link[mgc]{mgc.distance} for details.}

\item{dist.params.X}{a list of trailing arguments to pass to the distance function specified in \code{dist.xfm.X}.
Defaults to \code{list(method='euclidean')}.}

\item{dist.return.X}{the return argument for the specified \code{dist.xfm.X} containing the distance matrix. Defaults to \code{FALSE}.
\describe{
    \item{\code{is.null(dist.return)}}{use the return argument directly from \code{dist.xfm} as the distance matrix. Should be a \code{[n x n]} matrix.}
    \item{\code{is.character(dist.return) | is.integer(dist.return)}}{use \code{dist.xfm.X[[dist.return]]} as the distance matrix. Should be a \code{[n x n]} matrix.}
}}

\item{is.dist.Y}{a boolean indicating whether your \code{Y} input is a distance matrix or not. Defaults to \code{FALSE}.}

\item{dist.xfm.Y}{if \code{is.dist == FALSE}, a distance function to transform \code{Y}. If a distance function is passed,
it should accept an \code{[n x d]} matrix of \code{n} samples in \code{d} dimensions and return a \code{[n x n]} distance matrix
as the \code{dist.return.Y} return argument. See \link[mgc]{mgc.distance} for details.}

\item{dist.params.Y}{a list of trailing arguments to pass to the distance function specified in \code{dist.xfm.Y}.
Defaults to \code{list(method='euclidean')}.}

\item{dist.return.Y}{the return argument for the specified \code{dist.xfm.Y} containing the distance matrix. Defaults to \code{FALSE}.
\describe{
    \item{\code{is.null(dist.return)}}{use the return argument directly from \code{dist.xfm.Y(Y)} as the distance matrix. Should be a \code{[n x n]} matrix.}
    \item{\code{is.character(dist.return) | is.integer(dist.return)}}{use \code{dist.xfm.Y(Y)[[dist.return]]} as the distance matrix. Should be a \code{[n x n]} matrix.}
}}

\item{option}{is a string that specifies which global correlation to build up-on. Defaults to \code{'mgc'}.
\describe{
   \item{'mgc'}{use the MGC global correlation.}
   \item{'dcor'}{use the dcor global correlation.}
   \item{'mantel'}{use the mantel global correlation.}
   \item{'rank'}{use the rank global correlation.}
}}
}
\value{
A list contains the following:
\item{\code{corr}}{consists of all local correlations within [-1,1] by double matrix index}
\item{\code{varX}}{contains all local variances for X.}
\item{\code{varY}}{contains all local variances for X.}
}
\description{
Compute all local correlation coefficients in O(n^2 log n)
}
\examples{
library(mgc)

n=200; d=2
data <- mgc.sims.linear(n, d)
lcor <- mgc.localcorr(data$X, data$Y)

}
\author{
C. Shen
}
