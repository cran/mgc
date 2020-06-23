## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.width=4, fig.height=3)

## ---- message=FALSE-----------------------------------------------------------
require(mgc)
require(ggplot2)
n=400
d=2
K=5

## -----------------------------------------------------------------------------
plot_sim <- function(X, Y, name) {

  data <- data.frame(x1=X[,1], x2=X[,2], class=Y)
  ggplot(data, aes(x=x1, y=x2, color=class)) +
    geom_point() +
    xlab("x1") +
    ylab("x2") +
    ggtitle(name) +
    theme_bw()
}

## -----------------------------------------------------------------------------
sim <- discr.sims.linear(n, d, K, signal.lshift=3)
plot_sim(sim$X, sim$Y, "Linear")

## -----------------------------------------------------------------------------
sim <- discr.sims.fat_tails(n, d, K)
plot_sim(sim$X, sim$Y, "Fat Tails")

## -----------------------------------------------------------------------------
sim <- discr.sims.cross(n, d, 2)
plot_sim(sim$X, sim$Y, "Cross")

## -----------------------------------------------------------------------------
sim <- discr.sims.radial(n, d, K)
plot_sim(sim$X, sim$Y, "Radial")

