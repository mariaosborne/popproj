# Helper Functions

# Get Fx from NRR
nrr2Fx <- function(nrr, Fx.zero, Lx)
{
  nrr.zero <- sum(Fx.zero * Lx * .4886)
  rat <- nrr/nrr.zero
  Fx <- Fx.zero * rat
  Fx
}


# Get Lx from life expectancy
ezero2Lx <- function(e0, Lx.zero)
{
  f <- function(theta)
  {
    (sum(Lx.zero^theta) - e0)
  }
  theta.hat <- stats::uniroot(f, lower = .00001, upper = 100)$root
  ## do only mort change over age 50, so neutral
  Lx <- Lx.zero^c(rep(1, 50), rep(theta.hat, length(Lx.zero)-50))
  return(Lx)
}


# Derivative estimation
center_diff <- function(x, end.fill = F)
{
  ## approximate derivatives with discrete data by taking central
  ## differences d(x) = ([f(x+1) - f(x)] + [f(x) - f(x-1)])/2 =
  ## [f(x+1) - f(x-1)]/2 if end.fill = T, then first and last
  ## differences are not centered.

  ## useful for Bongaarts-Feeney, Gompertz fertility, and other
  ## fitting of models that are developed in continuous time and
  ## involve derivatives

  forward.diff = c(diff(x), NA)
  backward.diff = c(NA, diff(x))
  out = (forward.diff + backward.diff)/2
  if(end.fill)
  {
    out[1] = diff(x)[1]
    out[length(out)] = diff(x)[length(diff(x))]
  }
  ## preserve names if exist
  if(!is.null(names(x)))
    names(out) = names(x)
  return(out)
}


