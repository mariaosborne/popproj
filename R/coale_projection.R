#' Coale Scenario Projection
#'
#' Specialized projection over 100 years, where NRR declines
#' exponentially.
#'
#' @details
#' \eqn{exp(k1 * t + k2 * t^2)}
#'
#' should do the math formula for exp(exp(k1....)
#'
#' @param Lx_zero vector of initial Lx values
#' @param Fx_zero vector if initial Fx values
#' @param k1 rate of NRR decline
#' @param k2 additional curvature of NRR change
#' @param rho rate of e0 change
#' @param e0_start initial e0 value



coale_projection <- function(Lx_zero, Fx_zero,
                             k1 = -0.015, k2 = 0,
                             rho = 0, e0_start = 80) {

  # set time range
  t <- c(-50:50)

  # set input values
  nrr_t <- exp(k1 * t  + k2 * t^2) # how is the intial nrr set here???
  e0_vec <- exp(rho*t) * 80

  # do projection
  proj <- project_leslie1(time = -50:50,
                               nrr_vec = nrr_t,
                               e0_vec = e0_vec,
                               Lx_zero = Lx_zero,
                               Fx_zero = Fx_zero)
  return(proj)

  ####
  K.mat.out <- proj$K_mat

  # to be implemented, maybe

  ## normalize population size to 1.0 at time t = 0
  Nt.unnorm <- colSums(K.mat.out)
  K.mat.out <- K.mat.out/   Nt.unnorm[t == 0]
  K.mat.out <- radix*K.mat.out

  # get population size and births
  Nt <- colSums(K.mat.out)
  Bt <- proj$births_vec
}




