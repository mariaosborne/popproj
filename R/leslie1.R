#' Leslie Matrix with 1-Year Age Groups
#'
#' Makes a Leslie matrix out of 1-year age-interval Lx and Fx schedules
#'
#' @param Lx vector of 1-year Lx values
#' @param Fx vector of 1-year Fx values
#'
#' @return A Leslie metrix
#'
#' @export
#'

leslie1 <- function(Lx, Fx)
{
  ## note: Fx needs to have same length as Lx
  nLx <- Lx
  nFx <- Fx
  ## subdiagonal
  nLxpn <- c(nLx[-1], 0)             # nL(x+n), next age-group
  subdi <- (nLxpn/nLx)[-length(nLx)] # survivorship sub-diagonal of
  ## first row
  nFxpn <- c(nFx[-1], 0)             # nF(x+n), next age group of fert.
  nL0 <- nLx[1]
  firstrow <- (nL0/2)*(nFx + nFxpn * nLxpn/nLx)*0.4886
  ## put subdiagonal and first row into matrix format
  A <- rbind(firstrow,
             cbind(diag(subdi), 0))
  dimnames(A) <- NULL               # remove messy labels
  return(A)
}
