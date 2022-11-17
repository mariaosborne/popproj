#' Pad Fx
#'
#' ASFRs from HMD are measured at age 12-55. This method
#' Pads a vector of ASFRs with zeros for ages 0-11 and 55-110,
#' so that it contains the same age groups as a lifetable from HMD
#'
#' @param fx a vector of ASFRs for ages 12-55
#'
#' @returns a vector length 111
#'
#' @export
#'

pad_fx_vector <- function(fx) {
  # age grps 12 - 55
  # we want ages 0- 110
  # pad 12 zeros at beginning for ages 0-11
  # pad 55 zeros for ages 56-110 at end
  fx_padded <- c(rep(0,12), fx, rep(0, 55))
  return(fx_padded)
}
