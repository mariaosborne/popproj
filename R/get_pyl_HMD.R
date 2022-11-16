#' Get Person Years Lived from HMD
#'
#' Retrieve Lx values for a single year and country from the
#' Human Mortality Database (HMD). Requires a valid HMD username and password.
#'
#' @param country_code country from which to extract rates (run \code{HMDHFDplus::getHMDcountries()}
#' to see all valid country codes)
#' @param year year from which to extract rates
#' @param sex sex for which to extract rates. default is female ('f') but can also be set
#' to both ('b') or male ('m')
#' @param hfd_username HMD username
#' @param hfd_password HFD password
#'
#' @return a vector of Lx values of length 111 (ages 0-110)
#'

.datatable.aware=TRUE

get_pyl_HMD <- function(country_code, year, sex = 'f', hmd_username, hmd_password) {
  item = 'fltper_1x1'
  if (sex == 'b') {
    item = 'bltper_1x1'
  }
  life_table <- data.table::as.data.table(HMDHFDplus::readHMDweb(CNTRY = country_code, item = item,
                                         username= hmd_username, password = hmd_password))
  Lx <- life_table[Year == year, Lx]/(10^5)
  return(Lx)
}
