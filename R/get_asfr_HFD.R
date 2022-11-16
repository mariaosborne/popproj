#' Get Fertility Rates from HFD
#'
#' Retrieve Fx values for a single year and country from the
#' Human Fertility Database (HFD). Requires a valid HFD username and password.
#'
#' @param country_code country from which to extract rates (run \code{HMDHFDplus::getHMDcountries()}
#' to see all valid country codes)
#' @param year year from which to extract rates
#' @param hfd_username HFD username
#' @param hfd_password HFD password
#'
#' @return a vector of Fx values of length 111 (ages 0-110)
#'

.datatable.aware=TRUE

get_asfr_HFD <- function(country_code, year, hfd_username, hfd_password) {
  # get rates from HFD
  fert_table <- data.table::as.data.table(HMDHFDplus::readHFDweb(CNTRY = country_code, item = 'asfrRR',
                                         username= hfd_username, password=hfd_password))
  # select relevant year
  fertility_rates_year <- fert_table[Year==year, ASFR]

  Fx <- rep(0, 111)
  # index 13 represents fertility for ages 12-13
  Fx[13:56] <- fertility_rates_year

  # deal with NA values, if needed
  Fx[is.na(Fx)] <- 0.0
  return(Fx)
}
