#' Convert lifetable to matrix
#'
#' Takes HMD/HFD structured data and turns it into a matrix
#' for use in project_leslie1_dynamic(). Data need to have
#' Year, Age, and either and Lx or ASFR column.
#'
#' @param data data from HMD/HFD
#' @param years a range of years to select rates from
#' @param rate which vital rate to select, either ASFR or Lx
#' @param scaler radix
#'
#' @return Returns a list with the following components
#'
#' @export
#'

lifetable_to_matrix <- function(data, years, rate = c('Lx', 'ASFR'), scaler = 100000) {

  # pivot data to wide, turn into matrix
  rates_matrix <- data %>%
    dplyr::filter(Year %in% years) %>%
    dplyr::select(c(Year, Age, all_of(rate))) %>%
    tidyr::pivot_wider(
      id_cols = Age,
      names_from = Year,
      values_from = rate) %>%
    dplyr::select(-c('Age')) %>%
    unname() %>%
    as.matrix()

  # divide by radix if Lx
  if (rate == 'Lx') {
    rates_matrix <- rates_matrix/scaler
  }

  # fill out rows with zeros if fertility
  if (rate == 'ASFR') {
    rates_matrix = rbind(matrix(data = 0, nrow=12, ncol = length(years)),
                         rates_matrix,
                         matrix(data = 0, nrow=55, ncol = length(years)))
  }

  # return
  return(rates_matrix)
}

# get rid of non-numeric ages
# if(!is.numeric(female_lifetable$Age)) {
# data <- data %>%
#   mutate(Age = str_replace_all(Age, pattern = "[^0-9.-]", replacement = "")) %>%
#   mutate(Age = as.integer(Age))
# }
