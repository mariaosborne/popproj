% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_asfr_HFD.R, R/get_pyl_HMD.R
\docType{data}
\name{.datatable.aware}
\alias{.datatable.aware}
\title{Get Fertility Rates from HFD}
\format{
An object of class \code{logical} of length 1.

An object of class \code{logical} of length 1.
}
\usage{
.datatable.aware

.datatable.aware
}
\arguments{
\item{country_code}{country from which to extract rates (run \code{HMDHFDplus::getHMDcountries()}
to see all valid country codes)}

\item{year}{year from which to extract rates}

\item{sex}{sex for which to extract rates. default is female ('f') but can also be set
to both ('b') or male ('m')}

\item{hfd_username}{HMD username}

\item{hfd_password}{HFD password}
}
\value{
a vector of Fx values of length 111 (ages 0-110)

a vector of Lx values of length 111 (ages 0-110)
}
\description{
Retrieve Fx values for a single year and country from the
Human Fertility Database (HFD). Requires a valid HFD username and password.

Retrieve Lx values for a single year and country from the
Human Mortality Database (HMD). Requires a valid HMD username and password.
}
\keyword{datasets}
