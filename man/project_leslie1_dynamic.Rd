% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_leslie1_dynamic.R
\name{project_leslie1_dynamic}
\alias{project_leslie1_dynamic}
\title{Matrix Form of projection}
\usage{
project_leslie1_dynamic(
  nsteps,
  Lx_mat,
  Fx_mat,
  starting_pop = NULL,
  time = NULL
)
}
\arguments{
\item{nsteps}{number of 1-year steps in the population projection}

\item{Lx_mat}{matrix of Lx values for each step of the projection. must have same number
of columns as number of steps in the projection}

\item{Fx_mat}{matrix of Fx values for each step of the projection. must have same number of
columns as number of steps in the projection}

\item{starting_pop}{optional vector of initial population by 1-year age groups. If left unspecified,
projection will start from the stable age structure of the initial Leslie matrix}
}
\value{
Returns a list with the following components
\describe{
  \item{\code{Kx.mat}}{The projection matrix, where each column is a time step and each
  row in an age group}
  \item{\code{nrr.vec}}{A vectors of NRRs at each stage of the projection}
  \item{\code{e0.vec}}{A vector Life expectancy at each stage of the projection}
  \item{\code{births.vec}}{A vector The number of births at each stage}
 }
}
\description{
Project 1-Year Leslie Matrix with Time-Varying Lx and Fx rates
}
