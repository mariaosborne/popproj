#' Matrix Form of projection
#'
#' Project 1-Year Leslie Matrix with Time-Varying Lx and Fx rates
#'
#' @param nsteps number of 1-year steps in the population projection
#' @param Lx_mat matrix of Lx values for each step of the projection. must have same number
#' of columns as number of steps in the projection
#' @param Fx_mat matrix of Fx values for each step of the projection. must have same number of
#' columns as number of steps in the projection
#' @param starting_pop optional vector of initial population by 1-year age groups. If left unspecified,
#' projection will start from the stable age structure of the initial Leslie matrix
#'
#' @return Returns a list with the following components
#' \describe{
#'   \item{\code{Kx.mat}}{The projection matrix, where each column is a time step and each
#'   row in an age group}
#'   \item{\code{nrr.vec}}{A vectors of NRRs at each stage of the projection}
#'   \item{\code{e0.vec}}{A vector Life expectancy at each stage of the projection}
#'   \item{\code{births.vec}}{A vector The number of births at each stage}
#'  }
#'@export



## projection with matrix of time-varying Lxt and Fxt matrices

project_leslie1_dynamic <- function(nsteps,
                                    Lx_mat,
                                    Fx_mat,
                                    starting_pop = NULL,
                                    time = NULL) {

  # check if input matrices are same size as each other
  if((nrow(Fx_mat) != nrow(Lx_mat)) | (ncol(Fx_mat) != ncol(Lx_mat))) {
    stop("dimensions of input matrices don't match!")
  }

  # check to make sure nsteps = ncol(Lx.mat) -- how is year zero treated???
  if(!is.null(time)) {
    nsteps = length(time)
  } else{
    time = c(0:(nsteps-1))
  }

  Lx.start <- Lx_mat[,1]
  Fx.start <- Fx_mat[,1]

  ## make A, (leslie1)
  new.A <- leslie1(Lx = Lx.start, Fx = Fx.start)

  ## repace nans with zero
  new.A[is.nan(new.A)] = 0

  ## define starting population Kx.0
  if (is.null(starting_pop)) {
    # use the stable pop of starting A if not specified
    Kx.zero <- eigen(new.A)$vec[,1]
    Kx.zero <- Kx.zero/sum(Kx.zero)
  }
  else {
    # make sure the starting pop vector is valid
    if (length(starting_pop) != length(Lx.start)){stop("Invalid starting population vector")}
    Kx.zero <- starting_pop
  }

  # build initial matrix
  K.mat <- matrix(0,
                  nrow = nrow(Fx_mat),
                  ncol = ncol(Fx_mat))
  K.mat[,1] <- Re(Kx.zero)

  # build NRR vec
  # NRR = sum (nLx * nFx *ffab) if radix=1
  nrr.vec <- c(sum(Lx_mat[,1]*Fx_mat[,1])*0.4886)

  ## do time varying projection
  for (i in 1:(nsteps-1))
  {
    ## define Lx and Fx for each step
    new.Lx <- Lx_mat[,i]
    new.Fx <- Fx_mat[,i]

    ## make A, (leslie1)
    new.A <- leslie1(Lx = new.Lx, Fx = new.Fx)
    new.A[is.nan(new.A)] = 0

    new.nrr <- sum(Lx_mat[,i]*Fx_mat[,i])*0.4886
    nrr.vec <- c(nrr.vec, new.nrr)

    ## project
    K.mat[,i+1] <- new.A %*% K.mat[,i]
  }


  #return(K.mat)
  return(list(K_mat = K.mat,
              nrr_vec = nrr.vec,
              e0_vec = colSums(Lx_mat),
              births_vec = K.mat[1,],
              time_vec = time))
}
