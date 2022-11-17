#' Matrix Form of projection
#'
#' Project 1-Year Leslie Matrix with Time-Varying Lx and Fx rates
#'
#' @param nsteps number of 1-year steps in the population projection
#' @param Lx_mat matrix of Lx values for each step of the projection. must have same number
#' of columns as number of steps in the projection
#' @param Fx_mat matrix of Fx values for each step of the projection. must have same number of
#' columns as number of steps in the projection
#' @param starting.pop optional vector of initial population by 1-year age groups. If left unspecified,
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
#'



## projection with matrix of time-varying Lxt and Fxt matrices

project_leslie1_dynamic <- function(nsteps,
                                    Lx_mat,
                                    Fx_mat,
                                    starting.pop = NULL) {

  # check if input matrices are same size as each other
  if((nrow(Fx_mat) != nrow(Lx_mat)) | (ncol(Fx_mat) != ncol(Lx_mat))) {
    stop("dimensions of input matrices don't match!")
  }

  # check to make sure nsteps = ncol(Lx.mat) -- how is year zero treated???

  Lx.start <- Lx_mat[,1]
  Fx.start <- Fx_mat[,1]

  ## make A, (leslie1)
  new.A <- leslie1(Lx = Lx.start, Fx = Fx.start)

  ## repace nans with zero
  new.A[is.nan(new.A)] = 0

  ## define starting population Kx.0
  if (is.null(starting.pop)) {
    # use the stable pop of starting A if not specified
    Kx.zero <- eigen(new.A)$vec[,1]
    Kx.zero <- Kx.zero/sum(Kx.zero)
  }
  else {
    # make sure the starting pop vector is valid
    print(length(starting.pop))
    print(length(Lx.start))
    if (length(starting.pop) != length(Lx.start)){stop("Invalid starting population vector")}
    Kx.zero <- starting.pop
  }

  K.mat <- matrix(0,
                  nrow = nrow(Fx_mat),
                  ncol = ncol(Fx_mat))
  K.mat[,1] <- Re(Kx.zero)

  # build NRR vec
  nrr.vec <- c(sum(new.A[1,])) # first row of the Leslie matrix

  ## do time varying projection
  for (i in 1:(nsteps-1))
  {
    ## define Lx and Fx for each step
    new.Lx <- Lx_mat[,i]
    new.Fx <- Fx_mat[,i]

    ## make A, (leslie1)
    new.A <- leslie1(Lx = new.Lx, Fx = new.Fx)
    new.A[is.nan(new.A)] = 0

    new.nrr <- sum(new.A[1,])
    nrr.vec <- c(nrr.vec, new.nrr)

    ## project
    K.mat[,i+1] <- new.A %*% K.mat[,i]
  }

  ## note: we have to figure out NRR and e0 from this matrix
  # I think NRR is the sum of the first row of the Leslie matrix?
  # we can check this against real data

  # how do we get life exp from either Lx or the leslie matrix?
  # Add up Lx /radix -- everything is already divided out  here......

  #return(K.mat)
  return(list(K.mat = K.mat,
              nrr.vec = nrr.vec,
              e0.vec = colSums(Lx_mat),
              births.vec = K.mat[1,])) ## check this!!!
}
