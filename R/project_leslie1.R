#' Project with 1-Year Leslie Matrix
#'
#' Project a given population for n years with
#' changing leslie matrices
#'
#' @details This is where we explain how this is calculated
#'
#' @param nsteps number of steps (years) to project for
#' @param nrr.vec vector of net reproductive rates at each step in the projection.
#' must be length(nsteps)
#' @param e0.vec vector of life expectancies at each step in the projection. must
#' be length(nsteps)
#' @param Lx.zero vector of initial Lx values
#' @param Fx.zero vector of initial Fx values
#' @param starting.pop optional vector of initial population by 1-year age groups. If left unspecified,
#' projection will start from the stable age structure of the initial Leslie matrix
#' @param normalize whether to normalize the initial populaition to 1 (not yet implemented)
#'
#' @return A Matrix
#'
#' @export

project_leslie1 <- function(nsteps,
                            nrr.vec,
                            e0.vec,
                            Lx.zero, Fx.zero,
                            starting.pop = NULL,
                            normalize = TRUE,
                            time = NULL) {

  if(!is.null(time)) {
    nsteps = length(time)
  } else{
    time = c(1:nsteps)
  }

  # get Lx and Fx values
  new.Lx <- ezero2Lx(e0 = e0.vec[1],
                     Lx.zero)
  new.Fx <- nrr2Fx(nrr = nrr.vec[1],
                   Fx.zero,
                   Lx = new.Lx)

  ## make A, (leslie1)
  new.A <- leslie1(Lx = new.Lx, Fx = new.Fx)

  ## define starting population K.0
  Kx.zero <- NULL
  if (is.null(starting.pop)) {
    ## use the stable pop from starting A if not specified
    Kx.zero <- eigen(new.A)$vec[,1]
    Kx.zero <- Kx.zero/sum(Kx.zero)
  }
  else {
    ## make sure the supplied starting pop vector is valid
    if (length(starting.pop) != length(Lx.zero)){stop("Invalid starting population vector")}
    Kx.zero <- starting.pop
  }


  K.mat <- matrix(0,
                  nrow = length(Fx.zero),
                  ncol = length(nrr.vec))
  K.mat[,1] <- Re(Kx.zero)

  ## do time varying projection
  for (i in 1:(nsteps-1))
  {
    ## get Lx and Fx to match e0 and nrr
    new.Lx <- ezero2Lx(e0 = e0.vec[i],
                       Lx.zero)
    new.Fx <- nrr2Fx(nrr = nrr.vec[i],
                     Fx.zero,
                     Lx = new.Lx)

    ## make A, (leslie1)
    new.A <- leslie1(Lx = new.Lx, Fx = new.Fx)

    ## project
    K.mat[,i+1] <- new.A %*% K.mat[,i]
  }

  return(list(K_mat = K.mat,
              births_vec = births_vec <- K.mat[1,],
              nrr_vec = nrr.vec,
              e0_vec = e0.vec,
              time_vec = time))
  #return(K.mat)

}
