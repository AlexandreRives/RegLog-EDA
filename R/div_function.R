#' Normalize
#'
#' Process of putting different variables on the same scale
#' 
#' @param x A data frame or matrix
#' @author Alexandre Rives
#' 
#' @import PCAmixdata
#' 
#' @export
#' 
#' @examples 
#' normalize(iris)
#' 
#' @return A standardize data set
#' 
normalize <- function(x){
  x_split <- splitmix(x)
  x_scale <- apply(x_split$X.quanti, FUN=scale, MARGIN = 2)
  return(x_scale)
}

#' Get the cores of the CPU
#'
#' Process of detecting the number of cores in the CPU and return it to the function.
#' 
#' @param ncores A number of cores
#' @author Alexandre Rives
#' 
#' @import parallel
#' 
#' @export
#' 
#' @examples
#' ncores(8)
#' 
#' @return The number of cores.
#' 
ncores <- function(n_cores){
  cores = parallel::detectCores()
  if((n_cores <= 0) | (n_cores >= cores)){
    n_cores <- cores-1
    print(paste("You have entered a wrong number of cores. The algorithm is going to run with", n_cores, "cores."))
    return(n_cores)
  }else{
    return(n_cores)
  }
}

#' Re-coding variables
#'
#' Process of returning a data set with re-coding variables
#' 
#' @param x A data frame or matrix
#' @author Alexandre Rives
#' 
#' @import PCAmixdata
#' @import tidytable
#' 
#' @export
#' 
#' @examples
#' dummies(iris)
#' 
#' @return recording quantitative data of the dataset.
#' 
dummies <- function(x){
  x_split <- splitmix(x)
  x_dummies <- get_dummies.(x_split$X.quali, drop_first = TRUE)
  x_dummies <- splitmix(x_dummies)
  return(x_dummies$X.quanti)
}
