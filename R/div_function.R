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
#' Process of returning a data set with re-coding feature variables
#' 
#' @param x A data frame or matrix
#' @author Alexandre Rives
#' 
#' @import PCAmixdata
#' @import tidytable
#' 
#' @export
#' 
#' @return A re-coding quantitative data of the dataset.
#' 
dummies <- function(x){
  x_split <- splitmix(x)
  x_dummies <- get_dummies.(x_split$X.quali, drop_first = TRUE)
  x_dummies <- splitmix(x_dummies)
  return(x_dummies$X.quanti)
}

#' Filter feature variables
#'
#' Process of returning a data set filtered by the feature variables
#' 
#' @param [df, x] A data frame or matrix + feature variables
#' @author Alexandre Rives
#' 
#' 
#' @export
#' 
#' @return A data set filtered with the feature variables.
#' 
filtered_x <- function(df, x){
  df_x <- df[x]
  return(df_x)
}

#' Filter target variable
#'
#' Process of returning a data set filtered by the target variable
#' 
#' @param [df, y] A data frame or matrix + target variable
#' @author Alexandre Rives
#' 
#' 
#' @export
#' 
#' 
#' @return A data set filtered with the target variables.
#' 
filtered_y <- function(df, y){
  df_y <- df[y]
  return(df_y)
}

#' Re-coding target variable
#'
#' Process of returning a data set with re-coding the target variable
#' 
#' @param [df, y] A data frame or matrix + target variable
#' @author Alexandre Rives
#' 
#' @import tidytable
#' 
#' @export
#' 
#' 
#' @return A re-coding quantitative data of the target variable.
#' 
dummies_y <- function(df_y){
  df_y <- get_dummies.(df_y, drop_first = TRUE)
  return(df_y)
}
