#' Fit function
#'
#' Function that return a trained dataset using the gradient descent.
#' 
#' @param [formula, data, ncores, batch_size, normalize] Settings of the dataset, a data frame, the number of cores, the batch size for the mini-batch mode
#' @author Alexandre Rives
#' 
#' @export
#' 
#' @return A trained dataset
#' 
fit <- function(formula, data, ncores, batch_size, normalize){
  return(f_Formula(data))
}