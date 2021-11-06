#' Fit function
#'
#' Function that return a trained dataset using the gradient descent.
#' 
#' @param x A data frame or matrix
#' @author Alexandre Rives
#' 
#' @export
#' 
#' @examples 
#' fit(y ~ ., x, ncores = 4, normalize = TRUE)
#' 
#' @return A trained dataset
#' 
fit <- function(formula, data, ncores, batch_size, normalize){
  return(f_Formula(data))
}