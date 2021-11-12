#' Formula function
#'
#' Function that return a 2 vectors with the x and y variables separated.
#' 
#' @param df A data frame or matrix
#' @author Deffa Ndiaye
#' 
#' @export
#' 

f_Formula <- function(formula,donnee) {
  
  df<- model.frame(formula,data=donnee)
  
  return(df)
}