#' Formula function
#'
#' Function that return a filtered dataframe according to the equation given by the user.
#' 
#' @param formula the logistic regression equation
#' @param donnee A data frame or matrix
#' @author Deffa Ndiaye
#' 
#' @export
#' 

f_Formula <- function(formula,donnee) {
  
  df<- model.frame(formula,data=donnee)
  
  return(df)
}