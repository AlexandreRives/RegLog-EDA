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

fct_formula <- function(f){
  f = formula(f)
  y = all.vars(f)[1]
  x = all.vars(f)[2:length(all.vars(f))]
  return(list(y = y, x = x))
}

fct_formula("y ~ x1 + x2")
df = cbind.data.frame(x1 = c(1,3,5,8), x2 = c(1,3,5,8), y = c(1,3,5,8))
f_Formula(y ~ . , df)
