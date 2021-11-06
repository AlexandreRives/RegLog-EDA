#' Fit function
#'
#' Function that return a trained dataset using the gradient descent.
#' 
#' @param formula the data frame
#' @param data the data frame
#' @param ncores the number of cores to run the fit function faster. It's better to use it with more than 10 000 data.
#' @param batch_size the number of batch for the mini batch mode.
#' @param normalize TRUE if you want to scale your feature variables. It doesn't take the qualitative data.
#' 
#' @author Alexandre Rives
#' 
#' @export
#' 
#' @return A fitted dataset
#' 
norm = FALSE
ncores = NULL
data = NULL
formula = ""
batch_size = NULL

fit <- function(formula, data, ncores, batch_size, norm){
  if(data == NULL){
    print("You should enter a data set.")
  }
  else{
    vector_x_y <- f_Formula(data)
    df_x <- filtered_x(data[vector_x_y[1]])
    df_y <- filtered_y(data[vector_x_y[2]])
    if(norm == TRUE){
      df_x_normalize <- normalize(df_x)
      df_x_dummies <- dummies(df_x)
      df_x_final <- cbind(df_x_dummies, df_x_normalize)
      df_y_dummies <- dummies(df_y)
    }else{
      df_x_dummies <- dummies(df_x)
      split_df_x <- splitmix(df_x)
      df_x_quanti <- split_df_x$X.quanti
      df_x_final <- cbind(df_x_dummies, df_x_quanti)
      df_y_dummies <- dummies(df_y)
    }
  }
}