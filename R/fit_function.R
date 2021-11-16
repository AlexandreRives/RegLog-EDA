#' Fit function
#'
#' Function that return a trained dataset using the gradient descent.
#' 
#' @param formula the data frame
#' @param data the data frame
#' @param ncores the number of cores to run the fit function faster. It's better to use it with more than 10 000 data.
#' @param batch_size the number of observations for the mini batch mode.
#' @param normalize TRUE if you want to scale your features. It doesn't take the qualitative data.
#' 
#' @author Alexandre Rives
#' 
#' @export
#' 
#' @return A fitted dataset
#' 
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


# En fait on ne prend en entre que des variables quanti donc pas besoin de tout refaire dans le fit (vu que ca sera fait en amont)
fit_reg_log <- function(formula, data, mode, batch_size, ncores, normalize = FALSE, autres param. éventuels){
  if (data == NULL) {print("You should enter a data set.")}
  
  # # Etape 1 : Récuperer les valeurs de formula
  # var_x = fct_formula(formula)$x
  # var_y = fct_formula(formula)$y
  # 
  # # Etape 2 : Filter le dataframe
  # df_x = filter_df(data, var_x)
  # df_y = filter_df(data, var_y)
  
  # Etape 3 : Si normalize = TRUE, on normalise les données sinon rien 
  if (normalize == TRUE) {df_x = get_normalize_df(df_x)}
  
  # Etape 4 : Descente du gradient en fonction du mode
  if (mode == "batch"){
    coefs = descente_de_gradient_batch(.............)
  } else if (mode == "online"){
    coefs = descente_de_gradient_online(.............)
  } else if (mode == "mini_batch"){
    coefs = descente_de_gradient_mini_batch(.............)
  }
  
  # Etape 5 : Sortir toutes les metriques neccessaires 
  
  
  # Etape finale : Création de l'objet fit_reg_log de TYPE S3 dont les méthodes génériques "print" et "summary" au moins sont surchargées
  
  
  return(obj_fit_reg_log) # Retourner un objet de type S3
}









