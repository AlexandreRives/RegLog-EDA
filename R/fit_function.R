#' Fit function
#'
#' Function that return a trained dataset using the gradient descent.
#' 
#' 
#' @param formula the data frame
#' @param data the data frame
#' @param ncores the number of cores to run the fit function faster. It's better to use it with more than 10 000 data.
#' @param batch_size the number of observations for the mini batch mode.
#' @param normalize TRUE if you want to scale your features. It doesn't take the qualitative data.
#' @param learning_rate the learning_rate for the logistic regression
#' @param nb-iteration the number of iterations for the gradient descent 
#' 
#' 
#' @author Deffa Ndiaye
#' 
#' @export
#' 
#' @return A fitted dataset
#' @source 



fit_reg_log <- function(formula,data, mode, batch_size, normalize = FALSE,learning_rate,nb_iteration){
  
  #data.frame control
  ok <- is.data.frame(data)
  if (!ok){
    stop("This is not a data.frame")
  }
  
  
  df <- f_Formula(data)
  y <- df[1]
  
  #normalization
  if(norm == TRUE){
    X <- normalize(X)
    
  }else{
    X <- df[,-1]
    
  }
  #gradient descent
  
  if (mode == "batch"){
    coefs <- descente_de_gradient_ok(df,colnames(X),colnames(Y),learning_rate,nb_iteration)
  } else if (mode == "online"){
    coefs <- descente_de_gradient_online(df,colnames(X),colnames(Y),learning_rate,nb_iteration)
  } else if (mode == "mini_batch"){
    coefs <- descente_de_gradient_mini_batch(df,colnames(X),colnames(Y),batch_size,learning_rate,nb_iteration)
  }
  

  #return(coefs)

  #instanciation
  instance <- list()
  instance$df <- data
  instance$coefficients <-coefs
  class(instance) <- "reg_log"
  return(instance)
  
}


#Metrics 

#Null Deviance

#Residual Deviance 


#AIC


#surcharge de print
print.fit_reg_log <- function(object){
  #affichage de la liste des variables
  cat("Variables : ", colnames(object$df),"\n")
  
}


fit_reg_log(species~sepal_length+sepal_width+petal_length+petal_width,data=iris,mode="batch",normalize = TRUE,learning_rate =0.01 ,nb_iteration = 2)
  
  # Etape 5 : Sortir toutes les metriques neccessaires 
  
  
  # Etape finale : Création de l'objet fit_reg_log de TYPE S3 dont les méthodes génériques "print" et "summary" au moins sont surchargées
  
  
  return(obj_fit_reg_log) # Retourner un objet de type S3
}









