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
#' @author Frintz Elisa, NDiaye Deffa, Rives Alexandre
#'
#' @export
#'
#' @return A fitted dataset
#'
fit_reg_log <- function(formula,data, mode, batch_size, normalize = FALSE,learning_rate,nb_iteration){

  #data.frame control
  ok <- is.data.frame(data)
  if (!ok){
    stop("This is not a data.frame")
  }

  df <- f_Formula(formula,data)
  y <- df[1]
  X <- df[,-1]

  #normalization
  if(normalize == TRUE){
    X <- normalize(X)
  }

  #gradient descent

  if (mode == "batch"){

    coefs <- batch_gradient_descent(df,colnames(X),colnames(y),learning_rate,nb_iteration)

  } else if (mode == "online"){
    coefs <- online_stochastic_gradient_descent(df,colnames(X),colnames(y),learning_rate,nb_iteration)
  } else if (mode == "mini_batch"){
    coefs <- gradient_mini_batch(df,colnames(X),colnames(y),batch_size,learning_rate,nb_iteration)
  }


  #return(coefs)

  #instanciation
  objet <- list()
  objet$coefficients <-coefs
  objet$features <- colnames(X)
  objet$target <- colnames(y)
  objet$norm <- normalize
  class(objet) <- "reg_log"
  return(objet)

}



#Metrics

#Null Deviance


#Residual Deviance


#AIC


#print
print.reg_log <- function(object){
  #affichage de la liste des variables
  cat("------------------------------------------------------------------ \n")
  cat("Results of the logistic regression \n")
  cat("target : ", object$target,"\n")
  cat("features : ", object$features,"\n")
  cat("Coefficients : ", object$coefficients$best_theta, "\n")
  cat("------------------------------------------------------------------ \n")

 }

#summary


obj<-fit_reg_log(coeur~.,data=heart,mode="batch",normalize = TRUE,learning_rate =0.01 ,nb_iteration = 2)


print(obj)
# Etape 5 : Sortir toutes les metriques neccessaires
heart <- read.table("C:/Users/dia/Downloads/heart_propre.csv",header=T)

# Etape finale : Création de l'objet fit_reg_log de TYPE S3 dont les méthodes génériques "print" et "summary" au moins sont surchargées


#return(obj_fit_reg_log) # Retourner un objet de type S3 }













