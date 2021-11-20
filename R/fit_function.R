#' Fit function
#'
#' Function that return a trained dataset using the gradient descent.
#'
#' @param formula Enter the target and the feature.
#' @param data the data frame.
#' @param mode choose between : batch, mini-batch or online.
#' @param batch_size the number of observations for the mini batch mode.
#' @param normalize TRUE if you want to scale your features. It doesn't take the qualitative data.
#' @param iter number of iterations.
#' @param learning_rate used for the gradient descent.
#' @param ncores the number of cores to run the fit function faster.
#' 
#' @author Frintz Elisa, NDiaye Deffa, Rives Alexandre
#' 
#' @return object used to the predicted part
#'
#' @export
#'
#' @return A fitted dataset
#'
fit_reg_log <- function(formula, data, mode, batch_size, normalize = FALSE, learning_rate, iter, ncores = 1){

  call <- match.call()
  
  #data.frame control
  ok <- is.data.frame(data)
  if (!ok){
    stop("This is not a data.frame")
  }
  if(nrow(data) == 0){
    stop("You should enter data")
  }
  
  #separate features from target
  df <- f_Formula(formula,data)
  y <- df[1]
  X <- df[,-1]

  #normalization
  if(normalize == TRUE){
    X <- normalize(X)
  }

  #gradient descent
  if (mode == "batch"){
    coefs <- batch_gradient_descent(df,colnames(X),colnames(y),learning_rate,iter)
  } else if (mode == "online"){
    coefs <- online_stochastic_gradient_descent(df,colnames(X),colnames(y),learning_rate,iter)
  } else if (mode == "mini_batch"){
    coefs <- gradient_mini_batch(df,colnames(X),colnames(y),batch_size,learning_rate,iter)
  }
  
  #class
  object <- list()
  object$coefficients <- coefs$best_theta
  object$cost <- coefs$cost_list
  object$features <- colnames(X)
  object$target <- colnames(y)
  object$norm <- normalize
  object$call <- call
  class(object) <- "reg_log"
  return(object)

}

#' Predict function
#'
#' Function that return the predicted probs or the belonging class
#'
#' @param object class object created in the fit function
#' @param newdata test set
#' @param type probs or belonging class
#' 
#' @author Frintz Elisa, NDiaye Deffa, Rives Alexandre
#'
#' @export
#'
#' @return A list
#'
predict <- function(object, newdata, type){
  
  # Part 1 : Create a new data
  X_newdata = newdata[, object$features]
  if (object$norm == TRUE) {X_newdata = as.data.frame(apply(X_newdata, 2, scale))}
  X_newdata = add_constant(X_newdata)
  
  # Part 2 : Set the coefs on the new data
  prob = X_newdata %*% object$coefficients
  
  # Part 3 : Print coefs or appliance class
  if(type == "probs"){
    prob = sigmoid(prob)
  }else{
    class_pred = ifelse(prob > 0.5, 1, 0)
  }
}



#Metrics


#Null Deviance


#Residual Deviance


#AIC


#' Fit function
#'
#' Function that print the elements of the fitted object
#'
#' @param x the fitted object
#' @param ... other params
#'
#' @author Frintz Elisa, NDiaye Deffa, Rives Alexandre
#'
#' @export
#'
print.reg_log <- function(x, ...){
  cat("------------------------------------------------------------------ \n")
  cat("Results of the logistic regression \n")
  cat("target : ", x$target,"\n")
  cat("features : ", x$features,"\n")
  cat("Coefficients : ", x$coefficients$best_theta, "\n")
  cat("------------------------------------------------------------------ \n")
}

#' Fit function
#'
#' Function that print the summary of the fitted object
#'
#' @param object the fitted object
#' @param ... other params
#' 
#' @import utils
#'
#' @author Frintz Elisa, NDiaye Deffa, Rives Alexandre
#'
#' @export
#'
summary.reg_log <- function(object, ...){
  cat("------------------------------------------------------------------ \n")
  cat("Results of the logistic regression : \n")
  cat("Call : \n", paste(deparse(object$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("Coefficients & Features : ")
  cat("\n")
  write.table(object$coefficients)
  cat("------------------------------------------------------------------ \n")
}

# obj<-fit_reg_log(recode~.,data=breast,mode="batch",normalize = TRUE,learning_rate =0.01 ,iter = 100)
# summary(obj)








