#' Fit function
#'
#' Function that return a trained dataset using the gradient descent.
#'
#' @param formula Enter the target and the feature.
#' @param data the data frame.
#' @param mode choose between : batch, mini-batch or online.
#' @param batch_size the number of observations for the mini batch mode.
#' @param normalize TRUE if you want to scale your features. It doesn't take the qualitative data.
#' @param max_iter number of iterations.
#' @param learning_rate used for the gradient descent.
#' @param ncores the number of cores to run the fit function faster.
#'
#' @author Frintz Elisa, NDiaye Deffa, Rives Alexandre
#'
#' @return object used to the predicted part
#'
#' @import doParallel
#' @import foreach
#' @import parallel
#'
#' @export
#'
#' @return A fitted dataset
#'
fit_reg_log <- function(formula, data, mode, batch_size, normalize = FALSE, learning_rate, max_iter, ncores = 1){

  call <- match.call()

  #Test
  ok <- is.data.frame(data)
  if (!ok){
    stop("This is not a data.frame")
  }
  if(nrow(data) == 0){
    stop("You should enter data")
  }
  # if(mode != "mini_batch" | mode != "batch" | mode != "online"){
  #   stop("Wrong mode, please enter one of these : 'batch', 'mini_batch' or 'online'")
  # }
  if(batch_size < 1){
    stop("You should enter a batch size greater than 1")
  }
  if(batch_size > nrow(data)){
    stop("The batch size can't be greater than the length of the dataset")
  }
  if(learning_rate <= 0){
    stop("The learning rate must be greater than 0")
  }
  if(max_iter <= 0){
    stop("Max iter has to be greater than 0")
  }


  #separate features from target
  df <- f_Formula(formula,data)
  y <- df[1]
  X <- df[,-1]

  #normalization
  if(normalize == TRUE){
    X <- normalize(X)
    df <- cbind(X, y)
  }

  ncores <- ncores(ncores)
  blocs <- split(df, 1 + (1:nrow(df)) %% ncores)
  cl <- makeCluster(ncores)
  registerDoParallel(cl)

  #gradient descent
  if (mode == "batch"){
    coefs <- foreach(i = blocs, .combine = "cbind", .export = c("batch_gradient_descent", "sampled_df", "add_constant", "sigmoid", "log_loss_function")) %dopar% {
      coefs <- batch_gradient_descent(df, colnames(X), colnames(y), learning_rate, max_iter)
    }
    #coefs <- batch_gradient_descent(df, colnames(X), colnames(y), learning_rate, iter)
  } else if (mode == "online"){
    coefs <- foreach(i = blocs, .combine = "cbind", .export = c("online_stochastic_gradient_descent", "sampled_df", "add_constant", "sigmoid", "log_loss_function")) %dopar% {
      coefs <- online_stochastic_gradient_descent(df, colnames(X), colnames(y), learning_rate, max_iter)
    }
    #coefs <- online_stochastic_gradient_descent(df,colnames(X),colnames(y),learning_rate,iter)
  } else if (mode == "mini_batch"){
    coefs <- foreach(i = blocs, .combine = "cbind", .export = c("gradient_mini_batch", "sampled_df", "df_mini_batch", "add_constant", "sigmoid", "log_loss_function")) %dopar% {
      coefs <- gradient_mini_batch(df, colnames(X), colnames(y), batch_size, learning_rate, max_iter)
    }
    #coefs <- gradient_mini_batch(df,colnames(X),colnames(y),batch_size,learning_rate,iter)
  }
  stopCluster(cl)


  #Null Deviance

  Null_Deviance <- 2*(logLikelihood_function(coefs$best_theta[-1],df[-1]) - logLikelihood_function(coefs$best_theta[1],df[1]))

  #AIC

  AIC<- AIC_function(logLikelihood_function(coefs$best_theta,df),df)




  #Summary residuals
  sum_res <- residuals_summary_function(coefs$residuals)

  #class
  object <- list()
  object$coefficients <- coefs$best_theta
  object$cost <- coefs$cost_list
  object$features <- colnames(X)
  object$target <- colnames(y)
  object$norm <- normalize
  object$call <- call
  object$summary_residuals <- sum_res
  object$Null_Deviance<-Null_Deviance
  object$AIC<-AIC
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
predict_reg_log <- function(object, newdata, type){

  # Part 1 : Create a new data
  X_newdata = newdata[, object$features]
  if (object$norm == TRUE) {X_newdata = as.data.frame(apply(X_newdata, 2, scale))}
  X_newdata = add_constant(X_newdata)

  # Part 2 : Set the coefs on the new data
  prob = X_newdata %*% object$coefficients

  # Part 3 : Print coefs or appliance class
  if(type == "posterior"){
    prob = sigmoid(prob)
    print(prob)
  }else if(type == "class"){
    class_pred = ifelse(prob > 0.5, 1, 0)
    print(class_pred)
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
  cat("Coefficients : ", x$coefficients, "\n")
  cat("Null Deviance : ", x$Null_Deviance , "\n")
  cat("AIC : ", x$AIC, "\n")
  cat("------------------------------------------------------------------ \n")
  df_print <- as.data.frame(rbind(c(x$coefficients)))
  colnames(df_print) <- c("(Intercept)", x$features)

  cat("############################################################################################################### \n")
  cat("\n")
  cat("Results of the logistic regression : \n")
  cat("Call : \n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("\n")
  cat("Coefficients & Features : ")
  cat("\n")
  print(df_print)
  cat("\n")
  cat("############################################################################################################### \n")

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

  df_print <- as.data.frame(rbind(c(object$coefficients)))
  colnames(df_print) <- c("(Intercept)", object$features)

  cat("############################################################################################################### \n")
  cat("\n")
  cat("Results of the logistic regression : \n")
  cat("Call : \n", paste(deparse(object$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("Deviance Residuals :")
  cat("\n")
  print(object$summary_residuals)
  cat("\n")
  cat("Coefficients & Features : ")
  cat("\n")
  print(df_print)
  cat("\n")
  cat("############################################################################################################### \n")

}


#heart<- read.table("C:/Users/dia/Downloads/heart_propre.csv")


# tic()
#obj <- fit_reg_log(recode~., data=breast, mode="batch", normalize = TRUE, learning_rate =0.1 , max_iter = 100, ncores = 1)
# toc()

#
#obj <- fit_reg_log(coeur~., data=heart, mode="batch", normalize = FALSE, learning_rate =0.1 ,iter = 1000)
#
# fit_reg_log(recode~., data=breast, mode="mini_batch", batch_size = 10, normalize = FALSE, learning_rate =0.1 ,iter = 1000)
#
#summary(obj)

#print(obj)



# obj <- fit_reg_log(recode~., data=breast, mode="batch", normalize = FALSE, learning_rate =0.1 ,iter = 1000)
#
# fit_reg_log(recode~., data=breast, mode="mini_batch", batch_size = 10, normalize = FALSE, learning_rate =0.1 ,iter = 1000)
#
# summary(obj)
# print(obj)

#glm(coeur~., data=heart)
