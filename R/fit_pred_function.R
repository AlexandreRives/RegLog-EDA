#' -------------------------------------------------------#
#'
#'                     LogRegEDA
#'                Logistic Regression
#'
#' -------------------------------------------------------#
#'
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
#' @param cores the number of cores to run the fit function faster.
#' @param graph TRUE if you want to plot the cost list while the gradient descent is running.
#' @param epsilon Tolerance's threshold of the cost list convergence.
#'
#' @author Frintz Elisa, NDiaye Deffa, Rives Alexandre
#'
#' @return object used to the predicted part
#'
#' @import doParallel
#' @import foreach
#' @import parallel
#' @import tidyverse
#' @import dplyr
#' @import tidyr
#'
#' @export
#'
#' @return A fitted dataset
#'
fit_reg_log <- function(formula, data, mode, batch_size, normalize = FALSE, learning_rate = 0.1, max_iter = 100, cores = 1, graph = FALSE, epsilon = 0.00001){

  call <- match.call()

  #Test
  ok <- is.data.frame(data)
  if (!ok){
    stop("This is not a data.frame")
  }
  if(nrow(data) == 0){
    stop("You should enter data")
  }
  if((mode != "mini_batch") & (mode != "batch") & (mode != "online") & (mode != "batch_parallel")){
    stop("Wrong mode, please enter one of these : 'batch', 'mini_batch', 'online' or 'batch_parallel'")
  }
  if(mode == "mini_batch"){
    if(batch_size < 1){
      stop("You should enter a batch size greater than 1")
    }
  }
  if(mode == "mini_batch"){
    if(batch_size > nrow(data)){
      stop("The batch size can't be greater than the length of the dataset")
    }
  }
  if(learning_rate <= 0){
    stop("The learning rate must be greater than 0")
  }
  if(max_iter <= 0){
    stop("Max iter has to be greater than 0")
  }

  #split features from target
  df <- f_Formula(formula,data)
  y <- df[1]
  X <- df[,-1]

  #normalization
  if(normalize == TRUE){
    X <- normalize(X)
    df <- cbind(y, X)
  }

  #gradient descent
  if (mode == "batch"){

    start <- Sys.time() # Start time
    coefs <- batch_gradient_descent(df, colnames(X), colnames(y), learning_rate, max_iter, graph, epsilon)
    end <- Sys.time() # End time

    # Execution time
    time <- as.numeric(end - start)

  } else if (mode == "online"){

    start <- Sys.time() # Start time
    coefs <- online_stochastic_gradient_descent(df,colnames(X),colnames(y),learning_rate,max_iter, graph, epsilon)
    end <- Sys.time() # End time

    # Execution time
    time <- as.numeric(end - start)

  } else if (mode == "mini_batch"){

    start <- Sys.time() # Start time
    coefs <- gradient_mini_batch(df,colnames(X),colnames(y),batch_size,learning_rate, max_iter, graph, epsilon)
    end <- Sys.time() # End time

    # Execution time
    time <- as.numeric(end - start)

  } else if (mode == "batch_parallel"){

    start <- Sys.time() #Start time

    # Getting the number of cores
    cores <- ncores(cores)

    # Splitting the dataset in blocs
    blocs <- df %>% mutate(batch = row_number() %% cores) %>% nest(-batch) %>% pull(data)

    # Making the clusters
    cl <- makeCluster(cores)
    registerDoParallel(cl)

    # Foreach loop on the blocs
    res <- foreach(i = blocs, .combine = "cbind", .export = c("batch_gradient_descent", "sampled_df", "add_constant", "sigmoid", "log_loss_function")) %dopar% {
      coefs <- batch_gradient_descent(i, colnames(X), colnames(y), learning_rate, max_iter, graph, epsilon)
      return(coefs)
    }

    # Stopping the clusters
    stopCluster(cl)

    end <- Sys.time() #End time

    # Execution time
    time <- as.numeric(end - start)

    # Mean coefficients for parallel computing
    coefs <- res
    tab_mean_coefs <- as.data.frame(coefs[1])
    s <- seq(from=4, to=cores*3, by=3)

    for(i in s){
      tab_mean_coefs <- cbind(tab_mean_coefs, as.data.frame(coefs[i]))
    }
    coefs_mean <- apply(X = tab_mean_coefs, MARGIN = 1, FUN = mean)

    # Parallel class
    object_parallel <- list()
    object_parallel$coefficients <- coefs_mean
    object_parallel$norm <- normalize
    object_parallel$features <- colnames(X)
    object_parallel$time <- time
    object_parallel$call <- call
    class(object_parallel) <- "reg_log_parallel"

    return(object_parallel)
  }

  #Null Deviance
  #null_deviance <- 2*(logLikelihood_function(coefs$best_theta[-1],df[-1]) - logLikelihood_function(coefs$best_theta[1],df[1]))

  #AIC
  #AIC<- AIC_function(logLikelihood_function(coefs$best_theta,df),df)

  # Degrees of freedom
  ddl_null_deviance <-   nrow(data) - 1
  ddl_residual_deviance <- nrow(data) - ncol(X) - 1

  #Summary residuals
  sum_res <- residuals_summary_function(coefs$residuals)

  # Class
  object <- list()
  object$coefficients <- coefs$best_theta
  object$cost <- coefs$cost_list
  object$features <- colnames(X)
  object$target <- colnames(y)
  object$norm <- normalize
  object$call <- call
  object$summary_residuals <- sum_res
  #object$null_deviance <- null_deviance
  #object$AIC <- AIC
  object$ddl_null_deviance <- ddl_null_deviance
  object$ddl_residual_deviance <- ddl_residual_deviance
  object$time <- time
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
  proba = X_newdata %*% object$coefficients
  proba = sigmoid(proba)
  class_pred = ifelse(proba > 0.5, 1, 0)

  # Part 3 : Print coefs or appliance class
  return(list(proba = proba, class = class_pred))
}

#' Print function
#'
#' Function that print the elements of the fitted object
#'
#' @param x the fitted object
#' @param ... other params
#'
#' @import utils
#'
#' @author Frintz Elisa, NDiaye Deffa, Rives Alexandre
#'
#' @export
#'
print.reg_log <- function(x, ...){

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
  cat("Degrees of Freedom :", x$ddl_null_deviance, "Total (- intercept);",  x$ddl_residual_deviance ,"Residual\n")
  cat("Null Deviance : ", x$null_deviance , "Help 1 !\n")
  cat("AIC : ", x$AIC, "Help 2 !\n")
  cat("\n")
  cat("Execution time :", x$time, "sec.")
  cat("\n")
  cat("\n")
  cat("############################################################################################################### \n")

}

#' Summary function
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
  cat("Null Deviance : ", object$null_deviance , "on", object$ddl_null_deviance, "degrees of freedom\n")
  cat("Residual deviance : ", "on", object$ddl_residual_deviance, "degrees of freedom\n")
  cat("AIC :", object$AIC, " Help !\n")
  cat("\n")
  cat("Execution time :", object$time, "sec.")
  cat("\n")
  cat("\n")
  cat("############################################################################################################### \n")

}

#' Print function for parallel batch mode
#'
#' Function that print the elements of the fitted object
#'
#' @param x the fitted object
#' @param ... other params
#'
#' @import utils
#'
#' @author Frintz Elisa, NDiaye Deffa, Rives Alexandre
#'
#' @export
#'
print.reg_log_parallel <- function(x, ...){

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
  cat("Execution time :", x$time, "sec.")
  cat("\n")
  cat("\n")
  cat("############################################################################################################### \n")

}


