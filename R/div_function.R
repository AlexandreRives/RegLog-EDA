#' Formula function
#'
#' Function that return a filtered dataframe according to the equation given by the user.
#'
#' @param formula the logistic regression equation
#' @param data A data frame or matrix
#' @author Frintz Elisa, NDiaye Deffa, Rives Alexandre
#'
#' @import  stats
#'
#' @export
#'
#' @return a filtered dataframe
#
#'
f_Formula <- function(formula,data) {
  df<- stats::model.frame(formula,data=data)
  return(df)
}


#' Normalize
#'
#' Process of putting different features on the same scale
#'
#' @param x A data frame or matrix
#' @author Frintz Elisa, NDiaye Deffa, Rives Alexandre
#'
#' @import PCAmixdata
#'
#' @export
#'
#' @return A standardize data set
#'
normalize <- function(x){
  x_split <- splitmix(x)
  x_scale <- apply(x_split$X.quanti, FUN=scale, MARGIN = 2)
  return(x_scale)
}

#' Get the cores of the CPU
#'
#' Process of detecting the number of cores in the CPU and return it to the function.
#'
#' @param n_cores A number of cores
#' @author Frintz Elisa, NDiaye Deffa, Rives Alexandre
#'
#'
#' @import parallel
#' 
#' @export
#'
#' @return The number of cores.
#'
ncores <- function(n_cores){
  cores = parallel::detectCores()
  if((n_cores <= 0) | (n_cores >= cores)){
    n_cores <- cores-1
    print(paste("You have entered a wrong number of cores. The algorithm is going to run with", n_cores, "cores."))
    return(n_cores)
  }else{
    return(n_cores)
  }
}

#' Filter feature variables
#'
#' Process of returning a data set filtered by the features
#'
#' @param df dataframe
#' @param x features
#' @author Frintz Elisa, NDiaye Deffa, Rives Alexandre
#'
#' @export
#'
#' @return A data set filtered with the features.
#'
filtered_x <- function(df, x){
  df_x <- df[x]
  return(df_x)
}

#' Filter target variable
#'
#' Process of returning a data set filtered by the target variable
#'
#' @param df A data frame or matrix
#' @param y data of the target variable
#' @author Frintz Elisa, NDiaye Deffa, Rives Alexandre
#'
#' @export
#'
#' @return A data set filtered by the target variable.
#'
filtered_y <- function(df, y){
  df_y <- df[y]
  return(df_y)
}

#' Sample a dataset
#'
#' Process of returning a sampled dataset
#'
#' @param df dataset
#' @author Frintz Elisa, NDiaye Deffa, Rives Alexandre
#'
#' @export
#'
#' @return A sampled dataset.
#'
sampled_df <- function(df){
  rows <- sample(nrow(df))
  df_sampled <- df[rows,]
  return(df_sampled)
}

#' Sigmoid function
#'
#' Formula of the sigmoid function
#'
#' @param x dataset
#' @author Alexandre Rives, NDiaye Deffa, Frintz Elisa
#' 
#' @export
#'
#' @return sigmoid
#'
sigmoid <- function(x){
  res = 1 / (1 + exp(x))
  return(res)
}

#' Add constant function
#'
#' Process of adding a constant into the dataframe
#'
#' @param X X dataset
#' @author Alexandre Rives, NDiaye Deffa, Frintz Elisa
#'
#' @export
#'
#' @return X dataset with the constant
#'
add_constant <- function(X){
  X = as.matrix(X)
  X_bis = cbind(1,X)
  return(X_bis)
}

#' Mini batch
#'
#' Take a number of lines of the dataset
#'
#' @param df dataset without the last mini batch
#' @param df_initial full dataset
#' @param nb_batch size of the batch
#' @author Alexandre Rives, NDiaye Deffa, Frintz Elisa
#'
#' @export
#'
#' @return a mini batch
#'
df_mini_batch <- function(df, df_initial, nb_batch){
  if (nrow(df) < nb_batch){
    return(rbind(df, sampled_df(df_initial)))
  } else {
    return(df)
  }
}

#' Log Loss function
#'
#' The function that return the log loss function
#'
#' @param y_pred the y predicted
#' @param y targets
#' @author Alexandre Rives, NDiaye Deffa, Frintz Elisa
#'
#' @export
#'
#' @return the mean of the losses
#'
log_loss_function <- function(y_pred, y){
  loss = mean((-y * log(y_pred)) - ((1-y) * log(1-y_pred)))
  return(loss)
}


#' Residuals summary
#'
#' The function that return the residuals summary
#'
#' @param residuals residuals list
#' @author Alexandre Rives, NDiaye Deffa, Frintz Elisa
#'
#' @export
#'
#' @return residuals summary
#'
residuals_summary_function <- function(residuals){

  mean <- mean(residuals)
  Q1 <- quantile(residuals, 0.25)
  median <- median(residuals)
  Q3 <- quantile(residuals, 0.75)
  min <- min(residuals)
  max <- max(residuals)

  summary_residuals <- as.data.frame(rbind(round(c(min, Q1, median, Q3, max, mean),4)))
  colnames(summary_residuals) <- c('Min', 'Q1', 'Median', 'Q3', 'Max', 'Mean')

  return(summary_residuals)
}


#' logLikelihood
#'
#' The function that return the logLikelihood
#'
#' @param coef coefficients of the model
#' @param x a dataframe
#' @author Alexandre Rives, NDiaye Deffa, Frintz Elisa
#'
#' @export
#'
#' @return the Log-Likelihood
#'
logLikelihood_function<-function (coef,x){
  i<-1
  somme<-list()
  pi<-list()
  LL<-list()
  logLikelihood<-0
  for (i in 1:nrow(x)){

    somme[[i]]<-sum(x[i,2:ncol(x)]*coef[2:ncol(x)],coef[1])
    pi[[i]] <-exp(somme[[i]])/(1+exp(somme[[i]]))

    LL[[i]]= x[i,1]*log(pi[[i]])+(1-x[i,1])*log(1-pi[[i]])

    logLikelihood<-logLikelihood+LL[[i]]

  }

  return(logLikelihood)

}


#' AIC
#'
#' The function that return the Akaike Information Criterion (AIC)
#'
#' @param LL the Log-Likelihood
#' @param df the dataframe of the model
#' @author Alexandre Rives, NDiaye Deffa, Frintz Elisa
#'
#' @export
#'
#' @return the AIC of the model
#'
AIC_function<-function(LL,df){

  AIC<-(-2*LL+2*ncol(df))
  return(AIC)
}

