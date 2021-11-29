#' Batch gradient descent
#'
#' Batch gradient descent computes the gradient using the whole dataset
#'
#' @param df dataset
#' @param var_X names of the X columns
#' @param var_y names of the Y columns
#' @param learning_rate the learning rate
#' @param max_iter number of iterations
#' @param graph TRUE if you want to plot the cost list while the gradient descent is running.
#' @param epsilon Tolerance's threshold of the cost list convergence.
#'
#' @author Frintz Elisa, NDiaye Deffa, Rives Alexandre
#'
#' @import graphics
#'
#' @export
#'
#' @return list of theta and the cost list
#'
batch_gradient_descent <- function(df, var_X, var_y, learning_rate, max_iter, graph, epsilon){

  # Initializing theta
  theta = rep(1, times = length(var_X) + 1)

  # Creating an empty list
  cost_list = c()

  # Residuals
  residuals <- c()

  # Mixing X and splitting X and Y
  df = sampled_df(df)
  X_df = df[, var_X]
  y_df = df[, var_y]

  if (graph == TRUE){

    X = add_constant(X_df)
    Z = X %*% theta
    h = sigmoid(Z)
    gradient = (t(X) %*% (y_df - h)) / length(y_df)

    # Calculating the cost and adding it to the list
    cost_1 = log_loss_function(y_pred = h, y = y_df)

    Sys.sleep(0.1)
    plot(x = NULL, y = NULL, xlim = c(1,max_iter), ylim = c(0,cost_1),
         xlab = "Iteration", ylab = "Cost", main = "Gradient descent : Batch")
  }

  for (i in 1:max_iter){

    X = add_constant(X_df)
    Z = X %*% theta
    h = sigmoid(Z)
    gradient = (t(X) %*% (y_df - h)) / length(y_df)

    # Calculating the cost and adding it to the list
    cost = log_loss_function(y_pred = h, y = y_df)

    # Calculating the last cost
    last_cost = cost_list[length(cost_list)]

    if (graph == TRUE){
      Sys.sleep(0.1)
      points(x = i, y = cost)
    }
    cost_list = c(cost_list, cost)

    # Filling the residuals list
    residual <- y_df - h
    residuals <- c(residuals, residual)

    # Testing convergence for break
    if (max_iter > 1 & i > 1) {
      diff = abs(last_cost-cost)
      if (diff < epsilon) break
    }

    # Update theta
    theta = theta - (learning_rate * gradient)

  }
  best_theta  = theta

  return(list(best_theta  = best_theta, cost_list = cost_list, residuals = residuals))
}

#' Online stochastic gradient descent
#'
#' Online gradient descent computes the gradient by row using a sampled dataset at each iteration
#'
#' @param df dataset
#' @param var_X names of the X columns
#' @param var_y names of the Y columns
#' @param learning_rate the learning rate
#' @param max_iter number of iterations
#' @param graph TRUE if you want to plot the cost list while the gradient descent is running.
#' @param epsilon Tolerance's threshold of the cost list convergence.
#'
#' @author Frintz Elisa, NDiaye Deffa, Rives Alexandre
#'
#' @import graphics
#'
#' @export
#'
#' @return list of theta and the cost list
#'
online_stochastic_gradient_descent <- function(df, var_X, var_y, learning_rate, max_iter, graph, epsilon){

  # Initializing theta
  row_nb = sample(x = 1:nrow(df), size = 1)
  theta = add_constant(df[row_nb, var_X])

  # Creating an empty list
  cost_list = c()

  # Residuals
  residuals <- c()

  if (graph == TRUE){
    Sys.sleep(0.1)
    plot(x = NULL, y = NULL, xlim = c(1,max_iter), ylim = c(0,2),
         xlab = "Iteration", ylab = "Cost", main = "Gradient descent : Online")
  }

  for (it in 1:max_iter){

    # Mixing X and splitting X and Y
    df = sampled_df(df)
    X_df = df[, var_X]
    y_df = df[, var_y]

    for (i in 1:(nrow(X_df)-1)){
      next_X = add_constant(X_df[i+1,])
      Z = next_X * theta
      h = sigmoid(Z)
      gradient = next_X * (y_df[i] - h)

      # Filling the residuals list
      residual <- y_df[i] - h
      residuals <- c(residuals, residual)

      if (i == (nrow(X_df)-1)){

        # Adding cost to the cost list
        cost = log_loss_function(y_pred = h, y = y_df[i])

        # Calculating the last cost
        last_cost = cost_list[length(cost_list)]

        if (graph == TRUE){
          Sys.sleep(0.1)
          points(x = it, y = cost)
        }
        cost_list = c(cost_list, cost)

        # Testing convergence for break
        if (max_iter > 1 & it > 1) {
          diff = abs(last_cost-cost)
          if (diff < epsilon) break
        }

      }

      # Updating theta
      theta = theta - (learning_rate * gradient)
    }
  }
  best_theta  = theta
  return(list(best_theta  = best_theta, cost_list = cost_list, residuals = residuals))
}

#' Mini_batch gradient descent
#'
#' Mini_batch gradient descent computes the gradient using a batch size of the dataset
#'
#' @param df dataset
#' @param var_X names of the X columns
#' @param var_y names of the Y columns
#' @param learning_rate the learning rate
#' @param max_iter number of iterations
#' @param nb_batch batch size
#' @param graph TRUE if you want to plot the cost list while the gradient descent is running.
#' @param epsilon Tolerance's threshold of the cost list convergence.
#'
#' @author Frintz Elisa, NDiaye Deffa, Rives Alexandre
#'
#' @import graphics
#'
#' @export
#'
#' @return list of theta and the cost list
#'
gradient_mini_batch <- function(df, var_X, var_y, nb_batch, learning_rate, max_iter, graph, epsilon){

  # Initializing theta
  theta <- rep(1, times = length(var_X) + 1)

  # Saving initial dataset
  df_init <- df

  # Creating an empty list
  cost_list <-  c()

  # Residuals
  residuals <- c()

  if (graph == TRUE){
    Sys.sleep(0.1)
    plot(x = NULL, y = NULL, xlim = c(1,max_iter), ylim = c(0,15),
         xlab = "Iteration", ylab = "Cost", main = "Gradient descent : Mini-Batch")
  }

  for (it in 1:max_iter){
    df <- df_mini_batch(df = df, df_initial = df_init, nb_batch = nb_batch)
    # Collecting data
    df_mini = df[1:nb_batch,]
    # Dropping this data in df
    df <- df[-c(1:nb_batch),]

    # X != y
    X_df <- df_mini[, var_X]
    y_df <- df_mini[, var_y]

    X <- add_constant(X_df)
    Z <- X %*% theta
    h <- sigmoid(Z)
    gradient <- t(X) %*% (y_df - h) / length(y_df)

    # Calculating the cost and adding it to the list
    cost = log_loss_function(y_pred = h, y = y_df)

    # Calculating the last cost
    last_cost = cost_list[length(cost_list)]

    if (graph == TRUE){
      Sys.sleep(0.1)
      points(x = it, y = cost)
    }
    cost_list = c(cost_list, cost)

    # Filling the residuals list
    residual <- y_df - h
    residuals <- c(residuals, residual)

    # Testing convergence for break
    if (max_iter > 1 & it > 1) {
      diff = abs(last_cost-cost)
      if (diff < epsilon) break
    }

    # Update theta
    theta = theta - (learning_rate * gradient)
  }

  best_theta  = theta
  return(list(best_theta  = best_theta, cost_list = cost_list, residuals = residuals))

}



