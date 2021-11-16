###################################################################################
##                            Prog R - Projet - M2 SISE                          ##
###################################################################################


rm(list = ls(all = TRUE))

library(dplyr)
library(PCAmixdata)
library(tidytable)

setwd("~/Documents/M2_SISE/Prog_R/Projet_R")

# Fichier test iris (avec variable )
df <- read.table("iris_data.csv", header = TRUE, sep = ",")
# 
df1 = df %>% filter(species == "setosa" | species == "versicolor")
df1$species = ifelse(df1$species == "setosa", 1, 0)
head(df1)

Var_X1 = c("sepal_length", "sepal_width", "petal_length", "petal_width")
Var_y1 = c("species")


#-----------------------------------------------------# 
# Fonctions annexes                                   #
#-----------------------------------------------------# 

sigmoid <- function(x){
  res = 1 / (1 + exp(x))
  return(res)
}
sigmoid(0)


ajout_constante <- function(X){
  X = as.matrix(X)
  X_bis = cbind(1,X)
  return(X_bis)
}


sampled_df <- function(df){
  rows <- sample(nrow(df))
  df_sampled <- df[rows,]
  return(df_sampled)
}

df_mini_batch <- function(df, df_initial, nb_batch){
  if (nrow(df) < nb_batch){
    return(rbind(df, sampled_df(df_initial)))
  } else {
    return(df)
  }
}


fct_cout <- function(y_pred, y_reel){
  cost = mean(-y_reel * log(y_pred) - (1-y_reel) * log(1-y_pred))
  return(cost)
}


# #-----------------------------------------------------# 
# # Traitement du dataset pour les tests                #
# #-----------------------------------------------------# 
# 
# get_dummies_df <- function(X){
#   x_split <- splitmix(X)
#   if (length(x_split$X.quali) != 0){
#     quali_dummies = X %>% get_dummies.(drop_first = TRUE) %>% select(!colnames(X))
#     x_split <- splitmix(X)
#     df_dummies_ok = cbind(x_split$X.quanti, quali_dummies)
#     return(df_dummies_ok)
#   } else {return(X)}
# }
# 
# # Fichier test heart
# df_heart = read.table("heart_train_test.csv", header = TRUE, sep = ",")
# str(df_heart)
# X_df_heart = df_heart[,c("age","sexe","pression","cholester","sucre","electro","taux_max","angine","depression","pic","vaisseau")]
# y_df_heart = df_heart[,"coeur"]
# 
# # Transformation de X_df_heart en X_df_heart_ok
# X_df_heart_ok = get_normalize_df(X_df_heart)
# X_df_heart_ok = get_dummies_df(X_df_heart_ok)
# head(X_df_heart_ok)
# 
# # Transformation de y_df_heart en y_df_heart_ok
# y_df_heart_ok = get_dummies_y(y_df_heart)
# head(y_df_heart_ok)
# 
# df_heart_ok = cbind(X_df_heart_ok, y_df_heart_ok)
# # Pour les tests :
# df1 = df_heart_ok
# Var_X1 = colnames(X_df_heart_ok)
# Var_y1 = colnames(y_df_heart_ok)

#-----------------------------------------------------# 
# Descente de gradient classique - Batch              #
#-----------------------------------------------------# 

descente_de_gradient_ok <- function(df, Var_X, Var_y, Taux_apprentissage, nb_iteration){
  # Initialize theta
  theta = rep(1, times = length(Var_X) + 1) 
  
  # Creation of an empty list
  cost_list = c()
  
  for (i in 1:nb_iteration){
    # We mix and we differentiate X and y
    df = sampled_df(df)
    X_df = df[, Var_X]
    y_df = df[, Var_y]
    
    X = ajout_constante(X_df) 
    Z = X %*% theta 
    h = sigmoid(Z) 
    gradient = t(X) %*% (y_df - h) / length(y_df) 
    
    # Calculation of cost and add to the list
    cost = fct_cout(y_pred = h, y_reel = y_df)
    cost_list = c(cost_list, cost)
    
    # Update theta
    theta = Taux_apprentissage * gradient 
  }
  
  best_theta  = theta
  return(list(best_theta  = best_theta, cost_list = cost_list))
}

A = descente_de_gradient_ok(df = df1, Var_X = Var_X1, Var_y = Var_y1, Taux_apprentissage = 0.1, nb_iteration = 100) ; A
plot(A$cost_list, type = "l")

#-----------------------------------------------------# 
# Descente de gradient stochastique - Online          #
#-----------------------------------------------------# 

gradient_online_nbIt_ok_ok <- function(df, Var_X, Var_y, Taux_apprentissage, nb_iteration){
  # Initialize theta 
  num_ligne = sample(x = 1:nrow(df), size = 1)
  theta = ajout_constante(df[num_ligne, Var_X])
  
  # Creation of an empty list
  cost_list = c()
  
  for (it in 1:nb_iteration){
    # We mix and we differentiate X and y
    df = sampled_df(df)
    X_df = df[, Var_X]
    y_df = df[, Var_y]
    
    
    for (i in 1:(nrow(X_df)-1)){
      X_suivant = ajout_constante(X_df[i+1,])
      Z = X_suivant * theta 
      h = sigmoid(Z) 
      gradient = X_suivant * (y_df[i] - h) 
      
      if (i == (nrow(X_df)-1)){
        # Calculation of cost and add to the list
        cost = fct_cout(y_pred = h, y_reel = y_df[i])
        cost_list = c(cost_list, cost)
      }
      
      # Update theta
      theta = Taux_apprentissage * gradient 
    }
  }
  best_theta  = theta
  return(list(best_theta  = best_theta, cost_list = cost_list))
}

B = gradient_online_nbIt_ok_ok(df = df1, Var_X = Var_X1, Var_y = Var_y1, Taux_apprentissage = 0.1, nb_iteration = 100)
plot(B$cost_list, type = "l", main = "Online")

#-----------------------------------------------------# 
# Descente de gradient stochastique - Mini-batch      #
#-----------------------------------------------------# 

gradient_mini_batch_ok_ok <- function(df, Var_X, Var_y, nb_batch, Taux_apprentissage, nb_iteration){
  # Initialize theta
  theta = rep(1, times = length(Var_X) + 1) 
  
  # Save initial dataset
  df_init = df
  
  # Creation of an empty list
  cost_list = c()
  
  for (i in 1:nb_iteration){
    df = df_mini_batch(df = df, df_initial = df_init, nb_batch = nb_batch)
    # We collect data 
    df_mini = df[1:nb_batch,]
    # We drop this data in df
    df = df[-c(1:nb_batch),]
    
    # We differentiate X and y
    X_df = df_mini[, Var_X]
    y_df = df_mini[, Var_y]
    
    X = ajout_constante(X_df) 
    Z = X %*% theta 
    h = sigmoid(Z) 
    gradient = t(X) %*% (y_df - h) / length(y_df)
    
    # Calculation of cost and add to the list
    cost = fct_cout(y_pred = h, y_reel = y_df)
    cost_list = c(cost_list, cost)
    
    # Update theta
    theta = Taux_apprentissage * gradient 
  }
  best_theta  = theta
  return(list(best_theta  = best_theta, cost_list = cost_list))
  
}

C = gradient_mini_batch_ok_ok(df = df1, Var_X = Var_X1, Var_y = Var_y1, nb_batch = 70, Taux_apprentissage = 0.1, nb_iteration = 100) ; C
plot(C$cost_list, type = "l")

#######################################################################################
## Comparaison fianle
#######################################################################################

A = descente_de_gradient_ok(df = df1, Var_X = Var_X1, Var_y = Var_y1, Taux_apprentissage = 0.1, nb_iteration = 100) ; A
plot(A$cost_list, type = "l", main = "Batch")

B = gradient_online_nbIt_ok_ok(df = df1, Var_X = Var_X1, Var_y = Var_y1, Taux_apprentissage = 0.1, nb_iteration = 100)
plot(B$cost_list, type = "l", main = "Online")

C = gradient_mini_batch_ok_ok(df = df1, Var_X = Var_X1, Var_y = Var_y1, nb_batch = 70, Taux_apprentissage = 0.1, nb_iteration = 100) ; C
plot(C$cost_list, type = "l", main = "Mini-Batch")

#######################################################################################
# Comparaison avec la regression logistique de glm
myreg = glm(species ~ sepal_length+sepal_width+petal_length+petal_width ,data = df1)
print(myreg)
summary(myreg)



############################################################################################################################################
#install.packages("parallel")
library(tictoc)
library(parallel)

matprod_par <- function(nb_clust, matA, matB){
  if(ncol(matA) != nrow(matB)) stop("Matrices do not conform.")
  if (nrow(matA)*ncol(matB) < 700000){
    return(matA %*% matB)
  } else { 
    cl <- makeCluster(nb_clust)
    idx <- splitIndices(nrow(matA), length(cl))
    Alist <- lapply(idx, function(ii) matA[ii,,drop=FALSE])
    ans <- clusterApply(cl, Alist, get("%*%"), matB)
    stopCluster(cl)
    return(do.call(rbind, ans))
  }
}

# ?matrix
A <- matrix(data = 1:500, nrow = 1000, ncol = 600, byrow = TRUE)
B <- matrix(data = 1:500, nrow = 600, ncol = 1000, byrow = TRUE)

A <- matrix(data = 1:500, nrow = 1000, ncol = 700, byrow = TRUE)
B <- matrix(data = 1:500, nrow = 700, ncol = 1000, byrow = TRUE)

# Seuil à 700 000 de données 
#detectCores()

tic()
C = matprod_par(nb_clust = 7, matA = A, matB = B)
toc()

tic()
D = A %*% B
toc()

###########################################################################################"




