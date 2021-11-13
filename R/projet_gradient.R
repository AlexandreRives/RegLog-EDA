###################################################################################
##                            Prog R - Projet - M2 SISE                          ##
###################################################################################


rm(list = ls(all = TRUE))

library(dplyr)

setwd("~/Documents/M2_SISE/Prog_R/Projet_R")

# Fichier test iris (avec variable )
df <- read.table("iris_data.csv", header = TRUE, sep = ",")

df1 = df %>% filter(species == "setosa" | species == "versicolor")
df1$species = ifelse(df1$species == "setosa", 1, 0)
head(df1)

Var_X1 = c("sepal_length", "sepal_width", "petal_length", "petal_width")
Var_y1 = c("species")

#X_df1 = df1 %>% select("sepal_length", "sepal_width", "petal_length", "petal_width")
#y_df1 = df1$species
# 
# test = c("sepal_length", "sepal_width", "petal_length", "petal_width")
# df_test = df1 %>% select(test)
# head(df_test)


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

Creer_groupes_mini_batch <- function(df, nb_batch){
  # On melange
  df = sampled_df(df)
  nb_group = nrow(df) %/% nb_batch
  ls = list()
  for (i in 1:nb_group){
    df_mini = df[1:nb_batch,] # On prends les nb_batch premiers
    ls[[i]] = df_mini         # On ajoute le dataframe à la liste
    df = df[-c(1:nb_batch),]  # On supprime ce qu'on a prit du dataset df
  }
  return(ls)
}
Creer_groupes_mini_batch(df = df1, nb_batch = 8)

#-----------------------------------------------------# 
# Descente de gradient classique - Batch              #
#-----------------------------------------------------# 

# Batch + nb_iteration 

descente_de_gradient_ok <- function(df, Var_X, Var_y, Taux_apprentissage, nb_iteration){

  # Initialiser theta
  theta = rep(0, times = length(Var_X)+1) ; theta
  
  for (i in 1:nb_iteration){
    # On melange et on differencie X et y
    df = sampled_df(df)
    X_df = df[, Var_X]
    y_df = df[, Var_y]
    
    X = ajout_constante(X_df) ; X
    Z = X %*% theta ; Z
    h = sigmoid(Z) ; h
    gradient = t(X) %*% (y_df - h) / length(y_df) ; gradient
    theta = Taux_apprentissage * gradient ; theta
    #print(theta)
  }
  theta_final = theta
  return(theta_final)
}

descente_de_gradient_ok(df = df1, Var_X = Var_X1, Var_y = Var_y1, Taux_apprentissage = 0.1, nb_iteration = 10)


#-----------------------------------------------------# 
# Descente de gradient stochastique - Online          #
#-----------------------------------------------------# 

# Online + nb_iteration + Melange dataset 

gradient_online_nbIt_ok <- function(df, Var_X, Var_y, Taux_apprentissage, nb_iteration){
  # Initialiser theta (1ere ligne du tableau)
  num_ligne = sample(x = 1:nrow(df), size = 1)
  theta = ajout_constante(df[num_ligne, Var_X])
  #theta = rep(0, times = length(Var_X)+1) ; theta
  
  for (it in 1:nb_iteration){
    # On melange et on differencie X et y
    df = sampled_df(df)
    X_df = df[, Var_X]
    y_df = df[, Var_y]
    
    for (i in 1:(nrow(X_df)-2)){
      X_suivant = ajout_constante(X_df[i+1,])
      Z = X_suivant * theta 
      h = sigmoid(Z) 
      gradient = X_suivant * (y_df[i] - h) 
      theta = Taux_apprentissage * gradient 
      #print(theta)
    }
    #it = it + 1
  }
  theta_final = theta
  return(theta_final)
}

gradient_online_nbIt_ok(df = df1, Var_X = Var_X1, Var_y = Var_y1, Taux_apprentissage = 0.1, nb_iteration = 10)


#-----------------------------------------------------# 
# Descente de gradient stochastique - Mini-batch      #
#-----------------------------------------------------# 

gradient_mini_batch_ok <- function(df, Var_X, Var_y, nb_batch, Taux_apprentissage, nb_iteration){
  
  # Initialiser theta à 0
  theta = rep(0, times = length(Var_X)+1) ; theta
  
  # On stocke le dataset initial
  df_init = df
  
  # Calcul du nombre de tour possible en parcourant une fois le dataset 
  nb_tour = nrow(df_init) %/% nb_batch
  
  # On crée les groupes mini batch une première fois 
  #liste_mini_batch = Creer_groupes_mini_batch(df = df_init, nb_batch = nb_batch)
  
  for (it in 1:nb_iteration){
    # On crée les groupes mini batch 
    liste_mini_batch = Creer_groupes_mini_batch(df = df_init, nb_batch = nb_batch)
    
    for (i in 1:nb_tour){
      # On melange et on differencie X et y
      df = liste_mini_batch[[i]]
      X_df = df[, Var_X]
      y_df = df[, Var_y]
      
      X = ajout_constante(X_df) ; X
      Z = X %*% theta ; Z
      h = sigmoid(Z) ; h
      gradient = t(X) %*% (y_df - h) / length(y_df) ; gradient
      theta = Taux_apprentissage * gradient ; theta
      #print(theta)
    }
  }
  theta_final = theta
  return(theta_final)
}

gradient_mini_batch_ok(df = df1, Var_X = Var_X1, Var_y = Var_y1, nb_batch = 8, Taux_apprentissage = 0.1, nb_iteration = 10)

#######################################################################################
## Comparaison fianle
#######################################################################################

descente_de_gradient_ok(df = df1, Var_X = Var_X1, Var_y = Var_y1, Taux_apprentissage = 0.1, nb_iteration = 10)

gradient_online_nbIt_ok(df = df1, Var_X = Var_X1, Var_y = Var_y1, Taux_apprentissage = 0.1, nb_iteration = 10)

gradient_mini_batch_ok(df = df1, Var_X = Var_X1, Var_y = Var_y1, nb_batch = 8, Taux_apprentissage = 0.1, nb_iteration = 10)

############################################################################################################################################
#install.packages("parallel")
library(tictoc)
library(parallel)

matprod_par <- function(cl, matA, matB){
  if(ncol(matA) != nrow(matB)) stop("Matrices do not conform")
  idx <- splitIndices(nrow(matA), length(cl))
  Alist <- lapply(idx, function(ii) matA[ii,,drop=FALSE])
  ans <- clusterApply(cl, Alist, get("%%"), matB)
  #ans <- clusterApply(cl, Alist, function(aa, BB) aa %% BB, matB)
  do.call(rbind, ans)
}

matA <- matrix(1:500,100, 1000,TRUE)
matB <- matrix(1:500,1000, 100,TRUE)

detectCores()
cl <- makeCluster(5)
clusterExport(cl, c("matA", "matB","matprod_par"))
tic()
matprod_par(cl, matA, matB)
toc()
stopCluster(cl)

tic()
matA %*% matB
toc()

sessionInfo()
