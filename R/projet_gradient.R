###################################################################################
##                            Prog R - Projet - M2 SISE                          ##
###################################################################################

rm(list = ls(all = TRUE))

library(dplyr)
library(PCAmixdata)
library(tidytable)

setwd("~/Documents/M2_SISE/Prog_R/Projet_R")

# Fichier test -> iris_data.csv
df <- read.table("iris_data.csv", header = TRUE, sep = ",")
# 
df1 = df %>% filter(species == "setosa" | species == "versicolor")
df1$species = ifelse(df1$species == "setosa", 1, 0)
head(df1)

Var_X1 = c("sepal_length", "sepal_width", "petal_length", "petal_width")
Var_y1 = c("species")


# Fichier test -> visaPremierR.csv
df <- read.csv("visaPremierR.csv", header = TRUE)
str(df)

df1 = df[,2:ncol(df)]
colnames(df1)

Var_X1 = c("age","anciente","nbopguic","moycred3","aveparmo","endette","engagemt","engagemc","engagemm","nbcptvue","moysold3",
           "moycredi","agemvt","nbop","mtfactur","engageml","nbvie","mtvie","nbeparmo","mteparmo","nbeparlo","mteparlo","nblivret",
           "mtlivret","nbeparlt","mteparlt","nbpaiecb","nbcb","nbcbptar","avtscpte","aveparfi","nbjdebit","ptvente_2","ptvente_3","ptvente_4",
           "ptvente_5","ptvente_6","ptvente_7","sitfamil_Fcel","sitfamil_Fdiv","sitfamil_Fmar","sitfamil_Fsep","sitfamil_Fuli","sitfamil_Fveu","csp_Part","csp_Pcad","csp_Pemp")
Var_y1 = c("cartevpr")

# Fichier test -> heart_train_test.csv
df <- read.csv("heart_train_test.csv", header = TRUE)
str(df)
coeur = ifelse(df$coeur == "presence",1,0)
df_temp = as.data.frame(apply(df[,c("age","pression","cholester","taux_max","pic")], 2, scale))

df1 = cbind(df_temp, coeur)
head(df1)

Var_X1 = c("age","pression","cholester","taux_max","pic")
Var_y1 = c("coeur")



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
  cost = mean((-y_reel * log(y_pred)) - ((1-y_reel) * log(1-y_pred)))
  return(cost)
}



#-----------------------------------------------------# 
# Descente de gradient classique - Batch              #
#-----------------------------------------------------# 

descente_de_gradient_ok <- function(df, Var_X, Var_y, Taux_apprentissage, nb_iteration){
  # Initialize theta
  theta = rep(1, times = length(Var_X) + 1)
  
  # Creation of an empty list
  cost_list = c()
  
  # We differentiate X and y
  X_df = df[, Var_X]
  y_df = df[, Var_y]
  
  for (i in 1:nb_iteration){
    
    X = ajout_constante(X_df)
    Z = X %*% theta
    h = sigmoid(Z)
    gradient = (t(X) %*% (y_df - h)) / length(y_df)

    # Calculation of cost and add to the list
    cost = fct_cout(y_pred = h, y_reel = y_df)
    cost_list = c(cost_list, cost)
    
    # Update theta
    theta = theta - (Taux_apprentissage * gradient)
  }
  
  best_theta  = theta
  return(list(best_theta  = best_theta, cost_list = cost_list))
}

A = descente_de_gradient_ok(df = df1, Var_X = Var_X1, Var_y = Var_y1, Taux_apprentissage = 0.01, nb_iteration = 1000) ; A
plot(A$cost_list, type = "l")

#-----------------------------------------------------# 
# Descente de gradient stochastique - Online          #
#-----------------------------------------------------# 

gradient_online_nbIt_ok_ok <- function(df, Var_X, Var_y, Taux_apprentissage, nb_iteration){
  # Initialize theta 
  num_ligne = sample(x = 1:nrow(df), size = 1)
  theta = ajout_constante(df[num_ligne, Var_X])
  #theta = rep(1, times = length(Var_X) + 1)
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
      theta = theta - (Taux_apprentissage * gradient) 
    }
  }
  best_theta  = theta
  return(list(best_theta  = best_theta, cost_list = cost_list))
}

B = gradient_online_nbIt_ok_ok(df = df1, Var_X = Var_X1, Var_y = Var_y1, Taux_apprentissage = 0.01, nb_iteration = 1000); B
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
    theta = theta - (Taux_apprentissage * gradient)
  }
  best_theta  = theta
  return(list(best_theta  = best_theta, cost_list = cost_list))
  
}

C = gradient_mini_batch_ok_ok(df = df1, Var_X = Var_X1, Var_y = Var_y1, nb_batch = 100, Taux_apprentissage = 0.01, nb_iteration = 2000) ; C
plot(C$cost_list, type = "l")

#######################################################################################
## Comparaison fianle
#######################################################################################

A = descente_de_gradient_ok(df = df1, Var_X = Var_X1, Var_y = Var_y1, Taux_apprentissage = 0.01, nb_iteration = 2000) ; A
plot(A$cost_list, type = "l", main = "Batch")

B = gradient_online_nbIt_ok_ok(df = df1, Var_X = Var_X1, Var_y = Var_y1, Taux_apprentissage = 0.01, nb_iteration = 1000); B
plot(B$cost_list, type = "l", main = "Online")

C = gradient_mini_batch_ok_ok(df = df1, Var_X = Var_X1, Var_y = Var_y1, nb_batch = 100, Taux_apprentissage = 0.01, nb_iteration = 2000) ; C
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

library(doParallel)
library(parallel)

?clusterExport
descente_de_gradient_ok_for_each <- function(df, Var_X, Var_y, Taux_apprentissage, nb_iteration){
  cl = makeCluster(5)
  registerDoParallel(cl)
  clusterExport(cl=cl, c("df", "ajout_constante", "sigmoid", "fct_cout"))
  
  # Initialize theta
  theta = rep(1, times = length(Var_X) + 1)
  
  # Creation of an empty list
  cost_list = c()
  
  # We differentiate X and y
  X_df = df[, Var_X]
  y_df = df[, Var_y]
  
  foreach(i = 1:nb_iteration, .combine = "c") %do% {
    
    X = ajout_constante(X_df)
    Z = X %*% theta
    h = sigmoid(Z)
    gradient = (t(X) %*% (y_df - h)) / length(y_df)
    
    # Calculation of cost and add to the list
    cost = fct_cout(y_pred = h, y_reel = y_df)
    cost_list = c(cost_list, cost)
    
    # Update theta
    theta = theta - (Taux_apprentissage * gradient)
  }
  
  best_theta  = theta
  stopCluster(cl)
  stopImplicitCluster()
  return(list(best_theta  = best_theta, cost_list = cost_list))
}

A = descente_de_gradient_ok_for_each(df = df1, Var_X = Var_X1, Var_y = Var_y1, Taux_apprentissage = 0.01, nb_iteration = 1000) ; A
plot(A$cost_list, type = "l")


