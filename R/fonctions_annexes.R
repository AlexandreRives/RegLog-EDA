


fct_formula <- function(f){
  f = formula(f)
  y = all.vars(f)[1]
  x = all.vars(f)[2:length(all.vars(f))]
  return(list(y = y, x = x))
}

filter_df <- function(df, var){
  df_filter <- df[, var]
  return(df_filter)
}


# Fonction get_dummies_df : 
# Input : dataset entier 
# Output : dataset entier dont les variables quanti sont normalisé
get_normalize_df <- function(x){
  x_split <- splitmix(x)
  x_scale <- apply(x_split$X.quanti, FUN=scale, MARGIN = 2)
  x_scale = as.data.frame(x_scale)
  df_normalize_ok = cbind(x_scale, x_split$X.quali)
  return(df_normalize_ok)
}



# Fonction get_dummies_df : 
# Input : dataset entier 
# Output : dataset entier dont les variables quali sont devenu quanti
get_dummies_df <- function(X){
  x_split <- splitmix(X)
  if (length(x_split$X.quali) != 0){
    quali_dummies = X %>% get_dummies.(drop_first = TRUE) %>% select(!colnames(X))
    x_split <- splitmix(X)
    df_dummies_ok = cbind(x_split$X.quanti, quali_dummies)
    return(df_dummies_ok)
  } else {return(X)}
}


get_dummies_y <- function(y){
  y_dum = as.data.frame(y) %>% get_dummies.(drop_first = TRUE) %>% select(!y)
  return(y_dum)
}



df_mini_batch <- function(df, df_initial, nb_batch){
  if (nrow(df) < nb_batch){
    return(rbind(df, sampled_df(df_initial)))
  } else {
    return(df)
  }
}







