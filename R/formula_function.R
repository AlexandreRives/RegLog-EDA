#' Formula function
#'
#' Function that return a 2 vectors with the x and y variables separated.
#' 
#' @param x A data frame or matrix
#' @author Deffa Ndiaye
#' 
#' @export
#' 
f_Formula <- function(df) {
  ANSWER <- readline("")
  y<-sub("\\~.*", "", ANSWER)
  x<-sub('.*~', '', ANSWER)
  var_explicatives=strsplit(x,split="[+]")
  
  if(length(as.list(var_explicatives[[1]]))>1){
    
    
    #model.frame(df[[y]]~ paste(df[[unlist(var_explicatives)]], collapse= "+"))
    
    formule <- as.formula(paste(str_c(y," ~ "), paste(unlist(var_explicatives), collapse= "+")))
    model.frame(formule,data=df)
    
    
  }
  else if(length(as.list(var_explicatives[[1]]))==1){
    
    
    #model.frame(df[[y]]~df[[x]])
    formule <- as.formula(str_c(y," ~ ",x))
    model.frame(formule,data=df)
  }
  
  else if(x=="."){
    
    formule <- as.formula(paste(str_c(y," ~ "), paste(unlist(colnames(df)), collapse= "+")))
    model.frame(formule,data=df)
    
  }
  
}