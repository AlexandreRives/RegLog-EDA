#############################-------FORMULA--------#############################



f_Formula <- function(donnee) {
  ANSWER <- readline("")
  y<-sub("\\~.*", "", ANSWER)
  x<-sub('.*~', '', ANSWER)
  var_explicatives=strsplit(x,split="[+]")
  
  if(length(as.list(var_explicatives[[1]]))>1){
    
    
    #model.frame(donnee[[y]]~ paste(donnee[[unlist(var_explicatives)]], collapse= "+"))
    
    formule <- as.formula(paste(str_c(y," ~ "), paste(unlist(var_explicatives), collapse= "+")))
    model.frame(formule,data=donnee)
    
    
  }
  else if(length(as.list(var_explicatives[[1]]))==1){
    
    
    #model.frame(donnee[[y]]~donnee[[x]])
    formule <- as.formula(str_c(y," ~ ",x))
    model.frame(formule,data=donnee)
  }
  
  else if(x=="."){
    
    formule <- as.formula(paste(str_c(y," ~ "), paste(unlist(colnames(donnee)), collapse= "+")))
    model.frame(formule,data=donnee)
    
  }
  
}

f_Formula(iris)