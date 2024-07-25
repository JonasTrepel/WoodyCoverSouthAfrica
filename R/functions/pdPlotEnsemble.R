## function for partial dependence plots 

## will only work if you have a dataframe with unscaled and untransformed variables in the same row order than the predicted outcome
## also naming scheme must be: scaled variables contain "_scaled" and log-transformed variables contain "log_"


pdPlotEnsemble <- function(newdata, var, model, se.fit = FALSE){
  
  ## load packages 
  
  library(tidyverse)
  library(mgcv)
  library(data.table)
  library(MetBrewer)
  
  ND <- data.table()
    
    for(name in names(newdata)){
      
      x_col <- newdata %>% 
        dplyr::select({{name}}) %>% 
        as.data.table()
      
      vect <- c(x_col[,1])
      
      if(is.character(vect[[1]])){next}
      
      vect <- c(vect[[1]])
      
      if(is.list(vect)){
        
        vect <- vect[[1]]
      }
      
      if(sum(is.na(vect)) == nrow(data)){next}
      
      if(name == var){
        
        
        tmp <- as.data.table(seq(min(newdata[[var]], na.rm = T), max(newdata[[var]], na.rm = T), length.out = 100))
        
        
        var_name <- paste0(name)
        
        setnames(tmp, "V1", (var_name))
        
      }else{
        
      
        tmp <- as.data.table(rep(mean(newdata[[var]], na.rm = T), length.out = 100))
      
      
      var_name <- paste0(name)
      
      setnames(tmp, "V1", (var_name))
      }
      
      ND <- cbind(ND, tmp)
     
    }
    
    ## now new data should contain the mean of all columns, expect of the variable of interest 
    
    ## predict -----------

  if(se.fit == FALSE){
    pred <- as.data.frame(predict(model, ND)) %>% 
      rename(y = `predict(model, ND)`)
        
    newdata.pred <- ND %>% 
        cbind(pred)
    
    dt.pred <- newdata.pred %>% dplyr::select(all_of(var), y)
    
  }else{
    
    pred <- as.data.frame(predict(model, ND, se.fit = TRUE)) %>% 
      rename(fit = `predict(model, ND, se.fit = TRUE)`)
    
    newdata.pred <- ND %>% 
      cbind(pred) %>% 
      mutate(
        ci.ub = fit + 1.96*se.fit, 
        ci.lb = fit - 1.96*se.fit)
    
    dt.pred <- newdata.pred %>% dplyr::select(all_of(var), fit, ci.ub, ci.lb)
    
  }
    
    return(dt.pred)
    
}
