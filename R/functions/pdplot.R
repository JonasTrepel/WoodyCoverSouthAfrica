## function for partial dependence plots 

## will only work if you have a dataframe with unscaled and untransformed variables in the same row order than the predicted outcome
## also naming scheme must be: scaled variables contain "_scaled" and log-transformed variables contain "log_"


pdplot <- function(newdata, var, data, response, model, sig = FALSE, interaction = FALSE, moderator, 
                   point.color = NA, point.size = 1, line.color = NA, dt = FALSE, bayesian = FALSE,
                   palette.name = "Isfahan1"){
  
  ## load packages 
  
  library(tidyverse)
  library(mgcv)
  library(data.table)
  library(MetBrewer)

  ### interaction n ------------------------
if(!interaction == TRUE){  

  for(name in names(newdata)){
    
    #don't take the mean if it's our variable of interest
    if(name == var){next}
    
    #only take the mean if the name doesn't contain scaled
    if(!grepl("scaled", name)){next}

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
    
    tmp <- newdata %>% 
      mutate(mean_col = mean(vect, na.rm = TRUE)) %>%
      as.data.table() %>% 
        dplyr::select(mean_col)
      
      
      var_name <- paste0(name)
      
      setnames(tmp, "mean_col", (var_name))
      
      newdata <- newdata %>% 
        dplyr::select(-c({{var_name}})) %>% 
        cbind(tmp)

    }
    
  ## now new data should contain the mean of all columns, expect of the variable of interest 
  
  ## predict -----------
  pred <- as.data.frame(predict(model, newdata, se.fit = TRUE, allow_new_levels = TRUE))
  
  
  if(bayesian == TRUE){
    
    newdata.pred <- newdata %>% 
      cbind(pred) %>% 
      rename(
        ci.lb = Q2.5, 
        ci.ub = Q97.5, 
        fit = Estimate)
    
  }else{
    
    newdata.pred <- newdata %>% 
      cbind(pred) %>% 
      mutate(
        ci.ub = fit + 1.96*se.fit, 
        ci.lb = fit - 1.96*se.fit)
  }
  
  ## get clean variable name 
  clean.var.scaled <- gsub("log_", "", var)
  clean.var <- gsub("_scaled", "", clean.var.scaled)
  
  ## get colors 
  if(!is.na(point.color)){
  n.colors <- n_distinct(data %>% dplyr::select(all_of(point.color)))
  
  pal <- met.brewer(name = palette.name, n = n.colors)
  }else{pal = "black"}
  ## get data ready 
  
  ### plots -------------------------------

  if(!sig == TRUE){
  
    if(is.na(line.color)){
    pdp <- ggplot() +
       #geom_hline(yintercept = 0, linetype = #"dashed", color = "grey50") +
      geom_point(data = data, aes_string(x = clean.var, y = response, color = point.color, size = point.size), alpha = 0.3, color = "grey75") + 
      geom_point(data = data, aes_string(x = clean.var, y = response, color = point.color, size = point.size), alpha = 0.75) + 
      scale_color_manual(values = pal) +
      geom_ribbon(data = newdata.pred, aes_string(x = clean.var, ymin =  "ci.lb", ymax = "ci.ub"), alpha = 0.25, fill = "grey75") +
      geom_line(data = newdata.pred, aes_string(x = clean.var, y =  "fit"), linewidth = 1, color = "grey75") +
      theme_classic() 
    pdp
    }else{
      pdp <- ggplot() +
         #geom_hline(yintercept = 0, linetype = #"dashed", color = "grey50") +
        geom_point(data = data, aes_string(x = clean.var, y = response, color = point.color, size = point.size), alpha = 0.3, color = "grey75") + 
        geom_point(data = data, aes_string(x = clean.var, y = response, color = point.color, size = point.size), alpha = 0.75) + 
        scale_color_manual(values = pal) +
        scale_fill_manual(values = pal) +
        geom_ribbon(data = newdata.pred, aes_string(x = clean.var, ymin =  "ci.lb", ymax = "ci.ub", color = line.color, fill = line.color), alpha = 0.25) +
        geom_line(data = newdata.pred, aes_string(x = clean.var, y =  "fit"), linewidth = 1, color = "grey75") +
        theme_classic() 
      pdp 
    }

    
}else{
  if(is.na(line.color)){
  pdp <- ggplot() +
     #geom_hline(yintercept = 0, linetype = #"dashed", color = "grey50") +
    geom_point(data = data, aes_string(x = clean.var, y = response, color = point.color, size = point.size), alpha = 0.3, color = "grey75") + 
    geom_point(data = data, aes_string(x = clean.var, y = response, color = point.color, size = point.size), alpha = 0.75) +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    geom_ribbon(data = newdata.pred, aes_string(x = clean.var, ymin =  "ci.lb", ymax = "ci.ub"), alpha = 0.5, color = "grey50", fill = "grey50") +
    geom_line(data = newdata.pred, aes_string(x = clean.var, y =  "fit"), linewidth = 2, color = "black") +
    theme_classic()
  pdp
  }else{
    pdp <- ggplot() +
       #geom_hline(yintercept = 0, linetype = #"dashed", color = "grey50") +
      geom_point(data = data, aes_string(x = clean.var, y = response, color = point.color, size = point.size), alpha = 0.3, color = "grey75") + 
      geom_point(data = data, aes_string(x = clean.var, y = response, color = point.color, size = point.size), alpha = 0.75) +
      scale_color_manual(values = pal) +
      scale_fill_manual(values = pal) +
      geom_ribbon(data = newdata.pred, aes_string(x = clean.var, ymin =  "ci.lb", ymax = "ci.ub", color = line.color, fill = line.color), alpha = 0.5) +
      geom_line(data = newdata.pred, aes_string(x = clean.var, y =  "fit", color = line.color), linewidth = 2) +
     # geom_line(data = newdata.pred, aes_string(x = clean.var, y =  "fit"), linewidth = 2, color = "black") +
      theme_classic()
    pdp 
  }
   

  }  
  print("return regular") 
  dt.pred <- newdata.pred %>% dplyr::select(all_of(clean.var), fit, ci.ub, ci.lb)
  
  ## interaction = T -------------
  }else{
  
  newdata.high <-  newdata
  
  
  for(name in names(newdata.high)){
    
    #don't take the mean if it's our variable of interest
    if(name == var){next}
    
    #only take the mean if the name doesn't contain scaled
    if(!grepl("scaled", name)){next}
    
    
    if(name == moderator){
      
      x_col <- newdata.high %>% 
        dplyr::select({{name}}) %>% 
        as.data.table()
      
      vect <- c(x_col[,1])
      
      if(is.character(vect[[1]])){next}
      
      vect <- c(vect[[1]])
      
      if(is.list(vect)){
        
        vect <- vect[[1]]
      }
      
      if(sum(is.na(vect)) == nrow(data)){next}
      
      tmp.high <- newdata.high %>% 
        mutate(mean_col = quantile(vect, c(0.75), na.rm = TRUE)) %>%
        as.data.table() %>% 
        dplyr::select(mean_col)
      
    }else{
    
    x_col <- newdata.high %>% 
      dplyr::select({{name}}) %>% 
      as.data.table()
    
    vect <- c(x_col[,1])
    
    if(is.character(vect[[1]])){next}
    
    vect <- c(vect[[1]])
    
    if(is.list(vect)){
      
      vect <- vect[[1]]
    }
    
    if(sum(is.na(vect)) == nrow(data)){next}
    
    tmp.high <- newdata.high %>% 
      mutate(mean_col = mean(vect, na.rm = TRUE)) %>%
      as.data.table() %>% 
      dplyr::select(mean_col)
    
    }
    
    
    var_name <- paste0(name)
    
    setnames(tmp.high, "mean_col", (var_name))
  
    newdata.high <- newdata.high %>% 
      dplyr::select(-c({{var_name}})) %>% 
      cbind(tmp.high)
    
  } ### end for loop high names 
  
  ## now new data should contain the mean of all columns, expect of the variable of interest 
  
  unique(newdata.high$enclosure_name)
  
  ## predict for high -----------
  pred.high <- as.data.frame(predict(model, newdata.high, se.fit = TRUE, allow_new_levels = TRUE))
  
  if(bayesian == TRUE){
    
    newdata.high.pred <- newdata.high %>% 
      cbind(pred.high) %>% 
      rename(
        ci.lb = Q2.5, 
        ci.ub = Q97.5, 
        fit = Estimate) %>% 
      mutate(ci.ub.high = ci.ub, 
             ci.lb.high = ci.lb,
             fit.high = fit,
             linetype = "high")
    
  }else{
    
    newdata.high.pred <- newdata.high %>% 
      cbind(pred.high) %>% 
      mutate(
        ci.ub = fit + 1.96*se.fit, 
        ci.lb = fit - 1.96*se.fit,
        ci.ub.high = fit + 1.96*se.fit, 
        ci.lb.high = fit - 1.96*se.fit,
        fit.high = fit,
        linetype = "high")
  }
  
  
  ### low level of moderatpr 
  newdata.low <-  newdata
  
  
  for(name in names(newdata.low)){
    
    #don't take the mean if it's our variable of interest
    if(name == var){next}
    
    #only take the mean if the name doesn't contain scaled
    if(!grepl("scaled", name)){next}
    
    
    if(name == moderator){
      
      x_col <- newdata.low %>% 
        dplyr::select({{name}}) %>% 
        as.data.table()
      
      vect <- c(x_col[,1])
      
      if(is.character(vect[[1]])){next}
      
      vect <- c(vect[[1]])
      
      if(is.list(vect)){
        
        vect <- vect[[1]]
      }
      
      if(sum(is.na(vect)) == nrow(data)){next}
      
      tmp.low <- newdata.low %>% 
        mutate(mean_col = quantile(vect, c(0.25), na.rm = TRUE)) %>%
        as.data.table() %>% 
        dplyr::select(mean_col)
      
    }else{
      
      x_col <- newdata.low %>% 
        dplyr::select({{name}}) %>% 
        as.data.table()
      
      vect <- c(x_col[,1])
      
      if(is.character(vect[[1]])){next}
      
      vect <- c(vect[[1]])
      
      if(is.list(vect)){
        
        vect <- vect[[1]]
      }
      
      if(sum(is.na(vect)) == nrow(data)){next}
      
      tmp.low <- newdata %>% 
        mutate(mean_col = mean(vect, na.rm = TRUE)) %>%
        as.data.table() %>% 
        dplyr::select(mean_col)
      
    }
    
    
    var_name <- paste0(name)
    
    setnames(tmp.low, "mean_col", (var_name))
    
    newdata.low <- newdata.low %>% 
      dplyr::select(-c({{var_name}})) %>% 
      cbind(tmp.low)
    
  } ### end for loop low names 
  
  ## now new data should contain the mean of all columns, expect of the variable of interest and the moderator 
  
 ####predict for low ------------------
  pred.low <- as.data.frame(predict(model, newdata.low, se.fit = TRUE, allow_new_levels = TRUE))
  
  if(bayesian == TRUE){
    
    newdata.low.pred <- newdata.low %>% 
      cbind(pred.low) %>% 
      rename(
        ci.lb = Q2.5, 
        ci.ub = Q97.5, 
        fit = Estimate) %>% 
      mutate(ci.ub.low = ci.ub, 
             ci.lb.low = ci.lb,
             fit.low = fit,
             linetype = "low")
    
  }else{
    
    newdata.low.pred <- newdata.low %>% 
      cbind(pred.low) %>% 
      mutate(
        ci.ub = fit + 1.96*se.fit, 
        ci.lb = fit - 1.96*se.fit,
        ci.ub.low = fit + 1.96*se.fit, 
        ci.lb.low = fit - 1.96*se.fit,
        fit.low = fit,
        linetype = "low")
  }
  
  ## get clean variable name 
  clean.var.scaled <- gsub("log_", "", var)
  clean.var <- gsub("_scaled", "", clean.var.scaled)
  
  ## get colors 
  
  if(!is.na(point.color)){
    n.colors <- n_distinct(data %>% dplyr::select(all_of(point.color)))
    
    pal <- met.brewer(name = palette.name, n = n.colors)
  }else{pal = "black"}
  
  #### plots --------------------

  if(!sig == TRUE){
    
    pdp <- ggplot() +
       #geom_hline(yintercept = 0, linetype = #"dashed", color = "grey50") +
      geom_point(data = data, aes_string(x = clean.var, y = response, color = point.color, size = point.size), alpha = 0.3, color = "grey75") + 
      geom_point(data = data, aes_string(x = clean.var, y = response, color = point.color, size = point.size), alpha = 0.75) + 
      scale_color_manual(values = pal) +
      geom_ribbon(data = newdata.high.pred, aes_string(x = clean.var, ymin =  "ci.lb", ymax = "ci.ub"), alpha = 0.25, fill = "grey75") +
      geom_ribbon(data = newdata.low.pred, aes_string(x = clean.var, ymin =  "ci.lb", ymax = "ci.ub"), alpha = 0.25, fill = "grey75") +
      geom_line(data = newdata.high.pred, aes_string(x = clean.var, y =  "fit", linetype = "linetype"), linewidth = 1, color = "grey75") +
      geom_line(data = newdata.low.pred, aes_string(x = clean.var, y =  "fit", linetype = "linetype"), linewidth = 1, color = "grey75") +
      scale_linetype_manual(values = c("low" = "dashed", "high" = "dotted")) +
      theme_classic()
    
  }else{
    
    pdp <- ggplot() +
       #geom_hline(yintercept = 0, linetype = #"dashed", color = "grey50") +
      geom_point(data = data, aes_string(x = clean.var, y = response, color = point.color, size = point.size), alpha = 0.3, color = "grey75") + 
      geom_point(data = data, aes_string(x = clean.var, y = response, color = point.color, size = point.size), alpha = 0.75) + 
      scale_color_manual(values = pal) +
      geom_ribbon(data = newdata.high.pred, aes_string(x = clean.var, ymin =  "ci.lb", ymax = "ci.ub"), alpha = 0.5, fill = "grey50") +
      geom_ribbon(data = newdata.low.pred, aes_string(x = clean.var, ymin =  "ci.lb", ymax = "ci.ub"), alpha = 0.5, fill = "grey50") +
      geom_line(data = newdata.low.pred, aes_string(x = clean.var, y =  "fit", linetype = "linetype"), linewidth = 2, color = "black") +
      geom_line(data = newdata.high.pred, aes_string(x = clean.var, y =  "fit", linetype = "linetype"), linewidth = 2, color = "black") +
      scale_linetype_manual(values = c("low" = "dashed", "high" = "dotted")) +
      theme_classic()
  
    pdp
    
  }  
  
  dt.pred.low <- newdata.low.pred %>% dplyr::select(all_of(clean.var), fit.low, ci.ub.low, ci.lb.low)
  dt.pred.high <- newdata.high.pred %>% dplyr::select(fit.high, ci.ub.high, ci.lb.high)
  dt.pred <- cbind(dt.pred.low, dt.pred.high)
  print("return interaction") 
  

  } 
  
  if(dt == FALSE){return(pdp)}else{return(dt.pred)}
  
}

