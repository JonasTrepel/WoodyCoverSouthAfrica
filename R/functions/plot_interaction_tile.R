plot_interaction_tile <- function(m, var1, var2, data, 
                                  var1_name = NULL, var2_name = NULL, resp_name = NULL) {
  library(scales)
  library(tidyverse)
  library(data.table)
  library(MetBrewer)
  library(randomForest)
  library(caret)
  library(gbm)
  library(caretEnsemble)
  library(xgboost)
  
  if(is.null(var1_name)){var1_name = var1}
  if(is.null(var2_name)){var2_name = var2}
  if(is.null(resp_name)){resp_name = "Response"}
  
  
  get_mode <- function(x, na.rm = FALSE) {
    if(na.rm){
      x = x[!is.na(x)]
    }
    
    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
  }
  
  
  setDT(data)
  # Extract the final model from the caret train object
  model <- m$finalModel
  
  lim_var1 <- quantile(data[[var1]], c(.05, .95), na.rm = T)
  lim_var2 <- quantile(data[[var2]], c(.05, .95), na.rm = T)
  
  # Create a grid of values for var1 and var2
  var1_range <- seq(lim_var1[1], lim_var1[2], length.out = 100)
  var2_range <- seq(lim_var2[1], lim_var2[2], length.out = 100)
  grid <- expand.grid(var1 = var1_range, var2 = var2_range)  
  setnames(grid, c("var1", "var2"), c(var1, var2))
  
  # add remaining vars, keep at their median/me
  dt_sub <- data %>% dplyr::select(-all_of(c(var1, var2)))
  
  for(col in colnames(dt_sub)){
    if(col %in% c(var1, var2)){next}
    if(is.numeric(dt_sub[[col]])){
      grid[[col]] <- median(dt_sub[[col]], na.rm = T)
    }else{
      grid[[col]] <- get_mode(dt_sub[[col]], na.rm = T)
    }
  }  
  
  grid_numeric <- grid %>% select(where(is.numeric))
  grid_matrix <- as.matrix(grid_numeric[, model$xNames])
  # Predict outcomes on the grid
  predictions <- predict(model, newdata = grid_matrix)
  grid$prediction <- predictions
  
  mag <- max(grid$prediction, na.rm = T) - min(grid$prediction, na.rm = T)
  
  breaks <- round(c((min(grid$prediction, na.rm = T) + 0.2*mag), 
                    (min(grid$prediction, na.rm = T) + 0.5*mag),
                    (min(grid$prediction, na.rm = T) + 0.8*mag)), 1)
  # Plotting
  setnames(grid, c(var1, var2), c("var1", "var2"))
  
  
  (p <- ggplot(grid, aes(x = var1, y = var2, fill = prediction)) +
    geom_tile(alpha = 0.9) +
    # scale_fill_viridis_c(option = "D", breaks = breaks) +
    # scale_fill_met_c(name = "Pillement", breaks = breaks, direction = -1) +
    scale_x_continuous(breaks = extended_breaks(n = 3)) +
    scale_y_continuous(breaks = extended_breaks(n = 3)) +
    labs(x = var1_name, y = var2_name, fill = resp_name) +
    theme_classic() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 11), 
          axis.title = element_text(size = 12),
          legend.position = "right"))
  
  return(p)
}
