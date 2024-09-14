plotInteractionTile <- function(mod, var1, var2, data) {
  library(scales)
  library(tidyverse)
  library(data.table)
  
  
  Mode <- function(x, na.rm = FALSE) {
    if(na.rm){
      x = x[!is.na(x)]
    }
    
    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
  }
  
  
  setDT(data)
  # Extract the final model from the caret train object
  model <- mod

  limVar1 <- quantile(data[[var1]], c(.05, .95), na.rm = T)
  limVar2 <- quantile(data[[var2]], c(.05, .95), na.rm = T)
  
  # Create a grid of values for var1 and var2
  var1_range <- seq(limVar1[1], limVar1[2], length.out = 100)
  var2_range <- seq(limVar2[1], limVar2[2], length.out = 100)
  grid <- expand.grid(var1 = var1_range, var2 = var2_range)  
  setnames(grid, c("var1", "var2"), c(var1, var2))
  
  # add remaining vars, keep at their median/mode
  dtSub <- data %>% dplyr::select(-all_of(c(var1, var2)))
  
  for(col in colnames(dtSub)){
    if(col %in% c(var1, var2)){next}
    if(is.numeric(dtSub[[col]])){
      grid[[col]] <- median(dtSub[[col]], na.rm = T)
    }else{
      grid[[col]] <- Mode(dtSub[[col]], na.rm = T)
    }
  }  
  # Predict outcomes on the grid
  predictions <- predict(model, newdata = grid)
  grid$Prediction <- predictions
  
  mag <- max(grid$Prediction, na.rm = T) - min(grid$Prediction, na.rm = T)
  
  breaks <- round(c((min(grid$Prediction, na.rm = T) + 0.2*mag), 
              (min(grid$Prediction, na.rm = T) + 0.5*mag),
              (min(grid$Prediction, na.rm = T) + 0.8*mag)), 1)
  # Plotting
  ggplot(grid, aes_string(x = var1, y = var2, fill = "Prediction")) +
    geom_tile() +
    scale_fill_viridis_c(option = "E", breaks = breaks) +
    scale_x_continuous(breaks = extended_breaks(n = 3)) +
    scale_y_continuous(breaks = extended_breaks(n = 3)) +
    theme_classic() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 11), 
          axis.title = element_text(size = 12),
          legend.position = "bottom")
}
