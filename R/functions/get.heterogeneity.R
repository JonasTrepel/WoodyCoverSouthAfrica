
# function to extract heterogeneity of covariates 

get.heterogeneity <- function(vector, raster, grid = NULL, id.col){

  library(tidyverse)
  library(data.table)
  library(terra)
  library(sf)
  
  if(is.null(grid)){
    
    v.trans <- vector %>%
      st_transform(crs(raster))
    
    mean.extraction <- terra::extract(raster, # the rast layers
                                     vect(v.trans), # the spatial polygons, which you have to convert to a terra format
                                     mean, na.rm = T) # since we're dealing with polygons we need to summarize per overlapping pixel somehow
    
    mean.extraction 
    setDT(mean.extraction)
    
    names(mean.extraction) <- c("ID", "mean")
    
    v.id <- vector %>% dplyr::select({{id.col}}) %>% #The double curly braces ({{ }}) are used for non-standard evaluation (NSE) in dplyr
      as.data.table()
    v.id$geom <- NULL
    v.id$geometry <- NULL
    v.id$x <- NULL
    
    mean.extraction <- cbind(v.id, mean.extraction)
    mean.extraction$ID <- NULL
    

    return(mean.extraction)
    
    
    
  }else{

    #regular mean 
    v.trans <- vector %>%
      st_transform(crs(raster))
    
    mean.extraction <- terra::extract(raster, # the rast layers
                                      vect(v.trans), # the spatial polygons, which you have to convert to a terra format
                                      mean, na.rm = T) # since we're dealing with polygons we need to summarize per overlapping pixel somehow
    
    mean.extraction 
    setDT(mean.extraction)
    mean.extraction$x <- NULL
    
    names(mean.extraction) <- c("ID", "mean")
    
    v.id <- vector %>% dplyr::select({{id.col}}) %>% #The double curly braces ({{ }}) are used for non-standard evaluation (NSE) in dplyr
      as.data.table()
    v.id$geom <- NULL
    v.id$geometry <- NULL
    v.id$x <- NULL
    
    mean.extraction <- cbind(v.id, mean.extraction)

    names(mean.extraction) <- c("id.col", "ID", "mean")
    mean.extraction$ID <- NULL
    
    #grid stuff 
    g.trans <- grid %>%
      st_transform(crs(raster))
    
    grid.mean.extraction <- terra::extract(raster, # the rast layers
                                              vect(g.trans), # the spatial polygons, which you have to convert to a terra format
                                              mean, na.rm = T)
    
    grid.mean.extraction 
    setDT(grid.mean.extraction)
    
    names(grid.mean.extraction) <- c("ID", "mean")
    
    ## standard devation as measurement for heterogeneity 
    
    sd.extraction <- terra::extract(raster, # the rast layers
                                            vect(g.trans), # the spatial polygons, which you have to convert to a terra format
                                            sd, na.rm = T) # since we're dealing with polygons we need to summarize per overlapping pixel somehow
    
    sd.extraction 
    setDT(sd.extraction)
    
    names(sd.extraction) <- c("ID", "sd")
    
    
    dt.extr <- left_join(grid.mean.extraction, sd.extraction)
    
    dt.extr$cv <- dt.extr$sd/dt.extr$mean*100
    
    grid <- cbind(grid, dt.extr)
    
    sf_use_s2(FALSE)
    grid.v <- st_join(grid, vector)
    sf_use_s2(TRUE)
    
    # Group by polygon and calculate the mean and maximum number of species
    
    result <- grid.v %>% 
      group_by(eval(parse(text = id.col))) %>%
      summarise(
        grid.cv = mean(cv, na.rm = TRUE), # i wonder if the median would be better here?
        grid.sd = mean(sd, na.rm = TRUE), 
        grid.mean = mean(mean, na.rm = TRUE)
      ) %>% 
      rename(id.col = `eval(parse(text = id.col))`)
    
    
    result$V1 <- NULL
    setDT(result)
    
    final.result <- left_join(result, mean.extraction)
    
    return(final.result)
  }
}
