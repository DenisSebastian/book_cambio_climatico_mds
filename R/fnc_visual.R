# objetivos: Funciones de visualización
# autor denis berroetas
# creación: 15-06-2023


# read Raster -------------------------------------------------------------
library(terra)
library(dplyr)
library(lubridate)
library(mapview)
library(ggplot2)
library(viridis)



# carta from raster -------------------------------------------------------

map_raster <- function(r, title, name_legend, option_col = "A"){
  
  if("RasterLayer" == class(r)[1]){
    raster_df  <- raster::as.data.frame(r, xy = TRUE) %>% na.omit()
  }else if("SpatRaster" == class(r)[1]){
    raster_df  <- terra::as.data.frame(r, xy = TRUE) %>% na.omit()
  }else{
    stop("Raster no permite genera map")
  }
  names(raster_df)[3] <-  "layer"
  map_raster_df <- ggplot() +
    geom_tile(data = raster_df , 
              aes(x = x, y = y, 
                  fill = layer)) + 
    scale_fill_viridis_c(name = name_legend, 
                         option = option_col, direction = 1)+
    coord_equal()+
    ggtitle(paste0(title) ) +
    theme_bw() +
    theme(panel.grid.major = element_line(colour = "gray99"), 
          panel.grid.minor = element_line(colour = "gray99"))
  
  
  return(map_raster_df)
}



# ggsave(plot = map_raster_df,filename = "images/test_pr.png")
