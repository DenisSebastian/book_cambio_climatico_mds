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

map_raster <- function(r, title, name_legend, option_col = "A", 
                       limits = NULL, nbreaks = NULL){
  
  borders <- sf::st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))
  bbox <- sf::st_bbox(r)
  
  if("RasterLayer" == class(r)[1]){
    raster_df  <- raster::as.data.frame(r, xy = TRUE) %>% na.omit()
  }else if("SpatRaster" == class(r)[1]){
    raster_df  <- terra::as.data.frame(r, xy = TRUE) %>% na.omit()
  }else{
    stop("Raster no permite genera map")
  }
  names(raster_df)[3] <-  "layer"
  map_raster_df <- ggplot() +
    geom_tile(data = raster_df ,  na.rm = TRUE,
              aes(x = x, y = y, 
                  fill = layer)) + 
    geom_sf(data = borders, fill=NA, na.rm = TRUE, colour="white") +
    coord_sf(xlim = bbox[c(1,3)],ylim = bbox[c(2,4)]) +
    scale_fill_viridis_c(name = name_legend,     
                         limits = limits, n.breaks=nbreaks,
                         option = option_col, direction = 1)+
    guides(fill = guide_colourbar(barwidth = 0.8,
                                  barheight = 10))+
    ggtitle(title ) +
    theme_bw() +
    theme(panel.grid.major = element_line(colour = "gray90"), 
          panel.grid.minor = element_line(colour = "gray90"))
  
  
  return(map_raster_df)
}

map_raster_regions <- function(r, title, name_legend, option_col = "A", 
                       limits = NULL, nbreaks = NULL, 
                       regiones_num = c(paste0("0", 3:9), "16"), 
                       path_regions = "data/rds/Regiones_Chile.rds"){
  
  borders <- sf::st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))
  bbox <- sf::st_bbox(r)
  regiones <- readRDS(path_regions) %>%  sf::st_transform(4326)
  
  
  if(is.null(regiones_num)){
    bbox_filter <- sf::st_bbox(crs_assign(r, crs_new = 4326))%>% st_as_sfc()
    print("Filtrando las Regiones")
    reg_subset <-  sf::st_intersection(regiones, bbox_filter)
    
  }else{
    reg_subset <- regiones %>% dplyr::filter(REGION %in% regiones_num)
  }
  

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
    geom_sf(data = borders, fill=NA, colour="gray60", na.rm = TRUE,  linewidth= 0.5) +
    geom_sf(data = reg_subset, fill=NA, colour="white") +
    coord_sf(xlim = bbox[c(1,3)],ylim = bbox[c(2,4)]) +
    scale_fill_viridis_c(name = name_legend,     
                         limits = limits, n.breaks=nbreaks,
                         option = option_col, direction = 1)+
    guides(fill = guide_colourbar(barwidth = 0.8,
                                  barheight = 10))+
    ggtitle(paste0(title) ) +
    theme_bw() +
    theme(panel.grid.major = element_line(colour = "gray90"), 
          panel.grid.minor = element_line(colour = "gray90"))
  
  
  return(map_raster_df)
}

save_map_r <- function(r, title, name_legend, option_col = "A",
                       filename_out, width_out = 8, height_out = 10,
                       limits = NULL, nbreaks = NULL, div_adm =T, 
                       path_regions = "data/rds/Regiones_Chile.rds",
                       regiones_num = c(paste0("0", 3:9), "16")){
  

  
  if(isTRUE(div_adm)){
    mapa <- map_raster_regions(
      r = r,
      name_legend = name_legend,
      title = title,
      option_col = option_col,
      limits = limits, nbreaks = nbreaks
    )
    
    
  }else{
    
    mapa <- map_raster(
      r = r,
      name_legend = name_legend,
      title = title,
      option_col = option_col,
      limits = limits, nbreaks = nbreaks
    )
  }
  
  ggsave(
    plot = mapa,
    filename = filename_out,
    width = width_out,
    height = width_out
  )
  print(paste0("Image saved in ", filename_out))
  return(mapa)
}


# ggsave(plot = map_raster_df,filename = "images/test_pr.png")
