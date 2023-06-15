mul_factor <-  function(x, factor){
  return(x*factor)
}

crs_assign <- function(x, crs_new=4326){
  terra::crs(x) <-  sf::st_crs(crs_new)$proj4string
  return(x)
}


get_crs_rot <- function(nc_file) {
  # https://github.com/dquesadacr/CORDEX
  GI <- rgdal::GDALinfo(nc_file)
  rot.info <- attr(GI, "mdata")[grep("^rotated", attr(GI, "mdata"))]
  coord <- sapply(
    rot.info,
    FUN = function(x) {
      unlist(stringr::str_split(x, pattern = "="))[2]
    }
  )
  coord.num <- round(as.numeric(coord[2:3]), 2)
  if (coord.num[2] < 0) {
    coord.num[2] <- coord.num[2] + 360
  }
  target.crs <- paste0(
    "+proj=ob_tran +o_proj=longlat +o_lon_p=" ,
    coord.num[2],
    " +o_lat_p=",
    coord.num[1],
    " +lon_0=180 +to_meter=0.0174532925199433"
  )
  return(target.crs)
}

reproject_custom <-  function(rast_terra, crs_target, crs_base =4326){
  rast_terra <- crs_assign(rast_terra, crs_new = crs_base)
  rast_terra <- terra::project(rast_terra, crs_target)
  return(rast_terra)
}

# visaluzaciones

# sapply(nc.files, function(x){
#   
#   # This applies to CORDEX data nomenclature, where the variable name is at the beginning of the name
#   nc.var<- unlist(str_split(x, "_"))[1]
#   
#   # attributes of ncfile
#   GI<- rgdal::GDALinfo(paste0("data/historical/", x))
#   
#   rot.info<- attr(GI, "mdata")[grep("^rotated", attr(GI, "mdata"))]
#   
#   long.name<- unlist(str_split(attr(GI, "mdata")[grep("long_name", attr(GI, "mdata"), ignore.case = T)][
#     str_which(attr(GI, "mdata")[grep("long_name", attr(GI, "mdata"), ignore.case = T)], nc.var)], "="))[2]
#   
#   units<- unlist(str_split(attr(GI, "mdata")[grep("#units", attr(GI, "mdata"), ignore.case = T)][
#     str_which(attr(GI, "mdata")[grep("#units", attr(GI, "mdata"), ignore.case = T)], nc.var)], "="))[2]
#   
#   coord<- sapply(rot.info, FUN = function(x) {
#     unlist(str_split(x, pattern = "="))[2]})
#   
#   coord.num<- round(as.numeric(coord[2:3]),2)
#   
#   if(coord.num[2]<0) {coord.num[2]<- coord.num[2]+360}
#   
#   target.crs <- paste0("+proj=ob_tran +o_proj=longlat +o_lon_p=" , 
#                        coord.num[2], " +o_lat_p=", coord.num[1], 
#                        " +lon_0=180 +to_meter=0.0174532925199433")
#   
#   nc.stack<- raster::stack(paste0("data/historical/", x), varname= nc.var)
#   crs(nc.stack)<- sf::st_crs(4326)$proj4string
#   
#   # Just the first timestep for test, reprojecting the whole stack takes some time
#   first.reproj <- projectRaster(nc.stack[[1]], crs = target.crs)
#   # mapview(first.reproj)
#   
#   
#   # Transform units for pr
#   if(long.name %in% c("Precipitation", "precipitation")){first.reproj<- first.reproj*86400 ; units <- "mm/day"}
#   
#   # For ggplot:
#   ## Needs to be in a spdf format (see below)
#   
#   rast.spdf<- as(first.reproj, "SpatialPixelsDataFrame") %>% as.data.frame() %>% 
#     set_colnames(c("value", "x", "y"))
#   
#   # For borders
#   bbox <- st_bbox(first.reproj)
#   borders <- st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))  
#   
#   to.plot<- ggplot() + geom_tile(data=rast.spdf, aes(fill=value, x=x, y=y), color=NA) + 
#     geom_sf(data = borders, fill=NA, colour="black") + 
#     coord_sf(xlim = bbox[c(1,3)],ylim = bbox[c(2,4)]) +
#     theme_light(base_size = 11) +
#     scale_fill_viridis() +
#     labs(fill= units, title = long.name)+
#     theme(axis.title.y = element_blank(),
#           axis.title.x = element_blank(),
#           plot.title = element_text(hjust = 0.5))
#   
#   ggsave(plot = to.plot, filename = paste0("plots/", str_remove(x, ".nc"), "_first.png"),
#          height = 100, width = 150, units = "mm", dpi = 500, device = "png") 
# })
