mul_factor <-  function(x, factor){
  return(x*factor)
}

crs_assign <- function(x, crs_new=4326){
  terra::crs(x) <-  sf::st_crs(crs_new)$proj4string
  return(x)
}




reproject_custom <-  function(rast_terra, crs_target = NULL, crs_base =4326){
  if(is.null(crs_target)){
    crs_target <- "+proj=ob_tran +o_proj=longlat +o_lon_p=303.94 +o_lat_p=70.6 +lon_0=180 +to_meter=0.0174532925199433"
    
  }
  rast_terra <- crs_assign(rast_terra, crs_new = crs_base)
  rast_terra <- terra::project(rast_terra, crs_target)
  return(rast_terra)
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

# hist_mean <-  rast("data/tif/mean_historical.tif")
# hist_mean <- reproject_custom(hist_mean)
# 
# 
# r1 <- hist_mean2[[1000]]
# plot(r1)
