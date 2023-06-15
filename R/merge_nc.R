# objetivos: lectura de precipitaciones con terra

# Opciones Generales ------------------------------------------------------

source("R/fnc_raster.R")
options(scipen = 999)


# read Raster -------------------------------------------------------------
library(terra)
library(dplyr)
library(lubridate)
library(mapview)
library(purrr)



# Lectura de Insumos ------------------------------------------------------

file_1 <- "data/historical/pr_SAM-44_MIROC-MIROC5_historical_r1i1p1_SMHI-RCA4_v3_day_19510101-19551231.nc"
# lectura única
r_pr_base1  <- rast(file_1)
plot(r_pr_base1[[1]])


# Filtro espacial por coordendas ------------------------------------------

plot(r_pr_base1[[1]])
ext <- ext(c(162,  172 ,  -30,   -15 ))
# ext <- ext(c(162,  172 , -38,   -29 ))
plot(ext, add = T)

r_pr <- crop(x = r_pr_base1, y = ext, snap="out")
plot(r_pr[[2]])
# terra::writeRaster(r_pr[[1]], "data/tif/test_pr.tif")






# lectura Masiva ----------------------------------------------------------


factor = 86400

# extend
ext <- ext(c(162,  172 ,  -30,   -15 ))

# lectura, filtro espacial y transformación unidad de medida
list_nc <-  list.files("data/historical", pattern = "*nc$", full.names = T) %>% 
  map(terra::rast) %>% 
  map(~terra::crop(x = ., y = ext, snap="out")) %>% 
  map(~mul_factor(., factor))

  
nc_all <-  Reduce(f = c, x = list_nc)


# guardar resultados
terra::writeRaster(nc_all, "data/tif/MIROC5_historical.tif", overwrite = T)




# Reproyectar CRS conocido ------------------------------------------------

# TODO
# rotate coordenate system

# https://user-images.githubusercontent.com/12826141/36675223-b2df687a-1b08-11e8-812f-5fa187f68030.png
# ext <- ext(c(162,  172 , -38,   -29 ))
nc_file <-  list.files("data/historical/", pattern = "*nc$", full.names = T)[1]
crs_target <- get_crs_rot(nc_file)
reproject_custom(rast_terra =nc_all, crs_target = crs_target)
terra::crop(x = nc_all, y = ext, snap="out")
#  # https://github.com/dquesadacr/CORDEX





# crs
# graficos de series
# gif
#