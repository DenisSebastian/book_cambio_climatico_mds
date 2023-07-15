# objetivos: Get Means Models

# Opciones Generales ------------------------------------------------------

source("R/fnc_raster.R")
source("R/fnc_dryspell.R")
source("R/fnc_visual.R")
options(scipen = 999)



# Libraries ---------------------------------------------------------------
library(terra)
library(dplyr)
library(purrr)
library(lubridate)



# Read Historical ---------------------------------------------------------


hist_models <- list.files(path = "data/models", pattern = "*_hist.tif$", 
                          recursive = T, full.names = T) %>% 
  map(rast)



# cantidad de layer
hist_models %>% map(nlyr)

# minimo de layers
min_lay <- hist_models %>% map(nlyr) %>% map_dbl(c) %>% min()

# days de modelos con mas registros
hist_1 <-  hist_models[[1]]
time_days <- ymd(seq(as_date("1951-01-01"), as_date("2005-12-31"), "day"))
length(time_days)


time_days[min_lay]

# se selecciona diciembre del año anterior
min_lay <- 19724
time_days[min_lay]
time_days_min <- time_days[1:min_lay]



for(i in 1:length(hist_models)){
  hist_models[[i]] <- hist_models[[i]][[1:min_lay]]
  terra::time(hist_models[[i]])  <- time_days_min
  # hist_models[[i]] <-setZ(hist_models[[i]], time_days_min, 'days')
}

models_merged <- hist_models %>% map(terra::as.data.frame) %>% map(t)
m_models <- (models_merged[[1]]+ models_merged[[2]]+ models_merged[[3]])/3

mean_model_hist <- hist_models[[1]]
values(mean_model_hist) <-  as.vector(t(m_models))
mean_model_hist[[1:10]] %>% plot()
hist_models[[2]][[1:10]] %>% plot()
writeRaster(mean_model_hist, "data/tif/mean_historical.tif", overwrite=T)




# RCP 2.6 -----------------------------------------------------------------


rcp26_models <- list.files(path = "data/models", pattern = "*_RCP26.tif$", 
                          recursive = T, full.names = T) %>% 
  map(rast)



# cantidad de layer
rcp26_models %>% map(nlyr)

# minimo de layers
min_lay <- rcp26_models %>% map(nlyr) %>% map_dbl(c) %>% min()

# days de modelos con mas registros

time_days <- ymd(seq(as_date("2006-01-01"), as_date("2100-12-31"), "day"))
length(time_days)


time_days[min_lay]

# se selecciona diciembre del año anterior
time_days[33603]
day_end <- 33603
time_days_min <- time_days[1:day_end]



for(i in 1:length(rcp26_models)){
  rcp26_models[[i]] <- rcp26_models[[i]][[1:day_end]]
  terra::time(rcp26_models[[i]])  <- time_days_min
}



models_merged <- rcp26_models %>% map(terra::as.data.frame) %>% map(t)
m_models <- (models_merged[[1]]+ models_merged[[2]]+ models_merged[[3]])/3

mean_model_rcp26 <- rcp26_models[[1]]
values(mean_model_rcp26) <-  as.vector(t(m_models))
mean_model_rcp26[[1:10]] %>% plot()
rcp26_models[[1]][[1:10]] %>% plot()
writeRaster(mean_model_rcp26, "data/tif/mean_rcp26.tif", overwrite=T)




# RCP 4.5 -----------------------------------------------------------------


rcp45_models <- list.files(path = "data/models", pattern = "*_RCP45.tif$", 
                           recursive = T, full.names = T) %>% 
  map(rast)



# cantidad de layer
rcp45_models %>% map(nlyr)

# minimo de layers
min_lay <- rcp45_models %>% map(nlyr) %>% map_dbl(c) %>% min()

# days de modelos con mas registros

# time_days <- ymd(seq(as_date("2006-01-01"), as_date("2100-12-31"), "day"))
# length(time_days)


# time_days[min_lay]

# se selecciona diciembre del año anterior
# time_days[33603]
# day_end <- 33603
time_days_min <- time_days[1:day_end]



for(i in 1:length(rcp45_models)){
  rcp45_models[[i]] <- rcp45_models[[i]][[1:day_end]]
  terra::time(rcp45_models[[i]])  <- time_days_min
}



models_merged <- rcp45_models %>% map(terra::as.data.frame) %>% map(t)
m_models <- (models_merged[[1]]+ models_merged[[2]]+ models_merged[[3]])/3

mean_model_rcp45 <- rcp45_models[[1]]
values(mean_model_rcp45) <-  as.vector(t(m_models))
mean_model_rcp45[[1:10]] %>% plot()
rcp45_models[[1]][[1:10]] %>% plot()
terra::writeRaster(mean_model_rcp45, "data/tif/mean_rcp45.tif", overwrite=TRUE)



# RCP 8.5 -----------------------------------------------------------------


rcp85_models <- list.files(path = "data/models", pattern = "*_RCP85.tif$", 
                           recursive = T, full.names = T) %>% 
  map(rast)



# cantidad de layer
rcp85_models %>% map(nlyr)

# minimo de layers
min_lay <- rcp85_models %>% map(nlyr) %>% map_dbl(c) %>% min()

# days de modelos con mas registros

# time_days <- ymd(seq(as_date("2006-01-01"), as_date("2100-12-31"), "day"))
# length(time_days)


# time_days[min_lay]

# se selecciona diciembre del año anterior
# time_days[33603]
# day_end <- 33603
time_days_min <- time_days[1:day_end]



for(i in 1:length(rcp85_models)){
  rcp85_models[[i]] <- rcp85_models[[i]][[1:day_end]]
  terra::time(rcp85_models[[i]])  <- time_days_min
}



models_merged <- rcp85_models %>% map(terra::as.data.frame) %>% map(t)
m_models <- (models_merged[[1]]+ models_merged[[2]]+ models_merged[[3]])/3

mean_model_rcp85 <- rcp85_models[[1]]
values(mean_model_rcp85) <-  as.vector(t(m_models))
mean_model_rcp85[[1:10]] %>% plot()
rcp85_models[[2]][[1:10]] %>% plot()
terra::writeRaster(mean_model_rcp85, "data/tif/mean_rcp85.tif", overwrite=TRUE)

# i funciona pero lento
# hist_mat <- hist_models %>% map(as.array)
# mean_model_hist <- hist_models[[1]]
# values(mean_model_hist) <-  NA
# system.time({
#   for (j in 1:20) {
#     # print(j)
#     valores <- NULL
#     for (row in 1:nrow(mean_model_hist)) {
#       for (col in 1:ncol(mean_model_hist)) {
#         val <- mean(hist_mat[[1]][row, col, j],
#                     hist_mat[[2]][row, col, j],
#                     hist_mat[[3]][row, col, j])
#         valores <- c(valores, val)
#       }
#     }
#     
#     values(mean_model_hist[[j]]) <- valores
#   }
#   
# })


# 
# data <- as.vector(m_models)
# 
# ar <- array(data = data, dim = c(35, 24, 19724))
# promedios <-  rast(ar)
# promedios[[1:2]] %>% plot()
# 
# 
# promedios <- rast(x = data, nrows =35, ncols=24, lyrs =  19724)
# 
# 
# 
# 
# 
# models_merged <- hist_models %>% map(terra::as.data.frame) %>% 
#   map(t) %>% map(rast)
# 
# 
# models_merged <- c(models_merged[[1]],
#                    models_merged[[2]],
#                    models_merged[[3]])
# promedio_dia <- app(models_merged, mean)
# mean_hist  <- rast(x = values(promedio_dia), nrows=35, ncols=24, nlyrs=19724)
# 
# 
# 
# 
# 
# 
# 
# hist_mat <- hist_models %>% map(as.array)
# mean_model_hist <- hist_models[[1]]
# values(mean_model_hist) <-  NA
# 
# system.time({
# for (j in 1:10) {
#   print(j)
#   values(mean_model_hist[[j]]) <- m_models[j,]
#   # promedio_dia <- app(models_merged, mean)
# 
# }
# })
# 
# 
# system.time({
#   for (j in 1:5) {
#     print(j)
#     models_merged <- c(hist_models[[1]][[j]],
#                        hist_models[[2]][[j]],
#                        hist_models[[3]][[j]])
#     promedio_dia <- app(models_merged, mean)
#     
#   }
# })
# 
# 
# 