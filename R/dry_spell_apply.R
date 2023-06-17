# objetivos: lectura de precipitaciones con terra

# Opciones Generales ------------------------------------------------------

source("R/fnc_raster.R")
source("R/fnc_dryspell.R")
source("R/fnc_visual.R")
options(scipen = 999)


# read Raster -------------------------------------------------------------
library(terra)
library(dplyr)
library(lubridate)
library(mapview)
library(purrr)


# insumos -----------------------------------------------------------------
nc_all <-  rast("data/tif/MIROC5_historical.tif")
nlyr(nc_all)

muestra <-  nc_all[[1:1000]]




# Calcular valores generales ----------------------------------------------

# número de Periodos
system.time({
n_periodos <- app(nc_all, duration_dry_n)
})
# plot(n_periodos)

map_periodos <-  map_raster(r = n_periodos, name_legend = "N Pr", 
                             title = "Número de Periodos de Dry Spell")
ggsave(plot = map_periodos,filename = "images/MIROC5_hist_N_periods.png",
       width = 8, height = 10)


# promedio de duracion
mean_dry <- app(nc_all, duration_dry_mean)
# plot(mean_dry)
map_prom_dur <-  map_raster(r = mean_dry, name_legend = "Mean Pr", 
                            option_col = "D",
                            title = "Promedio Duración de Precipitación")
ggsave(plot = map_prom_dur,filename = "images/MIROC5_hist_prom_dur.png",
       width = 8, height = 10)


# Coeficiente de variabilidad
cv_dry <- app(nc_all, duration_dry_cv)
# plot(cv_dry)
map_cv <-  map_raster(r = cv_dry, name_legend = "CV Pr", 
                            option_col = "E",
                            title = "Coeficiente de Variabilidad de Dry Spell")
ggsave(plot = map_cv,filename = "images/MIROC5_hist_c_var.png",
       width = 8, height = 10)


# percentiles
perc_99 <- app(nc_all, duration_dry_percentail, percentail=0.99)
# plot(perc_99)
map_p99 <-  map_raster(r = perc_99, name_legend = "P_99 Pr", 
                      option_col = "F",
                      title = "Percentil 99 de  Dry Spell")
ggsave(plot = map_p99, filename = "images/MIROC5_hist_p99.png",
       width = 8, height = 10)


perc_999 <- app(nc_all, duration_dry_percentail, percentail=0.999)
# plot(perc_999)
map_p999 <-  map_raster(r = perc_999, name_legend = "P_999 Pr", 
                       option_col = "F",
                       title = "Percentil 99.9 de  Dry Spell")
ggsave(plot = map_p999, filename = "images/MIROC5_hist_p999.png",
       width = 8, height = 10)




# get series --------------------------------------------------------------

df_series <- nc_all %>% terra::as.data.frame()
df_series_t <- as.data.frame(t(df_series))
px_1 <- df_series_t[,1] %>% as.vector()
result <- Duration_dry(px_1, threshold = 0.1)

