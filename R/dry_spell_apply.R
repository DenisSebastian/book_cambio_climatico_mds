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
hist_mean <-  rast("data/tif/mean_historical.tif")
rcp26_mean <- rast("data/tif/mean_rcp26.tif")
rcp45_mean <- rast("data/tif/mean_rcp45.tif")
rcp85_mean <- rast("data/tif/mean_rcp85.tif")


# Cálculo de Número de Periodos -------------------------------------------



n_hist <- app(hist_mean, duration_dry_n, threshold = 1)
n_rcp26 <- app(rcp26_mean, duration_dry_n, threshold = 1)
n_rcp45 <- app(rcp45_mean, duration_dry_n, threshold = 1)
n_rcp85 <- app(rcp85_mean, duration_dry_n, threshold = 1)

n_hist <-  reproject_custom(n_hist)

mapa_n_hist <- save_map_r(r = n_hist, name_legend = "Nº Dry Spell",
                        title = "Historical - Número de Periodos de Dry Spell",
                        option_col = "A", filename_out = "images/historical_n_DS.png",
                        limits = c(0, 7500), nbreaks = 7, div_adm =F)


mapa_n_rcp26 <- save_map_r(r = n_rcp26, name_legend = "Nº Dry Spell",
                        title = "RCP 2.6 - Número de Periodos de Dry Spell",
                        option_col = "A", filename_out = "images/RCP2.6_n_DS.png",
                        limits = c(0, 7500), nbreaks = 7)


mapa_n_rcp45 <- save_map_r(r = n_rcp45, name_legend = "Nº Dry Spell",
                           title = "RCP 4.5 - Número de Periodos de Dry Spell",
                           option_col = "A", filename_out = "images/RCP4.5_n_DS.png",
                           limits = c(0, 7500), nbreaks = 7)


mapa_n_rcp85 <- save_map_r(r = n_rcp85, name_legend = "Nº Dry Spell",
                           title = "RCP 8.5 - Número de Periodos de Dry Spell",
                           option_col = "A", filename_out = "images/RCP8.5_n_DS.png",
                           limits = c(0, 7500), nbreaks = 7)




# Promedio de Duración ----------------------------------------------------
promedio_hist <- app(hist_mean, duration_dry_mean)
promedio_rcp26 <- app(rcp26_mean, duration_dry_mean)
promedio_rcp45 <- app(rcp45_mean, duration_dry_mean)
promedio_rcp85 <- app(rcp85_mean, duration_dry_mean)

max(promedio_hist,
    promedio_rcp26,
    promedio_rcp45,
    promedio_rcp85)

mapa_mean_hist <- save_map_r(r = promedio_hist, name_legend = "Mean \nDry Spell",
                          title = "Historical - Promedio de Periodos de Dry Spell",
                          option_col = "D", 
                          filename_out = "images/historical_mean_DS.png",
                          limits = c(0, 7), nbreaks = 7)


mapa_mean_rcp26 <- save_map_r(r = promedio_rcp26, name_legend = "Mean \nDry Spell",
                           title = "RCP 2.6 - Promedio de Periodos de Dry Spell",
                           option_col = "D", 
                           filename_out = "images/RCP2.6_mean_DS.png",
                           limits = c(0, 7), nbreaks = 7)


mapa_mean_rcp45 <- save_map_r(r = promedio_rcp45, name_legend = "Mean \nDry Spell",
                           title = "RCP 4.5 - Promedio de Periodos de Dry Spell",
                           option_col = "D", 
                           filename_out = "images/RCP4.5_mean_DS.png",
                           limits = c(0, 7), nbreaks = 7)


mapa_mean_rcp85 <- save_map_r(r = promedio_rcp85, name_legend = "Mean \nDry Spell",
                           title = "RCP 8.5 - Promedio de Periodos de Dry Spell",
                           option_col = "D", 
                           filename_out = "images/RCP8.5_mean_DS.png",
                           limits = c(0, 7), nbreaks = 7)



# Coeficiente de Variabilidad ---------------------------------------------


cv_hist <- app(hist_mean, duration_dry_cv)
cv_rcp26 <- app(rcp26_mean, duration_dry_cv)
cv_rcp45 <- app(rcp45_mean, duration_dry_cv)
cv_rcp85 <- app(rcp85_mean, duration_dry_cv)

max(cv_hist,
    cv_rcp26,
    cv_rcp45,
    cv_rcp85)

mapa_cv_hist <- save_map_r(r = cv_hist, name_legend = "CV \nDry Spell",
                           title = "Historical - Coef. Variabilidad Dry Spell",
                           option_col = "E", 
                           filename_out = "images/historical_cv_DS.png",
                           limits = c(0, 12), nbreaks = 7)


mapa_cv_rcp26 <- save_map_r(r = cv_rcp26, name_legend = "CV \nDry Spell",
                              title = "RCP 2.6 - Coef. Variabilidad Dry Spell",
                              option_col = "E", 
                              filename_out = "images/RCP2.6_cv_DS.png",
                            limits = c(0, 12), nbreaks = 7)


mapa_cv_rcp45 <- save_map_r(r = cv_rcp45, name_legend = "CV \nDry Spell",
                              title = "RCP 4.5 - Coef. Variabilidad Dry Spell",
                              option_col = "E", 
                              filename_out = "images/RCP4.5_cv_DS.png",
                            limits = c(0, 12), nbreaks = 7)


mapa_cv_rcp85 <- save_map_r(r = cv_rcp85, name_legend = "CV \nDry Spell",
                              title = "RCP 8.5 - Coef. Variabilidad Dry Spell",
                              option_col = "E", 
                              filename_out = "images/RCP8.5_cv_DS.png",
                            limits = c(0, 12), nbreaks = 7)


# Percentil 99 ------------------------------------------------------------

p99_hist <- app(hist_mean, duration_dry_percentail, percentail=0.99)
p99_rcp26 <- app(rcp26_mean, duration_dry_percentail, percentail=0.99)
p99_rcp45 <- app(rcp45_mean, duration_dry_percentail, percentail=0.99)
p99_rcp85 <- app(rcp85_mean, duration_dry_percentail, percentail=0.99)

max(p99_hist, p99_rcp26, p99_rcp45,p99_rcp85)


mapa_p99_hist <- save_map_r(r = p99_hist, name_legend = "Perc. 99 \nDry Spell",
                           title = "Historical - Percentil 99 deDry Spell",
                           option_col = "F", 
                           filename_out = "images/historical_p99_DS.png",
                           limits = c(0, 40), nbreaks = 10)


mapa_p99_rcp26 <- save_map_r(r = p99_rcp26, name_legend = "Perc. 99 \nDry Spell",
                            title = "RCP 2.6 - Percentil 99 de Dry Spell",
                            option_col = "F", 
                            filename_out = "images/RCP2.6_p99_DS.png",
                            limits = c(0, 40), nbreaks = 10)


mapa_p99_rcp45 <- save_map_r(r = p99_rcp45, name_legend = "Perc. 99 \nDry Spell",
                            title = "RCP 4.5 - Percentil 99  de Dry Spell",
                            option_col = "F", 
                            filename_out = "images/RCP4.5_p99_DS.png",
                            limits = c(0, 40), nbreaks = 10)


mapa_p99_rcp85 <- save_map_r(r = p99_rcp85, name_legend = "Perc. 99 \nDry Spell",
                            title = "RCP 8.5 - Percentil 99  Dry Spell",
                            option_col = "F", 
                            filename_out = "images/RCP8.5_p99_DS.png",
                            limits = c(0, 40), nbreaks = 10)



# Percentil 99.9 ----------------------------------------------------------

p999_hist <- app(hist_mean, duration_dry_percentail, percentail=0.999)
p999_rcp26 <- app(rcp26_mean, duration_dry_percentail, percentail=0.999)
p999_rcp45 <- app(rcp45_mean, duration_dry_percentail, percentail=0.999)
p999_rcp85 <- app(rcp85_mean, duration_dry_percentail, percentail=0.999)

max(p999_hist ,  p999_rcp26  , p999_rcp45 ,  p999_rcp85)

mapa_p999_hist <- save_map_r(r = p999_hist, name_legend = "Perc. 99.9 \nDry Spell",
                           title = "Historical - Percentil 99.9 deDry Spell",
                           option_col = "F", 
                           filename_out = "images/historical_p999_DS.png",
                           limits = c(0, 80), nbreaks = 10)


mapa_p999_rcp26 <- save_map_r(r = p999_rcp26, name_legend = "Perc. 99.9 \nDry Spell",
                            title = "RCP 2.6 - Percentil 99.9 de Dry Spell",
                            option_col = "F", 
                            filename_out = "images/RCP2.6_p999_DS.png",
                            limits = c(0, 80), nbreaks = 10)


mapa_p999_rcp45 <- save_map_r(r = p999_rcp45, name_legend = "Perc. 99.9 \nDry Spell",
                            title = "RCP 4.5 - Percentil 99.9  deDry Spell",
                            option_col = "F", 
                            filename_out = "images/RCP4.5_p999_DS.png",
                            limits = c(0, 80), nbreaks = 10)


mapa_p999_rcp85 <- save_map_r(r = p999_rcp85, name_legend = "Perc. 99.9 \nDry Spell",
                            title = "RCP 8.5 - Percentil 99.9  Dry Spell",
                            option_col = "F", 
                            filename_out = "images/RCP8.5_p999_DS.png",
                            limits = c(0, 80), nbreaks = 10)


# # Calcular valores generales ----------------------------------------------
# 
# # número de Periodos
# system.time({
# n_periodos <- app(nc_all, duration_dry_n)
# })
# # plot(n_periodos)
# 
# map_periodos <-  map_raster(r = n_periodos, name_legend = "N Pr", 
#                              title = "Número de Periodos de Dry Spell")
# ggsave(plot = map_periodos,filename = "images/MIROC5_hist_N_periods.png",
#        width = 8, height = 10)
# 
# 
# # promedio de duracion
# mean_dry <- app(nc_all, duration_dry_mean)
# # plot(mean_dry)
# map_prom_dur <-  map_raster(r = mean_dry, name_legend = "Mean Pr", 
#                             option_col = "D",
#                             title = "Promedio Duración de Precipitación")
# ggsave(plot = map_prom_dur,filename = "images/MIROC5_hist_prom_dur.png",
#        width = 8, height = 10)
# 
# 
# # Coeficiente de variabilidad
# cv_dry <- app(nc_all, duration_dry_cv)
# # plot(cv_dry)
# map_cv <-  map_raster(r = cv_dry, name_legend = "CV Pr", 
#                             option_col = "E",
#                             title = "Coeficiente de Variabilidad de Dry Spell")
# ggsave(plot = map_cv,filename = "images/MIROC5_hist_c_var.png",
#        width = 8, height = 10)
# 
# 
# # percentiles
# perc_99 <- app(nc_all, duration_dry_percentail, percentail=0.99)
# # plot(perc_99)
# map_p99 <-  map_raster(r = perc_99, name_legend = "P_99 Pr", 
#                       option_col = "F",
#                       title = "Percentil 99 de  Dry Spell")
# ggsave(plot = map_p99, filename = "images/MIROC5_hist_p99.png",
#        width = 8, height = 10)
# 
# 
# perc_999 <- app(nc_all, duration_dry_percentail, percentail=0.999)
# # plot(perc_999)
# map_p999 <-  map_raster(r = perc_999, name_legend = "P_999 Pr", 
#                        option_col = "F",
#                        title = "Percentil 99.9 de  Dry Spell")
# ggsave(plot = map_p999, filename = "images/MIROC5_hist_p999.png",
#        width = 8, height = 10)
# 



