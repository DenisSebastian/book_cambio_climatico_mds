# objetivos: Aplicar función dry spell a modelo histórico
# autor denis berroetas
# creación: 29-06-2023



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



# lectura -----------------------------------------------------------------


hist_ichec <-  rast("data/models/ICHEC/ICHEC_hist.tif")
hist_miroc5 <-  rast("data/models/MIROC5/MIROC5_hist.tif")
hist_mohc <-  rast("data/models/MOHC/MOHC_hist.tif")




# Asignar variable tiempo -------------------------------------------------

nlyr_a <- hist_ichec %>% nlyr() #n layers 
nlyr_b <- hist_miroc5 %>% nlyr() #n layers 
nlyr_c <- hist_mohc %>% nlyr() #n layers 

n_periods <-  min(nlyr_a, nlyr_b, nlyr_c)
day_init <- "1955-01-01"
time_days <- ymd(seq(as_date(day_init), 
                     ymd(day_init)+n_periods-1, "day"))

min_lay <- 19724
time_days[min_lay] # 31-12-2008
time_days_adj <- time_days[1:min_lay]#se ajusta para fechas cerrados


hist_ichec <- hist_ichec[[1:min_lay]] # filtar layers
terra::time(hist_ichec)  <- time_days_adj # add time

hist_miroc5 <- hist_miroc5[[1:min_lay]] # filtar layers
terra::time(hist_miroc5)  <- time_days_adj # add time

hist_mohc <- hist_mohc[[1:min_lay]] # filtar layers
terra::time(hist_mohc)  <- time_days_adj # add time




# Número periodos Dry Spell -----------------------------------------------

# cálculo de n
n_hist_ichec <- app(hist_ichec, duration_dry_n, threshold = 1)
n_hist_miroc5 <- app(hist_miroc5, duration_dry_n, threshold = 1)
n_hist_mohc <- app(hist_mohc, duration_dry_n, threshold = 1)

max(n_hist_ichec,    n_hist_miroc5,  n_hist_mohc) # ver para ajustar limites

# reproject
# Solo hacer si no se hizo al ajustar coodenadas despues de la descarga
n_hist_ichec <-  reproject_custom(n_hist_ichec)
n_hist_miroc5 <-  reproject_custom(n_hist_miroc5)
n_hist_mohc <-  reproject_custom(n_hist_mohc)


# Generar mapas y los guardarlos

mapa_n_hist_ichec <- 
  save_map_r(r = n_hist_ichec, name_legend = "Nº Dry Spell",
             title = "Historical ICHEC - Nº de Periodos de Dry Spell",
             option_col = "A", filename_out = "images/n_hist_ichec_DS.png",
             limits = c(0, 5000), nbreaks = 7, div_adm =F)


mapa_n_hist_miroc5 <- 
  save_map_r(r = n_hist_miroc5, name_legend = "Nº Dry Spell",
             title = "Historical MIROC5 - Nº de Periodos de Dry Spell",
             option_col = "A", filename_out = "images/n_hist_miroc5_DS.png",
             limits = c(0, 5000), nbreaks = 7, div_adm =F)

mapa_n_hist_mohc <- 
  save_map_r(r = n_hist_mohc, name_legend = "Nº Dry Spell",
             title = "Historical MOHC - Nº de Periodos de Dry Spell",
             option_col = "A", filename_out = "images/n_hist_mohc_DS.png",
             limits = c(0, 5000), nbreaks = 7, div_adm =F)


# Promediar resultados n periods
n_models <-  mean(n_hist_ichec,  n_hist_miroc5,  n_hist_mohc) 
mapa_n_hist_mean <- 
  save_map_r(r = n_models, name_legend = "Mean  \nDry Spell",
             title = "Historical - Promedio de Nº Periodos de Dry Spell",
             option_col = "A", filename_out = "images/n_hist_mean_DS.png",
             limits = c(0, 5000), nbreaks = 7, div_adm =F)


# Promedio de Duración ----------------------------------------------------

# calculo promedio
mean_hist_ichec <- app(hist_ichec, duration_dry_mean)
mean_hist_miroc5 <- app(hist_miroc5, duration_dry_mean)
mean_hist_mohc <- app(hist_mohc, duration_dry_mean)

max(mean_hist_ichec,
    mean_hist_miroc5,
    mean_hist_mohc)


mean_hist_ichec <-  reproject_custom(mean_hist_ichec)
mean_hist_miroc5 <-  reproject_custom(mean_hist_miroc5)
mean_hist_mohc <-  reproject_custom(mean_hist_mohc)


# Generar mapas y los guardarlos

mapa_mean_hist_ichec <- 
  save_map_r(r = mean_hist_ichec, name_legend = "Mean \nDry Spell",
             title = "Historical ICHEC - Promedio de Periodos de Dry Spell",
             option_col = "D", filename_out = "images/mean_hist_ichec_DS.png",
             limits = c(0, 25), nbreaks = 7, div_adm =F)


mapa_mean_hist_miroc5 <- 
  save_map_r(r = mean_hist_miroc5, name_legend = "Mean \nDry Spell",
             title = "Historical MIROC5 - Promedio de Periodos de Dry Spell",
             option_col = "D", filename_out = "images/mean_hist_miroc5_DS.png",
             limits = c(0, 25), nbreaks = 7, div_adm =F)

mapa_mean_hist_mohc <- 
  save_map_r(r = mean_hist_mohc, name_legend = "Mean \nDry Spell",
             title = "Historical MOHC - Promedio de Periodos de Dry Spell",
             option_col = "D", filename_out = "images/mean_hist_mohc_DS.png",
             limits = c(0, 25), nbreaks = 7, div_adm =F)


# Promediar resultados n periods
mean_models <-  mean(mean_hist_ichec,  mean_hist_miroc5,  mean_hist_mohc) 
mapa_hist_mean <- 
  save_map_r(r = mean_models, name_legend = "Mean  \nDry Spell",
             title = "Historical - Promedio de Periodos de Dry Spell",
             option_col = "D", filename_out = "images/hist_mean_DS.png",
             limits = c(0, 30), nbreaks = 7, div_adm =F)





# Coeficiente de Variabilidad ---------------------------------------------
# calculo cv
cv_hist_ichec <- app(hist_ichec, duration_dry_cv)
cv_hist_miroc5 <- app(hist_miroc5, duration_dry_cv)
cv_hist_mohc <- app(hist_mohc, duration_dry_cv)

max(cv_hist_ichec,
    cv_hist_miroc5,
    cv_hist_mohc)

# preproj
cv_hist_ichec <-  reproject_custom(cv_hist_ichec)
cv_hist_miroc5 <-  reproject_custom(cv_hist_miroc5)
cv_hist_mohc <-  reproject_custom(cv_hist_mohc)


# Generar mapas y los guardarlos

mapa_cv_hist_ichec <- 
  save_map_r(r = cv_hist_ichec, name_legend = "CV \nDry Spell",
             title = "Historical ICHEC - Coef. Variabilidad Dry Spell",
             option_col = "E", filename_out = "images/cv_hist_ichec_DS.png",
             limits = c(0, 50), nbreaks = 7, div_adm =F)


mapa_cv_hist_miroc5 <- 
  save_map_r(r = cv_hist_miroc5, name_legend = "CV \nDry Spell",
             title = "Historical MIROC5 - Coef. Variabilidad Dry Spell",
             option_col = "E", filename_out = "images/cv_hist_miroc5_DS.png",
             limits = c(0, 50), nbreaks = 7, div_adm =F)

mapa_cv_hist_mohc <- 
  save_map_r(r = cv_hist_mohc, name_legend ="CV \nDry Spell",
             title = "Historical MOHC - Coef. Variabilidad Dry Spell",
             option_col = "E", filename_out = "images/cv_hist_mohc_DS.png",
             limits = c(0, 50), nbreaks = 7, div_adm =F)


# Promediar resultados n periods
mean_models <-  mean(cv_hist_ichec,
                     cv_hist_miroc5,
                     cv_hist_mohc) 
mapa_cv_hist_mean <- 
  save_map_r(r = mean_models, name_legend = "CV \nDry Spell",
             title = "Historical - Coef. Variabilidad Dry Spell",
             option_col = "E", filename_out = "images/hist_cv_DS.png",
             limits = c(0, 50), nbreaks = 7, div_adm =F)


# Percentil 99 ------------------------------------------------------------


# calculo cv
p99_hist_ichec <- app(hist_ichec, duration_dry_percentail, percentail=0.99)
p99_hist_miroc5 <- app(hist_miroc5, duration_dry_percentail, percentail=0.99)
p99_hist_mohc <- app(hist_mohc, duration_dry_percentail, percentail=0.99)

max(p99_hist_ichec,
    p99_hist_miroc5,
    p99_hist_mohc)

# preproj
p99_hist_ichec <-  reproject_custom(p99_hist_ichec)
p99_hist_miroc5 <-  reproject_custom(p99_hist_miroc5)
p99_hist_mohc <-  reproject_custom(p99_hist_mohc)


# Generar mapas y los guardarlos

mapa_p99_hist_ichec <- 
  save_map_r(r = p99_hist_ichec, name_legend = "Perc. 99 \nDry Spell",
             title = "Historical ICHEC - Percentil 99 de Dry Spell",
             option_col = "F", filename_out = "images/p99_hist_ichec_DS.png",
             limits = c(0, 150), nbreaks = 7, div_adm =F)

mapa_p99_hist_miroc5 <- 
  save_map_r(r = p99_hist_miroc5, name_legend = "Perc. 99 \nDry Spell",
             title = "Historical MIROC5 - Percentil 99 de Dry Spell",
             option_col = "F", filename_out = "images/p99_hist_miroc5_DS.png",
             limits = c(0, 150), nbreaks = 7, div_adm =F)

mapa_p99_hist_mohc <- 
  save_map_r(r = p99_hist_mohc, name_legend ="Perc. 99 \nDry Spell",
             title = "Historical MOHC - Percentil 99 de Dry Spell",
             option_col = "F", filename_out = "images/p99_hist_mohc_DS.png",
             limits = c(0, 150), nbreaks = 7, div_adm =F)


# Promediar resultados n periods
mean_models <-  mean(p99_hist_ichec,
                     p99_hist_miroc5,
                     p99_hist_mohc) 

mapa_p99_hist_mean <- 
  save_map_r(r = mean_models, name_legend = "Perc. 99 \nDry Spell",
             title = "Historical - Mean Percentil 99 de Dry Spell",
             option_col = "F", filename_out = "images/hist_p99_DS.png",
             limits = c(0, 120), nbreaks = 7, div_adm =F)




# Percentil 99.9 ----------------------------------------------------------


# calculo cv
p999_hist_ichec <- app(hist_ichec, duration_dry_percentail, percentail=0.999)
p999_hist_miroc5 <- app(hist_miroc5, duration_dry_percentail, percentail=0.999)
p999_hist_mohc <- app(hist_mohc, duration_dry_percentail, percentail=0.999)

max(p999_hist_ichec,
    p999_hist_miroc5,
    p999_hist_mohc)

# preproj
p999_hist_ichec <-  reproject_custom(p999_hist_ichec)
p999_hist_miroc5 <-  reproject_custom(p999_hist_miroc5)
p999_hist_mohc <-  reproject_custom(p999_hist_mohc)


# Generar mapas y los guardarlos

mapa_p999_hist_ichec <- 
  save_map_r(r = p999_hist_ichec, name_legend = "Perc. 999 \nDry Spell",
             title = "Historical ICHEC - Percentil 99.9 de Dry Spell",
             option_col = "F", filename_out = "images/p999_hist_ichec_DS.png",
             limits = c(0, 300), nbreaks = 7, div_adm =F)

mapa_p99_hist_miroc5 <- 
  save_map_r(r = p99_hist_miroc5, name_legend = "Perc. 99 \nDry Spell",
             title = "Historical MIROC5 - Percentil 99.9 de Dry Spell",
             option_col = "F", filename_out = "images/p999_hist_miroc5_DS.png",
             limits = c(0, 300), nbreaks = 7, div_adm =F)

mapa_p99_hist_mohc <- 
  save_map_r(r = p99_hist_mohc, name_legend ="Perc. 99.9 \nDry Spell",
             title = "Historical MOHC - Percentil 99.9 de Dry Spell",
             option_col = "F", filename_out = "images/p999_hist_mohc_DS.png",
             limits = c(0, 300), nbreaks = 7, div_adm =F)


# Promediar resultados n periods
mean_models <-  mean(p999_hist_ichec,
                     p999_hist_miroc5,
                     p999_hist_mohc) 

mapa_p999_hist_mean <- 
  save_map_r(r = mean_models, name_legend = "Perc. 99.9 \nDry Spell",
             title = "Historical - Mean Percentil 99.9 de Dry Spell",
             option_col = "F", filename_out = "images/hist_p999_DS.png",
             limits = c(0, 200), nbreaks = 7, div_adm =F)

