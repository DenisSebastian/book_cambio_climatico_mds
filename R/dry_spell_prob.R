# objetivos: Funciones probabiliadad Dry Spell

# Opciones Generales ------------------------------------------------------

source("R/fnc_raster.R")
source("R/fnc_dryspell.R")
source("R/fnc_visual.R")
library(purrr)
options(scipen = 999)


# insumos -----------------------------------------------------------------
hist_mean <-  rast("data/tif/mean_historical.tif")
rcp26_mean <- rast("data/tif/mean_rcp26.tif")
rcp45_mean <- rast("data/tif/mean_rcp45.tif")
rcp85_mean <- rast("data/tif/mean_rcp85.tif")


# get series --------------------------------------------------------------

df_hist <- hist_mean %>% terra::as.data.frame() %>% t() %>% as.data.frame()
df_rcp26 <- rcp26_mean %>% terra::as.data.frame() %>% t() %>% as.data.frame()
df_rcp45 <- rcp45_mean %>% terra::as.data.frame() %>% t() %>% as.data.frame()
df_rcp85 <- rcp85_mean %>% terra::as.data.frame() %>% t() %>% as.data.frame()




# Cálculo de Densidad con pixe n ------------------------------------------

n = 120
px_hist <- df_hist[,n] %>% as.vector()
px_rcp26 <- df_rcp26[,n] %>% as.vector()
px_rcp45 <- df_rcp45[,n] %>% as.vector()
px_rcp85 <- df_rcp85[,n] %>% as.vector()

list_pixels <- list(px_hist, px_rcp26, px_rcp45, px_rcp85) %>% 
  map(~duration_dry(., threshold = 1))


dat <- data.frame(dens = c(list_pixels[[1]], list_pixels[[2]],
                           list_pixels[[3]], list_pixels[[4]]),
                  model = c(rep("Historical", each = length(list_pixels[[1]])),
                            rep("RCP_2.6", each = length(list_pixels[[2]])),
                            rep("RCP_4.5", each = length(list_pixels[[3]])),
                            rep("RCP_8.5", each = length(list_pixels[[4]]))))


# write.csv(dat, file = "data/csv/n_dry_spell_by_model.csv")

dens_models <- ggplot(dat, aes(x = dens, fill = model)) + 
  geom_histogram(alpha = 0.5)+
  scale_x_continuous(breaks=seq(0,65, 5))+
  facet_wrap(vars(model))+
  labs(title = "Función de Densidad Duración Dry Spell Historicos y Modelos", 
       subtitle = paste0("Pixel n=", n)) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "gray95"), 
        panel.grid.minor = element_line(colour = "gray95"))
  

filename_out = paste0("images/densidad_dur_p", n, ".png")
ggsave(
  plot = dens_models,
  filename = filename_out,
  width = 15,
  height = 8
)




# Aplicación de Forma masiva ----------------------------------------------



# Density calc ------------------------------------------------------------




lseries_hist <- lapply(df_hist, as.vector)
lseries_rcp26 <- lapply(df_rcp26, as.vector)
lseries_rcp45 <- lapply(df_rcp45, as.vector)
lseries_rcp85 <- lapply(df_rcp85, as.vector)

lista_series <- lapply(df_hist, as.vector)

library(purrr)

density_dur_hist <-  lseries_hist %>%
  map(duration_dry) %>% 
  map(density) 
  

density_dur_rcp26 <-  lseries_rcp26 %>%
  map(duration_dry) %>% 
  map(density) 

density_dur_rcp45 <-  lseries_rcp45 %>%
  map(duration_dry) %>% 
  map(density) 


density_dur_rcp85 <-  lseries_rcp85 %>%
  map(duration_dry) %>% 
  map(density) 



# Mean de la Densidad -----------------------------------------------------

# historical  
mean_den_hist <- NULL
for (i in 1:length(density_dur_hist)) {
  densidad <- density_dur_hist[[i]]
  mean_den_hist <- c(mean_den_hist, mean(densidad$y))
}

 # Matriz para almacenar la densidad consolidada
mat_densidad_mean <- matrix(data = mean_den_hist, 
                            nrow = nrow(hist_mean[[1]]),
                            ncol =  ncol(hist_mean[[1]]),
                            byrow = T) %>% 
  rast()
plot(mat_densidad_mean)

m_mean_dens_hist <- 
  save_map_r(r = mat_densidad_mean, name_legend = "Density Mean",
             title = "Historical - Promedio de Densidad de Duración",
             option_col = "A", width_out = 8, height_out = 8,
             filename_out = "images/hist_duration_mean_dens.png",
             limits = c(0, 1), nbreaks = 5)


# RCP 2.6

mean_den_rcp26 <- NULL
for (i in 1:length(density_dur_rcp26)) {
  densidad <- density_dur_rcp26[[i]]
  mean_den_rcp26 <- c(mean_den_rcp26, mean(densidad$y))
}

# Matriz para almacenar la densidad consolidada
mat_den_mean_rcp26 <- matrix(data = mean_den_rcp26,
                             nrow = nrow(hist_mean[[1]]),
                             ncol =  ncol(hist_mean[[1]]),
                             byrow = T) %>% 
  rast()
plot(mat_den_mean_rcp26)

m_mean_dens_rcp26 <- 
  save_map_r(r = mat_den_mean_rcp26, name_legend = "Density Mean",
             title = "RCP 2.6 - Promedio de Densidad de Duración",
             option_col = "A", width_out = 8, height_out = 8,
             filename_out = "images/rcp26_duration_mean_dens.png",
             limits = c(0, 1), nbreaks = 5)


mean_den_rcp45 <- NULL
for (i in 1:length(density_dur_rcp45)) {
  densidad <- density_dur_rcp45[[i]]
  mean_den_rcp45 <- c(mean_den_rcp45, mean(densidad$y))
}

# Matriz para almacenar la densidad consolidada
mat_den_mean_rcp45 <- matrix(data = mean_den_rcp45,
                             nrow = nrow(hist_mean[[1]]),
                             ncol =  ncol(hist_mean[[1]]),
                             byrow = T) %>% 
  rast()
plot(mat_den_mean_rcp45)

m_mean_dens_rcp45 <- 
  save_map_r(r = mat_den_mean_rcp45, name_legend = "Density Mean",
             title = "RCP 4.5 - Promedio de Densidad de Duración",
             option_col = "A", width_out = 8, height_out = 8,
             filename_out = "images/rcp45_duration_mean_dens.png",
             limits = c(0, 1), nbreaks = 5)


mean_den_rcp85 <- NULL
for (i in 1:length(density_dur_rcp85)) {
  densidad <- density_dur_rcp85[[i]]
  mean_den_rcp85 <- c(mean_den_rcp85, mean(densidad$y))
}

# Matriz para almacenar la densidad consolidada
mat_den_mean_rcp85 <- matrix(data = mean_den_rcp85,
                             nrow = nrow(hist_mean[[1]]),
                             ncol =  ncol(hist_mean[[1]]),
                             byrow = T) %>% 
  rast()
plot(mat_den_mean_rcp85)

m_mean_dens_rcp45 <- 
  save_map_r(r = mat_den_mean_rcp85, name_legend = "Density Mean",
             title = "RCP 8.5 - Promedio de Densidad de Duración",
             option_col = "A", width_out = 8, height_out = 8,
             filename_out = "images/rcp85_duration_mean_dens.png",
             limits = c(0, 1), nbreaks = 5)







# Varianza ----------------------------------------------------------------

# var_densities <- NULL
# for (i in 1:length(duration_list)) {
#   densidad <- duration_list[[i]]
#   var_densities <- c(var_densities, var(densidad$y))
# }
# 
# # Matriz para almacenar la densidad consolidada
# mat_densidad_var <- matrix(data = var_densities, nrow = nrow(nc_all[[1]]), 
#                        ncol =  ncol(nc_all[[1]]), byrow = T) %>% 
#   rast()
# plot(mat_densidad_var)
# 
# map_var_dens <- map_raster(r = mat_densidad_var, title = "Varianza de Densidad", 
#            name_legend = "Density Var", option_col = "B")
# 
# 
# ggsave(plot = map_var_dens, filename = "images/MIROC5_hist_var_dens.png",
#        width = 8, height = 10)



# Coeficiente de variabilidad ---------------------------------------------



# historical  
cv_den_hist <- NULL
for (i in 1:length(density_dur_hist)) {
  densidad <- density_dur_hist[[i]]
  cv_den_hist <- c(cv_den_hist, (var(densidad$y)/mean(densidad$y)))
}

# Matriz para almacenar la densidad consolidada
mat_densidad_cv <- matrix(data = cv_den_hist, 
                            nrow = nrow(hist_mean[[1]]),
                            ncol =  ncol(hist_mean[[1]]),
                            byrow = T) %>% 
  rast()
plot(mat_densidad_cv)

m_mean_dens_hist <- 
  save_map_r(r = mat_densidad_mean, name_legend = "Density Mean",
             title = "Historical - Promedio de Densidad de Duración",
             option_col = "A", width_out = 8, height_out = 8,
             filename_out = "images/hist_duration_mean_dens.png",
             limits = c(0, 1), nbreaks = 5)


# RCP 2.6

mean_den_rcp26 <- NULL
for (i in 1:length(density_dur_rcp26)) {
  densidad <- density_dur_rcp26[[i]]
  mean_den_rcp26 <- c(mean_den_rcp26, mean(densidad$y))
}

# Matriz para almacenar la densidad consolidada
mat_den_mean_rcp26 <- matrix(data = mean_den_rcp26,
                             nrow = nrow(hist_mean[[1]]),
                             ncol =  ncol(hist_mean[[1]]),
                             byrow = T) %>% 
  rast()
plot(mat_den_mean_rcp26)

m_mean_dens_rcp26 <- 
  save_map_r(r = mat_den_mean_rcp26, name_legend = "Density Mean",
             title = "RCP 2.6 - Promedio de Densidad de Duración",
             option_col = "A", width_out = 8, height_out = 8,
             filename_out = "images/rcp26_duration_mean_dens.png",
             limits = c(0, 1), nbreaks = 5)


mean_den_rcp45 <- NULL
for (i in 1:length(density_dur_rcp45)) {
  densidad <- density_dur_rcp45[[i]]
  mean_den_rcp45 <- c(mean_den_rcp45, mean(densidad$y))
}

# Matriz para almacenar la densidad consolidada
mat_den_mean_rcp45 <- matrix(data = mean_den_rcp45,
                             nrow = nrow(hist_mean[[1]]),
                             ncol =  ncol(hist_mean[[1]]),
                             byrow = T) %>% 
  rast()
plot(mat_den_mean_rcp45)

m_mean_dens_rcp45 <- 
  save_map_r(r = mat_den_mean_rcp45, name_legend = "Density Mean",
             title = "RCP 4.5 - Promedio de Densidad de Duración",
             option_col = "A", width_out = 8, height_out = 8,
             filename_out = "images/rcp45_duration_mean_dens.png",
             limits = c(0, 1), nbreaks = 5)


mean_den_rcp85 <- NULL
for (i in 1:length(density_dur_rcp85)) {
  densidad <- density_dur_rcp85[[i]]
  mean_den_rcp85 <- c(mean_den_rcp85, mean(densidad$y))
}

# Matriz para almacenar la densidad consolidada
mat_den_mean_rcp85 <- matrix(data = mean_den_rcp85,
                             nrow = nrow(hist_mean[[1]]),
                             ncol =  ncol(hist_mean[[1]]),
                             byrow = T) %>% 
  rast()
plot(mat_den_mean_rcp85)

m_mean_dens_rcp45 <- 
  save_map_r(r = mat_den_mean_rcp85, name_legend = "Density Mean",
             title = "RCP 8.5 - Promedio de Densidad de Duración",
             option_col = "A", width_out = 8, height_out = 8,
             filename_out = "images/rcp85_duration_mean_dens.png",
             limits = c(0, 1), nbreaks = 5)





cv_densities <- NULL
for (i in 1:length(duration_list)) {
  densidad <- duration_list[[i]]
  cv_densities <- c(cv_densities, (var(densidad$y)/mean(densidad$y)))
}

# Matriz para almacenar la densidad consolidada
mat_densidad_cv <- matrix(data = cv_densities, nrow = nrow(nc_all[[1]]), 
                           ncol =  ncol(nc_all[[1]]), byrow = T) %>% 
  rast()
plot(mat_densidad_cv)

map_cv_dens <- map_raster(r = mat_densidad_cv, 
                         title = "Coeficiente de Variabilidad de Densidad",
                         name_legend = "Density CV", option_col = "E")

ggsave(plot = map_cv_dens, filename = "images/MIROC5_hist_vc_dens.png",
       width = 8, height = 10)




# x <- 1:length(duration_list[[1]]$y) 
# y <- 1:length(duration_list)
# densidad_consolidada <- matrix(0, nrow = length(y), ncol = length(x)) 

