# objetivos: Funciones probabiliadad Dry Spell

# Opciones Generales ------------------------------------------------------

source("R/fnc_raster.R")
source("R/fnc_dryspell.R")
source("R/fnc_visual.R")
options(scipen = 999)


# insumos -----------------------------------------------------------------
nc_all <-  rast("data/tif/MIROC5_historical.tif")


# get series --------------------------------------------------------------

df_series <- nc_all %>% terra::as.data.frame()
df_series_t <- as.data.frame(t(df_series))

# Cálculo de Densidad con pixe n ------------------------------------------

n = 2
px_1 <- df_series_t[,n] %>% as.vector()
duration_n <- duration_dry(px_1, threshold = 0.1)

data_duration <-  data.frame(n = 1:length(duration_n),
                             duration = duration_n)

p <- ggplot(data_duration, aes(x=duration)) + 
  geom_density(color = "#3690c0", linewidth = 0.8)+
  geom_vline(aes(xintercept=mean(duration)),
             color="magenta", linetype="dashed", linewidth=0.5)+
  labs(title = "Función de Densidad", subtitle = paste0("Pixel n=", n)) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "gray80"), 
        panel.grid.minor = element_line(colour = "gray80"))
p

dim(data_duration)

densidad <-  density(data_duration$duration)
plot(densidad, main = "Función de Densidad de Períodos de Sequía", xlab = "Duración (días)", ylab = "Densidad")




# Aplicación de Forma masiva ----------------------------------------------

lista_series <- lapply(df_series_t, as.vector)

library(purrr)

duration_list <-  lista_series %>% 
  map(duration_dry) %>% 
  map(density) 
  
duration_list[[n]] %>% plot()
  

# Mean de la Densidad -----------------------------------------------------

  
mean_densities <- NULL
for (i in 1:length(duration_list)) {
  densidad <- duration_list[[i]]
  mean_densities <- c(mean_densities, mean(densidad$y))
}

 # Matriz para almacenar la densidad consolidada
mat_densidad_mean <- matrix(data = mean_densities, nrow = nrow(nc_all[[1]]), 
                       ncol =  ncol(nc_all[[1]]), byrow = T) %>% 
  rast()
plot(mat_densidad_mean)

map_mean_dens <- map_raster(r = mat_densidad_mean, title = "Promedio de Densidad", 
           name_legend = "Density Mean", option_col = "D")

ggsave(plot = map_mean_dens, filename = "images/MIROC5_hist_mean_dens.png",
       width = 8, height = 10)

# Varianza ----------------------------------------------------------------

var_densities <- NULL
for (i in 1:length(duration_list)) {
  densidad <- duration_list[[i]]
  var_densities <- c(var_densities, var(densidad$y))
}

# Matriz para almacenar la densidad consolidada
mat_densidad_var <- matrix(data = var_densities, nrow = nrow(nc_all[[1]]), 
                       ncol =  ncol(nc_all[[1]]), byrow = T) %>% 
  rast()
plot(mat_densidad_var)

map_var_dens <- map_raster(r = mat_densidad_var, title = "Varianza de Densidad", 
           name_legend = "Density Var", option_col = "B")


ggsave(plot = map_var_dens, filename = "images/MIROC5_hist_var_dens.png",
       width = 8, height = 10)

# Coeficiente de variabilidad ---------------------------------------------

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

