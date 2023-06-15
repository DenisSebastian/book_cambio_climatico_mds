options(scipen = 999)
library(dplyr)

# lectura con stars -------------------------------------------------------

library(stars)

file_1 <- "data/historical/pr_SAM-44_MIROC-MIROC5_historical_r1i1p1_SMHI-RCA4_v3_day_19510101-19551231.nc"
prec = read_stars(file_1)
# plot_hook = function() plot(nc_outline, border = 'red', add = TRUE)

prec %>%
  slice(index = 1:12, along = "time") %>%
  plot(downsample = c(3, 3, 1), hook = plot_hook)

sf::read_sf(system.file("gpkg/nc.gpkg", package = "sf"), "nc.gpkg") %>%
  st_transform(st_crs(prec)) -> nc 

a = aggregate(prec, by = nc, FUN = max)
plot(a, max.plot = 23, border = 'grey', lwd = .5)
prec = read_stars(file_1)



# ncdf4 -------------------------------------------------------------------

library(ncdf4)
our_nc_data <- nc_open(file_1)
print(our_nc_data)
attributes(our_nc_data$var)
attributes(our_nc_data$dim)


lat <- ncvar_get(our_nc_data, "lat")
nlat <- dim(lat)
            

lon <- ncvar_get(our_nc_data, "lon")
nlon <- dim(lon)

print(c(nlon, nlat))

time <- ncvar_get(our_nc_data, "time")
tunits <- ncatt_get(our_nc_data, "time", "units")
nt <- dim(time)

pr_array <- ncvar_get(our_nc_data, "pr") 
fillvalue <- ncatt_get(our_nc_data, "pr", "_FillValue")
dim(pr_array)
pr_array[pr_array==fillvalue$value] <- NA


time_obs <- as.POSIXct(time, origin = "1949-12-01", tz="GMT")
dim(time_obs)
range(time_obs)

lswt_slice <- pr_array[ , , 1500] 
lswt_slice <- pr_array[ , , 25]
image(lon, lat, lswt_slice)



lonlattime <- as.matrix(expand.grid(lon,lat,time_obs))



# raster ------------------------------------------------------------------
library(raster)
library(dplyr)
library(lubridate)
library(mapview)

crs_SAM_44 <- "+proj=ob_tran +o_proj=longlat +o_lon_p=146 +o_lat_p=167"
# crs_SAM_44 <- "+proj=ob_tran +o_proj=moll +o_lat_p=45 +o_lon_p=-90 +lon_0=-180"

r_pr_base  <- brick(file_1, varname = "pr")

plot(r_pr_base[[1]])
# ext <-  drawExtent()
# ext
ext <- extent(c(162,  172 ,  -29,  -38 ))

r_pr <- crop(x = r_pr_base, y = ext, snap="out")

plot(r_pr_base[[15]])
plot(r_pr[[15]])
plot(ext, add = T) 

# *86,400

# pr <- rast(r_pr, crs = 4326)


plot(r_pr[[50]], col = rev(hcl.colors(10)))
sample_r <- r_pr[[50]]
mapview(sample_r)
time_days <- names(r_pr) %>% gsub("X", "", .) %>% 
  gsub("\\.", "-", .) %>% 
  yday()

time_days <- time_days[time_days!=366]


# time_days <- yday(seq(as_date("1951-01-01"), as_date("1955-12-31"), "day"))

# calculate the average
beginCluster(4)
r_pr_mean <- clusterR(r_pr, stackApply, args = list(indices = time_days, fun = mean))
endCluster()

val_mean_daily <- cellStats(r_pr_mean, stat = "mean")
val_max_daily <- cellStats(r_pr_mean, stat = "max")
daily_mean <- calc(r_pr, mean)



# Crear setie de tiempo
df <- data.frame(date = seq(as_date("1951-01-01"), as_date("1951-12-31"), "day"),
                 pr = val_mean_daily)

df_max <- data.frame(date = seq(as_date("1951-01-01"), as_date("1951-12-31"), "day"),
                 pr = val_max_daily)
rownames(df) <- NULL

library(ggplot2)
ggplot(df_max, 
       aes(date, pr)) + 
  geom_line(color = "#c6dbef") + 
  scale_x_date(date_breaks = "month", date_labels = "%b") +
  scale_y_continuous() +
  labs(y = "maximum precipitation", x = "", colour =  "") +
  theme_minimal()


daily_smooth <- function(x, span = 0.5){
  
  if(all(is.na(x))){
    
    return(x) 
    
  } else {
    
    df <- data.frame(yd = 1:length(x), ta = x)
    m <- loess(ta ~ yd, span = span, data = df)
    est <- predict(m, 1:length(x))
    
    return(est)
    
  }
}

# smooth the temperature
library(tidyr)
df_soft <- mutate(df, pr_smoothed = daily_smooth(pr)) %>% 
  pivot_longer(2:3, names_to = "var", values_to = "pr")


ggplot(df_soft, 
       aes(date, pr, 
           colour = var)) + 
  geom_line(size =0.8) + 
  scale_x_date(date_breaks = "month", date_labels = "%b") +
  # scale_y_continuous(breaks = seq(5, 28, 2)) +
  scale_colour_manual(values = c("#c6dbef", "#1d91c0")) +
  labs(y = "Mean Precipiattion", x = "", colour =  "") +
  theme_minimal()
  


r_pr_smooth <- calc(r_pr_mean, fun = daily_smooth)
val_mean_daily <- cellStats(r_pr_smooth, stat = "mean")

df<- data.frame(date = seq(as_date("1951-01-01"), as_date("1951-12-31"), "day"),
                     pr = val_mean_daily)
rownames(df) <- NULL

library(ggplot2)
ggplot(df, 
       aes(date, pr)) + 
  geom_line(color = "#1d91c0") + 
  scale_x_date(date_breaks = "month", date_labels = "%b") +
  scale_y_continuous() +
  labs(y = "maximum precipitation", x = "", colour =  "") +
  theme_minimal()



# https://dominicroye.github.io/en/2020/climate-animation-of-maximum-temperatures/