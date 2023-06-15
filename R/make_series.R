# objetivos: generar grafico de serie de tiempo


# librerias ---------------------------------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(mapview)
library(purrr)
library(terra)


# insumos -----------------------------------------------------------------

nc_all <-  rast("data/tif/MIROC5_historical.tif")
nlyr(nc_all)


# mean --------------------------------------------------------------------
system.time({
  mean_pr <- global(nc_all, mean)
}) 



# dataframe ---------------------------------------------------------------

df_mean <- data.frame(date = as_date(time(nc_all)), pr = mean_pr$mean)
saveRDS(df_mean, "data/rds/dayly_mean_MIROC5.rds")

# plot series -------------------------------------------------------------

max_hist <- df_mean[which(x = df_mean$pr>20), ]
lab <- paste0("Maxim Pr: ", round(max_hist[1, 2],1), " \nDate: ",
             as.character(max_hist[1, 1]))
subt = paste0("MIROC5 Historical (", min(df_mean$date), " - ", max(df_mean$date), ")")


daily_pr <- ggplot(df_mean, 
       aes(x = date, y = pr)) + 
  geom_line(color = "#69b3a2", linewidth =0.1) + 
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  scale_y_continuous(breaks = seq(0, 22, by =2)) +
  annotate(geom="text", x=as.Date(max_hist[1, 1])-1000, y=max_hist[1, 2], 
           label =lab, color = "gray40", size =2) +
  annotate(geom="point", x=as.Date(max_hist[1, 1]), y=max_hist[1, 2], 
           size=2, shape=21, fill="transparent", color = "gray30") +
  geom_hline(yintercept=0.5, linetype="dashed", color = "red")+
  labs(y = "Mean Dialy Precipitation", x = "", colour =  "") +
  ggtitle(label = "Mean Dialy Precipitation", subtitle = subt)+
  theme_minimal()+
  theme (plot.title = element_text(colour ="gray20", size =11),
         plot.subtitle = element_text(colour ="gray50", size =9),
         axis.title.y = element_text(colour ="gray50", size =7))

daily_pr

ggsave(plot = daily_pr,filename = "images/ts_mean_daily_pr.png", 
       width = 12, height = 5, bg = "white")



# grafico agregado mensual ------------------------------------------------


datos <- df_mean
datos$Mes <- floor_date(datos$date, unit = "month")
datos_mensuales <- aggregate(pr ~ Mes, data = datos, FUN = mean)
datos_mensuales <- datos_mensuales[order(datos_mensuales$Mes), ]
colnames(datos_mensuales) <- c("Mes", "PR_mean")

saveRDS(datos_mensuales, "data/rds/monthly_mean_MIROC5.rds")

max_hist_mean <- datos_mensuales[which(x = datos_mensuales$PR_mean>8), ]
lab_m <- paste0("Maxim mean Pr: ", round(max_hist_mean[1, 2],1), " \nDate: ",
              as.character(max_hist_mean[1, 1]))
subt = paste0("MIROC5 Historical (",min(substr(datos_mensuales$Mes,start = 1, 7 )) ,
              " - ", max(substr(datos_mensuales$Mes,start = 1, 7 )), ")")

monthly_pr <- ggplot(datos_mensuales, 
       aes(x = Mes, y = PR_mean)) + 
  geom_line(color = "#69b3a2") + 
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(1, 8, by =0.5)) +
  annotate(geom="text", x=as.Date(max_hist_mean[1, 1])-1500, y=max_hist_mean[1, 2], 
           label =lab_m, color = "gray60", size = 3) +
  annotate(geom="point", x=as.Date(max_hist_mean[1, 1]), y=max_hist_mean[1, 2], 
           size=5, shape=21, fill="transparent", color = "gray30") +
  geom_hline(yintercept=2, linetype="dashed", color = "red")+
  labs(y = "Mean Monthly Precipitation", x = "", colour =  "") +
  ggtitle( label = "Mean MonthlyPrecipitation", subtitle = subt)+
  theme_minimal()+
  theme (plot.title = element_text(colour ="gray20", size =11),
         plot.subtitle = element_text(colour ="gray50", size =9),
         axis.title.y = element_text(colour ="gray50", size =8))


monthly_pr

ggsave(plot = monthly_pr,filename = "images/ts_mean_monthly_pr.png", 
       width = 12, height = 5, bg = "white")



# gplotly -----------------------------------------------------------------

library(plotly)


fig <- plot_ly(df_mean,type = 'scatter', mode = 'lines' )%>%
  add_trace(x = ~date, y = ~pr, name = "Latam",
            line = list(color = "#69b3a2"), opacity=0.7)%>%
  layout(title = 'Precipitaciones Diarias', showlegend = F,
         legend=list(title=list(text='Pr')),
         xaxis = list(rangeslider = list(visible = T),
                       rangeselector=list(
                         buttons=list(
                           list(count=1, label="1m", step="month", stepmode="backward"),
                           list(count=6, label="6m", step="month", stepmode="backward"),
                           list(count=1, label="YTD", step="year", stepmode="todate"),
                           list(count=1, label="1y", step="year", stepmode="backward"),
                           list(count=10, label="10y", step="year", stepmode="backward"),
                           list(step="all")
                         )))
         # ,xaxis = list(dtick = "y", tickformat="%Y")
         )

fig




fig <- plot_ly(datos_mensuales,type = 'scatter', mode = 'lines' )%>%
  add_trace(x = ~Mes, y = ~PR_mean, name = "Pr",
            line = list(color = "#69b3a2"), opacity=0.7)%>%
  layout(title = 'Promedio de Precipitaciones Mensuales', showlegend = F,
         legend=list(title=list(text='Pr')),
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector=list(
                        buttons=list(
                          list(count=1, label="1m", step="month", stepmode="backward"),
                          list(count=6, label="6m", step="month", stepmode="backward"),
                          list(count=1, label="YTD", step="year", stepmode="todate"),
                          list(count=1, label="1y", step="year", stepmode="backward"),
                          list(count=10, label="10y", step="year", stepmode="backward"),
                          list(step="all")
                        )))
         # ,xaxis = list(dtick = "y", tickformat="%Y")
  )

fig


 # Dry Spell ---------------------------------------------------------------

median_pr <- app(x = nc_all, fun = Duration_Dry_Spell_wave,
                  cores=4)

Duration_Dry_Spell_wave <- function(pr, threshold =1) {
  I_on <- which(pr > threshold)
  I_off <- which(pr <= threshold)
  
  sigma <- rep(0, length(pr))
  
  sigma[I_on] <- 1 # 1 if above threshold
  sigma[I_off] <- 0
  
  I <- diff(sigma)
  ti_tem <- which(abs(I - 1) < 0.1) + 1
  tf <- which(abs(I + 1) < 0.1) + 1
  
  if (T[1] >= threshold) {
    ti <- numeric(length(ti_tem) + 1)
    ti[1] <- 1
    ti[2:length(ti)] <- ti_tem
  } else {
    ti <- ti_tem
  }
  
  if (length(ti) > length(tf)) {
    ti <- ti[1:(length(ti) - 1)]
  }
  
  duration <- tf - ti
  mean_dur <- mean(duration)
  std_dur <- sd(duration)
  tL <- std_dur ^ 2 / mean_dur
  
  return(duration)
  
return(list(
  duration = duration,
  mean_dur = mean_dur,
  std_dur = std_dur,
  tL = tL
)
)
}


# calcular_mediana_por_capa <- function(serie_tiempo) {
#   n_layers <- nlyr(serie_tiempo)
#   mediana_por_capa <- double(n_layers)
#   
#   for (i in 1:n_layers) {
#     print(paste0(i , " de ", n_layers))
#     capa <- serie_tiempo[[i]]
#     mediana_por_capa[i] <- mean(capa[], na.rm = TRUE)
#   }
#   
#   return(mediana_por_capa)
# }
