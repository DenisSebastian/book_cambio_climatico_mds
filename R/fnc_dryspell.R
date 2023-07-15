
# function 1 --------------------------------------------------------------



Duration_Dry_Spell_wave <- function(T, threshold =1) {
  I_on <- which(T > threshold)
  I_off <- which(T <= threshold)
  
  sigma <- rep(0, length(T))
  
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
  
  
  # return(duration)
  
  return(list(
    duration = duration,
    mean_dur = mean_dur,
    std_dur = std_dur,
    tL = tL
  ))
}



# function 2 --------------------------------------------------------------

Duration_dry_all <- function(prec, threshold = 1) {
  I_wet <- which(prec > threshold)
  I_dry <- which(prec <= threshold)
  
  sigma <- rep(0, length(prec))
  sigma[I_wet] <- 1  # 1 if it rains that day, 0 if not
  sigma[I_dry] <- 0
  
  I <- diff(sigma)
  ti_tem <- which(abs(I + 1) < 0.1) + 1
  tf <- which(abs(I - 1) < 0.1) + 1
  
  if (length(ti_tem) < 10) {
    duration <- NA
    ti <- NA
    tf <- NA
    n <- NA
    mean_dur <- NA
    tL <- NA
    t99 <- NA
    t999 <- NA
  } else {
    if (prec[1] > threshold) {
      ti <- numeric(length(ti_tem))
      ti <- ti_tem
    } else {
      ti <- numeric(length(ti_tem) + 1)
      ti[1] <- 1
      ti[2:length(ti)] <- ti_tem
    }
    
    if (length(ti) > length(tf)) {
      ti <- ti[-length(ti)]
    }
    
    duration <- tf - ti
    n <- length(duration)
    
    ti <- as.integer(ti)
    tf <- as.integer(tf)
    
    mean_dur <- mean(duration, na.rm = T)
    tL <- var(duration, na.rm = T) / mean(duration, na.rm = T)
  }
  # return(t999)
  
  return(list(duration = duration, ti = ti, tf = tf, n = n, mean_dur = mean_dur, tL = tL))
}

duration_dry <- function(prec, threshold = 0.1){
  results <- Duration_dry_all(prec = prec, threshold = threshold)
  duration <-  results$duration
  if(any(is.na(duration)) & length(duration)==1 ){
    duration <- c(0,0) 
  }
  
  return(duration)
}

duration_dry_percentail <- function(prec, threshold = 1, percentail =0.99){
  results <- Duration_dry_all(prec = prec, threshold = threshold)
  duration <-  results$duration
  percentail <-  quantile(duration, percentail, na.rm = TRUE)
  return(percentail)
}


duration_dry_n <- function(prec, threshold = 1, na_zero = T) {
  results <- Duration_dry_all(prec = prec, threshold = threshold)
  n_dry = results$n
  if(na_zero == T){
    n_dry[is.na(n_dry)] <- 0    
  }
  return(n_dry)
}

duration_dry_mean <- function(prec, threshold = 1) {
  results <- Duration_dry_all(prec = prec, threshold = threshold)
  mean_dry = results$mean_dur
  return(mean_dry)
}

duration_dry_cv <- function(prec, threshold = 1) {
    results <- Duration_dry_all(prec = prec, threshold = threshold)
    tl_dry = results$tL
    return(tl_dry)
}

na_zero <- function(x) ifelse(is.na(x),0, x )

# mapas de modelos de los 5 mediana
# distribucaiones de probabilidad conbinar
# Modelo pixel duraciones 
# Los demas 1 milimitro

# Combinar las duraciones en cada punto de la grilla

