###########################################
### Calculate future climate covariates ###
###########################################

### Hannah White 28.08.2020

### calculates variance in precipitation and extreme events under future climate scenarios
## naming of variables:
# var/ext = variance/extreme events
# pr/tas = precipitation/temperature 
# R/H = RCA4/HIRHAM model
# 45/85 = RCP scenario
# 50/80 = final year if modelled data (2050 or 2080)

#### Extreme event function

### fat tail function taken from Schmid and Trede 2003

fattail_func <- function(x){
  quants <- quantile(x, probs = c(0.975, 0.025, 0.875, 0.125), na.rm = TRUE)
  ft <- (quants[1]-quants[2])/(quants[3]-quants[4])
  return(ft)
}

## read in coordinates
load('F:\\FutureClimate\\EURO_CORDEX\\new.coord.RData')
### Cut out long-lat of Ireland
indLong1 <- which(new.coord$lon.new[1,] > -11 & new.coord$lon.new[1,] < -5.5) # have added index as only want index of one dimension of array
indLong2 <- which(new.coord$lon.new[412,] > -11 & new.coord$lon.new[412,] < -5.5)
indLat1 <- which(new.coord$lat.new[,1] > 49 & new.coord$lat.new[,1] < 55.4) # Use max latitude of 55.3 to remove Scotland
indLat2 <- which(new.coord$lat.new[,424] >49 & new.coord$lat.new[,424] <55.4)

long.min <- min(c(indLong1, indLong2))
long.max <- max(c(indLong1, indLong2))
lat.min <- min(c(indLat1, indLat2))
lat.max <- max(c(indLat1, indLat2))

east <- new.coord$lon.new[lat.min:lat.max, long.min:long.max]
north <- new.coord$lat.new[lat.min:lat.max, long.min:long.max] 

##########
## RCA4 ##
##########

#pr
load('F:\\FutureClimate\\ProcessedCORDEX\\RCA4\\RCP45pr06_50.RData')

varpr.R45.50 <- apply(e, c(1,2), var, na.rm = TRUE)
extpr.R45.50 <- apply(e, c(1,2), FUN = fattail_func)
rm(e)

load('F:\\FutureClimate\\ProcessedCORDEX\\RCA4\\RCP85pr06_50.RData')

varpr.R85.50 <- apply(e, c(1,2), var, na.rm = TRUE)
extpr.R85.50 <- apply(e, c(1,2), FUN = fattail_func)
rm(e)

load('F:\\FutureClimate\\ProcessedCORDEX\\RCA4\\RCP45pr31_80.RData')

varpr.R45.80 <- apply(d, c(1,2), var, na.rm = TRUE)
extpr.R45.80 <- apply(d, c(1,2), FUN = fattail_func)
rm(d)

load('F:\\FutureClimate\\ProcessedCORDEX\\RCA4\\RCP85pr31_80.RData')

varpr.R85.80 <- apply(dpr85, c(1,2), var, na.rm = TRUE)
extpr.R85.80 <- apply(dpr85, c(1,2), fattail_func)
rm(dpr85)

# tas
load('F:\\FutureClimate\\ProcessedCORDEX\\RCA4\\RCP45tas06_50.RData')

vartas.R45.50 <- apply(etas45, c(1,2), var, na.rm = TRUE)
exttas.R45.50 <- apply(etas45, c(1,2), FUN = fattail_func)
rm(etas45)

load('F:\\FutureClimate\\ProcessedCORDEX\\RCA4\\RCP85tas06_50.RData')

vartas.R85.50 <- apply(etas85, c(1,2), var, na.rm = TRUE)
exttas.R85.50 <- apply(etas85, c(1,2), FUN = fattail_func)
rm(etas85)

load('F:\\FutureClimate\\ProcessedCORDEX\\RCA4\\RCP45tas31_80.RData')

vartas.R45.80 <- apply(dtas45, c(1,2), var, na.rm = TRUE)
exttas.R45.80 <- apply(dtas45, c(1,2), FUN = fattail_func)
rm(dtas45)

load('F:\\FutureClimate\\ProcessedCORDEX\\RCA4\\RCP85tas31_80.RData')

vartas.R85.80 <- apply(dtas85, c(1,2), var, na.rm = TRUE)
exttas.R85.80 <- apply(dtas85, c(1,2), fattail_func)
rm(dtas85)

############
## HIRHAM ##
############

#pr
load('F:\\FutureClimate\\ProcessedCORDEX\\HIRHAM\\RCP45pr06_50.RData')

varpr.H45.50 <- apply(eprhir, c(1,2), var, na.rm = TRUE)
extpr.H45.50 <- apply(eprhir, c(1,2), FUN = fattail_func)
rm(eprhir)

load('F:\\FutureClimate\\ProcessedCORDEX\\HIRHAM\\RCP85pr06_50.RData')

varpr.H85.50 <- apply(eprhir, c(1,2), var, na.rm = TRUE)
extpr.H85.50 <- apply(eprhir, c(1,2), FUN = fattail_func)
rm(eprhir)

load('F:\\FutureClimate\\ProcessedCORDEX\\HIRHAM\\RCP45pr31_80.RData')

varpr.H45.80 <- apply(dprhir45, c(1,2), var, na.rm = TRUE)
extpr.H45.80 <- apply(dprhir45, c(1,2), FUN = fattail_func)
rm(dprhir45)

load('F:\\FutureClimate\\ProcessedCORDEX\\HIRHAM\\RCP85pr31_80.RData')

varpr.H85.80 <- apply(dprhir85, c(1,2), var, na.rm = TRUE)
extpr.H85.80 <- apply(dprhir85, c(1,2), fattail_func)
rm(dprhir85)

# tas
load('F:\\FutureClimate\\ProcessedCORDEX\\HIRHAM\\RCP45tas06_50.RData')

vartas.H45.50 <- apply(ehirtas45, c(1,2), var, na.rm = TRUE)
exttas.H45.50 <- apply(ehirtas45, c(1,2), FUN = fattail_func)
rm(ehirtas45)

load('F:\\FutureClimate\\ProcessedCORDEX\\RCA4\\RCP85tas06_50.RData')

vartas.H85.50 <- apply(etas85, c(1,2), var, na.rm = TRUE)
exttas.H85.50 <- apply(etas85, c(1,2), FUN = fattail_func)
rm(etas85)

load('F:\\FutureClimate\\ProcessedCORDEX\\HIRHAM\\RCP45tas31_80.RData')

vartas.H45.80 <- apply(dtashir45, c(1,2), var, na.rm = TRUE)
exttas.H45.80 <- apply(dtashir45, c(1,2), FUN = fattail_func)
rm(dtashir45)

load('F:\\FutureClimate\\ProcessedCORDEX\\HIRHAM\\RCP85tas31_80.RData')

vartas.H85.80 <- apply(dtashir85, c(1,2), var, na.rm = TRUE)
exttas.H85.80 <- apply(dtashir85, c(1,2), fattail_func)
rm(dtashir85)


## Create full dataframe
future.clim <- data.frame(east = east[1:10980], north = north[1:10980],
                          varpr.R45.50 = varpr.R45.50[1:10980], extpr.R45.50 = extpr.R45.50[1:10980],
                          varpr.R85.50 = varpr.R85.50[1:10980], extpr.R85.50 = extpr.R85.50[1:10980],
                          varpr.R45.80 = varpr.R45.80[1:10980], extpr.R45.80 = extpr.R45.80[1:10980],
                          varpr.R85.80 = varpr.R85.80[1:10980], extpr.R85.80 = extpr.R85.80[1:10980],
                          vartas.R45.50 = vartas.R45.50[1:10980], exttas.R45.50 = exttas.R45.50[1:10980],
                          vartas.R85.50 = vartas.R85.50[1:10980], exttas.R85.50 = exttas.R85.50[1:10980],
                          vartas.R45.80 = vartas.R45.80[1:10980], exttas.R45.80 = exttas.R45.80[1:10980],
                          vartas.R85.80 = vartas.R85.80[1:10980], exttas.R85.80 = exttas.R85.80[1:10980],
                          varpr.H45.50 = varpr.H45.50[1:10980], extpr.H45.50 = extpr.H45.50[1:10980],
                          varpr.H85.50 = varpr.H85.50[1:10980], extpr.H85.50 = extpr.H85.50[1:10980],
                          varpr.H45.80 = varpr.H45.80[1:10980], extpr.H45.80 = extpr.H45.80[1:10980],
                          varpr.H85.80 = varpr.H85.80[1:10980], extpr.H85.80 = extpr.H85.80[1:10980],
                          vartas.H45.50 = vartas.H45.50[1:10980], exttas.H45.50 = exttas.H45.50[1:10980],
                          vartas.H85.50 = vartas.H85.50[1:10980], exttas.H85.50 = exttas.H85.50[1:10980],
                          vartas.H45.80 = vartas.H45.80[1:10980], exttas.H45.80 = exttas.H45.80[1:10980],
                          vartas.H85.80 = vartas.H85.80[1:10980], exttas.H85.80 = exttas.H85.80[1:10980])

save(future.clim, file = 'E:\\Postdoc Grassland Resilience\\Climate Data\\future.clim.RData')

