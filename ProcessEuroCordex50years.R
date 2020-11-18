######################################################################
### Combines different EURO-CORDEX data files for climate analyses ###
######################################################################

### Hannah White 26.08.2020
library(weathermetrics) # use to convert from kelvin to celsius
library(abind)
library(ncdf4)

## Have to combine all and process all later - cannot cut out Ireland because of inconsistent coords

## Get coordinates
load('F:\\FutureClimate\\EURO_CORDEX\\new.coord.RData')

### Get lats and longs
indLong1 <- which(new.coord$lon.new[1,] > -11 & new.coord$lon.new[1,] < -5.5) # have added index as only want index of one dimension of array
indLong2 <- which(new.coord$lon.new[412,] > -11 & new.coord$lon.new[412,] < -5.5)
indLat1 <- which(new.coord$lat.new[,1] > 49 & new.coord$lat.new[,1] < 55.4) # Use max latitude of 55.3 to remove Scotland
indLat2 <- which(new.coord$lat.new[,424] >49 & new.coord$lat.new[,424] <55.4)

long.min <- min(c(indLong1, indLong2))
long.max <- max(c(indLong1, indLong2))
lat.min <- min(c(indLat1, indLat2))
lat.max <- max(c(indLat1, indLat2))

long.length <- length(long.min:long.max)
lat.length <- length(lat.min:lat.max)

##### Dates 2031-2080 #####

### RCP 45

## RCA4

# pr

pr31 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2031-2040\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20310101-20351230.nc')
pr36 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2031-2040\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20360101-20401230.nc')
pr41 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2041-2050\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20410101-20451230.nc')
pr46 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2041-2050\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20460101-20501230.nc')
pr51 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2051-2060\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20510101-20551230.nc')
pr56 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2051-2060\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20560101-20601230.nc')
pr61 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2061-2070\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20610101-20651230.nc')
pr66 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2061-2070\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20660101-20701230.nc')
pr71 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2071-2080\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20710101-20751230.nc')
pr76 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2071-2080\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20760101-20801230.nc')


pr45 <- list(pr31, pr36, pr41, pr46, pr51, pr56, pr61, pr66, pr71, pr76)

d.list <- lapply(pr45, function(x) ncvar_get(x, varid = 'pr', start=c(lat.min, long.min, 1), count=c(lat.length, long.length, -1), 
                                 verbose = FALSE, collapse_degen=FALSE))
d <- abind(d.list) # combines list of arrays into single array in correct order

d <- d*86400 # converts to mm per day

save(d, file = 'F:\\FutureClimate\\ProcessedCORDEX\\RCA4\\RCP45pr31_80.RData')

# tas

tas31 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2031-2040\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20310101-20351230.nc')
tas36 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2031-2040\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20360101-20401230.nc')
tas41 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2041-2050\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20410101-20451230.nc')
tas46 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2041-2050\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20460101-20501230.nc')
tas51 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2051-2060\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20510101-20551230.nc')
tas56 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2051-2060\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20560101-20601230.nc')
tas61 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2061-2070\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20610101-20651230.nc')
tas66 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2061-2070\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20660101-20701230.nc')
tas71 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2071-2080\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20710101-20751230.nc')
tas76 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2071-2080\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20760101-20801230.nc')

tas45 <- list(tas31, tas36, tas41, tas46, tas51, tas56, tas61, tas66, tas71, tas76)

dtas45.list <- lapply(tas45, function(x) ncvar_get(x, varid = 'tas', start=c(lat.min, long.min, 1), count=c(lat.length, long.length, -1), 
                                             verbose = FALSE, collapse_degen=FALSE))
dtas45 <- abind(dtas45.list) # combines list of arrays into single array in correct order

##convert temp from Kelvin to celsius
dtas45 <- kelvin.to.celsius(dtas45)

save(dtas45, file = 'F:\\FutureClimate\\ProcessedCORDEX\\RCA4\\RCP45tas31_80.RData')


## HIRHAM

# pr

prhir31 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2031-2040\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20310101-20351230.nc')
prhir36 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2031-2040\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20360101-20401230.nc')
prhir41 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2041-2050\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20410101-20451230.nc')
prhir46 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2041-2050\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20460101-20501230.nc')
prhir51 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2051-2060\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20510101-20551230.nc')
prhir56 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2051-2060\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20560101-20601230.nc')
prhir61 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2061-2070\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20610101-20651230.nc')
prhir66 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2061-2070\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20660101-20701230.nc')
prhir71 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2071-2080\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20710101-20751230.nc')
prhir76 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2071-2080\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20760101-20801230.nc')

prhir45 <- list(prhir31, prhir36, prhir41, prhir46, prhir51, prhir56, prhir61, prhir66, prhir71, prhir76)

dprhir45.list <- lapply(prhir45, function(x) ncvar_get(x, varid = 'pr', start=c(lat.min, long.min, 1), count=c(lat.length, long.length, -1), 
                                             verbose = FALSE, collapse_degen=FALSE))
dprhir45 <- abind(dprhir45.list) # combines list of arrays into single array in correct order

dprhir45 <- dprhir45*86400 #converts from kg m-2 s-1 to mm day-1

save(dprhir45, file = 'F:\\FutureClimate\\ProcessedCORDEX\\HIRHAM\\RCP45pr31_80.RData')

# tas
tashir31 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2031-2040\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20310101-20351230.nc')
tashir36 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2031-2040\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20360101-20401230.nc')
tashir41 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2041-2050\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20410101-20451230.nc')
tashir46 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2041-2050\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20460101-20501230.nc')
tashir51 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2051-2060\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20510101-20551230.nc')
tashir56 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2051-2060\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20560101-20601230.nc')
tashir61 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2061-2070\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20610101-20651230.nc')
tashir66 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2061-2070\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20660101-20701230.nc')
tashir71 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2071-2080\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20710101-20751230.nc')
tashir76 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2071-2080\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20760101-20801230.nc')

tashir45 <- list(tashir31, tashir36, tashir41, tashir46, tashir51, tashir56, tashir61, tashir66, tashir71, tashir76)

dtashir45.list <- lapply(tashir45, function(x) ncvar_get(x, varid = 'tas', start=c(lat.min, long.min, 1), count=c(lat.length, long.length, -1), 
                                                       verbose = FALSE, collapse_degen=FALSE))
dtashir45 <- abind(dtashir45.list) # combines list of arrays into single array in correct order

## convert to celsius
dtashir45 <- kelvin.to.celsius(dtashir45)

save(dtashir45, file = 'F:\\FutureClimate\\ProcessedCORDEX\\HIRHAM\\RCP45tas31_80.RData')

### RCP85

## RCA4

# pr

pr31 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2031-2040\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20310101-20351230.nc')
pr36 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2031-2040\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20360101-20401230.nc')
pr41 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2041-2050\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20410101-20451230.nc')
pr46 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2041-2050\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20460101-20501230.nc')
pr51 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2051-2060\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20510101-20551230.nc')
pr56 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2051-2060\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20560101-20601230.nc')
pr61 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2061-2070\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20610101-20651230.nc')
pr66 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2061-2070\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20660101-20701230.nc')
pr71 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2071-2080\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20710101-20751230.nc')
pr76 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2071-2080\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20760101-20801230.nc')


pr85 <- list(pr31, pr36, pr41, pr46, pr51, pr56, pr61, pr66, pr71, pr76)

dpr85.list <- lapply(pr85, function(x) ncvar_get(x, varid = 'pr', start=c(lat.min, long.min, 1), count=c(lat.length, long.length, -1), 
                                             verbose = FALSE, collapse_degen=FALSE))
dpr85 <- abind(dpr85.list) # combines list of arrays into single array in correct order

dpr85 <- dpr85*86400

save(dpr85, file = 'F:\\FutureClimate\\ProcessedCORDEX\\RCA4\\RCP85pr31_80.RData')


# tas

tas31 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2031-2040\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20310101-20351230.nc')
tas36 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2031-2040\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20360101-20401230.nc')
tas41 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2041-2050\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20410101-20451230.nc')
tas46 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2041-2050\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20460101-20501230.nc')
tas51 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2051-2060\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20510101-20551230.nc')
tas56 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2051-2060\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20560101-20601230.nc')
tas61 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2061-2070\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20610101-20651230.nc')
tas66 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2061-2070\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20660101-20701230.nc')
tas71 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2071-2080\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20710101-20751230.nc')
tas76 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2071-2080\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20760101-20801230.nc')

tas85 <- list(tas31, tas36, tas41, tas46, tas51, tas56, tas61, tas66, tas71, tas76)

dtas85.list <- lapply(tas85, function(x) ncvar_get(x, varid = 'tas', start=c(lat.min, long.min, 1), count=c(lat.length, long.length, -1), 
                                                   verbose = FALSE, collapse_degen=FALSE))
dtas85 <- abind(dtas85.list) # combines list of arrays into single array in correct order

##convert temp from Kelvin to celsius
dtas85 <- kelvin.to.celsius(dtas85)

save(dtas85, file = 'F:\\FutureClimate\\ProcessedCORDEX\\RCA4\\RCP85tas31_80.RData')

## HIRHAM

# pr

prhir31 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2031-2040\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20310101-20351230.nc')
prhir36 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2031-2040\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20360101-20401230.nc')
prhir41 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2041-2050\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20410101-20451230.nc')
prhir46 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2041-2050\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20460101-20501230.nc')
prhir51 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2051-2060\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20510101-20551230.nc')
prhir56 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2051-2060\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20560101-20601230.nc')
prhir61 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2061-2070\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20610101-20651230.nc')
prhir66 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2061-2070\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20660101-20701230.nc')
prhir71 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2071-2080\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20710101-20751230.nc')
prhir76 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2071-2080\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20760101-20801230.nc')

prhir85 <- list(prhir31, prhir36, prhir41, prhir46, prhir51, prhir56, prhir61, prhir66, prhir71, prhir76)

dprhir85.list <- lapply(prhir85, function(x) ncvar_get(x, varid = 'pr', start=c(lat.min, long.min, 1), count=c(lat.length, long.length, -1), 
                                                       verbose = FALSE, collapse_degen=FALSE))
dprhir85 <- abind(dprhir85.list) # combines list of arrays into single array in correct order

dprhir85 <- dprhir85*86400

save(dprhir85, file = 'F:\\FutureClimate\\ProcessedCORDEX\\HIRHAM\\RCP85pr31_80.RData')

# tas
tashir31 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2031-2040\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20310101-20351230.nc')
tashir36 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2031-2040\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20360101-20401230.nc')
tashir41 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2041-2050\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20410101-20451230.nc')
tashir46 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2041-2050\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20460101-20501230.nc')
tashir51 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2051-2060\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20510101-20551230.nc')
tashir56 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2051-2060\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20560101-20601230.nc')
tashir61 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2061-2070\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20610101-20651230.nc')
tashir66 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2061-2070\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20660101-20701230.nc')
tashir71 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2071-2080\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20710101-20751230.nc')
tashir76 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2071-2080\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20760101-20801230.nc')

tashir85 <- list(tashir31, tashir36, tashir41, tashir46, tashir51, tashir56, tashir61, tashir66, tashir71, tashir76)

dtashir85.list <- lapply(tashir85, function(x) ncvar_get(x, varid = 'tas', start=c(lat.min, long.min, 1), count=c(lat.length, long.length, -1), 
                                                         verbose = FALSE, collapse_degen=FALSE))
dtashir85 <- abind(dtashir85.list) # combines list of arrays into single array in correct order

## convert to celsius
dtashir85 <- kelvin.to.celsius(dtashir85)

save(dtashir85, file = 'F:\\FutureClimate\\ProcessedCORDEX\\HIRHAM\\RCP85tas31_80.RData')

###############################################################################################################################################################
##### 2006-2050 #####

#### RCP45

### RCA4

## pr

epr06 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2006-2010\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20060101-20101230.nc')
epr11 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2011-2020\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20110101-20151230.nc')
epr16 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2011-2020\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20160101-20201230.nc')
epr21 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2021-2030\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20210101-20251230.nc')
epr26 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2021-2030\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20260101-20301230.nc')
epr31 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2031-2040\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20310101-20351230.nc')
epr36 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2031-2040\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20360101-20401230.nc')
epr41 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2041-2050\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20410101-20451230.nc')
epr46 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2041-2050\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20460101-20501230.nc')


epr45 <- list(epr06, epr11, epr16, epr21, epr26, epr31, epr36, epr41, epr46)

e.list <- lapply(epr45, function(x) ncvar_get(x, varid = 'pr', start=c(lat.min, long.min, 1), count=c(lat.length, long.length, -1), 
                                             verbose = FALSE, collapse_degen=FALSE))
e <- abind(e.list) # combines list of arrays into single array in correct order

e <- e*86400

save(e, file = 'F:\\FutureClimate\\ProcessedCORDEX\\RCA4\\RCP45pr06_50.RData')

# tas

etas06 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2006-2010\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20060101-20101230.nc')
etas11 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2011-2020\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20110101-20151230.nc')
etas16 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2011-2020\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20160101-20201230.nc')
etas21 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2021-2030\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20210101-20251230.nc')
etas26 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2021-2030\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20260101-20301230.nc')
etas31 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2031-2040\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20310101-20351230.nc')
etas36 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2031-2040\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20360101-20401230.nc')
etas41 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2041-2050\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20410101-20451230.nc')
etas46 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2041-2050\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_SMHI-RCA4_v1_day_20460101-20501230.nc')

etas45 <- list(etas06, etas11, etas16, etas21, etas26, etas31, etas36, etas41, etas46)

etas45.list <- lapply(etas45, function(x) ncvar_get(x, varid = 'tas', start=c(lat.min, long.min, 1), count=c(lat.length, long.length, -1), 
                                                   verbose = FALSE, collapse_degen=FALSE))
etas45 <- abind(etas45.list) # combines list of arrays into single array in correct order

##convert temp from Kelvin to celsius
etas45 <- kelvin.to.celsius(etas45)

save(etas45, file = 'F:\\FutureClimate\\ProcessedCORDEX\\RCA4\\RCP45tas06_50.RData')


### HIRHAM
## pr

eprhir06 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2006-2010\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20060101-20101230.nc')
eprhir11 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2011-2020\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20110101-20151230.nc')
eprhir16 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2011-2020\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20160101-20201230.nc')
eprhir21 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2021-2030\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20210101-20251230.nc')
eprhir26 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2021-2030\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20260101-20301230.nc')
eprhir31 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2031-2040\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20310101-20351230.nc')
eprhir36 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2031-2040\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20360101-20401230.nc')
eprhir41 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2041-2050\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20410101-20451230.nc')
eprhir46 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2041-2050\\pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20460101-20501230.nc')


eprhir45 <- list(eprhir06, eprhir11, eprhir16, eprhir21, eprhir26, eprhir31, eprhir36, eprhir41, eprhir46)


eprhir.list <- lapply(eprhir45, function(x) ncvar_get(x, varid = 'pr', start=c(lat.min, long.min, 1), count=c(lat.length, long.length, -1), 
                                              verbose = FALSE, collapse_degen=FALSE))
eprhir <- abind(eprhir.list) # combines list of arrays into single array in correct order

eprhir <- eprhir*86400

save(eprhir, file = 'F:\\FutureClimate\\ProcessedCORDEX\\HIRHAM\\RCP45pr06_50.RData')

# tas

ehirtas06 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2006-2010\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20060101-20101230.nc')
ehirtas11 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2011-2020\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20110101-20151230.nc')
ehirtas16 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2011-2020\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20160101-20201230.nc')
ehirtas21 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2021-2030\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20210101-20251230.nc')
ehirtas26 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2021-2030\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20260101-20301230.nc')
ehirtas31 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2031-2040\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20310101-20351230.nc')
ehirtas36 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2031-2040\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20360101-20401230.nc')
ehirtas41 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2041-2050\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20410101-20451230.nc')
ehirtas46 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2041-2050\\tas_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_DMI-HIRHAM5_v2_day_20460101-20501230.nc')

ehirtas45 <- list(ehirtas06, ehirtas11, ehirtas16, ehirtas21, ehirtas26, ehirtas31, ehirtas36, ehirtas41, ehirtas46)

ehirtas45.list <- lapply(ehirtas45, function(x) ncvar_get(x, varid = 'tas', start=c(lat.min, long.min, 1), count=c(lat.length, long.length, -1), 
                                                    verbose = FALSE, collapse_degen=FALSE))
ehirtas45 <- abind(ehirtas45.list) # combines list of arrays into single array in correct order

##convert temp from Kelvin to celsius
ehirtas45 <- kelvin.to.celsius(ehirtas45)

save(ehirtas45, file = 'F:\\FutureClimate\\ProcessedCORDEX\\HIRHAM\\RCP45tas06_50.RData')

################################

#### RCP85

### RCA4

## pr

epr06 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2006-2010\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20060101-20101230.nc')
epr11 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2011-2020\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20110101-20151230.nc')
epr16 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2011-2020\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20160101-20201230.nc')
epr21 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2021-2030\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20210101-20251230.nc')
epr26 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2021-2030\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20260101-20301230.nc')
epr31 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2031-2040\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20310101-20351230.nc')
epr36 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2031-2040\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20360101-20401230.nc')
epr41 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2041-2050\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20410101-20451230.nc')
epr46 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2041-2050\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20460101-20501230.nc')


epr85 <- list(epr06, epr11, epr16, epr21, epr26, epr31, epr36, epr41, epr46)

e.list <- lapply(epr85, function(x) ncvar_get(x, varid = 'pr', start=c(lat.min, long.min, 1), count=c(lat.length, long.length, -1), 
                                              verbose = FALSE, collapse_degen=FALSE))
e <- abind(e.list) # combines list of arrays into single array in correct order

e <- e*86400

save(e, file = 'F:\\FutureClimate\\ProcessedCORDEX\\RCA4\\RCP85pr06_50.RData')

# tas

etas06 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2006-2010\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20060101-20101230.nc')
etas11 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2011-2020\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20110101-20151230.nc')
etas16 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2011-2020\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20160101-20201230.nc')
etas21 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2021-2030\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20210101-20251230.nc')
etas26 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2021-2030\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20260101-20301230.nc')
etas31 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2031-2040\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20310101-20351230.nc')
etas36 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2031-2040\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20360101-20401230.nc')
etas41 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2041-2050\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20410101-20451230.nc')
etas46 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\RCA4\\2041-2050\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20460101-20501230.nc')

etas85 <- list(etas06, etas11, etas16, etas21, etas26, etas31, etas36, etas41, etas46)

etas85.list <- lapply(etas85, function(x) ncvar_get(x, varid = 'tas', start=c(lat.min, long.min, 1), count=c(lat.length, long.length, -1), 
                                                    verbose = FALSE, collapse_degen=FALSE))
etas85 <- abind(etas85.list) # combines list of arrays into single array in correct order

##convert temp from Kelvin to celsius
etas85 <- kelvin.to.celsius(etas85)

save(etas85, file = 'F:\\FutureClimate\\ProcessedCORDEX\\RCA4\\RCP85tas06_50.RData')


### HIRHAM
## pr

eprhir06 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2006-2010\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20060101-20101230.nc')
eprhir11 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2011-2020\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20110101-20151230.nc')
eprhir16 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2011-2020\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20160101-20201230.nc')
eprhir21 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2021-2030\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20210101-20251230.nc')
eprhir26 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2021-2030\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20260101-20301230.nc')
eprhir31 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2031-2040\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20310101-20351230.nc')
eprhir36 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2031-2040\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20360101-20401230.nc')
eprhir41 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2041-2050\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20410101-20451230.nc')
eprhir46 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2041-2050\\pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20460101-20501230.nc')


eprhir85 <- list(eprhir06, eprhir11, eprhir16, eprhir21, eprhir26, eprhir31, eprhir36, eprhir41, eprhir46)


eprhir.list <- lapply(eprhir85, function(x) ncvar_get(x, varid = 'pr', start=c(lat.min, long.min, 1), count=c(lat.length, long.length, -1), 
                                                      verbose = FALSE, collapse_degen=FALSE))
eprhir <- abind(eprhir.list) # combines list of arrays into single array in correct order

eprhir <- eprhir*86400

save(eprhir, file = 'F:\\FutureClimate\\ProcessedCORDEX\\HIRHAM\\RCP85pr06_50.RData')

# tas

ehirtas06 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2006-2010\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20060101-20101230.nc')
ehirtas11 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2011-2020\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20110101-20151230.nc')
ehirtas16 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2011-2020\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20160101-20201230.nc')
ehirtas21 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2021-2030\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20210101-20251230.nc')
ehirtas26 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2021-2030\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20260101-20301230.nc')
ehirtas31 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2031-2040\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20310101-20351230.nc')
ehirtas36 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2031-2040\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20360101-20401230.nc')
ehirtas41 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2041-2050\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20410101-20451230.nc')
ehirtas46 <- nc_open('F:\\FutureClimate\\EURO_CORDEX\\HIRHAM\\2041-2050\\tas_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_DMI-HIRHAM5_v2_day_20460101-20501230.nc')

ehirtas85 <- list(ehirtas06, ehirtas11, ehirtas16, ehirtas21, ehirtas26, ehirtas31, ehirtas36, ehirtas41, ehirtas46)

ehirtas85.list <- lapply(ehirtas85, function(x) ncvar_get(x, varid = 'tas', start=c(lat.min, long.min, 1), count=c(lat.length, long.length, -1), 
                                                          verbose = FALSE, collapse_degen=FALSE))
ehirtas85 <- abind(ehirtas85.list) # combines list of arrays into single array in correct order

##convert temp from Kelvin to celsius
ehirtas85 <- kelvin.to.celsius(ehirtas85)

save(ehirtas85, file = 'F:\\FutureClimate\\ProcessedCORDEX\\HIRHAM\\RCP85tas06_50.RData')






