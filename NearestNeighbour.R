######################################################
### Nearest neighbour for future climate variables ###
######################################################

### Hannah White 31.08.2020
## Gets coordinates that are used from covars dataframe and 
## finds the nearest neighbour to the RCM-derived climate variables

library(raster)
library(rgdal)
library(rgeos)
library(spdep)

## Get correct coords
## Read in data and extract what is required
covars <- read.csv('E:\\Postdoc Grassland Resilience\\EnvironmentData\\covariates.csv', header = TRUE)
coords <- covars[,1:2]
# make into spatial dataframe
coords.data.sp <- SpatialPointsDataFrame(coords, covars) # turn into spatial data so that can use nearest neighbour approach 
proj4string(coords.data.sp) <- CRS('+init=epsg:29903') # Irish grid



## read in future climate variables
load('E:\\Postdoc Grassland Resilience\\Climate Data\\future.clim.RData')

# convert to Irish grid
coords.clim <- future.clim[,c(1,2)]
coords.clim.sp <- SpatialPointsDataFrame(coords.clim, future.clim) 
proj4string(coords.clim.sp) <- CRS('+init=epsg:4326') # project to WGS 84 latitude and longitude

## transform to ITM
clim.itm <- spTransform(coords.clim.sp, CRS('+init=epsg:29903')) #  Irish grid

####### Nearest Neighbour

clim.array <- array(data = NA, dim = c(830, 34))

clim.df <- data.frame(clim.itm)

for (j in 1:830){
  temp.coords <- coords.data.sp[j,]
  distances <- spDists(temp.coords@coords, clim.itm@coords)
  
  min.index <- which.min(distances)
  clim.array[j,1:34] <- as.matrix(clim.df[min.index,c(35:36, 3:34)])
  
}


clim.nneigh <- data.frame(clim.array)
names(clim.nneigh) <- c('east.real', 'north.real', names(clim.df)[3:34])
clim.nneigh <- data.frame(coords, clim.nneigh)

save(clim.nneigh, file = 'E:\\Postdoc Grassland Resilience\\Climate Data\\future.clim.nneigh.RData')
