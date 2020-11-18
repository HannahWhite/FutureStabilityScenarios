#####################################################
### Convert rotated lat-long to standard lat-long function ###
#####################################################

### Hannah White 19.08.2020

### Adapted matlab code from here 
## https://gis.stackexchange.com/questions/10808/manually-transforming-rotated-lat-lon-to-regular-lat-lon/14445#14445
## https://uk.mathworks.com/matlabcentral/fileexchange/43435-rotated-grid-transform


library(ncdf4) # package for netcdf manipulation

### read in a netCDF file

#pr.40 <- nc_open('pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20460101-20501230.nc')
#{
#  sink('pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20460101-20501230.txt')
#  print(pr.40)
#  sink()
#}

unrotate <- function(x){
  # extract important data
  lon <- ncvar_get(x, "rlon")
  lat <- ncvar_get(x, "rlat", verbose = F) 
  day <- ncvar_get(x, "time") # in days since 1949-12-01
  
  ##### Conversion #####
  
  # create 2 dimensional arrays of lon and lat separately
  
  lon.array <- t(array(data = rep(lon, 412), dim = c(424, 412))) 
  lat.array <- array(data = rev(rep(lat, 424)), dim = c(412, 424))
  
  #lon.vec <- as.vector(lon)
  #lat.vec <- as.vector(lat)
  
  lon.rad <- (lon.array*pi)/180 # Convert degrees to radians
  lat.rad <- (lat.array*pi)/180
  
  x.rotate <- cos(lon.rad)*cos(lat.rad) # cartesian coords for trig
  y.rotate <- sin(lon.rad)*cos(lat.rad)
  z.rotate <- sin(lat.rad)
  
  
  theta <- 90 + -39.25 # Rotation around y-axis (north pole at 39.5 deg N so sputh pole lat = -39.25)
  phi <- -18 # Rotation around z-axis (pole at 162 deg W so south pole long = -18)
  
  phi <- (phi*pi)/180 # Convert degrees to radians
  theta <- (theta*pi)/180
  
  
  phi <- -phi
  theta <- -theta
  
  ## Rotated to regular
  
  x.new <- cos(theta)*cos(phi)*x.rotate + sin(phi)*y.rotate + sin(theta)*cos(phi)*z.rotate 
  y.new <- -cos(theta)*sin(phi)*x.rotate + cos(phi)*y.rotate - sin(theta)*sin(phi)*z.rotate
  z.new <- -sin(theta)*x.rotate + cos(theta)*z.rotate
  
  lon.new.cart <- atan2(y.new,x.new) # Convert cartesian back to spherical coordinates
  lat.new.cart <- asin(z.new)
  
  lon.new <- (lon.new.cart*180)/pi # Convert radians back to degrees
  lat.new <- (lat.new.cart*180)/pi
  
  return(list(lon.new = lon.new, lat.new = lat.new))
  
}

