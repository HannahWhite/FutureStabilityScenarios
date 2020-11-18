########################################################################################
### Historical climate models of stability for prediction - 50 year climatic history ###
########################################################################################

### Hannah White - Started 25.08.2020
### Edited 16.09.2020 to include fit errors
### Models using 50 years climatic history

#### Predictive modelling of stability measures in pasture across the island of Ireland
#### Currently no predictive model of future variability as null model performs well

library(nlme)
library(AICcmodavg)

## Read in data and extract what is required

covars <- read.csv('J:\\Postdoc Grassland Resilience\\EnvironmentData\\covariates.csv', header = TRUE)

# stability metrics

stability10km <- read.csv('J:\\Postdoc Grassland Resilience\\MODIS6\\Resilience\\REFIT2stability10km2sd.csv', header = TRUE)

stability10km$evi.rec <- stability10km$evi.rec*8
stability10km$evi.recrate <- stability10km$evi.recrate/8


# extract metrics required for modelling

stability.red <- stability10km[, c(1:4, 6:7)]

# merge and get stability just down to 830 squares

stability.env <- merge(covars, stability.red, by.x = c('east', 'north'), by.y = c('eastings', 'northings'), all.x = TRUE, all.y = FALSE)

# change Spnum_out to nat.fres
names(stability.env)[4] <- 'nat.fres'

## Create dominant habitat column at 50% threshold

dominant <- function (x, threshold) {
  ifelse(max(x) >=threshold, names(which.max(x)), 'Heterogenous') 
}

stability.env$dom50 <- apply(stability.env[,6:20], 1, function(x) dominant(x, threshold = 0.5))

stability.env$hetero.dom50 <- ifelse(stability.env$dom50 == 'Heterogenous', 'H', 'N')

stability.env$pasture.dom50 <- ifelse(stability.env$dom50 == 'Pasture', 'P', 'N')


stability.env$hetpast <- ifelse(stability.env$dom50 == 'Heterogenous', 'H',
                                ifelse(stability.env$dom50 == 'Pasture', 'P', 'N'))

### 

# Extract pasture 

pasture.div <- stability.env[stability.env$hetpast == 'P',]

### Scale and centre covariates (nat.fres, landscape diversity and climate)

pasture.div[, c(4, 22:32)] <- scale(pasture.div[, c(4, 22:32)]) 

## read in future climate data and cut to just pasture
load('J:\\Postdoc Grassland Resilience\\Climate Data\\future.clim.nneigh.RData')
coords <- pasture.div[,1:2]

future.clim <- merge(clim.nneigh, coords, by = c('east', 'north'), all.x = FALSE, all.y = TRUE)

## scale future.clim
future.clim[ ,5:36] <- scale(future.clim[,5:36])


rm(clim.nneigh, covars)

#### MODELS ####

###################
### Variability ###
###################

m.var.null <- gls(log(evi.var) ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
                  method = 'ML', data = pasture.div)
## AIC = -642.1002

m.var.clim <- gls(log(evi.var) ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                  method = 'ML', data = pasture.div)
## AIC = -640.8766


##################
### Resistance ###
##################

m.resist.null <- gls(evi.mag ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
                     method = 'ML', data = pasture.div) 
## AIC = -2515.945

m.resist.clim <- gls(evi.mag ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                     method = 'ML', data = pasture.div)
## AIC = -2529.379

## Predictive models for resistance

### RCA4

## RCP45
clim.R45.50 <- data.frame(future.clim[, 1:2], future.clim[, grepl('R45.50', names(future.clim))])
names(clim.R45.50) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.R45.50 <- predict(m.resist.clim, clim.R45.50)
se.R45.50 <- predictSE.gls(m.resist.clim, clim.R45.50)$se.fit

clim.R45.80 <- data.frame(future.clim[, 1:2], future.clim[, grepl('R45.80', names(future.clim))])
names(clim.R45.80) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.R45.80 <- predict(m.resist.clim, clim.R45.80)
se.R45.80 <- predictSE.gls(m.resist.clim, clim.R45.80)$se.fit


## RCP85
clim.R85.50 <- data.frame(future.clim[, 1:2], future.clim[, grepl('R85.50', names(future.clim))])
names(clim.R85.50) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.R85.50 <- predict(m.resist.clim, clim.R85.50)
se.R85.50 <- predictSE.gls(m.resist.clim, clim.R85.50)$se.fit

clim.R85.80 <- data.frame(future.clim[, 1:2], future.clim[, grepl('R85.80', names(future.clim))])
names(clim.R85.80) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.R85.80 <- predict(m.resist.clim, clim.R85.80)
se.R85.80 <- predictSE.gls(m.resist.clim, clim.R85.80)$se.fit


### HIHAM5

## RCP45
clim.H45.50 <- data.frame(future.clim[, 1:2], future.clim[, grepl('H45.50', names(future.clim))])
names(clim.H45.50) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.H45.50 <- predict(m.resist.clim, clim.H45.50)
se.H45.50 <- predictSE.gls(m.resist.clim, clim.H45.50)$se.fit

clim.H45.80 <- data.frame(future.clim[, 1:2], future.clim[, grepl('H45.80', names(future.clim))])
names(clim.H45.80) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.H45.80 <- predict(m.resist.clim, clim.H45.80)
se.H45.80 <- predictSE.gls(m.resist.clim, clim.H45.80)$se.fit


## RCP85
clim.H85.50 <- data.frame(future.clim[, 1:2], future.clim[, grepl('H85.50', names(future.clim))])
names(clim.H85.50) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.H85.50 <- predict(m.resist.clim, clim.H85.50)
se.H85.50 <- predictSE.gls(m.resist.clim, clim.H85.50)$se.fit

clim.H85.80 <- data.frame(future.clim[, 1:2], future.clim[, grepl('H85.80', names(future.clim))])
names(clim.H85.80) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.H85.80 <- predict(m.resist.clim, clim.H85.80)
se.H85.80 <- predictSE.gls(m.resist.clim, clim.H85.80)$se.fit

## combine resistance predictions into dataframe
pred.resist <- data.frame(future.clim[,1:2], pred.R45.50, pred.R45.80, pred.H45.50, pred.H45.80,
                          pred.R85.50, pred.R85.80, pred.H85.50, pred.H85.80)
se.resist <- data.frame(future.clim[,1:2], se.R45.50, se.R45.80, se.H45.50, se.H45.80,
                        se.R85.50, se.R85.80, se.H85.50, se.H85.80)
#####################
### Recovery time ###
#####################

m.rec.null <- gls(log(evi.rec) ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
                  method = 'ML', data = pasture.div)
## AIC = 414.2189  

m.rec.clim <- gls(log(evi.rec) ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                  method = 'ML', data = pasture.div)

## AIC = 390.3898

## Predictive models for recovery time

### RCA4

## RCP45
clim.R45.50 <- data.frame(future.clim[, 1:2], future.clim[, grepl('R45.50', names(future.clim))])
names(clim.R45.50) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.R45.50 <- predict(m.rec.clim, clim.R45.50)
se.R45.50 <- predictSE.gls(m.rec.clim, clim.R45.50)$se.fit # remeber this cannot be back transformed

clim.R45.80 <- data.frame(future.clim[, 1:2], future.clim[, grepl('R45.80', names(future.clim))])
names(clim.R45.80) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.R45.80 <- predict(m.rec.clim, clim.R45.80)
se.R45.80 <- predictSE.gls(m.rec.clim, clim.R45.80)$se.fit

## RCP85
clim.R85.50 <- data.frame(future.clim[, 1:2], future.clim[, grepl('R85.50', names(future.clim))])
names(clim.R85.50) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.R85.50 <- predict(m.rec.clim, clim.R85.50)
se.R85.50 <- predictSE.gls(m.rec.clim, clim.R85.50)$se.fit

clim.R85.80 <- data.frame(future.clim[, 1:2], future.clim[, grepl('R85.80', names(future.clim))])
names(clim.R85.80) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.R85.80 <- predict(m.rec.clim, clim.R85.80)
se.R85.80 <- predictSE.gls(m.rec.clim, clim.R85.80)$se.fit

### HIHAM5

## RCP45
clim.H45.50 <- data.frame(future.clim[, 1:2], future.clim[, grepl('H45.50', names(future.clim))])
names(clim.H45.50) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.H45.50 <- predict(m.rec.clim, clim.H45.50)
se.H45.50 <- predictSE.gls(m.rec.clim, clim.H45.50)$se.fit

clim.H45.80 <- data.frame(future.clim[, 1:2], future.clim[, grepl('H45.80', names(future.clim))])
names(clim.H45.80) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.H45.80 <- predict(m.rec.clim, clim.H45.80)
se.H45.80 <- predictSE.gls(m.rec.clim, clim.H45.80)$se.fit

## RCP85
clim.H85.50 <- data.frame(future.clim[, 1:2], future.clim[, grepl('H85.50', names(future.clim))])
names(clim.H85.50) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.H85.50 <- predict(m.rec.clim, clim.H85.50)
se.H85.80 <- predictSE.gls(m.rec.clim, clim.H85.80)$se.fit

clim.H85.80 <- data.frame(future.clim[, 1:2], future.clim[, grepl('H85.80', names(future.clim))])
names(clim.H85.80) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.H85.80 <- predict(m.rec.clim, clim.H85.80)
se.H85.80 <- predictSE.gls(m.rec.clim, clim.H85.80)$se.fit


pred.rec <- data.frame(future.clim[,1:2], pred.R45.50, pred.R45.80, pred.R85.50, pred.R85.80,
                       pred.H45.50, pred.H45.80, pred.H85.50, pred.H85.80)
se.rec <- data.frame(future.clim[,1:2], se.R45.50, se.R45.80, se.R85.50, se.R85.80,
                     se.H45.50, se.H45.80, se.H85.50, se.H85.80)

## need to unlog pred.rec

pred.rec[,3:10] <- exp(pred.rec[,3:10])

#####################
### Recovery rate ###
#####################

m.recrate.null <- gls(evi.recrate ~ 1, correlation=corExp(form=~east+north, metric='euclidean'),
                      method = 'ML', data = pasture.div)
## AIC = 2701.899

m.recrate.clim <- gls(evi.recrate ~ var.tg + var.rr + ft.tg + ft.rr, correlation=corExp(form=~east+north, metric='euclidean'),
                      method = 'ML', data = pasture.div)
## AIC = 2689.825  

## Predictive models for recovery rate

### RCA4

## RCP45
clim.R45.50 <- data.frame(future.clim[, 1:2], future.clim[, grepl('R45.50', names(future.clim))])
names(clim.R45.50) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.R45.50 <- predict(m.recrate.clim, clim.R45.50)
se.R45.50 <- predictSE.gls(m.recrate.clim, clim.R45.50)$se.fit

clim.R45.80 <- data.frame(future.clim[, 1:2], future.clim[, grepl('R45.80', names(future.clim))])
names(clim.R45.80) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.R45.80 <- predict(m.recrate.clim, clim.R45.80)
se.R45.80 <- predictSE.gls(m.recrate.clim, clim.R45.80)$se.fit


## RCP85
clim.R85.50 <- data.frame(future.clim[, 1:2], future.clim[, grepl('R85.50', names(future.clim))])
names(clim.R85.50) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.R85.50 <- predict(m.recrate.clim, clim.R85.50)
se.R85.50 <- predictSE.gls(m.recrate.clim, clim.R85.50)$se.fit

clim.R85.80 <- data.frame(future.clim[, 1:2], future.clim[, grepl('R85.80', names(future.clim))])
names(clim.R85.80) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.R85.80 <- predict(m.recrate.clim, clim.R85.80)
se.R85.80 <- predictSE.gls(m.recrate.clim, clim.R85.80)$se.fit

### HIHAM5

## RCP45
clim.H45.50 <- data.frame(future.clim[, 1:2], future.clim[, grepl('H45.50', names(future.clim))])
names(clim.H45.50) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.H45.50 <- predict(m.recrate.clim, clim.H45.50)
se.H45.50 <- predictSE.gls(m.recrate.clim, clim.H45.50)$se.fit

clim.H45.80 <- data.frame(future.clim[, 1:2], future.clim[, grepl('H45.80', names(future.clim))])
names(clim.H45.80) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.H45.80 <- predict(m.recrate.clim, clim.H45.80)
se.H45.80 <- predictSE.gls(m.recrate.clim, clim.H45.80)$se.fit

## RCP85
clim.H85.50 <- data.frame(future.clim[, 1:2], future.clim[, grepl('H85.50', names(future.clim))])
names(clim.H85.50) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.H85.50 <- predict(m.recrate.clim, clim.H85.50)
se.H85.50 <- predictSE.gls(m.recrate.clim, clim.H85.50)$se.fit

clim.H85.80 <- data.frame(future.clim[, 1:2], future.clim[, grepl('H85.80', names(future.clim))])
names(clim.H85.80) <- c('east', 'north', 'var.rr', 'ft.rr', 'var.tg', 'ft.tg')
pred.H85.80 <- predict(m.recrate.clim, clim.H85.80)
se.H85.80 <- predictSE.gls(m.recrate.clim, clim.H85.80)$se.fit

pred.recrate <- data.frame(future.clim[,1:2], pred.R45.50, pred.R45.80, pred.R85.50, pred.R85.80,
                           pred.H45.50, pred.H45.80, pred.H85.50, pred.H85.80)
se.recrate <- data.frame(future.clim[,1:2], se.R45.50, se.R45.80, se.R85.50, se.R85.80,
                         se.H45.50, se.H45.80, se.H85.50, se.H85.80)

save(pred.resist, pred.rec, pred.recrate, file = 'E:\\Postdoc Grassland Resilience\\MODIS6\\Resilience\\FutureStability\\future.stability.RData')
save(se.resist, se.rec, se.recrate, file = 'J:\\Postdoc Grassland Resilience\\MODIS6\\Resilience\\FutureStability\\future.stability.error.RData')


