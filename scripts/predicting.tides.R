# set working directory on Windows
setwd("C:/Users/dhardy/Dropbox/sesync/outreach/DarienNews/hurricanes/R")


## NOAA harmonic tidal constituents
## https://tidesandcurrents.noaa.gov/map/index.shtml?type=HarmonicConstituents&region=Georgia
## NOAA tide predictions
## https://tidesandcurrents.noaa.gov/gmap3/index.shtml?type=TidePredictions&region=Georgia



#############################################
#############################################
## good fit for semi-sinusoidal model in R 
## https://stats.stackexchange.com/questions/60500/how-to-find-a-good-fit-for-semi-sinusoidal-model-in-r


## import data
df <- read.csv("data/original/irma_predicted-hilo_fp.csv", header = TRUE)
df$datetime <- as.POSIXct(df$datetime)
df$period <- as.numeric(df$period)

## first harmonic only
dflm <- lm(height ~ sin((2*pi)*datetime.num)+cos((2*pi)*datetime.num) +
             sin((4*pi)*datetime.num)+cos((4*pi)*datetime.num) +
             sin((6*pi)*datetime.num)+cos((6*pi)*datetime.num)
             ,data=df)
summary(dflm)

plot(height~datetime.num,ylim=c(-1,10),data=df)
lines(df$datetime.num, dflm$fitted, col=2)



#############################################
#############################################

## package oce
## http://clarkrichards.org/r/oce/tides/contributed/2017/04/14/predicting-tides-in-R/
library(oce)
library(SWMPr)
