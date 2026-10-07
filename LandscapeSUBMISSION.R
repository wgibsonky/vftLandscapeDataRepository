#libraries and set wd----
setwd("C:/Users/wgibs/OneDrive/Desktop/VFTs/R/VFT-Detection/LandscapeNEW/")

library(terra)
library(sf)
library(sp)
library(dplyr)
library(stringr)
library(AICcmodavg)
library(ggeffects)
library(ggplot2)
library(dismo)
library(broom)
library(purrr)

#bring in data----
df = read.csv("vftdataframe.csv")

####remove NAs----
df$moist[is.na(df$moist)] = mean(df$moist, na.rm=T)
df$NDVI[is.na(df$NDVI)] = mean(df$NDVI, na.rm=T)
df$clay[is.na(df$clay)] = mean(df$clay, na.rm=T)
df$silt[is.na(df$silt)] = mean(df$silt, na.rm=T)
df$sand[is.na(df$sand)] = mean(df$sand, na.rm=T)

#say if points are vfts or randomly generated (1 = vft)
df = dplyr::mutate(df, pttype = ifelse((ptvalue == "1"), "1", "0"))
df$pttype = as.numeric(df$pttype)

####scaling variables----
df2 = as.data.frame(scale(df)) #scale everything
#re-insert columns that weren't supposed to be scaled
df2$point = df$point
df2$ptvalue = df$ptvalue
df2$pttype = df$pttype

####correlations----
cors = as.data.frame(cor(df2)) #get correlation matrix
cors[cors < 0.6] = NA #only silt and clay are correlated

#determining structure----
##elev----
glm.elev = glm(pttype~elev, data = df2, family = "binomial")
summary(glm.elev)
glm.elev2 = glm(pttype~elev + I(elev^2), data = df2, family = "binomial")
summary(glm.elev2)

elevlist = list("glm.elev" = glm.elev, "glm.elev2" = glm.elev2)
aictab(elevlist) #elev2

##slope----
glm.slope = glm(pttype~slope , data = df2, family = "binomial")
summary(glm.slope)
glm.slope2 = glm(pttype~slope + I(slope^2), data = df2, family = "binomial")
summary(glm.slope2)

slopelist = list("glm.slope" = glm.slope, "glm.slope2" = glm.slope2)
aictab(slopelist) #slope

##decid----
glm.DTNdecid = glm(pttype~DTNdecid , data = df2, family = "binomial")
summary(glm.DTNdecid)
glm.DTNdecid2 = glm(pttype~DTNdecid + I(DTNdecid^2), data = df2, family = "binomial")
summary(glm.DTNdecid2)

DTNdecidlist = list("glm.DTNdecid" = glm.DTNdecid, "glm.DTNdecid2" = glm.DTNdecid2)
aictab(DTNdecidlist) #DTNdecid2; competing

##matpine----
glm.DTNmatpine = glm(pttype~DTNmatpine , data = df2, family = "binomial")
summary(glm.DTNmatpine)
glm.DTNmatpine2 = glm(pttype~DTNmatpine + I(DTNmatpine^2), data = df2, family = "binomial")
summary(glm.DTNmatpine2)

DTNmatpinelist = list("glm.DTNmatpine" = glm.DTNmatpine, "glm.DTNmatpine2" = glm.DTNmatpine2)
aictab(DTNmatpinelist) #DTNmatpine; competing

##grass----
glm.DTNgrass = glm(pttype~DTNgrass , data = df2, family = "binomial")
summary(glm.DTNgrass)
glm.DTNgrass2 = glm(pttype~DTNgrass + I(DTNgrass^2), data = df2, family = "binomial")
summary(glm.DTNgrass2)

DTNgrasslist = list("glm.DTNgrass" = glm.DTNgrass, "glm.DTNgrass2" = glm.DTNgrass2)
aictab(DTNgrasslist) #DTNgrass; competing

##NDVI----
glm.NDVI = glm(pttype~NDVI , data = df2, family = "binomial")
summary(glm.NDVI)
glm.NDVI2 = glm(pttype~NDVI + I(NDVI^2), data = df2, family = "binomial")
summary(glm.NDVI2)

NDVIlist = list("glm.NDVI" = glm.NDVI, "glm.NDVI2" = glm.NDVI2)
aictab(NDVIlist) #NDVI2

##soil moisture----
glm.moist = glm(pttype~moist , data = df2, family = "binomial")
summary(glm.moist)
glm.moist2 = glm(pttype~moist + I(moist^2), data = df2, family = "binomial")
summary(glm.moist2)

moistlist = list("glm.moist" = glm.moist, "glm.moist2" = glm.moist2)
aictab(moistlist) #moist2

## soil silt----
glm.silt = glm(pttype~silt , data = df2, family = "binomial")
summary(glm.silt)
glm.silt2 = glm(pttype~silt + I(silt^2), data = df2, family = "binomial")
summary(glm.silt2)

siltlist = list("glm.silt" = glm.silt, "glm.silt2" = glm.silt2)
aictab(siltlist) #silt; competing


#apriori modeling----
#null
glm.null = glm(pttype~1, data = df2, family = "binomial")

##biotic----
#landscape heterogeneity (lh)
glm.lh = glm(pttype~DTNmatpine + I(DTNmatpine^2) + DTNgrass + I(DTNgrass^2) + DTNdecid + I(DTNdecid^2), data = df2, family = "binomial")

#openness (o)
glm.o = glm(pttype~DTNgrass + I(DTNgrass^2), data = df2, family = "binomial")

#deciduous wetlands (dw)
glm.dw = glm(pttype~DTNdecid + I(DTNdecid^2), data = df2, family = "binomial")

#longleaf pine association (lpa)
glm.lpa = glm(pttype~DTNmatpine + I(DTNmatpine^2), data = df2, family = "binomial")

#wetlands within pine (wwp)
glm.wwp = glm(pttype~DTNmatpine + I(DTNmatpine^2) + DTNdecid + I(DTNdecid^2), data = df2, family = "binomial")

#productivity and landscape heterogeneity (plh) - top model; no competing
glm.plh = glm(pttype~DTNmatpine + I(DTNmatpine^2) + DTNgrass + I(DTNgrass^2) + DTNdecid + I(DTNdecid^2) + NDVI + I(NDVI^2), data = df2, family = "binomial")

#productivity and openness (po)
glm.po = glm(pttype~DTNgrass + I(DTNgrass^2) + NDVI + I(NDVI^2), data = df2, family = "binomial")

#productivity and deciduous wetlands (pdw)
glm.pdw = glm(pttype~DTNdecid + I(DTNdecid^2) + NDVI + I(NDVI^2), data = df2, family = "binomial")

#productivity and longleaf pine association (plpa)
glm.plpa = glm(pttype~DTNmatpine + I(DTNmatpine^2) + NDVI + I(NDVI^2), data = df2, family = "binomial")

#productivity and wetlands about pine (pwwp)
glm.pwwp = glm(pttype~DTNmatpine + I(DTNmatpine^2) + DTNdecid + I(DTNdecid^2) + NDVI + I(NDVI^2), data = df2, family = "binomial")

###ranking----
bioticlist = list("glm.null" = glm.null, "glm.lh" = glm.lh, "glm.o" = glm.o, "glm.dw" = glm.dw,
                  "glm.lpa" = glm.lpa, "glm.wwp" = glm.wwp, "glm.plh" = glm.plh, "glm.po" = glm.po,
                  "glm.pdw" = glm.pdw, "glm.plpa" = glm.plpa, "glm.pwwp" = glm.pwwp)

aictab(bioticlist)

####making bio models into tables----
results = map2_dfr(
  bioticlist,
  names(bioticlist),
  ~ tidy(.x) %>%
    mutate(model = .y)
)

write.csv(results, "C:/Users/wgibs/OneDrive/Desktop/VFTs/R/VFT-Detection/LandscapeNEW/biomodels.csv")


##abiotic----
#soil characteristics (sc)
glm.sc = glm(pttype~moist + I(moist^2) + silt, data = df2, family = "binomial")

#ruggedness (r)
glm.r = glm(pttype~elev + I(elev^2) + slope, data = df2, family = "binomial")

#topographic (t)
glm.t = glm(pttype~elev + I(elev^2), data = df2, family = "binomial")

#soil stability (ss)
glm.ss = glm(pttype~slope, data = df2, family = "binomial")

#ruggedness and soil characteristics (rsc) - dAIC = 0.00
glm.rsc = glm(pttype~elev + I(elev^2) + slope + moist + I(moist^2) + silt, data = df2, family = "binomial")

#topographic and soil characteristics (tsc) - dAIC = 0.04
glm.tsc = glm(pttype~elev + I(elev^2) + moist + I(moist^2) + silt, data = df2, family = "binomial")

#soil stability and soil characteristics (sssc)
glm.sssc = glm(pttype~slope + moist + I(moist^2) + silt, data = df2, family = "binomial")

###ranking----
abioticlist = list("glm.sc" = glm.sc, "glm.r" = glm.r, "glm.t" = glm.t,
                   "glm.ss" = glm.ss, "glm.rsc" = glm.rsc, "glm.tsc" = glm.tsc,
                   "glm.sssc" = glm.sssc,"glm.null"= glm.null)

aictab(abioticlist)

####making abio models into tables----
results = map2_dfr(
  abioticlist,
  names(abioticlist),
  ~ tidy(.x) %>%
    mutate(model = .y)
)

write.csv(results, "C:/Users/wgibs/OneDrive/Desktop/VFTs/R/VFT-Detection/LandscapeNEW/abiomodels.csv")

##combining top of each----
#producing two combined models, since we had competing abiotic models

glm.combo1 = glm(pttype~DTNmatpine + I(DTNmatpine^2) + DTNgrass + I(DTNgrass^2) + DTNdecid + I(DTNdecid^2) + NDVI + I(NDVI^2) + elev + I(elev^2) + slope + moist + I(moist^2) + silt, data = df2, family = "binomial")

glm.combo2 = glm(pttype~DTNmatpine + I(DTNmatpine^2) + DTNgrass + I(DTNgrass^2) + DTNdecid + I(DTNdecid^2) + NDVI + I(NDVI^2) + elev + I(elev^2) + moist + I(moist^2) + silt, data = df2, family = "binomial") #top model; no competing

combolist = list("glm.null"= glm.null, "glm.combo1" = glm.combo1, "glm.combo2" = glm.combo2)

aictab(combolist) #combo2 is top ranked

summary(glm.combo2) #confidence intervals for matpine2, grass2, moist, moist2, and silt overlap 0 so are removed

####making combo models into tables----
results = map2_dfr(
  combolist,
  names(combolist),
  ~ tidy(.x) %>%
    mutate(model = .y)
)

write.csv(results, "C:/Users/wgibs/OneDrive/Desktop/VFTs/R/VFT-Detection/LandscapeNEW/combomodels.csv")


##topmod!----
#removing uninformative covs into a top model

glm.combo3 = glm(pttype~DTNmatpine + DTNgrass + DTNdecid + I(DTNdecid^2) + NDVI + I(NDVI^2) + elev + I(elev^2), data = df2, family = "binomial") #top model; no competing

topmod = glm.combo3

#performance metrics----
dataforROC <- df2
dataforROC$sub1 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75)
nrow(subset(dataforROC, sub1 == 1))/nrow(dataforROC) # did it work? Yes

# now let's make em for the rest of the 10 subsets
dataforROC$sub2 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75)
dataforROC$sub3 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75)
dataforROC$sub4 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75)
dataforROC$sub5 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75)
dataforROC$sub6 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75)
dataforROC$sub7 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75)
dataforROC$sub8 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75)
dataforROC$sub9 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75)
dataforROC$sub10 = rbinom(n = nrow(dataforROC), size = 1, prob = 0.75)


# make blank dataset to hold results
AUCTable <- data.frame("DataSubset" = 0, "AUC" = 0, "BrierScore" = 0)

for(i in 1:10){
  #i = 1
  datasubset <- subset(dataforROC, dataforROC[,18+i] == 1) # first subset is col 75
  
  # predict and arrange data for AUC
  testsubset <- subset(dataforROC, dataforROC[,18+i] == 0)
  pred_1 <- predict(topmod, type = "response", newdata = testsubset)
  testsubset$predicted <- pred_1
  DataforAUC <- data.frame("naive" = testsubset$pttype, "predicted" = testsubset$predicted)
  head(DataforAUC)
  
  # calculate AUC
  p <- subset(DataforAUC, naive == 1)
  p <- p$predicted
  a <- subset(DataforAUC, naive == 0)
  a <- a$predicted
  e <- dismo::evaluate(p, a)
  aucvalue = e@auc
  
  # brier score
  brierval <- sum((DataforAUC$predicted - DataforAUC$naive)^2) / nrow(DataforAUC)
  
  # add this data to the table
  newrow <- c(i, round(aucvalue,2), round(brierval,2))
  AUCTable <- rbind(AUCTable, newrow)
}

AUCTable <- AUCTable[2:11,]
AUCTable
mean(AUCTable$BrierScore) #0.106
mean(AUCTable$AUC) #0.864

#creating plots----
par(mfrow = c(3,2))

##topmod unscaled----
topmodunscaled = glm(pttype~DTNmatpine + DTNgrass + DTNdecid + I(DTNdecid^2) + NDVI + I(NDVI^2) + elev + I(elev^2), data = df, family = "binomial")

##elevation----
newdat <- data.frame(Perp_Dist = seq(0,20,length.out=100),
                     DTNdecid = rep(median(df$DTNdecid), 100),
                     DTNgrass = rep(median(df$DTNgrass), 100),
                     DTNmatpine = rep(median(df$DTNmatpine), 100),
                     elev = seq(min(df$elev), max(df$elev), length.out = 100),
                     NDVI = rep(median(df$NDVI), 100))

pred1 <- predict(topmodunscaled, newdat, type="link", se.fit=TRUE) #type link gives logit scale, response gives actual probability

critval <- 1.96 ## approx 95% CI
pred1$upr <- pred1$fit + (critval * pred1$se.fit)
pred1$lwr <- pred1$fit - (critval * pred1$se.fit)
fit <- pred1$fit

## transform
mod <- topmodunscaled
fit2 <- mod$family$linkinv(fit)
upr2 <- mod$family$linkinv(pred1$upr)
lwr2 <- mod$family$linkinv(pred1$lwr)

pred1<-cbind(newdat, fit2, upr2, lwr2)

plot(-10,-10, xlim=c(0,60), ylim=c(0,1), xlab="Elevation (m above sea level)",
     ylab="VFT Occurrence Probability")
lines(pred1$elev, pred1$fit2, lty=1, col="black", lwd = 5)
lines(pred1$elev, pred1$lwr2, lty=2, col="black", lwd = 5)
lines(pred1$elev, pred1$upr2, lty=2, col="black", lwd = 5)



##NDVI----
newdat <- data.frame(Perp_Dist = seq(0,20,length.out=100),
                     DTNdecid = rep(median(df$DTNdecid), 100),
                     DTNgrass = rep(median(df$DTNgrass), 100),
                     DTNmatpine = rep(median(df$DTNmatpine), 100),
                     DTNwater = rep(median(df$DTNwater), 100),
                     elev = rep(median(df$elev), 100),
                     moist = rep(median(df$moist), 100),
                     NDVI = seq(min(df$NDVI), max(df$NDVI), length.out = 100),
                     silt = rep(median(df$silt), 100))

pred1 <- predict(topmodunscaled, newdat, type="link", se.fit=TRUE) #type link gives logit scale, response gives actual probability

critval <- 1.96 ## approx 95% CI
pred1$upr <- pred1$fit + (critval * pred1$se.fit)
pred1$lwr <- pred1$fit - (critval * pred1$se.fit)
fit <- pred1$fit

## transform
mod <- topmodunscaled
fit2 <- mod$family$linkinv(fit)
upr2 <- mod$family$linkinv(pred1$upr)
lwr2 <- mod$family$linkinv(pred1$lwr)

pred1<-cbind(newdat, fit2, upr2, lwr2)

plot(-10,-10, xlim=c(0,0.8), ylim=c(0,1), xlab="NDVI",
     ylab="VFT Occurrence Probability")
lines(pred1$NDVI, pred1$fit2, lty=1, col="black", lwd = 5)
lines(pred1$NDVI, pred1$lwr2, lty=2, col="black", lwd = 5)
lines(pred1$NDVI, pred1$upr2, lty=2, col="black", lwd = 5)

##DTNgrass----
newdat <- data.frame(Perp_Dist = seq(0,20,length.out=100),
                     DTNdecid = rep(median(df$DTNdecid), 100),
                     DTNgrass = seq(min(df$DTNgrass), max(df$DTNgrass), length.out = 100),
                     DTNmatpine = rep(median(df$DTNmatpine), 100),
                     DTNwater = rep(median(df$DTNwater), 100),
                     elev = rep(median(df$elev), 100),
                     moist = rep(median(df$moist), 100),
                     NDVI = rep(median(df$NDVI), 100),
                     silt = rep(median(df$silt), 100))

pred1 <- predict(topmodunscaled, newdat, type="link", se.fit=TRUE) #type link gives logit scale, response gives actual probability

critval <- 1.96 ## approx 95% CI
pred1$upr <- pred1$fit + (critval * pred1$se.fit)
pred1$lwr <- pred1$fit - (critval * pred1$se.fit)
fit <- pred1$fit

## transform
mod <- topmodunscaled
fit2 <- mod$family$linkinv(fit)
upr2 <- mod$family$linkinv(pred1$upr)
lwr2 <- mod$family$linkinv(pred1$lwr)

pred1<-cbind(newdat, fit2, upr2, lwr2)

plot(-10,-10, xlim=c(0,1200), ylim=c(0,1), xlab="Distance from Nearest Grassy Cover (m)",
     ylab="VFT Occurrence Probability")
lines(pred1$DTNgrass, pred1$fit2, lty=1, col="black", lwd = 5)
lines(pred1$DTNgrass, pred1$lwr2, lty=2, col="black", lwd = 5)
lines(pred1$DTNgrass, pred1$upr2, lty=2, col="black", lwd = 5)


##matpine----
newdat <- data.frame(Perp_Dist = seq(0,20,length.out=100),
                     DTNdecid = rep(median(df$DTNdecid), 100),
                     DTNgrass = rep(median(df$DTNgrass), 100),
                     DTNmatpine = seq(min(df$DTNmatpine), max(df$DTNmatpine), length.out = 100),
                     DTNwater = rep(median(df$DTNwater), 100),
                     elev = rep(median(df$elev), 100),
                     moist = rep(median(df$moist), 100),
                     NDVI = rep(median(df$NDVI), 100),
                     silt = rep(median(df$silt), 100))

pred1 <- predict(topmodunscaled, newdat, type="link", se.fit=TRUE) #type link gives logit scale, response gives actual probability

critval <- 1.96 ## approx 95% CI
pred1$upr <- pred1$fit + (critval * pred1$se.fit)
pred1$lwr <- pred1$fit - (critval * pred1$se.fit)
fit <- pred1$fit

## transform
mod <- topmodunscaled
fit2 <- mod$family$linkinv(fit)
upr2 <- mod$family$linkinv(pred1$upr)
lwr2 <- mod$family$linkinv(pred1$lwr)

pred1<-cbind(newdat, fit2, upr2, lwr2)

plot(-10,-10, xlim=c(0,120), ylim=c(0,1), xlab="Distance from Nearest Mature Pine Cover (m)",
     ylab="VFT Occurrence Probability")
lines(pred1$DTNmatpine, pred1$fit2, lty=1, col="black", lwd = 5)
lines(pred1$DTNmatpine, pred1$lwr2, lty=2, col="black", lwd = 5)
lines(pred1$DTNmatpine, pred1$upr2, lty=2, col="black", lwd = 5)

##decid----
newdat <- data.frame(Perp_Dist = seq(0,20,length.out=100),
                     DTNdecid = seq(min(df$DTNdecid), max(df$DTNdecid), length.out = 100),
                     DTNgrass = rep(median(df$DTNgrass), 100),
                     DTNmatpine = rep(median(df$DTNmatpine), 100),
                     DTNwater = rep(median(df$DTNwater), 100),
                     elev = rep(median(df$elev), 100),
                     moist = rep(median(df$moist), 100),
                     NDVI = rep(median(df$NDVI), 100),
                     silt = rep(median(df$silt), 100))

pred1 <- predict(topmodunscaled, newdat, type="link", se.fit=TRUE) #type link gives logit scale, response gives actual probability

critval <- 1.96 ## approx 95% CI
pred1$upr <- pred1$fit + (critval * pred1$se.fit)
pred1$lwr <- pred1$fit - (critval * pred1$se.fit)
fit <- pred1$fit

## transform
mod <- topmodunscaled
fit2 <- mod$family$linkinv(fit)
upr2 <- mod$family$linkinv(pred1$upr)
lwr2 <- mod$family$linkinv(pred1$lwr)

pred1<-cbind(newdat, fit2, upr2, lwr2)

plot(-10,-10, xlim=c(0,200), ylim=c(0,1), xlab="Distance from Nearest Deciduous Wetland (m)",
     ylab="VFT Occurrence Probability")
lines(pred1$DTNdecid, pred1$fit2, lty=1, col="black", lwd = 5)
lines(pred1$DTNdecid, pred1$lwr2, lty=2, col="black", lwd = 5)
lines(pred1$DTNdecid, pred1$upr2, lty=2, col="black", lwd = 5)

