# This is the script to create graphs of the futues event window 
rm(list = ls())
ls()
require(eventstudies)
require(PerformanceAnalytics)
##################################################################################
# Set conditions
par(mfrow = c(2,2))
# currency is CAD, GBP, JPY, EUR or CHF
currency = "CAD"
# window is number of weeks
wind = as.numeric(6)
# extreme is high or low
extreme = "high"
hi = as.numeric(0.90)
lo = as.numeric(0.10)
# cumulative is either whole or after
cum = "whole lot"
# Sentiment index is S1 or S2 or S3
S = "S1"
##############################################################################
da <- read.csv("C:/Users/Toshiba/Desktop/Futures.csv", header = TRUE)
da$Date <- as.Date(da$Date, format = "%d/%m/%Y")
da$S1 <- (da$NCL-da$NCS)/(da$NCL+da$NCS+da$NCSP)
da$S2 <- (da$NCL-da$NCS)/da$OI
da$S3 <- (da$NCL+da$NCS+da$NCSP)/da$OI
################################################################################
da.CAD <- subset(da, subset = da$Exchange == "CAD")
da.CHF <- subset(da, subset = da$Exchange == "CHF")
da.EUR <- subset(da, subset = da$Exchange == "EUR")
da.GBP <- subset(da, subset = da$Exchange == "GBP")
da.JPY <- subset(da, subset = da$Exchange == "JPY")
###############################################################################
if(currency == "CAD"){
FX = da.CAD
} else if(currency == "CHF"){
FX = da.CHF
} else if(currency == "EUR"){
FX = da.EUR
} else if(currency == "GBP"){
FX = da.GBP
} else if(currency == "JPY"){
FX = da.JPY
} else("Error with exchange rate")
#############################################################################
 if(S == "S1") {
FX$S <- FX$S1
} else if(S == "S2"){
FX$S <- FX$S2
} else if(S == "S3"){
FX$S <- FX$S3
} else("Error with sentiment index")
##############################################################################
if(extreme == "high"){
FX$ex <- as.numeric(FX$S >= quantile(FX$S,  
         hi, na.rm = TRUE)) 
} else if(extreme == "low") {
FX$ex <- as.numeric(FX$S <= quantile(FX$S, 
         lo, na.rm = TRUE))
} else(print("No Extreme"))
#############################################################################
# Create zoo object
FX.z <- as.zoo(FX, order.by = FX$Date)
#############################################################################
eventslist.FXex <- data.frame(index(FX.z$ex[FX.z$ex ==1]))
eventslist.FXex$unit <- as.character("r")
colnames(eventslist.FXex) <- c("when", "unit")
a <- phys2eventtime(FX.z, eventslist.FXex, width = wind)
############################################################################
# Go back to stripped 2 for just one file.
FX.exw <- window(a$z.e, start = wind * -1, end = wind)
FX.exa <- window(a$z.e, start = 0, end = wind)
FX.exwc <- remap.cumsum(FX.exw, is.pc = FALSE, base = 0)
FX.exac <- remap.cumsum(FX.exa, is.pc = FALSE, base = 0)
############################################################################
FX.excm <- as.matrix(FX.exwc)
if(cum == "whole lot"){
dim = seq(-wind, wind, 1)
} else if(cum == "after"){
dim = seq(0, wind, 1)
} else(print("Error"))
FX.exc.des <-matrix(nrow = length(dim), ncol = 5, dimnames = list(dim,
    c("mean", "sd", "median", "skew", "kurt")))
for(i in 1:(length(dim))){
FX.exc.des[i,2] <- sd(FX.excm[i,])
}
###################################################################################
#title2 = title = paste("Standard Deviation of ", currency, sep = "")
#matplot(x = dim, FX.exc.des[,2], type = 'l', main = title2, 
#    xlim = c(min(dim), max(dim)), ylab = "Standard Deviation", 
#    xlab = "Event Window")
#abline(v = 0)
###################################################################################
# the bootstrap
FX.exc.b <- inference.Ecar(FX.exwc)
FX.exc.b
FX.exc.bm <- matrix(FX.exc.b, ncol = 3, dimnames = list(dim, 
             c("2.5%", "Mean", "97.5")))
title3 = title = paste("Cumulative ", currency, " returns", sep = "")
matplot(x = dim, FX.exc.bm[,1:3], type = 'l', lty = c(2, 1, 2), 
      main = title3, ylab = "Returns", xlab = "Event Weeks",  
      xlim = c(min(dim), max(dim)), ylim = c(min(FX.exc.bm[,1]), 
       max(FX.exc.bm[,3])))
abline(v = 0)


################################################################################	