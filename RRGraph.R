# This will create the graph for paper
# See event(final) for fuller explanation
rm(list = ls())
ls()
require(eventstudies)
require(PerformanceAnalytics)
require(xtable)
################################################################################
par(mfrow = c(3,2))
currency = "AUD"
extreme = "low"
# select percentile
hi = as.numeric(0.95)
lo = as.numeric(0.05)
# select window either "whole lot" or "after".
cum = "whole lot"
wind = as.numeric(16) 
###############################################################################
daz <- read.zoo("C:/Users/Toshiba/Desktop/Event2.csv", header = TRUE, 
  format = "%d-%b-%y", sep = ",")
FX.z <-daz[,c(paste(currency), paste(currency, "RR", sep = ""))]
FX.z$r <- 100*diff(log(FX.z[,1]))
(eventslist.FXex)
##################################################################################
if(extreme == "high"){
FX.z$ex <- as.numeric(FX.z[,2] >= quantile(FX.z[,2],  
         hi, na.rm = TRUE)) 
} else if(extreme == "low") {
FX.z$ex <- as.numeric(FX.z[,2] <= quantile(FX.z[,2], 
         lo, na.rm = TRUE))
} else(print("No Extreme"))
################################################################################
eventslist.FXex <- data.frame(index(FX.z$ex[FX.z$ex ==1]))
eventslist.FXex$unit <- as.character("r")
colnames(eventslist.FXex) <- c("when", "unit")
eventslist.FXex$when <-as.Date(eventslist.FXex$when)
a <- phys2eventtime(FX.z, eventslist.FXex, width = wind)
################################################################################
head(a$z.e)
if(cum == "whole lot"){
FX.ex <- window(a$z.e, start = wind * -1, end = wind)
} else if(cum == "after"){
FX.ex <- window(a$z.e, start = 0, end = wind)
} else(print("Error"))
################################################################################
FX.exc <- remap.cumsum(FX.ex, is.pc = FALSE, base = 0)
###############################################################################
# 
# the bootstrap
dim = seq(-wind, wind, 1)
FX.exc.b <- inference.Ecar(FX.exc)
FX.exc.b
FX.exc.bm <- matrix(FX.exc.b, ncol = 3, dimnames = list(dim, 
             c("2.5%", "Mean", "97.5")))
title3 = title = paste("Cumulative ", currency, " returns", sep = "")
matplot(x = dim, FX.exc.bm[,1:3], type = 'l', lty = c(2, 1, 2), 
      main = title3, ylab = "Returns", xlab = "Event Days",  
      xlim = c(min(dim), max(dim)), ylim = c(min(FX.exc.bm[,1]), 
       max(FX.exc.bm[,3])))
abline(v = 0)
#################################################################################