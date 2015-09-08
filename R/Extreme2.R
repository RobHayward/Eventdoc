require(eventstudies) 
require(PerformanceAnalytics)
require(xtable)
# Read in the data------------------------------------------
daz <- read.zoo("Data/RR.csv", header = TRUE, 
                format = "%d-%b-%y", sep = ",")
summary(daz)
FX.z <- daz[,c("AUDRR", "EURGBPRR", "EURSEKRR", "GBPRR", "CHFRR", "EURCHFRR", 
               "EURJPYRR", "EURRR","CADRR", "JPYRR")]
head(FX.z)
# Create extreme readings and prepare event files-------------------------
currency = "CAD"
extreme = "high"
hi = as.numeric(0.95)
lo = as.numeric(0.05)
# select window either "whole lot" or "after".
cum = "whole lot"
wind = as.numeric(10) 
# Read file (as zoo object) if not already done
daz <- read.zoo("Data/RR.csv", header = TRUE, 
                format = "%d-%b-%y", sep = ",")
FX.z <-daz[,c(paste(currency), paste(currency, "RR", sep = ""))]
FX.z$r <- 100*diff(log(FX.z[,1]))
# THis has been changed from the original so that the dataframe has a high and low
# column to show the high and low extreme. 
exlist <- c("high", "low")
for(ex in exlist){
if(ex == "high"){
  FX.z$exh <- as.numeric(FX.z[,2] >= quantile(FX.z[,2],  
                                             hi, na.rm = TRUE)) 
} else if(ex == "low") {
  FX.z$exl <- as.numeric(FX.z[,2] <= quantile(FX.z[,2], 
                                             lo, na.rm = TRUE))
} else(print("No Extreme"))
}
head(FX.z)


# Create event list-----------------------------------------
eventslist.FXex <- data.frame(index(FX.z$exh[FX.z$exh ==1]))
eventslist.FXex$unit <- as.character("r")
colnames(eventslist.FXex) <- c("when", "unit")
eventslist.FXex$when <-as.Date(eventslist.FXex$when)
# Align returns with extreme events-----------------------
a <- phys2eventtime(FX.z, eventslist.FXex, width = wind)
str(a)
# Create windows (exw - whole, exa - after, cumsum - cumulative sum)
FX.exw <- window(a$z.e, start = wind * -1, end = wind)
FX.exa <- window(a$z.e, start = 0, end = wind)
head(FX.exa)
FX.exwc <- remap.cumsum(FX.exw, is.pc = FALSE, base = 0)
FX.exac <- remap.cumsum(FX.exa, is.pc = FALSE, base = 0)
# window size-----------------------------------------
dims = seq(-wind, wind, 1)
dim(eventslist.FXex)
FX.exwcm <- as.matrix(FX.exwc)
FX.exacm <- as.matrix(FX.exac)
dim(FX.exacm)
hist(FX.exacm[10,], prob = TRUE, breaks = 10, main = "Distribution of extreme")
lines(density(FX.exacm[10,]), col = "red")
MRW <- mean(FX.exwcm[(wind*2 + 1),])
MRA <- mean(FX.exacm[(wind + 1),])
# Mean return whole
MRW
# Mean return after event
MRA
# Apply the boostrap to calcualte confidence intervals---------------
FX.exwc.b <- inference.Ecar(FX.exwc, to.plot = TRUE)
FX.exwc.b
FX.exac.b <- inference.Ecar(FX.exac)
FX.exac.b
# create charts for comparison of extreme and regular returns. 
FX.exac # This is the list of returns. 
hist(FX.exacm[6,], breaks = 10, prob = TRUE)
mean(FX.exacm[6,], na.rm = TRUE)
sd(FX.exacm[6,])
mean(FX.exacm[6,])/sd(FX.exacm[6,])
par(mfrow = c(2,1))
plot(dist(FX.exacm[2,]))
# need to sample 
plot(dist(FX.z[,1]))
