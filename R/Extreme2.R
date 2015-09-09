# Load required packages
require(eventstudies) 
require(PerformanceAnalytics)
require(xtable)
# Load data-------------------------
daz <- read.zoo("Data/RR.csv", header = TRUE, 
                format = "%d-%b-%y", sep = ",")
# Not now needed, Part of function
# currency = "EUR"
# extreme = "high"
# Currencydf is a dataframe to store currency, returns and extremes
currencydf <- function(currency){
hi = as.numeric(0.95)
lo = as.numeric(0.05)
# are these needed?
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
return(FX.z)
}
EUR <- currencydf("EUR")
head(EUR)
# Create zoo object for events----------------------------------------
# Events must be stored in particular format.
# Requires currencydf zoo object, wind for size of window and hilo for high or low extreme.
eventzoo <- function(currencydf, wind, hilo){
  if(hilo == "hi"){
    ho = 4
  } else if(hilo == "low"){
    ho = 5
  } else {
    print("No Extreme")
  }
eventslist.FXex <- data.frame(index(currencydf[,ho][currencydf[,ho] ==1]))
eventslist.FXex$unit <- as.character("r")
colnames(eventslist.FXex) <- c("when", "unit")
eventslist.FXex$when <-as.Date(eventslist.FXex$when)
# Align returns with extreme events-----------------------
a <- phys2eventtime(FX.z, eventslist.FXex, width = wind)
# Create windows (exw - whole, exa - after, cumsum - cumulative sum)
FX.exw <- window(a$z.e, start = wind * -1, end = wind)
FX.exa <- window(a$z.e, start = 0, end = wind)
FX.exwc <- remap.cumsum(FX.exw, is.pc = TRUE, base = 0)
FX.exac <- remap.cumsum(FX.exa, is.pc = TRUE, base = 0)
g <- list(FX.exw = FX.exw, FX.exwc = FX.exwc, FX.exac = FX.exac, hilo = hilo)
return(g)
}
t <- eventzoo(currencydf = EUR, wind = 2, hilo = "low")
hilo = "low"
head(currencydf$exl)
str(t)
currencydf = EUR
head(currencydf)
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
FX.exac.b <- inference.Ecar(FX.exac, to.plot = TRUE)
FX.exac.b
# create charts for comparison of extreme and regular returns. 
 
par(mfrow = c(2,1))
plot(density(100*FX.exacm[2,]), xlim = c(-2.0, 2.0), main = "Extreme Density",
     lwd = 2, col = "red")
abline(v = mean(FX.exacm[2,1]))
text(x = .25, y = 0.4, labels = "Mean")
# need to sample 
plot(density(sample(FX.z[-1,3], 113), na.rm = TRUE), xlim = c(-2.0, 2.0), 
     main = "Regular Density", lwd = 2, col = "red")
abline(v = mean(FX.z[-1, 3], na.rm = TRUE))
text(x = .22, y = 0.4, labels = "Mean")
# Mean and standard deviation of return day after extreme
me <- round(mean(FX.exacm[1,], na.rm = TRUE),4)
sde <- round(sd(FX.exacm[11,]), 4)
tex <- round(mean(FX.exacm[11,])/sd(FX.exacm[11,]),4)
# Mean and standard deviation of return (full sample)
mr <- round(mean(FX.z[-1,3], na.rm = TRUE),4)
sdr <- round(sd(FX.z[-1,3], na.rm = TRUE), 4)
tr <- round(mean(FX.z[-1,3], na.rm = TRUE)/sd(FX.z[-1,3], na.rm = TRUE),4)
df <- data.frame(ex = c(me, sde, tex), r = c(mr, sdr, tr))
rownames(df) <- c("Mean", "SD", "T-stat")
df

