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
# Requires currencydf zoo object, wind for size of window and hilo 
#for high or low extreme.
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
a <- phys2eventtime(currencydf, eventslist.FXex, width = wind)
# Create windows (exw - whole, exa - after, cumsum - cumulative sum)
FX.exw <- window(a$z.e, start = wind * -1, end = wind)
FX.exa <- window(a$z.e, start = 0, end = wind)
FX.exwc <- remap.cumsum(FX.exw, is.pc = TRUE, base = 0)
FX.exac <- remap.cumsum(FX.exa, is.pc = TRUE, base = 0)
dims = seq(-wind, wind, 1)
FX.exwcm <- as.matrix(FX.exwc)
FX.exacm <- as.matrix(FX.exac)
MRW <- mean(FX.exwcm[(wind*2 + 1),])
MRA <- mean(FX.exacm[(wind + 1),])
FX.exwc.b <- inference.Ecar(FX.exwc, to.plot = FALSE)
FX.exac.b <- inference.Ecar(FX.exac, to.plot = FALSE)
g <- list(MRW = MRW, MRA = MRA, hilo = hilo, wind = wind, currdf = FX.exacm)
return(g)
}
EUR2hi <- eventzoo(currencydf = EUR, wind = 2, hilo = "hi")
str(EUR2hi)
 # create charts for comparison of extreme and regular returns. 
twochart <- function(currencydf, currdf){ 
par(mfrow = c(2,1))
plot(density(100*currdf$currdf[2,]), xlim = c(-2.0, 2.0), main = "Extreme Density",
     lwd = 2, col = "red")
abline(v = mean(currdf$currdf[2,], na.rm = TRUE))
text(x = mean(currdf$currdf[2, ], na.rm = TRUE) + 0.2, y = 0.2, labels = "Mean")
# need to sample 
plot(density(sample(currencydf[-1,3], 113), na.rm = TRUE), xlim = c(-2.0, 2.0), 
     main = "Regular Density", lwd = 2, col = "red")
abline(v = mean(currencydf[-1, 3], na.rm = TRUE))
text(x = mean(currencydf[-1,3], na.rm = TRUE) + 0.2, y = 0.4, labels = "Mean")
}
twochart(EUR, EUR2hi)
# Compare next day return for extreme and regular
# THis requires a currencydf and currdf
comparedist <- function(currencydf, currdf){
# Mean and standard deviation of return day after extreme
me <- round(mean(currdf$currdf[2,], na.rm = TRUE),4)
sde <- round(sd(currdf$currdf[2,]), 4)
tex <- round(mean(currdf$currdf[2,])/sd(currdf$currdf[2,]),4)
# Mean and standard deviation of return (full sample)
mr <- round(mean(currencydf[-1, 3], na.rm = TRUE),4)
sdr <- round(sd(currencydf[-1, 3], na.rm = TRUE), 4)
tr <- round(mean(currencydf[-1, 3], na.rm = TRUE)/
              sd(currencydf[-1, 3], na.rm = TRUE),4)
df <- data.frame(ex = c(me, sde, tex), r = c(mr, sdr, tr))
rownames(df) <- c("Mean", "SD", "T-stat")
return(df)  
}
comparedist(EUR, EUR2hi)

