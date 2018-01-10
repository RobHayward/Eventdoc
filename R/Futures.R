# The following code draws and prepares the data from the file 
# (Futures.csv) and calculates the descriptive statistics of exchange 
# rates and measures of sentiment.    
# Read spec position data----------------------------------
require(eventstudies)
da <- read.csv("./Data/Futures.csv", header = TRUE)
da$S1 <- (da$NCL-da$NCS)/(da$NCL+da$NCS+da$NCSP)
da$S2 <- (da$NCL-da$NCS)/da$OI
da$S3 <- (da$NCL+da$NCS+da$NCSP)/da$OI
da <- da[,c(2, 3, 5, 19, 20, 21)]
str(da)
head(da)
# Sub set files------------------------------------------
da.CAD <- subset(da[,2:6], subset = da$Exchange == "CAD")
da.CHF <- subset(da[,2:6], subset = da$Exchange == "CHF")
da.EUR <- subset(da[,2:6], subset = da$Exchange == "EUR")
da.GBP <- subset(da[,2:6], subset = da$Exchange == "GBP")
da.JPY <- subset(da[,2:6], subset = da$Exchange == "JPY")
# Calculate descriptive stats---------------------------
mystats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  s <- sd(x) 
  t <- m/s
  med <- median(x)
  n <- length(x)
  skew <- sum((x-m)^3/s^3)/n
  ps <- ((6*n*(n-1))/((n-1)*(n+1)*(n+3)))^0.5
  kurt <- sum((x-m)^4/s^4)/n - 3
  pk <- ((n^2-1)/((n-3)*(n + 5)))^0.5
  max <- max(x)
  min <- min(x)
  return(c(n=n, mean=m * 100, t= t, medium = med *100, stdev=s *100, 
           skew=skew, ps = ps, kurtosis=kurt, pk = pk, max = max *100, min = min * 100))
}
# change currency file for each exchange rates (ie da.CHF etc). 
FX.d <- apply(da.CAD, 2, FUN = mystats, na.omit = TRUE)
FX.d
# Create extreme events and conditions----------------------
# Set the conditions for each run
# currency is CAD, GBP, JPY, EUR or CHF
currency = "CHF"
# window is number of weeks
wind = as.numeric(2)
# extreme is high or low
extreme = "high"
hi = as.numeric(0.95)
lo = as.numeric(0.05)
# cumulative is either whole or after
cum = "whole lot"
# Sentiment index is S1 or S2 or S3
S = "S1"
# read file if not already done-----------------------------
da <- read.csv("./Futures.csv", header = TRUE)
da$Date <- as.Date(da$Date, format = "%d/%m/%Y")
# Create S1 and S2 variables
da$S1 <- (da$NCL-da$NCS)/(da$NCL+da$NCS+da$NCSP)
da$S2 <- (da$NCL-da$NCS)/da$OI
da$S3 <- (da$NCL+da$NCS+da$NCSP)/da$OI
# Subset data
da.CAD <- subset(da, subset = da$Exchange == "CAD")
da.CHF <- subset(da, subset = da$Exchange == "CHF")
da.EUR <- subset(da, subset = da$Exchange == "EUR")
da.GBP <- subset(da, subset = da$Exchange == "GBP")
da.JPY <- subset(da, subset = da$Exchange == "JPY")
# Draw appropriate files
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
# Draw sentiment index
if(S == "S1") {
  FX$S <- FX$S1
} else if(S == "S2"){
  FX$S <- FX$S2
} else if(S == "S3"){
  FX$S <- FX$S3
} else("Error with sentiment index")
# High or low
if(extreme == "high"){
  FX$ex <- as.numeric(FX$S >= quantile(FX$S,  
                                       hi, na.rm = TRUE)) 
} else if(extreme == "low") {
  FX$ex <- as.numeric(FX$S <= quantile(FX$S, 
                                       lo, na.rm = TRUE))
} else(print("No Extreme"))
# Create zoo object
FX.z <- as.zoo(FX, order.by = FX$Date)
# Create the files for the event study package-----------
eventslist.FXex <- data.frame(index(FX.z$ex[FX.z$ex ==1]))
eventslist.FXex$unit <- as.character("r")
colnames(eventslist.FXex) <- c("when", "unit")
a <- phys2eventtime(FX.z, eventslist.FXex, width = wind)
# Set up files exw - whole window, exa - after event
FX.exw <- window(a$z.e, start = wind * -1, end = wind)
FX.exa <- window(a$z.e, start = 0, end = wind)
# Cumulative returns
FX.exwc <- remap.cumsum(FX.exw, is.pc = FALSE, base = 0)
FX.exac <- remap.cumsum(FX.exa, is.pc = FALSE, base = 0)
# Size of event window
dim(eventslist.FXex)
# Create whole and after matrix
FX.exwcm <- as.matrix(FX.exwc)
FX.exacm <- as.matrix(FX.exac)
MRW <- mean(FX.exwcm[(wind*2 + 1),])
MRA <- mean(FX.exacm[(wind + 1),])
# Whole event window
MRW
# Post-event window
MRA
# Boostrap the confidence intervales of event window return estimates-------
FX.exwc.b <- inference.Ecar(FX.exwc)
FX.exwc.b
FX.exac.b <- inference.Ecar(FX.exac)
FX.exac.b
