require(eventstudies) 
require(PerformanceAnalytics)
require(xtable)
# Read in the data------------------------------------------
daz <- read.zoo("./RR.csv", header = TRUE, 
                format = "%d-%b-%y", sep = ",")
summary(daz)
FX.z <- daz[,c("AUDRR", "EURGBPRR", "EURSEKRR", "GBPRR", "CHFRR", "EURCHFRR", 
               "EURJPYRR", "EURRR","CADRR", "JPYRR")]
head(FX.z)
# Descriptive statistics of RR-------------------------------
mystats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  md <- median(x)
  s <- sd(x) 
  t <- m/s
  n <- length(x)
  skew <- sum((x-m)^3/s^3)/n
  ps <- ((6*n*(n-1))/((n-1)*(n+1)*(n+3)))^0.5
  kurt <- sum((x-m)^4/s^4)/n - 3
  pk <- ((n^2-1)/((n-3)*(n + 5)))^0.5
  max <- max(x)
  min <- min(x)
  return(c(n=n, mean=m, t= t, median = md, stdev=s, skew=skew, ses = 2 * ps, 
           kurtosis=kurt, sek  = 2 * pk, max = max, min = min))
}
FX.d <- apply(FX.z, 2, FUN = mystats, na.omit = TRUE)
FX.d
# This generates la tex code for table 
print(xtable(FX.d))
# Create extreme readings and prepare event files-------------------------
currency = "EURGBP"
extreme = "high"
hi = as.numeric(0.95)
lo = as.numeric(0.05)
# select window either "whole lot" or "after".
cum = "whole lot"
wind = as.numeric(1) 
# Read file (as zoo object) if not already done
daz <- read.zoo("./RR.csv", header = TRUE, 
                format = "%d-%b-%y", sep = ",")
FX.z <-daz[,c(paste(currency), paste(currency, "RR", sep = ""))]
FX.z$r <- 100*diff(log(FX.z[,1]))
if(extreme == "high"){
  FX.z$ex <- as.numeric(FX.z[,2] >= quantile(FX.z[,2],  
                                             hi, na.rm = TRUE)) 
} else if(extreme == "low") {
  FX.z$ex <- as.numeric(FX.z[,2] <= quantile(FX.z[,2], 
                                             lo, na.rm = TRUE))
} else(print("No Extreme"))
# Create event list-----------------------------------------
eventslist.FXex <- data.frame(index(FX.z$ex[FX.z$ex ==1]))
eventslist.FXex$unit <- as.character("r")
colnames(eventslist.FXex) <- c("when", "unit")
eventslist.FXex$when <-as.Date(eventslist.FXex$when)
# Align returns with extreme events-----------------------
a <- phys2eventtime(FX.z, eventslist.FXex, width = wind)
# Create windows (exw - whole, exa - after, cumsum - cumulative sum)
FX.exw <- window(a$z.e, start = wind * -1, end = wind)
FX.exa <- window(a$z.e, start = 0, end = wind)
FX.exwc <- remap.cumsum(FX.exw, is.pc = FALSE, base = 0)
FX.exac <- remap.cumsum(FX.exa, is.pc = FALSE, base = 0)
# window size-----------------------------------------
dims = seq(-wind, wind, 1)
dim(eventslist.FXex)
FX.exwcm <- as.matrix(FX.exwc)
FX.exacm <- as.matrix(FX.exac)
MRW <- mean(FX.exwcm[(wind*2 + 1),])
MRA <- mean(FX.exacm[(wind + 1),])
# Mean return whole
MRW
# Mean return after event
MRA
# Apply the boostrap to calcualte confidence intervals---------------
FX.exwc.b <- inference.Ecar(FX.exwc)
FX.exwc.b
FX.exac.b <- inference.Ecar(FX.exac)
FX.exac.b
