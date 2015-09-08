# measure distributions
# http://www.r-bloggers.com/
# practical-kullback-leibler-kl-divergence-discrete-case/
require(eventstudies) 
require(PerformanceAnalytics)
require(xtable)
require(dplyr)
# Read in the data------------------------------------------
daz <- read.zoo("Data/RR.csv", header = TRUE, 
                format = "%d-%b-%y", sep = ",")
das <- read.csv("Data/RR.csv", header = TRUE)
das[,1] <- as.Date(das[,1], format = "%d-%b-%y")
head(daz)
str(das)
#------------------------------------------------------------------
# use dplyr
riskrev <- paste(currency, "RR", sep = "")
da <- select(das, Date, EUR, EURRR) %>% 
  mutate(EUR.R = log(EUR) - log(lag(EUR))) %>% 
  head()
da$exh <- 0
da$exh[,da$EURRR >= quantile(da$EURRR, 0.95)]
quantile(da$EURRR, 0.05)
da$EURRR >= quantile(da$EURRR, 0.95)
head(da)
currencylist <- colnames(daz)[c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)]
currencylist
highlow = c("high","low")
hist(da$EURRR)
# Create extreme readings and prepare event files-------------------------
fxlist = list()
extreme = "high"
hi = as.numeric(0.95)
lo = as.numeric(0.05)
# select window either "whole lot" or "after".
cum = "whole lot"
wind = as.numeric(1) 
# Read file (as zoo object) if not already done
currency = "GBP"
fxret <- function(currency){
title <- paste(currency, ".r")
dat <- daz[,c(paste(currency), paste(currency, "RR", sep = ""))]
dat[,3] <- 100*diff(log(dat[,1]))
colnames(dat)[3] <- title
fxlist[currency] <- dat
fxlist$currency <- dat
}
head(dat)
str(fxlist)
str(dat)
a <- fxret("EUR")
dat[-1,currency] <- 100*diff(log(dat[,1]))
title
dat$title <- 100
head(FX.z)
for(extreme in highlow)
if(extreme == "high"){
  FX.z$ex <- as.numeric(FX.z[,2] >= quantile(FX.z[,2],  
                                             hi, na.rm = TRUE)) 
} else if(extreme == "low") {
  FX.z$ex <- as.numeric(FX.z[,2] <= quantile(FX.z[,2], 
                                             lo, na.rm = TRUE))
} else(print("No Extreme"))

# Now get the extreme distribution
FX.ex <- FX.z[FX.z$ex == 1, ]
head(FX.ex)
hist(FX.ex$r, prob = TRUE)
lines(density(FX.ex$r, na.rm = TRUE), col = "red")
mystats(FX.ex$r, na.omit = TRUE)
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
