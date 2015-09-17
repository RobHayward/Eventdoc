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
# these set the high and low quantoles.  Can be changed or programmed.
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
GBP <- currencydf("GBP")
head(GBP)
#-------------------------------------------------------------
# Create the currencydf files for each currency and put them in a list
clist <- list()
currencylist <- colnames(daz)[seq(2, 20, 2)]
for(i in currencylist){
# this assign idea comes frmom 
#http://librestats.com/2013/12/16/rules-for-naming-objects-in-r/
clist[i] <- list(assign(x = i, value = currencydf(i)))
}
# Create zoo object for events----------------------------------------
# Events must be stored in particular format.
# Requires  fxdf zoo object created with currencydf function
# and wind for size of window and hilo for whether extreme high or extreme low 
#for high or low extreme.
eventzoo <- function(fxdf, wind, hilo){
  if(hilo == "hi"){
    ho = 4
  } else if(hilo == "lo"){
    ho = 5 
  } else {
    print("No Extreme")
  }
# THe next line gives the data (taking the index) for the extremes.
eventslist.FXex <- data.frame(index(fxdf[,ho][fxdf[,ho] ==1]))
eventslist.FXex$unit <- as.character("r")
colnames(eventslist.FXex) <- c("when", "unit")
eventslist.FXex$when <-as.Date(eventslist.FXex$when)
# Align returns with extreme events-----------------------
a <- phys2eventtime(fxdf, eventslist.FXex, width = wind)
# Create windows (exw - whole, exa - after, cumsum - cumulative sum)
FX.exw <- window(a$z.e, start = wind * -1, end = wind)
FX.exa <- window(a$z.e, start = 0, end = wind)
FX.exwc <- remap.cumsum(FX.exw, is.pc = TRUE, base = 0)
FX.exac <- remap.cumsum(FX.exa, is.pc = TRUE, base = 0)
# labels for the window and chart
dims = seq(-wind, wind, 1)
# create matrix for calculations
FX.exwcm <- as.matrix(FX.exwc)
FX.exacm <- as.matrix(FX.exac)
# Mean cumulative return for whole window
MRW <- mean(FX.exwcm[(wind*2 + 1),])
# Mean cumulative return for after window.
MRA <- mean(FX.exacm[(wind + 1),])
# creates the boostraps for distribution of the average returns
FX.exwc.b <- inference.Ecar(FX.exwc, to.plot = FALSE)
FX.exac.b <- inference.Ecar(FX.exac, to.plot = FALSE)
# The list returns the means return hilo and wind details as well as the 
# cumulatie after matrix.  This last oculd be programmed and changed., 
g <- list(MRW = MRW, MRA = MRA, hilo = hilo, wind = wind, FX.exacm = FX.exacm)
return(g)
}
EUR2lo <- eventzoo(fxdf = EUR, wind = 2, hilo = "lo")
str(EUR2lo)
#-----------------------------------------------------
# Use lapply to apply function (currencydf to the currdf list of zoo objects)
# Can the high and low each be added to the components of the list and can the 
# the optional output be selected in the function? 
# eventlist.lo is for extreme low and event.list is for extreme high
eventlist.hi <- lapply(clist, FUN = eventzoo, wind = 2, hilo = "hi")
eventlist.lo <- lapply(clist, FUN = eventzoo, wind = 2, hilo = "lo")
#-------------------------------------------------------------
# create charts for comparison of extreme and regular returns. 
# this requires the currencydf and currdf (best to take from list)
twochart <- function(currencydf, currdf){ 
par(mfrow = c(2,1))
title1 <- paste("Extreme", colnames(clist[[i]])[1], sep = " ")
title2 <- paste("Regular", colnames(clist[[i]])[1], sep = " ")
plot(density(100*eventlist[[i]]$FX.exacm[2,]), xlim = c(-2.0, 2.0), main = title1,
     lwd = 2, col = "red")
abline(v = mean(eventlist[[i]]$FX.exacm[2,], na.rm = TRUE))
text(x = mean(eventlist[[i]]$FX.exacm[2,], na.rm = TRUE) + 0.2, y = 0.2, labels = "Mean")
# need to sample 
plot(density(sample(clist[[i]]$r,113), na.rm = TRUE), xlim = c(-2.0, 2.0), 
     main = title2, lwd = 2, col = "red")
abline(v = mean(sample(clist[[i]]$r, 113), na.rm = TRUE))
text(x = mean(sample(clist[[i]]$r, 113), na.rm = TRUE) + 0.2, y = 0.4, labels = "Mean")
}
twochart(clist[[i]], eventlist[[i]])
#------------------------------------------------
# This will create the pdf figures for the distribution
# The graphs are not put on the same page
pdf("Figures/Extreme/Dist.pdf", paper= "a4r", width = 9, 
    title = "Distribution of returns")
par(mfcol=c(3,1), oma = c(0,0,1,0))
for(i in c("EUR", "GBP", "JPY")){
  twochart(clist[[i]], eventlist[[i]])
}
dev.off()
twochart(clist[[3]], eventlist[[3]])
#--------------------------------------------------------------
# Compare next day return for extreme and regular
# THis requires an element from the clist and the eventlist
df <- matrix(NA, ncol = 6, nrow = 10)
rownames(df) <- currencylist
for(i in currencylist){
df[i, 1] <- round(mean(eventlist.hi[[i]]$FX.exacm[2,], na.rm = TRUE)*100, 4)
df[i, 2] <- round(sd(eventlist.hi[[i]]$FX.exacm[2,], na.rm = TRUE)*100, 4)
df[i, 3] <- round(mean(eventlist.lo[[i]]$FX.exacm[2,], na.rm = TRUE)*100, 4)
df[i, 4] <- round(sd(eventlist.lo[[i]]$FX.exacm[2,], na.rm = TRUE)*100, 4) 
df[i, 5] <- round(mean(clist[[i]]$r, na.rm = TRUE), 4)
df[i, 6] <- round(sd(clist[[i]]$r, na.rm = TRUE), 4)
}
df
dfx <- xtable(df, digits = 4)
dfx

# This is a fuller list of comparison that could be builr
comparedist(clist[[3]], eventlist[[3]])
colnames(clist[[1]])[1]
eventlist[[1]][3]
title1 <- paste("Extreme", colnames(clist[[1]])[1], sep = " ")
title2 <- paste("Regular", colnames(clist[[1]])[1], sep = " ")
head(clist[[1]]$r)
b <- round(apply(clist[[1]][-1,3], FUN = mystats, na.omit = TRUE, MARGIN = 2),4)
d <- round(apply(eventlist[[1]][[5]], FUN = mystats, MARGIN = 1)[, 2], 4)


# e and d need to be turned into a table. 
# mystats function to post a list of descriptive statistics. 
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
#-------------Speculation before large move
# This will extract the extreme movements  and create a dataframe for 
# comparison with the extreme positions. 
clist[[6]][clist[[6]]$r <= quantile(clist[[6]]$r, 0.01, na.rm = TRUE)]
clist[[6]][clist[[6]]$r >= quantile(clist[[6]]$r, 0.99, na.rm = TRUE)]
# the next two lines count the number of highs and lows. 
# large-loss and a high; large-loss and a lo; large gain and high, large gain low
i = "CHF"
df3 <- matrix(NA, nrow = 10, ncol = 6, dimnames = list(c(currencylist), 
                      c("N", "llh", "lll", "N", "lgh", "lgl")))
for(i in currencylist){
n1 <- length(clist[[i]][clist[[i]]$r <= quantile(clist[[i]]$r, 
                                          0.01, na.rm = TRUE)]$exh) 
n2 <- length(clist[[i]][clist[[i]]$r >= quantile(clist[[i]]$r, 
                                                 0.99, na.rm = TRUE)]$exh)
llh <- sum(clist[[i]][clist[[i]]$r <= quantile(clist[[i]]$r, 
                           0.01, na.rm = TRUE)]$exh, na.rm = TRUE) 
lll <- sum(clist[[i]][clist[[i]]$r <= quantile(clist[[i]]$r, 0.01, 
                             na.rm = TRUE)]$exl, na.rm = TRUE) 
lgh <- sum(clist[[i]][clist[[i]]$r >= quantile(clist[[i]]$r, 
                         0.99, na.rm = TRUE)]$exh, na.rm = TRUE) 
lgl <- sum(clist[[i]][clist[[i]]$r >= quantile(clist[[i]]$r, 0.99,
                                 na.rm = TRUE)]$exl, na.rm = TRUE) 
df3[i,1] <- n1
df3[i,2] <- llh
df3[i,3] <- lll
df3[i,4] <- n2
df3[i,5] <- lgh
df3[i,6] <- lgl
}
df3
df3x <- xtable(df3)

# VR test------------------
# This is not yet started. 
require(vrtest)
Lo.Mac(EUR$r[-1], kvec = c(2, 5, 10)) 
i <- 2
y <- as.matrix(EUR$r)
class(y)
kvec <- c(2, 5, 10)
n <- nrow(y)
n
mq <- matrix(NA, nrow = length(kvec), ncol = 2)
mq
for(I in 1:length(kvec)){
  k <- kvec[i]
  LM <- LM_stat(y, k)
  mq[1, ] <- cbind(LM$m1, LM$m2)
  }
# this uses the Lo.Mac test from the 
Lo.Mac.Adj <- function (y, kvec) 
{
  y <- as.matrix(y)
  n <- nrow(y)
  mq <- matrix(NA, nrow = length(kvec), ncol = 2)
  for (i in 1:length(kvec)) {
    k <- kvec[i]
    LM <- LM_stat(y, k)
    mq[i, ] <- cbind(LM$m1, LM$m2)
  }
  VR <- mq
  rownames(VR) <- paste("k=", kvec, sep = "")
  colnames(VR) <- c("M1", "M2")
  return(list(Stats = VR))
}
# This is the LM_stat function.  This comes from 
# https://github.com/cran/vrtest/blob/master/R/LM_stat.R

LM_stat <-
  function(y,k)
  {
    y <- as.matrix(y) 
    y1 <- (y-mean(y))^2 
    n <- nrow(y)
    m <- mean(y)
    vr1 <- sum( (y-m)^2 )/n
    
    # use the filter function
    flt = filter(y, rep(1,k), method = "convolution")
    flt = flt[!is.na(flt)]
    summ = sum((flt - k * m)^2)
    
    vr2 <- summ/(n*k)
    vr <- vr2/vr1
    
    tem1 <- 2*(2*k-1)*(k-1)
    tem2 <- 3*k
    
    m1 <- sqrt(n)*(vr-1)/sqrt( tem1/tem2 )
    
    w <- 4*as.matrix((1-(1:(k-1))/k)^2,nrow=k-1)
    dvec <- matrix(NA, nrow=(k-1), ncol=1)
    for (j in 1:(k-1))
    { 
      dvec[j] <- sum(y1[(j+1):n] * y1[1:(n-j)])/( sum(y1)^2 )
    }
    summ <- crossprod(w,dvec)
    m2 <- sqrt(n)*(vr-1)*((n*summ)^(-.5) )
    return(list(LM1=m1,LM2=m2))
  }