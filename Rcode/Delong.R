# Build the DeLong function made of 'rho' as the actual misperception, rhomu as
# the average misperception, r as the risk free rate, mu is the weight of 
# speculators in the total market participants, ra as the parameter for 
# risk aversion, rhosd is the standard deviation of the misperception. 
require(zoo)
require(eventstudies)
# Function to calculate price under DeLong model-----------------
DeLong <- function(rho, rhomu, r, mu, ra, rhosd){
  p = 100 + (mu * (rho - rhomu))/(1 + r) + (mu * rhomu)/r - 
    ((2 * ra * mu^2 * rhosd^2)/(r * (1 + r)^2))
  return(p)
}
# Set misperception as a random vaiable with a mean of 1 and a 
# standard deviation of 1
set.seed(1)
rho <- rnorm(1000, 4, 2)
p <- DeLong(rho = rho, rhomu = 4, r = 0.1, mu = 0.5,
            ra = 0.3, rhosd = 2)
length(p)
plot(p, type = 'l', main = "Delong model with misperception as random variable")
da <- cbind(p, rho)
head(da)
pz = as.zoo(da)
# Create returns
pz$r <- 100*diff(log(pz$p))
str(pz)
head(pz)
# Create extreme as above 95th percentile
pz$e = as.numeric(pz$rho >= quantile(pz$rho, 0.95, na.rm = TRUE))
head(pz)
eventslist.ex <- data.frame(index(pz$e[pz$e == 1]))
eventslist.ex$unit <- as.character("r")
colnames(eventslist.ex) <- c("when", "unit")
dim(eventslist.ex)
tail(eventslist.ex)
# Align events
a <- phys2eventtime(pz, eventslist.ex, width = 2)
P.ex <- window(a$z.e, start = 0, end = 1)
P.ex
# Calculate descriptive statistics of rho extreme-------------------
P.ext <- t(P.ex)
P.ext
mystats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  s <- sd(x) 
  t <- m/s
  n <- length(x)
  skew <- sum((x-m)^3/s^3)/n
  ps <- ((6*n*(n-1))/((n-1)*(n+1)*(n+3)))^0.5
  kurt <- sum((x-m)^4/s^4)/n - 3
  pk <- ((n^2-1)/((n-3)*(n + 5)))^0.5
  max <- max(x)
  min <- min(x)
  return(c(n=n, mean=m, t= t, stdev=s, skew=skew, ps = ps, 
           kurtosis=kurt, pk = pk, max = max, min = min))
}
# change currency file. 
P.d <- apply(P.ext, 2, FUN = mystats, na.omit = TRUE)
P.d
# Create series with proportion of speculators (mu) changing----------- 
# Create smoothed mu series
set.seed(1)
mu1 <- as.zoo(rnorm(1000, 0.5, 0.2))
class(mu1)
mu2 <- lag(mu1, 1)
class(mu2)
mu <- 0.5 * mu1 + 0.5 * mu2
class(mu)
plot(mu,type = 'l')
# Now weight of speculators adjusts
p <- DeLong(rho = 1, rhomu = 4, r = 0.1, mu = mu,
            ra = 0.3, rhosd = 2)
length(p)
# Plot simulated time seriess----------------------------
pdf("DelongSim.pdf", paper= "a4", width = 9, height = 11, title = "residts")
par(mfcol=c(2,1), oma = c(0,0,0,0))
plot(mu, type = 'l', main = "Change in proportion of noise traders")
plot(p, type = 'l', main = "Price when proportion of noise traders changes")
dev.off()
# Create table-------------------------------
da <- cbind(p, mu)
head(da)
pz = as.zoo(da)
pz$r <- 100*diff(log(pz$p))
str(pz)
head(pz)
# Create extreme event of extreme specualtive weight
pz$e = as.numeric(pz$mu >= quantile(pz$mu, 0.95, na.rm = TRUE))
head(pz)
eventslist.ex <- data.frame(index(pz$e[pz$e == 1]))
eventslist.ex$unit <- as.character("r")
colnames(eventslist.ex) <- c("when", "unit")
dim(eventslist.ex)
tail(eventslist.ex)
# Create event file
a <- phys2eventtime(pz, eventslist.ex, width = 2)
P.ex <- window(a$z.e, start = 0, end = 1)
P.ex
#Descriptive statistics of the extreme speculator weight-----------
P.ext <- t(P.ex)
P.ext
# change currency file. 
P.d <- apply(P.ext, 2, FUN = mystats, na.omit = TRUE)
P.d
# Descriptive statistics of whole Delong series---------------------
# Use the standard model. Here rho and mu adjust as specified. 
# Cut who to equal length of smoothed mu
rho <- rho[1:999]
p <- DeLong(rho = rho, rhomu = 4, r = 0.1, mu = mu,
            ra = 0.3, rhosd = 2)
da <- cbind(p, mu)
head(p)
pz = as.zoo(da)
pz$r <- 100*diff(log(pz$p))
# Descriptive statistics of total Delong series---------------------
pz <- as.matrix(pz)
# change currency file. 
P.d <- apply(pz, 2, FUN = mystats, na.omit = TRUE)
P.d
