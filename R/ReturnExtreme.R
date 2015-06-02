# This will try to look at the level speculation before a major price move
# The following code draws and prepares the data from the file 
# (Futures.csv) need to install mystats function
# rates and measures of sentiment.    
# Read spec position data----------------------------------
require(eventstudies)
da <- read.csv("./Futures.csv", header = TRUE)
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
# subset extreme data--------------------------
head(da.CAD)
str(da.CAD)
# chose currency (not yet lapply) "CAD", "EUR", "GBP", "CHF", "JPY"))
j = "GBP"
a <- paste("da.", j, sep = "")
X <- matrix(NA, nrow = 2, ncol = 6)
q = 1
for(i in c(0.01, 0.05)) { 
FX.l <- as.numeric(get(a)[,2] <= quantile(get(a)[,2], i, na.rm = TRUE))                   
FX.g <- as.numeric(get(a)[,2] >= quantile(get(a)[,2], 1-i, na.rm = TRUE))
FX.d <- subset(get(a), FX.l == 1)
FX.u <- subset(get(a), FX.g == 1) 
FX.all <- apply(get(a), 2, FUN = mystats, na.omit = TRUE)
FX.up <- apply(FX.u, 2, FUN = mystats, na.omit = TRUE)
FX.down <- apply(FX.d, 2, FUN = mystats, na.omit = TRUE)
X[q,1] <- FX.all[2,2]
X[q,2] <- FX.all[2,3]
X[q,3] <- FX.up[2,2]
X[q,4] <- FX.up[2,3]
X[q,5] <- FX.down[2,2]
X[q,6]<- FX.down[2,3]
q = q+1
}
colnames(X) <- c("r. Total", "S1.total", "r.up", "S1.up", "r.down", "S1.down")
rownames(X) <- c(0.01, 0.05)
X
