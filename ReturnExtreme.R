# This will try to look at the level speculation before a major price move
# The following code draws and prepares the data from the file 
# (Futures.csv) and calculates the descriptive statistics of exchange 
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
da.CAD$d <- as.numeric(da.CAD[,2] <= quantile(da.CAD[,2], 0.01, na.rm = TRUE))                   
da.CAD$u <- as.numeric(da.CAD[,2] >= quantile(da.CAD[,2], 0.99, na.rm = TRUE))
da.CAD.d <- subset(da.CAD, da.CAD$d == 1)
da.CAD.u <- subset(da.CAD, da.CAD$u == 1)
da.CAD.all <- apply(da.CAD, 2, FUN = mystats, na.omit = TRUE)
da.CAD.up <- apply(da.CAD.u, 2, FUN = mystats, na.omit = TRUE)
da.CAD.down <- apply(da.CAD.d, 2, FUN = mystats, na.omit = TRUE)
da.CAD.all
da.CAD.up
da.CAD.down
X <- data.frame(3)
X[2,1] <- da.CAD.all[2,3]
X[2,2] <- da.CAD.up[2,3]
X[2,3] <- da.CAD.down[2,3]
colnames(X) <- c("Total", "Up", "Down")
rownames(X) <- "0.05"
X