#Comparison.R
#This will compare the extreme readings for each of the methods
da <- read.csv("./Data/Futures.csv", header = TRUE, stringsAsFactors = FALSE)
da$Date <- as.Date(da$Date, format = "%d/%m/%Y")
da$S1 <- (da$NCL-da$NCS)/(da$NCL+da$NCS+da$NCSP)
da$S2 <- (da$NCL-da$NCS)/da$OI
da$S3 <- (da$NCL+da$NCS+da$NCSP)/da$OI
da <- da[,c(1, 2, 3, 5, 19, 20, 21)]
str(da)
head(da)
# Sub set files------------------------------------------
CADfut <- subset(da, subset = da$Exchange == "CAD")
CHFfut <- subset(da, subset = da$Exchange == "CHF")
EURfut <- subset(da, subset = da$Exchange == "EUR")
GBPfut <- subset(da, subset = da$Exchange == "GBP")
JPYfut <- subset(da, subset = da$Exchange == "JPY") 
# Rank to find extremes
CADS1lo <- head(CADfut[order(CADfut$S1), ], n = 10)
CADS1hi <- tail(CADfut[order(CADfut$S1), ], n = 10)
# plot===================
plot(CADfut$Date, CADfut$S1, type = 'l')
#====Now compare to risk reversal
da2 <- read.csv('./Data/RR.csv', stringsAsFactors = FALSE)
head(da2)
head(da2[order(da2$EURGBPRR), c(1, 4, 5)], n = 20)
tail(da2[order(da2$EURGBPRR), c(1, 4, 5)], n = 150)
# this last one does not seem to work. 
da3 <- subset(da2, select = c(da2$Date, da2$EURGBP, da2$EURGBPRR))
head(da3)
