#o Create zoo object for events----------------------------------------
# Events must be stored in particular format.
# Requires fxdf which is a zoo object compatible with event studies package.
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
dims = seq(-wind, wind, 1)
FX.exwcm <- as.matrix(FX.exwc)
FX.exacm <- as.matrix(FX.exac)
MRW <- mean(FX.exwcm[(wind*2 + 1),])
MRA <- mean(FX.exacm[(wind + 1),])
FX.exwc.b <- inference.Ecar(FX.exwc, to.plot = FALSE)
FX.exac.b <- inference.Ecar(FX.exac, to.plot = FALSE)
g <- list(MRW = MRW, MRA = MRA, hilo = hilo, wind = wind, FX.exacm = FX.exacm)
return(g)
}
EURSEK2lo <- eventzoo(fxdf = EURSEK, wind = 2, hilo = "lo")
index(fxdf[, ho][fxdf[, ho] == 1])
for(i in currencylist){
title <- paste(i, 2, "hi", sep = "")
  eventlist[[title]] <- assign(x = title, value = eventzoo(clist[[i]], 
                                                   wind = 2, hilo = "hi"))
}

# This is ultimately what we will use to create the graphs etc 
head(EURSEL2lo$FX.exacm)
#http://stackoverflow.com/questions/2641653/
 # pass-a-data-frame-column-name-to-a-function
eventlist <- list()
for(i in currencylist){
title <- paste(i, 2, "hi")
eventlist[[i]] <- assign(x = title, value = eventzoo(clist[[i]], 
                                                   wind = 2, hilo = "hi"))
}
i <- "EUR"
eventzoo(clist[[i]], wind = 2, hilo = "lo")
head(clist[[i]])
#-------------------------------------------
eventlist <- lapply(clist, FUN = eventzoo, wind = 2, hilo = "lo")



somefunction <- function(y, data){
  arguments <- as.list(match.call())
  y = eval(arguments$y, data)
sum(y)
}
#-------------------
myData = data.frame(A = c(1, 2, 3), B = c(10, 9, 8))
somefunction(A, data = myData)
somefunction(B, data = myData)
somefunction(A)
#--------------------
x = c(1, 2, 3, 4, 5)
somefunction(x)
somefunction(x, data = myData)
#------------------------
A = c(1,2)
somefunction(A)
somefunction(A, data = myData)
