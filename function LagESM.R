
# Function to compute lagged n variable and add it to data
# dat1 is the dataset
# lagn is the numer of lags (maximum is 3)
# varnames is a character vector with the variable names to be lagged 
# Functions returns data set with lagged variables added and properly named with addtional L1, L2 and L3 respectively.
# 
# Uses: function arange() from dplyr package
 
# P. Verboon, august, 2017



LagESM <- function(dat1, subjnr=subjnr,daynr=daynr,beepnr=beepnr,lagn=1, varnames) {
 
require(dplyr)

  dat1 <- arrange(dat1, subjnr, daynr, beepnr) 
  
 if (lagn > 3) {print("number of lags should not exceed 3"); return() }
   
## add additional beeps at the end of each day with missings
  
vdaynr <- rep(sort(unique(dat1$daynr)), length(unique(dat1$subjnr)))
vsubjnr <- rep(unique(dat1$subjnr), each=length(unique(dat1$daynr)) )
a <- data.frame(cbind(vsubjnr,vdaynr))

a2 <- NULL; a3 <- NULL

                a1 <- a;   a1$beepnr <- max(unique(dat1$beepnr)) + 1 
if (lagn > 1)  {a2 <- a;   a2$beepnr <- max(unique(dat1$beepnr)) + 2 }
if (lagn == 3) {a3 <- a;   a3$beepnr <- max(unique(dat1$beepnr)) + 3 }

a <- rbind(a1,a2,a3)                               
a[,c(4:dim(dat1)[2])] <- NA
names(a) <- names(dat1)
b <- as.data.frame(rbind(dat1, a))

b <- arrange(b, subjnr, daynr, beepnr) 

## add lagged variables 

L <- length(varnames)

for (i in 1:L) {   
  newname <- paste(varnames[i],"L",lagn, sep="")
  b[,newname] <- lag(b[,c(varnames[i])], n=lagn)
}


## remove additional beeps

b <- b[b$beepnr <= (max(unique(dat1$beepnr))),]

return(b)

}   # end function








