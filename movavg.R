##
## Source: http://www.cookbook-r.com/Manipulating_data/Calculating_a_moving_average/
##
rm(list=ls())
movingAverage <- function(x, n=1, centered=FALSE) {
  
  if (centered) {
    before <- floor  ((n-1)/2)
    after  <- ceiling((n-1)/2)
  } else {
    before <- n-1
    after  <- 0
  }
  
  # Track the sum and count of number of non-NA items
  s     <- rep(0, length(x))
  count <- rep(0, length(x))
  
  # Add the centered data 
  new <- x
  # Add to count list wherever there isn't a 
  count <- count + !is.na(new)
  # Now replace NA_s with 0_s and add to total
  new[is.na(new)] <- 0
  s <- s + new
  
  # Add the data from before
  i <- 1
  while (i <= before) {
    # This is the vector with offset values to add
    new   <- c(rep(NA, i), x[1:(length(x)-i)])
    
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    
    i <- i+1
  }
  
  # Add the data from after
  i <- 1
  while (i <= after) {
    # This is the vector with offset values to add
    new   <- c(x[(i+1):length(x)], rep(NA, i))
    
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    
    i <- i+1
  }
  
  # return sum divided by count
  s/count
}
## Book Title: Technical Analysis with R (second-edition)
## URL: https://bookdown.org/kochiuyu/technical-analysis-with-r-second-edition/
## Author: Ko Chiu Yu
## 
# myEMA <- function (price,n){
#   ema <- c()
#   ema[1:(n-1)] <- NA
#   ema[n]<- mean(price[1:n])
#   beta <- 2/(n+1)
#   for (i in (n+1):length(price)){
#     ema[i]<-beta * price[i] + 
#       (1-beta) * ema[i-1]
#   }
#   ema <- reclass(ema,price)
#   return(ema)
# }
##
## Import ECDPC COVID-19 data
##

EUCOVID <- read.csv("../DATA/COVID-19-ECDPC-2020-08-07.csv")
EUCOVID$dateRep <- gsub("/","-",EUCOVID$dateRep)
EUCOVID$dateRep <- as.Date(EUCOVID$dateRep,format="%m-%d-%Y")
ES <- subset(EUCOVID,geoId =="ES" & cases >0)
ES <- ES[c(1,5:8)]
## 
## Calculate Moivng Average
##
ES$rollmean <- movingAverage(ES$cases,7)
ES$rollmean2 <- movingAverage(ES$cases,28)
# ES$EMA <- myEMA(ES$cases,7)
plot(ES$dateRep,ES$cases,type="l",main = "Comparing 7 Day Moving Average \n to 28 Day Moving Average",
     xlab="Date Reported",ylab="Mean Cases",col="red")
lines(ES$dateRep,ES$rollmean,col="blue")
lines(ES$dateRep,ES$rollmean2,col="green")
legend("topleft",
       c("Daily Cases","7 Day Mov Av","28 Mov Avg."),
       col=c("red","blue","green"),
       lty = c(1, 1),
       cex = 0.5)
grid()

