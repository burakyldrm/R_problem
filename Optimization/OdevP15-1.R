#
#   Burak Yýldýrým
#


library(triangle)
library(ggplot2)

burak <- function(loop = 1000, donenDeger = "profit", FirstClassRows = 3, Rows = 40, fixedCost = 100000, FirstClassRevenue = c(400, 400, 450), TouristClassRevenue = c(175, 150, 200)){
  
  FirstClassRows        <- FirstClassRows
  Rows                  <- Rows
  TouristClassRows      <- Rows - (FirstClassRows * 2)
  fixedCost             <- fixedCost
  FirstClassRevenue     <- matrix(FirstClassRevenue, nrow = 3)
  TouristClassRevenue   <- matrix(TouristClassRevenue, nrow = 3)
  eachRows              <- matrix(c(FirstClassRows, TouristClassRows), nrow = 2)
  
  profit                <- matrix(0, nrow = loop)
  breakevenP            <- matrix(0, nrow = loop)
  say                   = 0
  set.seed(loop)
  for (j in 1:loop){
    demand              <- matrix(c(as.integer(rtriangle(1, 160, 220, 180)), 
                                    as.integer(rtriangle(1, 140, 240, 200)), 
                                    as.integer(rtriangle(1, 150, 225, 200))), 
                                    nrow = 3)
    fract               <- matrix(sample(x = c(5, 12, 15), size = 3, replace=TRUE, 
                                         prob = c(.2, .5, .3)), nrow = 3)
    
    dTourist            <- matrix(0, nrow = 3)
    dFirst              <- matrix(0, nrow = 3)
    for (i in 1:3){
      dFirst[i]         = as.integer((demand[i] * fract[i]) / 100)
      dTourist[i]       = as.integer(demand[i] - dFirst[i])
    }
    
    eachSeats           <- matrix(c(4, 6), nrow = 2)
    seats               <- matrix(0, nrow = 2)
    for (i in 1:2){
      seats[i]          = eachRows[i] * eachSeats[i]
    }
    
    
    satisfiedDemandFirst          <- matrix(0, nrow = 3)
    satisfiedDemandTourist        <- matrix(0, nrow = 3)
    for (i in 1:3){
      satisfiedDemandFirst[i]     = min(dFirst[i], seats[1])
      satisfiedDemandTourist[i]   = min(dTourist[i], seats[2])
    }
    
    revenue                       <- sum((FirstClassRevenue * satisfiedDemandFirst) + 
                                          (TouristClassRevenue * satisfiedDemandTourist))
    
    
    profit[j]                     <- (revenue - fixedCost)
    
    if (profit[j] > 0){
      breakevenP[j] <- 1
      say <- (say + 1)
    }else{
      breakevenP[j] <- 0
    }
  }
  if(donenDeger == "profit"){
    return (profit)
  }else if(donenDeger == "breakevenP"){
    return (breakevenP)
  }else if( donenDeger == "olasilik"){
    olasilik = (say / loop)
    return (olasilik)
  }
}


#a = burak()
#a = burak(loop = 1000)
#a = burak(loop = 100, donenDeger = "breakevenP")

#hist(a)
#plot(a)
#boxplot(a)

veri <- burak()
qplot(veri, geom = "histogram", binwidth = 500, main = "Profit Simulation", 
      xlab = "Profit",  fill = I("blue"), col = I("red"), alpha = I(.2)) 

sprintf("Þirketin Karlýlýk olasýlýðý: %s", burak(donenDeger = "olasilik"))


