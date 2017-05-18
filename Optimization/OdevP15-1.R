library(triangle)

FirstClassRows <- 3
Rows <- 40
TouristClassRows <- Rows - (FirstClassRows * 2)
eachRows <- matrix(c(FirstClassRows, TouristClassRows), nrow = 2)
fixedCost <- 100000
FirstClassRevenue <- matrix(c(400, 400, 450), nrow = 3)
TouristClassRevenue <- matrix(c(175, 150, 200), nrow = 3)


demand <- matrix(c(as.integer(rtriangle(1, 160, 220, 180)), 
                   as.integer(rtriangle(1, 140, 240, 200)), 
                   as.integer(rtriangle(1, 150, 225, 200))), 
                 nrow = 3)

fract  <- matrix(sample(x=c(5, 12, 15), size = 3, 
                        replace=TRUE, prob=c(.2, .5, .3)), nrow = 3)

dTourist <- matrix(0, nrow = 3)
dFirst <- matrix(0, nrow = 3)
for (i in 1:3){
  dFirst[i] = as.integer((demand[i] * fract[i]) / 100)
  dTourist[i] = as.integer(demand[i] - dFirst[i])
}

eachSeats <- matrix(c(4, 6), nrow = 2)
seats <- matrix(0, nrow = 2)
for (i in 1:2){
  seats[i] = eachRows[i] * eachSeats[i]
}


satisfiedDemandFirst <- matrix(0, nrow = 3)
satisfiedDemandTourist <- matrix(0, nrow = 3)
for (i in 1:3){
  satisfiedDemandFirst[i] = min(dFirst[i], seats[1])
  satisfiedDemandTourist[i] = min(dTourist[i], seats[2])
}


revenue <- sum((FirstClassRevenue * satisfiedDemandFirst) + 
                 (TouristClassRevenue * satisfiedDemandTourist))

profit <- revenue - fixedCost

if (profit > 0){
  breakevenP = 1
}else{
  breakevenP = 0
}