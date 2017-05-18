library(triangle)

fixedCost <- 100000

real1 <- as.integer(rtriangle(1, 160, 220, 180))
real2 <- as.integer(rtriangle(1, 140, 240, 200))
real3 <- as.integer(rtriangle(1, 150, 225, 200))

demand <- matrix(c(real1, real2, real3), nrow = 3)
fract  <- matrix(sample(x=c(5, 12, 15), size = 3, 
       replace=TRUE, prob=c(.2, .5, .3)), nrow = 3)

dFirst <- matrix(0, nrow = 3)
for (i in 1:3){
  dFirst[i] = as.integer((demand[i] * fract[i]) / 100)
}

dTourist <- matrix(0, nrow = 3)
for (i in 1:3){
  dTourist[i] = as.integer(demand[i] - dFirst[i])
}

#########################

FirstClassRows <- 3
Rows <- 40
TouristClassRows <- Rows - (FirstClassRows * 2)
eachRows <- matrix(c(FirstClassRows, TouristClassRows), nrow = 2)

eachSeats <- matrix(c(4, 6), nrow = 2)
seats <- matrix(0, nrow = 2)
for (i in 1:2){
  seats[i] = eachRows[i] * eachSeats[i]
}

##########################

satisfiedDemandFirst <- matrix(0, nrow = 3)
satisfiedDemandTourist <- matrix(0, nrow = 3)
for (i in 1:3){
  satisfiedDemandFirst[i] = min(dFirst[i], seats[1])
  satisfiedDemandTourist[i] = min(dTourist[i], seats[2])
}

FirstClassRevenue <- matrix(c(400, 400, 450), nrow = 3)
TouristClassRevenue <- matrix(c(175, 150, 200), nrow = 3)

revenue <- sum((FirstClassRevenue * satisfiedDemandFirst) + 
                 (TouristClassRevenue * satisfiedDemandTourist))

profit <- revenue - fixedCost

if (profit > 0){
  breakevenP = 1
}else{
  breakevenP = 0
}