#SMM641 Problem Set #2 Tobias Bonger R Script
#Load in packages and data
library(nloptr)
library(stargazer)
library(lattice)
data <- read.csv("CongestionPricing.csv", header = T)
total.cars <- 192000

############################################ PART A #############################################
# Histograms of WTPs
defpar = par()
par(mfrow=c(1,2))

hist(data$Peak_WTP, main="Distribution of Peak WTP", 
     xlab="WTP (£)")

hist(data$Nonpeak_WTP, main="Distribution of Non-Peak WTP", 
     xlab="WTP (£)")

# Compute maximum willingness to pay across time slots and enter it as a new column
N=nrow(data)
for (i in 1:N){
  data$maxWTP[i]=max(data[i,2:3])
}

# The maximium WTP in data, we can use this as the upper bound for our price search.
maxprice=max(data$maxWTP)

# Defining empty array variables we will be introducing
demand=rep(NA,maxprice)
revenue=rep(NA,maxprice)

# Find how many people buy at each price level
for (p in 1:maxprice){
  demand[p]=sum(data$maxWTP>=p)
  revenue[p]=p*demand[p]
}

# Identifying the Best Price
revenueBest=max(revenue)
priceBest=which(revenue == revenueBest)
print(paste("If a single price is to be charged across all time slots, the optimal price is: £",priceBest))


par(mfrow=c(1,2))
# Plotting Demand vs Price
xaxis=1:maxprice
plot(xaxis,demand,pch = 16, type="s", col="blue", las=1, xaxt="n",
     xlab="Price",ylab="Demand")
xticks <- seq(0, maxprice, by=50)
axis(side = 1, at = xticks)

# Plotting Revenue vs Price
xaxis=1:maxprice
plot(xaxis,revenue,pch = 16, type="s",col="blue",las=1, xaxt="n",
     xlab="Price",ylab="Revenue (£)")
xticks <- seq(0, maxprice, by=50)
axis(side = 1, at = xticks) 
axis(side = 1, at = priceBest) 
lines(c(priceBest,priceBest),c(0, revenueBest),lty=2)
axis(side = 2, at = revenueBest,las=1,pos=20, tick=F)
lines(c(20,priceBest),c(revenueBest, revenueBest),lty=2)

# Total level of emissions
## Total revenue
rev.a <- revenueBest/priceBest
percentage.a <- (rev.a/345)

## Number of cars that can afford Price = £8
number.cars <- 192*percentage.a

## Average speed of the car
avg.speed <- (30-(0.0625*(192*percentage.a)))

## Emissions per car
epc.a <- (617.5-(16.7*avg.speed))

# Total level of emissions at universal price
print(paste("At price £8, the total level of emissions is:",prettyNum(round((epc.a*number.cars)*1000, digits = 0), big.mark=",", scientific = F),"g/km"))

############################################ PART B #############################################
#Base price for non-peak set to £7
basePrice=7

# Introducing additional temporary variables to keep track 
## Demand at non peak
demandNonPeak<-rep(0,maxprice)

## Demand at peak
demandPeak<-rep(0,maxprice)

## Revenue
revenue<-rep(0,maxprice)

# For each client we will obtain their maximum WTP and maximum Surplus among the Nonpeak time slots
maxWTPNonPeak<-rep(0,N)
maxsurplusNonPeak<-rep(0,N)

for (i in 1:N){
  maxWTPNonPeak[i]=max(data[i,c(3)])
  maxsurplusNonPeak[i]=max(data[i,c(3)]-basePrice)
  
  # Generate new columns and add this information to our data
  data$maxWTPNonPeak[i]=max(data[i,c(3)])
  data$maxsurplusNonPeak[i]=max(data[i,c(3)]-basePrice)
}

# Create a matrix of dimension
surplusPeak<-matrix(0,N,maxprice)

for (p in 1:maxprice){
  for (i in 1:N){
    surplusPeak[i,p]=data[i,2]-p
  }
}

# Viewing a part of data in surplusPeak
colnames(surplusPeak)=paste0("p=",1:maxprice)

## 11 total prices at increments of £2
surplusPeak[1:11,c(3,5,7,9,11,13,15,17)]

# Compare each client's surplus from NonPeak and Peak for each Peak price point p
# At each price points p's, count how many clients will buy NonPeak and how many clients will buy Peak.
for (p in 1:maxprice){
  # If for a Client, their maximum surplus from Non Peak exceeds their surplus from Peak
  # Then they will prefer NonPeak, indicated by TRUE.  Otherwise for that client we have FALSE.
  # The Client also needs that their maximum surplus from Non Peak is >= 0 to buy.
  demandNonPeak[p]=sum((maxsurplusNonPeak>surplusPeak[,p])*(maxsurplusNonPeak>=0))
  demandPeak[p]=sum((surplusPeak[,p]>=maxsurplusNonPeak)*(surplusPeak[,p]>=0))
  revenue[p]=basePrice*demandNonPeak[p]+p*demandPeak[p]
}

par(mfrow=c(1,1))
# Plotting NonPeak Demand vs Peak Period Price
xaxis=1:maxprice
plot(xaxis,demandNonPeak,pch = 16, type="s",col="blue", las=1, xaxt="n",
     xlab="Price for Peak Period (07:00-18:00)",ylab="Non-Peak Period Demand")
xticks <- seq(0, maxprice, by=50)
axis(side = 1, at = xticks)

# Plotting Peak Demand vs Peak Period Price
xaxis=1:maxprice
plot(xaxis,demandPeak,pch = 16, type="s",col="blue", las=1, xaxt="n",
     xlab="Price for Peak Period (07:00-18:00)",ylab="Peak Period Demand")
xticks <- seq(0, maxprice, by=50)
axis(side = 1, at = xticks)

# Plotting Revenue vs Peak Period Price
xaxis=1:maxprice
plot(xaxis,revenue,pch = 16, type="s",col="blue", las=1, xaxt="n",
     xlab="Price for Peak Period (07:00-18:00)",ylab="Total Revenue")
xticks <- seq(0, maxprice, by=50)
axis(side = 1, at = xticks)
revenueBest=max(revenue[basePrice:maxprice])
priceBest=which(revenue == revenueBest)
axis(side = 1, at = priceBest) 
lines(c(priceBest,priceBest),c(0, revenueBest),lty=2)
axis(side = 2, at = round(revenueBest),las=1)
lines(c(0,priceBest),c(revenueBest, revenueBest),lty=2)

print(paste("When Non-Peak has a base price of 7, the optimal price for Peak:",priceBest))

demandPeak.b <- (demandPeak[9]/N)*total.cars
demandNonPeak.b <- (demandNonPeak[9]/N)*total.cars

avg.speed.peak.b <- (30-(0.0625*(demandPeak.b/1000)))
avg.speed.nonpeak.b <- (30-(0.0625*(demandNonPeak.b/1000)))

epc.peak.b <- (617.5-(16.7*avg.speed.peak.b))
epc.nonpeak.b <- (235.0 -(1.4*avg.speed.nonpeak.b))

total.peak.b <- epc.peak.b*demandPeak.b
total.nonpeak.b <- epc.nonpeak.b*demandNonPeak.b

revenue.b <- priceBest*demandPeak.b + basePrice*demandNonPeak.b
print(paste("At peak (price £9) and non-peak (price £7), the total revenue is: £",
            prettyNum(round(revenue.b, digits = 0), big.mark=",", scientific = F)))

print(paste("At peak (price £9) and non-peak (price £7), the total level of emissions is:",
            prettyNum(round(total.nonpeak.b+total.peak.b, digits = 0), big.mark=",", scientific = F),"g/km"))

############################################ PART C #############################################
# Using revenue values
revenue.c <- revenue
revenue.c <- (revenue.c/N)*total.cars

# Subset for revenue values when revenue is >= £1.1 million
revenue.c.2 <- which(revenue.c>=1100000)

# Create an empty emission array
epc.c <- rep(0, length(revenue.c.2))

# Run a for loop with the same calculations from Part B that calculates the total emissions 
# At the price points that satisfy the minimum revenue constraint
for (i in revenue.c.2) {
  demandPeak.c <- (demandPeak[i]/N)*total.cars
  demandNonPeak.c <- (demandNonPeak[i]/N)*total.cars
  
  avg.speed.peak <- (30-(0.0625*(demandPeak.c/1000)))
  avg.speed.nonpeak <- (30-(0.0625*(demandNonPeak.c/1000)))
  
  # Adding two if statements for peak and non peak based on the emission per car calculations
  if (avg.speed.peak < 25){
    epc.peak <- (617.5-(16.7*avg.speed.peak))
  } else {
    epc.peak <- (235.0 -(1.4*avg.speed.peak))
  }
  
  if (avg.speed.nonpeak < 25){
    epc.non.peak <- (617.5-(16.7*avg.speed.nonpeak))
  } else {
    epc.non.peak <- (235.0 -(1.4*avg.speed.nonpeak))
  }
  
  # Calculating the total peak and non peak emissions
  total.peak.c <- epc.peak*demandPeak.c
  total.nonpeak.c <- epc.non.peak*demandNonPeak.c
  
  epc.c[i-5] = total.nonpeak.c+total.peak.c
  
}

lowest.emission.price <- which(epc.c==min(epc.c))
print(paste("While statisfying contraints, the optimal price for Peak charge is: £", lowest.emission.price+5))

print(paste("The total emissions at the optimal price for Peak charge is:", prettyNum(round(epc.c[9], digits = 0), big.mark=",", scientific = F),"g/km"))
      