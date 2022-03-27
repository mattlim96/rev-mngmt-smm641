# A NUMERICAL EXAMPLE USING POISSON DISTRIBUTION
mL=100          # Mean Demand for Low-Fare, Poisson
mH=80           # Mean Demand for High-Fare, Poisson
pL=60           # Price for Low-Fare
pH=100          # Price for Low-Fare
capacity=100    # Capacity 
ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
    protect=i-1
    availforLowFare=capacity-protect;
    ExpRevenue[i]=0;
    for(dL in 0:200){
        soldLowFare=min(availforLowFare,dL)
        remainforHighFare=capacity-soldLowFare
        for(dH in 0:200){
            soldHighFare=min(remainforHighFare,dH)
            RevenueThisIter=pL*soldLowFare+pH*soldHighFare
            ExpRevenue[i]=ExpRevenue[i]+
                RevenueThisIter*dpois(dL,mL)*dpois(dH,mH)
        }
    }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest=Protectindexbest-1
OptimalExpRevenue=max(ExpRevenue)
print(paste("The Optimal Protection Level for High-Fare Demand:", ProtectBest))

# Plotting Expected Revenue vs Protection Level
xaxis=0:capacity
plot(xaxis,ExpRevenue/1000,pch = 16, cex = 0.5,las=1, xaxt="n",
     xlab="Seats Protected",ylab="Expected Revenue (in £1K)",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
xticks <- seq(0, capacity, by=50)
axis(side = 1, at = xticks) 
axis(side = 1, at = ProtectBest) 
lines(c(ProtectBest,ProtectBest),c(0, max(ExpRevenue)/1000),lty=2)
axis(side = 2, at = round(max(ExpRevenue)/1000,2),las=1)
lines(c(0,ProtectBest),c(max(ExpRevenue)/1000, max(ExpRevenue)/1000),lty=2)

# ANALYTICAL EXPRESSION
critFrac=(pH-pL)/pH
protect<-qpois(critFrac, mH)

# Plotting:
plot.new()
x <- seq(50, 120, length=71)
y <- dpois(x, mH)
plot(x,y,type="h",xaxt="n",yaxt="n", bty="n",
     xlab = "Distribution of High-Fare Demand", ylab = "")

polygon(c(x[x<=qpois(critFrac, mH)],qpois(critFrac, mH)),
        c(y[x<=qpois(critFrac, mH)],y[x==50]),
        col="antiquewhite3")
lines(c(protect,protect),c(0, qpois(critFrac, mH)))
axis(side = 1, at = round(protect))

bookingLimit=capacity-ProtectBest
print(bookingLimit)

### A Lower Bound for Expected Revenue (FCFS)
mL=100          # Mean Demand for Low-Fare, Poisson
mH=80           # Mean Demand for High-Fare, Poisson
pL=60           # Price for Low-Fare
pH=100          # Price for Low-Fare
capacity=100    # Capacity 
ExpRevenue=rep(0,capacity+1)
for (i in 1:1){
    protect=i-1
    availforLowFare=capacity-protect;
    ExpRevenue[i]=0;
    for(dL in 0:200){
        soldLowFare=min(availforLowFare,dL)
        remainforHighFare=capacity-soldLowFare
        for(dH in 0:200){
            soldHighFare=min(remainforHighFare,dH)
            RevenueThisIter=pL*soldLowFare+pH*soldHighFare
            ExpRevenue[i]=ExpRevenue[i]+RevenueThisIter*dpois(dL,mL)*dpois(dH,mH)
        }
    }
}
RevenueFCFS=ExpRevenue[1]
print(paste("Lower Bound for Expected Revenue (FCFS):", round(RevenueFCFS,1)))

### An Upper Bound for Expected Revenue (Perfect Foresight)
mL=100          # Mean Demand for Low-Fare, Poisson
mH=80           # Mean Demand for High-Fare, Poisson
pL=60           # Price for Low-Fare
pH=100          # Price for Low-Fare
capacity=100    # Capacity
ExpRevenueUB=0
for(dL in 0:200){
    for(dH in 0:200){
        soldHighFare=min(dH,capacity)
        remainforLowFare=capacity-soldHighFare
        soldLowFare=min(dL,remainforLowFare)
        RevenueThisIter=pL*soldLowFare+pH*soldHighFare
        ExpRevenueUB=ExpRevenueUB+RevenueThisIter*dpois(dL,mL)*dpois(dH,mH)
    }
}
print(paste("Upper Bound for Expected Revenue (Perfect Foresight):", round(ExpRevenueUB,1)))

### Revenue Potential of the Optimal Protection Level
barplot(c(RevenueFCFS,OptimalExpRevenue,ExpRevenueUB),
        names.arg = c("FCFS","Optimal Protection","Perfect Foresight"),
        main="Expected Revenue Comparison",
        ylim=range(pretty(c(0, ExpRevenueUB))))

print(paste("Optimal protection achieves:",
            round((OptimalExpRevenue-RevenueFCFS)/(ExpRevenueUB-RevenueFCFS),2),
            " of revenue potantial of perfect foresight."))
