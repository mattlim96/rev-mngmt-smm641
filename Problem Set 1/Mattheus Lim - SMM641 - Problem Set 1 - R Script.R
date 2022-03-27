##############
### Q1 (a) ###
##############

mL=50           # Mean Demand for Breakfast, Poisson
mH=20           # Mean Demand for Lunch, Poisson
pL=1            # Price for Breakfast
pH=1.5          # Price for Lunch
capacity=50     # Capacity 
ExpRevenue=rep(0,capacity+1)
for (i in 1:1){
  protect=i-1
  availforBreakfast=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldBreakfast=min(availforBreakfast,dL)
    remainforLunch=capacity-soldBreakfast
    for(dH in 0:200){
      soldLunch=min(remainforLunch,dH)
      RevenueThisIter=pL*soldBreakfast+pH*soldLunch
      ExpRevenue[i]=ExpRevenue[i]+RevenueThisIter*dpois(dL,mL)*dpois(dH,mH)
    }
  }
}
RevenueFCFS=ExpRevenue[1]
print(paste("Expected Daily Revenue (FCFS):", round(RevenueFCFS,2)))

##############
### Q1 (b) ###
##############

mL=50           # Mean Demand for Breakfast, Poisson
mH=20           # Mean Demand for Lunch, Poisson
pL=1            # Price for Breakfast
pH=1.5          # Price for Lunch
capacity=50     # Capacity 
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
print(paste("The Optimal Protection Level to maximise Expected Daily Revenue:", ProtectBest))

##############
### Q1 (c) ###
##############

print(paste("Expected Daily Revenue with an Optimal Protection Level of", ProtectBest,":", round(OptimalExpRevenue,2)))
percImprovement = (OptimalExpRevenue - RevenueFCFS) / RevenueFCFS * 100
print(paste("% improvement compared to Expected Daily Revenue (FCFS):", round(percImprovement,2),"%"))

##############
### Q1 (d) ###
##############

### interactive visualisation using shiny to assess how the Protection level varies
### according to different expected demand, revenues and capacity
### NOTE: please run from Row 74 to Row 173

library(shiny)

### function to calculate the ExpRevenue, ProtectBest and OptimalExpRevenue
findBestAllocation <- function (meanDemandForCroissant=50,meanDemandForSandwich=20,
                                capacity=50,priceForCroissant=1, priceForSandwich=1.5){
  mL <- meanDemandForCroissant
  mH <- meanDemandForSandwich
  pL <- priceForCroissant
  pH <- priceForSandwich
  capacity <- capacity
  ExpRevenue=rep(0,capacity+1) 
  for (i in 1:(capacity+1)){ 
    protect=i-1
    availforLowFare=capacity-protect; 
    ExpRevenue[i]=0; 
    for(dL in 0:100){ 
      soldLowFare=min(availforLowFare,dL) 
      remainforHighFare=capacity-soldLowFare 
      for(dH in 0:100){ 
        soldHighFare=min(remainforHighFare,dH) 
        RevenueThisIter=pL*soldLowFare+pH*soldHighFare 
        ExpRevenue[i]=ExpRevenue[i]+ RevenueThisIter*dpois(dL,mL)*dpois(dH,mH)
      } 
    } 
  }
  Protectindexbest = which(ExpRevenue == max(ExpRevenue)) 
  ProtectBest=Protectindexbest-1 
  OptimalExpRevenue=max(ExpRevenue)
  res =   list(
    meanDemandForCroissant= meanDemandForCroissant,
    meanDemandForSandwich = meanDemandForSandwich,
    pL = priceForCroissant,
    pH = priceForSandwich,
    capacity = capacity,
    ExpRevenue=ExpRevenue,
    ProtectBest=ProtectBest,
    OptimalExpRevenue=OptimalExpRevenue
  )
  
  return(res)
}
bestAllocation = findBestAllocation()

ui = fluidPage(
  # Application title
  titlePanel("Influence of varibales change"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      sliderInput("croissants",
                  "Mean demand for croissants:",
                  min = 1,  max = 100, value = 50),
      sliderInput("sandwiches",
                  "Mean demand for sandwiches:",
                  min = 1,  max = 100, value = 20),
      sliderInput("capacity",
                  "Max capacity:",
                  min = 1,  max = 100, value = 50),
      sliderInput("priceForCroissant",
                  "Price for croissant:",
                  min = 0,  max = 10, value = 1, step = 0.5),
      sliderInput("priceForSandwich",
                  "Price for sandwich:",
                  min = 0,  max = 10, value = 1.5, step = 0.5),
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

server = function(input, output) {

  output$plot = renderPlot({
    
    allocation = findBestAllocation(input$croissants,input$sandwiches,input$capacity,
                                    input$priceForCroissant,input$priceForSandwich)
    print(allocation)
    capacity <- allocation$capacity
    ExpRevenue <- allocation$ExpRevenue
    ProtectBest <- allocation$ProtectBest
    xaxis=1:length(ExpRevenue)
    
    plot(xaxis,ExpRevenue,pch = 16, cex = 0.5,las=1, xaxt="n", ylim=c(0,max(ExpRevenue)+10),
         xlab="Croissants Protected",ylab="Expected Revenue",cex.lab=1.25, 
         cex.axis=1, cex.main=1.25)
    xticks <- seq(0, capacity, by=50)
    axis(side = 1, at = xticks)
    axis(side = 1, at = ProtectBest)
    lines(c(ProtectBest,ProtectBest),c(0, max(ExpRevenue)),lty=2,col="red")
    axis(side = 2, at = round(max(ExpRevenue),2),las=1)
    lines(c(0,ProtectBest),c(max(ExpRevenue), max(ExpRevenue)),lty=2,col="red")
    
  })
  
}

shinyApp(ui = ui, server = server)
