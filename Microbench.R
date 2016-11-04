library(microbenchmark) 
library(ggplot2)

source('forLoopFunctions.R')

mbm = microbenchmark(

#1.1 Base apply
applyF = apply(WData[,c('TMAX','TMIN')],2,function(x) tapply(x, list(WData$Site,WData$Month), mean)),

#1.2 Aggregate
aggregatef = aggregate(cbind(TMAX,TMIN) ~ Year + Month,WData,mean),

#2 dplyr
dplyrF = WData %>%
  group_by(Site, Month) %>%
  summarise(
    TmaxMean = mean(TMAX),
    TminMean = mean(TMIN),
    PptSum = sum(PPT)
  ),

#3 data.table
datatableF = WDataDT[,.( TmaxMean = mean(TMAX),
                                   TminMean = mean(TMIN),
                                   PptSum = sum(PPT)),
                               by = .(Site, Month)],

#4 For loop (now in a function)
DFforloop = forloopFunction1(WData),
Listforloop = forloopFunction2(WData)

)
mbm
autoplot(mbm)





