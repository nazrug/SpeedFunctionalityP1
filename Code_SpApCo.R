#--------------------------NAZRUG--------------------------#
#---------------Speed and Functionality in R---------------#
#---------------Split-Apply-Combine Functions--------------#
#---------------------4 November 2016----------------------#
#----------------------Caitlin Andrews---------------------#


## ------------ The Libraries ------------ ##
library(data.table)
library(dplyr)

## ------------ The Data ------------ ##
### Daily weather data for 8 sites

# Read in the data - daily weather data
dir.prj <- 'C:/users/candrews/Desktop/Presentations/NAZRUG_Functions_Speed/DATA'
WData <- read.csv(file.path(dir.prj,'EFSites_WeatherData.csv'),header=TRUE)

# Inspect the data
head(WData)
str(WData)
unique(WData$Site)
table(WData$Site)

#### GOAL - Monthly averages Per Site for the time period  for Tmax & Tmin
#8 sites * 12 months = 96 
#96 *2 =192 values
# Prepare the data

unique(WData$Year)

## ------------ 5ish Different Ways to do the same thing ------------ ##

#1 Base R Functions

#1.1 Apply Functions

# apply(X, MARGIN, FUN)
## where  X is an array (or data.frame), MARGIN is a 1 or 2
#1 - rows
#2 - columns
WData_Output_Apply_1 <- apply(WData[,c('TMAX','TMIN')],1,mean)
# BUT Not able to 'group'
str(WData_Output_Apply_1)
WData_Output_Apply_1

#tapply - split-combine-apply for the apply functions
#tapply(X,list of Factors or grouping variables,FUN)
#where X is a vector ....
WData_Output_Apply_2 <- tapply(WData$TMAX,list(WData$Site,WData$Month),mean)
str(WData_Output_Apply_2)
head(WData_Output_Apply_2)

WData_Output_Apply_2 <- melt(tapply(WData$TMAX,list(Site = WData$Site,Month =WData$Month),mean))
str(WData_Output_Apply_2)
head(WData_Output_Apply_2)

#The actual way to do what we want with apply functions ....
#A combination of apply and tapply
#The FUN in the apply argument is tapply
WData_Output_Apply_3 <- apply(WData[,c('TMAX','TMIN')],2,function(x) tapply(x, list(Site = WData$Site, Month =WData$Month), mean))
str(WData_Output_Apply_3)
head(WData_Output_Apply_3)
dim(WData_Output_Apply_3)

#1.2 Aggregate
#Aggregate is a base function
#syntax: aggregate(X,list of factors,FUN)

WData_Output_Aggregate <- aggregate(WData[,c('TMAX','TMIN')],by = list(WData$Site,WData$Month),mean)
str(WData_Output_Aggregate)
head(WData_Output_Aggregate)

# syntax: aggregate(formula, X, FUN)
WData_Output_Aggregate <- aggregate(cbind(TMAX,TMIN) ~ Site + Month,WData,mean)
str(WData_Output_Aggregate)
head(WData_Output_Aggregate)


#2 dplyr
#%>% - string together functions
#only for dataframes
WData_Output_dplyr <- WData %>%
                  group_by(Site, Month) %>%
                  summarise(
                    TmaxMean = mean(TMAX),
                    TminMean = mean(TMIN)
                  )

str(WData_Output_dplyr)
head(WData_Output_dplyr)

#3 data.table
#Note: Data needs to be in data.table format
## Either read in the data as a data.table with function 'fread' OR
## set the data to data.table

WDataDT <- as.data.table(WData)
str(WDataDT)

#Syntax of data.table -> DT[i,j,by] where...
## i = where
## j = select or functionality
## by = group by
## . = list
WData_Output_DT <- WDataDT[,.(TmaxMean = mean(TMAX),
                              TminMean = mean(TMIN)),
                              by = .(Site, Month)]

str(WData_Output_DT)
head(WData_Output_DT)

# 4 - for loop (as a data.frame)
# I do not recommend this for s-a-c, just want to show the possibilitly and time comparison with other functions

#Set up an empty output object that results will go into
WData_Output_Loop <- data.frame()
#Pre deciding my factors that I will subset by
#Sites
USites <- unique(WData$Site)
USites
#Months
Months <- c(1:12)

for(s in seq_along(USites)){
#for every value of s within  1:8
  
  #manually split the data.frame twice
  #First
  WData_Sub <- subset(WData,Site == USites[s])
  
  WData_Sub_AllMonths <- data.frame()
 
  for(m in seq_along(Months)){  
    
    WData2M <- subset(WData_Sub,Month == Months[m])
      
    WData2M_Results<- data.frame(MeanTMAX =  mean(WData2M$TMAX),MeanTMIN =mean(WData2M$TMIN),Month=m)
    
    WData_Sub_AllMonths <-rbind(WData_Sub_AllMonths,WData2M_Results) 

  }
  
  WData_Sub_AllMonths$Sites <-  USites[s]
  WData_Output_Loop <- rbind(WData_Output_Loop,WData_Sub_AllMonths)
  
}

str(WData_Output_Loop)
head(WData_Output_Loop)
dim(WData_Output_Loop)
