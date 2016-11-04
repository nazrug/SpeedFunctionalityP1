forloopFunction1 <- function(DF){
  WData_Output_4 <- data.frame()
  USites <- unique(DF$Site)
  Months <- c(1:12)
  
  for(s in seq_along(USites)){
    
    WData_Sub <- subset(DF,Site == USites[s])
    WData_Sub_AllMonths <- data.frame()
    
    for(m in seq_along(Months)){  
      
      WData2M <- subset(WData_Sub,Month == Months[m])
      
      WData2M_Results<- data.frame(MeanTMAX =  mean(WData2M$TMAX),MeanTMIN =mean(WData2M$TMIN),Month=m)
      
      WData_Sub_AllMonths <-rbind(WData_Sub_AllMonths,WData2M_Results) 
      #print(c(address(WData_Sub_AllMonths),refs(WData_Sub_AllMonths)))
    }
    
    WData_Sub_AllMonths$Sites <-  USites[s]
    WData_Output_4 <- rbind(WData_Output_4,WData_Sub_AllMonths)
    
  }
}

# Can I make this for loop faster?

forloopFunction2 <- function(DF){
  WData_Output_List <- vector("list",8)
  USites <- unique(DF$Site)
  Months <- c(1:12)
  
  for(s in seq_along(USites)){
    
    WData_Sub <- subset(DF,Site == USites[s])
    WData_Sub_AllMonths <- vector("list",12)
    
    for(m in seq_along(Months)){  
      
      WData2M <- subset(WData_Sub,Month == Months[m])
      
      WData2M_Results<- c(MeanTMAX =  mean(WData2M$TMAX),MeanTMIN =mean(WData2M$TMIN),Month=m)
      
      WData_Sub_AllMonths[[m]] <-WData2M_Results
      #print(c(address(WData_Sub_AllMonths),refs(WData_Sub_AllMonths)))
      

    }
    
    WData_Output_List[[USites[s]]] <- WData_Sub_AllMonths
    
  }
}