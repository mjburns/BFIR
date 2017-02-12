
### BFI INDEX FUNCTION BASED ON LADSON AJWR 2013

bfi_ladson <- function(val_to_reflect, alpha, raw_data) {
  
  #START BFI!
  
  padded <- NULL
  
  j <- 1	
  
  for(i in 1:((NROW(raw_data)+(2*val_to_reflect)))) {
    
    if(i >= 1 & i <= val_to_reflect) {
      
      padded[i] <- raw_data[val_to_reflect + (2-i)] }
    
    if(i > val_to_reflect & i <= (NROW(raw_data)+val_to_reflect)) {
      
      padded[i] <- raw_data[i-val_to_reflect] }
    
    if(i > (NROW(raw_data)+val_to_reflect)) {
      
      #putting check here
      
      pos_here <- NROW(raw_data) - j   	    
      padded[i] <- raw_data[pos_here]   
      j <- j + 1  }}
  
  #padded is the time series of which to calculate baseflow
  #now calculate baseflow
  
  #Declare an array 9 times [quick flow, forward backward etc] *    NROW(padded)
  baseflow_data <- array(dim = c(NROW(padded), 9))
  
  #start the BFI calculation
  for(i in 1:NROW(padded)) {
    
    #do first quickflow, initial value of quickflow = first value of padded data (from reflected set)
    
    if (i==1) {	baseflow_data[i,1] <- padded[i] }
    
    else {
      
      baseflow_data[i,1] <- (alpha * baseflow_data[(i-1), 1]) + (((1 + alpha)/(2))*(padded[i] - padded[i-1])) }
    
    #first condition of QF
    if(baseflow_data[i,1] > 0) { 
      baseflow_data[i,2] <- baseflow_data[i,1] } 
    else {
      baseflow_data[i,2] <- 0  }     
    
    #first pass baseflow 
    
    baseflow_data[i,3] <- padded[i] - baseflow_data[i,2] }
  
  ######### SECOND, BACKWARD pass
  for(i in 1:NROW(padded)) {
    
    if(i == 1) {
      #second backward qf, == last row of first pass baseflow
      baseflow_data[NROW(padded),4] <- baseflow_data[NROW(padded),3]   }
    
    else {
      #qf backward, post initial value
      baseflow_data[(NROW(padded)-i+1),4] <- (alpha * baseflow_data[(NROW(padded)-i+2),4]) + (((1 + alpha)/(2))*(baseflow_data[(NROW(padded)-i+1),3] - baseflow_data[(NROW(padded)-i+2),3]))   }  }
  
  ####condition second pass
  for(i in 1:NROW(padded)) {	
    #second pass conditioned of B
    if(baseflow_data[i,4] > 0) { 
      baseflow_data[i,5] <- baseflow_data[i,4] }
    else {	baseflow_data[i,5] <- 0  }
    
    #second pass baseflow
    baseflow_data[i,6] <- baseflow_data[i,3] - baseflow_data[i,5] }
  
  
  #### DO THIRD FORWARD PASS NOW 
  #quick flow third pass		
  
  for(i in 1:NROW(padded)) {
    
    #third quickflow initial value = value of base_6
    
    if (i==1) {	baseflow_data[i,7] <- baseflow_data[i,6] }
    
    #post initial value 
    
    else {
      
      baseflow_data[i,7] <- (alpha * baseflow_data[(i-1), 7]) + (((1 + alpha)/(2))*(baseflow_data[i,6] -baseflow_data[(i-1),6])) }
    
    ### condition third pass
    if(baseflow_data[i,7] > 0) { 
      baseflow_data[i,8] <- baseflow_data[i,7] } 
    else {
      baseflow_data[i,8] <- 0  }     
    
    #third pass baseflow 
    baseflow_data[i,9] <- baseflow_data[i,6] - baseflow_data[i,8] }
  
  
  #final baseflow 
  final_baseflow <- baseflow_data[(val_to_reflect+1):(NROW(padded)-val_to_reflect),9]
  
  return(final_baseflow) } 

