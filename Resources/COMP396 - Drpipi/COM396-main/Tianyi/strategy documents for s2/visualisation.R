# This piece of code can plot the changes of the current position 
# during operation into intuitive line charts. 
# Through 10 line charts of 10 time series, combined with 
# previously made close price charts, we can know whether our strategy 
# is doing the right thing at the right time, 
# which can guide the improvement of our strategy. 
# Author: Tianyi Wang

# THE LOCATION IS IMPORTANT
# At the top of the strategy file, before the getOrders() function:
# ncol needs to be manually aligned with the actual number of time series!!

# Used to store the value of currentPos
# Row: Number of days in operation.
# Column: current position value of the time series used.
strategyMatrix <- matrix(ncol = 10) 

runningDays <- 1000 #!! Needs to be manually aligned with the number of days
                    # In sample tests, it can be 500 days!

date <- vector() # Store Date objects to generate x axis


# getOrders <- function(store,newRowList,currentPos,info,params) {
  
    currentPosition <- vector()
    
    # if (store$iter > params$cciLookback) {
      
      # for (i in 1:length(params$series)) {
    
        # The code below needs to be at the end of the for loop!!
        # Append currentPos to a vector
        currentPosition <- append(currentPosition,
                                  currentPos[params$series[i]])
        
      # }
        
        # The code below is in the if statement but not in the for loop!!
        # Generate the x axis of the line chart
        # Since all time series run on same dates, it is only necessary to 
        # append once outside the for loop
        # Have to be <<- , or ERROR
        date <<- append(date,index(newRowList[[1]]))
        
        # Outside the for loop, used to store all currentPos
        # Have to be <<- , otherwise ERROR
        strategyMatrix <<- rbind(strategyMatrix,currentPosition)
        
        # Plotting
        # For some reason, the backtester function will not run the last 
        # 2 days, so runningDays needs to be subtracted from these days
        if(store$iter==runningDays-2){
          
          # Because of the initialization, the first row of the matrix 
          # will be NA, need to delete it before plotting
          strategyMatrix <- strategyMatrix[-1,]
          
          # Produce all the plots of running series
          for(i in 1:length(params$series)){
            
            # Save as png
            png(paste("Graph", toString(params$series[i]), ".png"),
                width = 1920, height = 1080, units = "px")
            
            # x axis: date, generate from newRowList
            # y axis: current position of the specified time series
            matplot(date,strategyMatrix[,i],
                    ylab="Current Position", type='l')
            dev.off()
          }
        }
        
    # } # The end of the if statement
# } # The end of the getOrder function