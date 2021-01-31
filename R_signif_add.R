
#Example usage of very basic R script to proform t tests and add significance bars to plots

data <- read.csv("data.csv") #read in data (example data is data.csv file)
boxplot(data, frame.plot = FALSE, xaxt = 'n') #plot data without boarders
axis(side=1,a=1:length(data),labels = colnames(data))

#uncomment next two lines to show indevidual values
#install.packages('beeswarm')
#beeswarm(data,add=T, corral = 'omit', pch = 20) 

# start function
Add_Labels <- function(data)
{
  #Make an empty dataframe to store the comparison and p-value from t.test
  comparison_df <- data.frame(matrix(ncol=3,nrow=0)); 
  colnames(comparison_df) <- c('condition1','condition2','pvalue') 
  
  #H store the y value to put lines and text
  H <- vector()
  
  # for every value in data, t.test each other element in dataset. (Side note: t.test is used for simple functionality make sure the statistical test you use works for your dataset)
  for(i in 1:length(data))
  {
    for(j in 1:length(data))
    {
      condition1 <- names(data[i])
      condition2 <- names(data[j])
      sig <- t.test(data[i],data[j])$p.value #getting p-value for each comparison 
      comparison_df <- rbind(comparison_df,c(condition1,condition2,sig)) #storing comparisons into dataframe
      
      if (i<j) #Used as a check to not make the same comparison twice
      {
        if (sig < 0.05) #only do calculations if the comparison is significant
        {
          #The following calculations determine a good y value to put the lines and text
          max <- max(data[i:j]) #get the max y needed to clear to not draw line through data
          movement_up <- max(data/40) # add a proportional distance above to clear highest point
          h <- (max+movement_up+(movement_up/3)) #h = the height or y value to start adding labels
          
          if(length(H) >= 1) #if bars have already been plotted
          {
            for(v in 1:length(H)) #go through the values that have been plotted
            {
              if (as.numeric(h) == as.numeric(H[v])) #if the value has been plotted before
              {
                h <- h + (movement_up/3) #change the height to be above the previously plotted line
                break #with a new height set leave this loop
              }
            }
          }
          #setting up parameter for lines to be plotted
          x <- i:j #make the lines go from the first datapoint to the second
          y <- rep((h-(movement_up)),(abs(i-j)+1)) #make y value have same number of points as x to be plotted with a slope of 0 at determined y value
          H <- append(H,h) # record the new height into the height vector to not overlap lines
        }
        
        #Check what the p-value was, if it has a given significance add the text to the graph at the correct point
        if(sig < 0.00005)
        {
          lines(x=x,y=y)     
          text(x=((i+j)/2), y=h, labels= "****" )
        }
        else if (sig < 0.0005)
        {
          lines(x=x,y=y)     
          text(x=((i+j)/2), y=h, labels= "***")
        }
        else if (sig < 0.005)
        {
          lines(x=x,y=y)     
          text(x=((i+j)/2), y=h, labels= "**")
        }
        else if (sig < 0.05)
        {
          lines(x=x,y=y)     
          text(x=((i+j)/2), y=h, labels= "*")
        }
      }
    }
  }

  return(comparison_df) #this will return the dataframe containing all the comparisons make with their respective p_values
}

Add_Labels(data) #Running the fuction