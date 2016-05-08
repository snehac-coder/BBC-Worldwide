library(plyr)

#############7 QNS 3 Turkers
#Read dataset
input <- read.csv('test_7.csv', stringsAsFactors = FALSE)

#Select the required columns (28 is the URL column)
input <- subset(input,select = c(28,48,49,50,51,52,53,54))

#Get Unique URLs
HITid <- unique(input$Input.drwhovideo)
output <- as.data.frame(HITid)

#Function to calculate the mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#For questions where multi select is not an option
for (i in c(3,4,5,6,7,8)) {
  #Create the new column
  output[,ncol(output) + 1] <- ""
  length <- ncol(output)
  colnames(output)[ncol(output)] <- colnames(input)[i]
  
  #For each hit
  for (j in 1:length(HITid)) {
    #Get a subset for that column for that URL
    temp <- subset(input, input$Input.drwhovideo == HITid[j],select = i)
    
    #If no of unique vals = 1, take the value, if it is, take the mode, if its 3, needs to be verified
    if (length(unique(temp[,1])) == 1) {
      output[j,length] <- temp[1,1]
    }
    if (length(unique(temp[,1])) == 2) {
      output[j,length] <-  Mode(temp[,1])
    }
    if (length(unique(temp[,1])) == 3) {
      output[j,length] <-  "to be checked"
    }
  }
}


#If multi  select is enabled
i <- 2

#Create new column
output[,ncol(output) + 1] <- ""
length <- ncol(output)
colnames(output)[ncol(output)] <- colnames(input)[i]

#For each URL
for (j in 1:length(HITid)) {
  temp <- subset(input, input$Input.drwhovideo == HITid[j],select = i)
  
  #Combine all answers, seperated by |
  all <- paste(temp[1,1],temp[2,1],temp[3,1],sep = "|")
  
  #Split using | and create a data frame
  allsplit <-
    as.data.frame(unlist(strsplit(all, '|',fixed = TRUE)),stringsAsFactors = FALSE)
  colnames(allsplit)[1] <- "cols"
  
  #Use ddply to find count of each word
  ddall <- ddply(allsplit,.(cols),summarise,len = length(cols))
  sub <- ""
  
  #If there is more than one ocurence
  if ((any(ddall[,2] > 1)) == TRUE) {
    #Get all rows for which the count is greater than 1
    ddall1 <- subset(ddall, ddall[,2] > 1)
    
    if (nrow(ddall1) <= 1) {
      #Get the final value to be stored
      sub <- ddall1[1,1]
    } else{
      #Get the final value to be stored
      for (i1 in 1:nrow(ddall1)) {
        sub <- paste(sub,ddall1[i1,1],sep = "|")
      }
    }
  } else{
    #Get the final value to be stored
    sub <- "to be checked"
  }
  #Store the value
  output[j,length] <- sub
}
