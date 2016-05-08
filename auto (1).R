library(plyr)

#############7 QNS 3 Turkers
#Read dataset
input <- read.csv('test_7.csv', stringsAsFactors = FALSE)

#Select the required columns (28 is the URL column)
input <- subset(input,select = c(28,48,49,50,51,52,53,54))

#Get Unique URLs
HITid <- unique(input$Input.drwhovideo)
output <- as.data.frame(HITid, stringsAsFactors = FALSE)

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
      output[j,length] <-  NA
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
    sub <- NA
  }
  #Store the value
  output[j,length] <- sub
}

#############5 QNS 5 Turkers
#Same method as above
input <- read.csv('test_5.csv', stringsAsFactors = FALSE)
input <- subset(input,select = c(28,48,49,50,51,52))

#Get Unique HIT ids
HITid <- unique(input$Input.drwhovideo)
output1 <- as.data.frame(HITid, stringsAsFactors = FALSE)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

for (i in c(3,5)) {
  output1[,ncol(output1) + 1] <- ""
  length <- ncol(output1)
  colnames(output1)[ncol(output1)] <- colnames(input)[i]
  
  for (j in 1:length(HITid)) {
    temp <- subset(input, input$Input.drwhovideo == HITid[j],select = i)
    
    if (length(unique(temp[,1])) == 1) {
      output1[j,length] <- temp[1,1]
    }
    if (length(unique(temp[,1])) == 2) {
      output1[j,length] <-  Mode(temp[,1])
    }
    if (length(unique(temp[,1])) == 3) {
      output1[j,length] <-  Mode(temp[,1])
    }
    if (length(unique(temp[,1])) == 4) {
      output1[j,length] <-  Mode(temp[,1])
    }
    if (length(unique(temp[,1])) == 5) {
      output1[j,length] <-  NA
    }
  }
}


for (i in c(2,4,6)) {
  output1[,ncol(output1) + 1] <- ""
  length <- ncol(output1)
  colnames(output1)[ncol(output1)] <- colnames(input)[i]
  
  for (j in 1:length(HITid)) {
    temp <- subset(input, input$Input.drwhovideo == HITid[j],select = i)
    
    all <- paste(temp[1,1],temp[2,1],temp[3,1],sep = "|")
    allsplit <-
      as.data.frame(unlist(strsplit(all, '|',fixed = TRUE)),stringsAsFactors = FALSE)
    colnames(allsplit)[1] <- "cols"
    ddall <- ddply(allsplit,.(cols),summarise,len = length(cols))
    sub <- ""
    
    if ((any(ddall[,2] > 1)) == TRUE) {
      ddall1 <- subset(ddall, ddall[,2] > 1)
      
      if (nrow(ddall1) <= 1) {
        sub <- ddall1[1,1]
      } else{
        for (i1 in 1:nrow(ddall1)) {
          sub <- paste(sub,ddall1[i1,1],sep = "|")
        }
      }
    } else{
      sub <- NA
    }
    #sub1 <- gsub("[|]","",sub)
    output1[j,length] <- sub
  }
}

final <- merge(output,output1,by = "HITid")
colnames(final)[1] <- "url"

final2 <- na.omit(final)
final2[,ncol(final2)+1] <- substr(final2[,1],27,37)
colnames(final2)[ncol(final2)] <- "videoID"

y <- read.csv("test_y.csv")

merged <- merge(final2,y,by.x = "videoID", by.y = "Video.ID")
merged2 <- subset(merged,select = c(1,3:14,16,17,20,21,51,54,57,58,65))

colnames(merged2)[1] <- "VideoID"
colnames(merged2)[2] <- "2_BBCLogo"
colnames(merged2)[3] <- "3_DrWhoTitle"
colnames(merged2)[4] <- "4_PercentageMusic"
colnames(merged2)[5] <- "6_DrWhoSubscription"
colnames(merged2)[6] <- "8_BBCTARDIS"
colnames(merged2)[7] <- "9_MashupLongScene"
colnames(merged2)[8] <- "10_Mons"
colnames(merged2)[9] <- "12_ThumbNail"
colnames(merged2)[10] <- "5_DrWhoScore"
colnames(merged2)[11] <- "11_Doctor"
colnames(merged2)[12] <- "1_Emo"
colnames(merged2)[13] <- "7_VidType"
colnames(merged2)[14] <- "Length"
colnames(merged2)[15] <- "Date"
colnames(merged2)[16] <- "AvgPctViewed"

for(i in 1:nrow(merged2)){
  if(merged2[i,4] == 'PctMusic50to75'){
    merged2[i,4] = 'MoreThanOrEqualTo50'
  }
  if(merged2[i,4] == 'PctMusic25to50'){
    merged2[i,4] = 'LessThan50'
  }
  if(merged2[i,4] == 'PctMusic0to25'){
    merged2[i,4] = 'NoMusic'
  }
  if(substr(merged2[i,8],1,1) == "|"){
    merged2[i,8] <- substr(merged2[i,8],2,nchar(merged2[i,8]))
  }
  if(substr(merged2[i,11],1,1) == "|"){
    merged2[i,11] <- substr(merged2[i,11],2,nchar(merged2[i,11]))
  }
  if(substr(merged2[i,12],1,1) == "|"){
    merged2[i,12] <- substr(merged2[i,12],2,nchar(merged2[i,12]))
  }
  if(substr(merged2[i,13],1,1) == "|"){
    merged2[i,13] <- substr(merged2[i,13],2,nchar(merged2[i,13]))
  }
  
  if(merged2[i,10] == 'Score'){
    merged2[i,10] = 'InstruOrScore'
  }
  if(merged2[i,10] == 'DrWhoScoreNo'){
    merged2[i,10] = 'NoMusic'
  }
}

private <- read.csv("test_private.csv", stringsAsFactors = FALSE)
colnames(private)[1] <- "VideoID"

merged3 <- merged2[!(merged2$VideoID %in% private$VideoID),]





