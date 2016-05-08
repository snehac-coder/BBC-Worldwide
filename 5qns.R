#############5 QNS 5 Turkers
#Same method as above
input <- read.csv('test_5.csv', stringsAsFactors = FALSE)
input <- subset(input,select = c(28,48,49,50,51,52))

#Get Unique HIT ids
HITid <- unique(input$Input.drwhovideo)
output1 <- as.data.frame(HITid)

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
      output1[j,length] <-  "to be checked"
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
      sub <- "to be checked"
    }
    #sub1 <- gsub("[|]","",sub)
    output1[j,length] <- sub
  }
}

final <- merge(output,output1,by = "HITid")