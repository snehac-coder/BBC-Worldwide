data <- read.csv("regvar.csv",stringsAsFactors = FALSE)
data <- subset(data,select = c(1,4:16))

for(i in 1:nrow(data)){
  for(j in 1:ncol(data)){
    if(substr(data[i,j],1,1) == "|"){
      data[i,j] = substr(data[i,j],2,nchar(data[i,j]))
    }
  }
}
data[,ncol(data)+1] <- ""
colnames(data)[ncol(data)] <- "All"

for(i in 1:nrow(data)){
  data[i,ncol(data)] <- paste(data[i,2],data[i,3],data[i,4],data[i,5],data[i,6],data[i,7],data[i,8],data[i,9],data[i,10],data[i,11],data[i,12],data[i,13],data[i,14],sep = "|")
}

data1 <- as.data.frame(data$All, stringsAsFactors = FALSE)

write.csv(data1,"data1.csv")

ab <- as.data.frame(t(c(a = "",b = "")),stringsAsFactors = FALSE)

data1 <- read.csv("data1.csv", stringsAsFactors = FALSE)

ss <- 2
for(i in 1:nrow(data2)){
  for(j in 2:ncol(data2)){
    if(data2[i,j] != ""){
    ab[ss,1] <- data2$id[i]
    ab[ss,2] <- data2[i,j]
    ss <- ss + 1
    }
  }
}

ab <- ab[-1,]
write.csv(ab,"ab.csv")

abc <- split(mdata$value,mdata$id)
tmaster<-as(abc,"transactions")

rules<-apriori(tmaster, parameter=list(supp=0.1,conf = 0.1,minlen=2,maxlen=4), appearance = list(default = 'lhs',rhs = 'y = no'))
rules <- sort(rules, by="lift")

aa <- read.csv("bidasas_bank.csv")
