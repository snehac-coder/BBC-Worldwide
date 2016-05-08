final_data =  read.csv(file="final_regression_data.csv", header=TRUE, sep=",")   #64columns

final_data1 =  read.csv(file="final_regression_data (1).csv", header=TRUE, sep=",")   #64columns

col_names = names(final_data[,4:53]  )
final_data[,col_names] <- lapply(final_data[,col_names] , factor)

## Regression on Average Percentage Viewed ##
 
# prepare the dataframe
apw_dat = final_data[ , 4:54]
apw = final_data[ ,56 ]
apw_data =  cbind(apw_dat , apw)

#
apw_dat1 = final_data[ , 4:57]
apw1 = final_data[ ,59 ]
apw_data1 =  cbind(apw_dat1 , apw1)
fit_n = lm(apw1 ~ ., data = apw_data1)
step_fitn <- stepAIC(fit_n, direction = "both")


## Regression on Views ##

view_dat = final_data[ , 4:54]
views = final_data[ ,57 ]
view_dat =  cbind(view_dat , views)

fit2 = lm(views ~ ., data = view_dat)
summary(fit)
step(lm(views ~ ., data = view_dat) , direction = "both")

step_fit2 <- stepAIC(fit2, direction = "both")
#
view_dat1 = final_data[ , 4:57]
views = final_data[ ,60 ]
view_dat1 =  cbind(view_dat1 , views)
fit_v = lm(views ~ ., data = view_dat1)
step_fit_v <- stepAIC(fit_v, direction = "both")


## Regression on Likes   ##

likes_dat = final_data[ , 4:54]
likes = final_data[ ,58 ]
likes_dat =  cbind(likes_dat , likes)

fit3= lm(likes ~ ., data = likes_dat)
#summary(fit)
step(lm(likes ~ ., data = likes_dat) , direction = "both")

step_fit3 <- stepAIC(fit3, direction = "both")
#

## Regression on Shares   ##

shares_dat = final_data[ , 4:54]
shares = final_data[ ,59 ]
shares_dat =  cbind(shares_dat , shares)

fit4 = lm(shares ~ ., data = shares_dat)
#summary(fit)
step(lm(shares ~ ., data = shares_dat) , direction = "both")
step_fit4 <- stepAIC(fit4, direction = "both")

## Regression on subscriptions   ##

subs_dat = final_data[ , 4:54]
subs = final_data[ ,61 ]
subs_dat =  cbind(subs_dat , subs)

fit5= lm(subs ~ ., data = subs_dat)
#summary(fit)
step(lm(subs ~ ., data = subs_dat) , direction = "both")
step_fit5 <- stepAIC(fit5, direction = "both")

###    
emotion = data[ , 42:48 ]
likes = data[ , 66]
likes_emo = cbind[emotion , likes]





