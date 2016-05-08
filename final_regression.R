#Load the package for stepwise regression and for tidying the regression results
library(MASS)
library(broom)

#Read the data
reg <- read.csv("regression.csv")

#_____________________________1). Like per 1000 views vs independent variables

#Subsetting the dataset to retain the variables required for regression only
#Columns 4:53 represent the independent variables and 64 represents the dependent variable (Likes per 1000 views).
reg1 <- subset(reg,select = c(4:53,64))

#runnning the regression
reg_lpv <- lm(Likes.1000 ~ ., reg1)

#Running stepwise regression
step_reg_lpv <- stepAIC(reg_lpv)

#Running the final regression with the subset of variables selected from Stepwise regression
new_lm_lpv <- lm(Likes.1000 ~ DrWhoTitleNone + DrWhoTitleAfter + DrWhoSubscriptionAliens + 
                   DrWhoSubscriptionDrWho + DrWhoSubscriptionCastMember + Sontarans + 
                   WeepingAngels + InstrumentalOrScore + NoMusic + PeterCapaldi + 
                   WilliamHartnell + NoDoctor + EmotionHumor + EmotionSuspense + 
                   EmotionRomantic + Interview + Tribute + VideoLength, reg1)

#Saving the results to a data frame
lpv_reg <- tidy(new_lm_lpv)
###############################################################################################################

#_____________________________2). Average Percentage Views views vs independent variables

#Removing the 16 rows that have less that 60 views
reg2 <- subset(reg,reg$Views > 55)

#Subsetting the dataset to retain the variables required for regression only
#Columns 4:53 represent the independent variables and 64 represents the dependent variable (Average Percentage Viewed).
reg2 <- subset(reg2,select = c(4:53,55))

#runnning the regression
reg_apv <- lm(AvgPctViewed ~ ., reg2)

#Running stepwise regression
step_reg_apv <- stepAIC(reg_apv)

#Running the final regression with the subset of variables selected from Stepwise regression
new_lm_apv <- lm(AvgPctViewed ~ DrWhoTitleAfter + DrWhoSubscriptionAliens + DrWhoSubscriptionDrWho + 
                   DrWhoSubscriptionWritten + BBCTARDISNo + Sontarans + MattSmith + 
                   PeterCapaldi + ChristopherEccleston + DavidTenant + NoDoctor + 
                   EmotionSeriousSad + EmotionSuspense + EmotionHorror + VideoTrailerTeaser + 
                   Episode + Tribute + VideoLength, reg2)

#Saving the results to a data frame
apv_reg <- tidy(new_lm_apv)
###############################################################################################################

#_____________________________3). Ln(Views) views vs independent variables

#Subsetting the dataset to retain the variables required for regression only
#Columns 4:53 represent the independent variables and 64 represents the dependent variable (Ln(Views)).
reg3 <- subset(reg,select = c(4:54,56))

#Taking log for the Views column
reg3$Views<-log(reg3$Views)

#runnning the regression
reg_lnv <- lm(Views ~ . + I(Age^2), reg3)

#Running stepwise regression
step_reg_lnv <- stepAIC(reg_lnv)

#Running the final regression with the subset of variables selected from Stepwise regression
new_lm_lnv <- lm(Views ~ BBCLogoYes + DrWhoSubscriptionDrWho + BBCTARDISNo + Long.Scene + 
                   Sontarans + Daleks + NoMonster + InstrumentalOrScore + NoOfDoctors + 
                   PeterDavison + SylvesterMcCoy + JonPertwee + NoDoctor + ColinBaker + 
                   EmotionSeriousSad + ArchivedVideo + VideoTrailerTeaser + 
                   Age + I(Age^2), reg3)

#Saving the results to a data frame
lnv_reg <- tidy(new_lm_lnv)
