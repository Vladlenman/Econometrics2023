install.packages("corrplot")
install.packages("Metrics")
install.packages("fastDummies")
library(car)
library(dplyr)
library(Metrics)
library(MASS)
library(fastDummies)
train <- read.csv("train.csv", header = TRUE)
train <- as.data.frame(train)
test <- read.csv(file = 'test.csv')
test <- as.data.frame(test)
train <- dummy_cols(train)

table(train$GarageType)
boxplot(train$SalePrice~train$GarageType)

train[,"G.unfin"]=as.integer(train[,"GarageFinish"]=='Unf' | train[,"GarageFinish"]=='NA')
train$G.unfin[is.na(train$G.unfin)] = 1
train[,"remod"]=as.integer(train[,"YearBuilt"]== train[,"YearRemodAdd"])
train[,"is.normal"]=as.integer(train[,"SaleCondition"] == "Normal")
train[,"is.residential"]=as.integer(train[,"Zone"] == "RH" | train[,"Zone"] == "RL" | train[,"Zone"] == "RM")
train[,"has.pool"]=as.integer(train[,"PoolArea"] == 0)
train[,"exter.cond.Ex"]=as.integer(train[,"ExterCond"] == "Ex")
train[,"exter.cond.Gd"]=as.integer(train[,"ExterCond"] == "Gd")
train[,"exter.cond.TA"]=as.integer(train[,"ExterCond"] == "TA")
train[,"exter.cond.FA"]=as.integer(train[,"ExterCond"] == "Fa")

train$BsmtCond[is.na(train$BsmtCond)] <- "Po"

train$BsmtExposure[is.na(train$BsmtExposure)] <- "No"
train$BsmtExposure_Gd[is.na(train$BsmtExposure_Gd)] <- 0


train$BsmtQual[train$BsmtQual=="Ex"] <- 5
train$BsmtQual[train$BsmtQual=="Gd"] <- 4
train$BsmtQual[train$BsmtQual=="TA"] <- 3
train$BsmtQual[train$BsmtQual=="Fa"] <- 2
train$BsmtQual[is.na(train$BsmtQual)] <- 2
train$BsmtQual<-as.numeric(train$BsmtQual)


train$BsmtRating[train$BsmtRating=="GLQ"] <- 5
train$BsmtRating[train$BsmtRating=="ALQ"] <- 4
train$BsmtRating[train$BsmtRating=="BLQ"] <- 3
train$BsmtRating[train$BsmtRating=="Rec"] <- 3
train$BsmtRating[train$BsmtRating=="LwQ"] <- 2
train$BsmtRating[train$BsmtRating=="Unf"] <- 1
train$BsmtRating[is.na(train$BsmtRating)] <- 0
train$BsmtRating<-as.numeric(train$BsmtRating)

train$Age =  train$YrSold - train$YearBuilt
train[train$Age<1,]$Age = 1
train$AgeRemod =  train$YrSold - train$YearRemodAdd


train[train$LivAreaSF>2600,]$LivAreaSF = 2600

sum(is.na(train$BsmtExposure_Gd))

model = lm(log(SalePrice) ~Functional_Min+Functional_Typ+HouseStyle_1Story+sqrt(OverallCond)
           +SaleCondition_Normal*AgeRemod+Fireplaces+Zone+BsmtExposure_Gd
           +BsmtQual+OverallQual+BsmtRating+GarageCars+
             +sqrt(LivAreaSF)+sqrt(Age)+log(LotArea)+BsmtSF,data=train)

summary(model)

test <- dummy_cols(test)

test[,"G.unfin"]=as.integer(test[,"GarageFinish"]=='Unf' | train[,"GarageFinish"]=='NA')
test$G.unfin[is.na(test$G.unfin)] = 1
test[,"remod"]=as.integer(test[,"YearBuilt"]== test[,"YearRemodAdd"])
test[,"is.normal"]=as.integer(test[,"SaleCondition"] == "Normal")
test[,"is.residential"]=as.integer(test[,"Zone"] == "RH" | test[,"Zone"] == "RL" | test[,"Zone"] == "RM")
test[,"has.pool"]=as.integer(train[,"PoolArea"] == 0)
test[,"exter.cond.Ex"]=as.integer(test[,"ExterCond"] == "Ex")
test[,"exter.cond.Gd"]=as.integer(test[,"ExterCond"] == "Gd")
test[,"exter.cond.TA"]=as.integer(test[,"ExterCond"] == "TA")
test[,"exter.cond.FA"]=as.integer(test[,"ExterCond"] == "Fa")

test$BsmtQual[test$BsmtQual=="Ex"] <- 5
test$BsmtQual[test$BsmtQual=="Gd"] <- 4
test$BsmtQual[test$BsmtQual=="TA"] <- 3
test$BsmtQual[test$BsmtQual=="Fa"] <- 2
test$BsmtQual[test$BsmtQual=="Po"] <- 1
test$BsmtQual[test$BsmtQual=="NA"] <- 0
test$BsmtQual<-as.numeric(test$BsmtQual)

test$BsmtRating[test$BsmtRating=="GLQ"] <- 5
test$BsmtRating[test$BsmtRating=="ALQ"] <- 4
test$BsmtRating[test$BsmtRating=="BLQ"] <- 3
test$BsmtRating[test$BsmtRating=="Rec"] <- 3
test$BsmtRating[test$BsmtRating=="LwQ"] <- 2
test$BsmtRating[test$BsmtRating=="Unf"] <- 1
test$BsmtRating[test$BsmtRating=="NA"] <- 0
test$BsmtRating<-as.numeric(test$BsmtRating)



test$Age =  test$YrSold - test$YearBuilt
test$AgeRemod =  test$YrSold - test$YearRemodAdd

test[,"G.unfin"]=as.integer(test[,"GarageFinish"]=='Unf' | test[,"GarageFinish"]=='NA')
test$G.unfin[is.na(test$G.unfin)] = 1
test[,"remod"]=as.integer(test[,"YearBuilt"]== test[,"YearRemodAdd"])
test[,"is.normal"]=as.integer(test[,"SaleCondition"] == "Normal")
test[,"is.residential"]=as.integer(test[,"Zone"] == "RH" | test[,"Zone"] == "RL" | test[,"Zone"] == "RM")
test[,"has.pool"]=as.integer(test[,"PoolArea"] == 0)
test[,"exter.cond.Ex"]=as.integer(test[,"ExterCond"] == "Ex")
test[,"exter.cond.Gd"]=as.integer(test[,"ExterCond"] == "Gd")
test[,"exter.cond.TA"]=as.integer(test[,"ExterCond"] == "TA")
test[,"exter.cond.FA"]=as.integer(test[,"ExterCond"] == "Fa")

test$BsmtCond[is.na(test$BsmtCond)] <- "Po"

test$BsmtExposure[is.na(test$BsmtExposure)] <- "No"
test$BsmtExposure_Gd[is.na(test$BsmtExposure_Gd)] <- 0


test$BsmtQual[test$BsmtQual=="Ex"] <- 5
test$BsmtQual[test$BsmtQual=="Gd"] <- 4
test$BsmtQual[test$BsmtQual=="TA"] <- 3
test$BsmtQual[test$BsmtQual=="Fa"] <- 2
test$BsmtQual[is.na(test$BsmtQual)] <- 2
test$BsmtQual<-as.numeric(test$BsmtQual)


test$BsmtRating[test$BsmtRating=="GLQ"] <- 5
test$BsmtRating[test$BsmtRating=="ALQ"] <- 4
test$BsmtRating[test$BsmtRating=="BLQ"] <- 3
test$BsmtRating[test$BsmtRating=="Rec"] <- 3
test$BsmtRating[test$BsmtRating=="LwQ"] <- 2
test$BsmtRating[test$BsmtRating=="Unf"] <- 1
test$BsmtRating[is.na(test$BsmtRating)] <- 0
test$BsmtRating<-as.numeric(test$BsmtRating)

test$Age =  test$YrSold - test$YearBuilt
test[test$Age<1,]$Age = 1
test$AgeRemod =  test$YrSold - test$YearRemodAdd


test[test$LivAreaSF>2600,]$LivAreaSF = 2600
train[test$LivAreaSF>2600,]$LivAreaSF = 2600

predicted_test <- predict(model, newdata = test)
predicted_test <- exp(predicted_test)


#Binding the submission file
df_submit <- cbind(test[,"Id"], predicted_test)
sum(is.na(df_submit))
colnames(df_submit) <- c("Id", "Predicted")
rownames(df_submit) <- c()
write.csv(df_submit, file = "Submission.csv", quote = FALSE, row.names = FALSE)

