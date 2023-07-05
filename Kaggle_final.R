install.packages("corrplot")
library(car)
train <- read.csv("train.csv", header = TRUE)
train <- as.data.frame(train)
train <- subset(train, select = -FireplaceQu)
q1 <- quantile(train$SalePrice, 0.25)
q3 <- quantile(train$SalePrice, 0.75)
iqr <- q3 - q1
upper <- q3 + 1.5*iqr
lower <- q1 - 1.5*iqr
train <- subset(train, train$SalePrice >= lower & train$SalePrice <= upper)
train$Zone[train$Zone == "C"] <- "RH"
train$Zone[train$Zone == "RH"] <- "RM"
q1 <- quantile(train$LotArea, 0.25)
q3 <- quantile(train$LotArea, 0.75)
iqr <- q3 - q1
upper <- q3 + 1.5*iqr
lower <- q1 - 1.5*iqr
train <- subset(train, train$LotArea >= lower & train$LotArea <= upper)
table(train$LotConfig)
boxplot(train$SalePrice~train$LotConfig)
train$LotConfig[train$LotConfig == "Inside"] <- "Corner"
train$LotConfig[train$LotConfig == "FR2"] <- "Corner"
table(train$BldgType)
boxplot(train$SalePrice~train$BldgType)
table(train$HouseStyle)
train$HouseStyle[train$HouseStyle == "1.5Sto"] <- "1Story"
train$HouseStyle[train$HouseStyle == "2.5Sto"] <- "2Story"
boxplot(train$SalePrice~train$HouseStyle)
train$OverallQual[train$OverallQual == 3] <- 4
train$OverallQual[train$OverallQual == 9] <- 8
table(train$OverallQual)
boxplot(train$SalePrice~train$OverallQual)
train$Age <- 2010 - train$YearBuilt
train$AgeRemod <- 2010 - train$YearRemodAdd
train$OverallCond[train$OverallCond == 2] <- 4
train$OverallCond[train$OverallCond == 3] <- 4
train$OverallCond[train$OverallCond == 9] <- 8
train$OverallCond[train$OverallCond == 6] <- 7
table(train$OverallCond)
boxplot(train$SalePrice~train$OverallCond)
table(train$RoofStyle)
boxplot(train$SalePrice~train$RoofStyle)
train$ExterQual[train$ExterQual == "Ex"] <- "Gd"
train$ExterQual[train$ExterQual == "Fa"] <- "TA"
table(train$ExterQual)
boxplot(train$SalePrice~train$ExterQual)
train$ExterCond[train$ExterCond == "Ex"] <- "Gd"
train$ExterCond[train$ExterCond == "Fa"] <- "TA"
table(train$ExterCond)
boxplot(train$SalePrice~train$ExterCond)
table(train$Foundation)
boxplot(train$SalePrice~train$Foundation)
train$BsmtQual[train$BsmtQual == "Ex"] <- "Gd"
train$BsmtQual[train$BsmtQual == "Fa"] <- "TA"
train$BsmtQual[is.na(train$BsmtQual)] <- "No"
table(train$BsmtQual)
boxplot(train$SalePrice~train$BsmtQual)
train$BsmtCond[is.na(train$BsmtCond)] <- "Fa"
train$BsmtCond[train$BsmtCond == "Po"] <- "Fa"
boxplot(train$SalePrice~train$BsmtCond)
train$BsmtExposure[is.na(train$BsmtExposure)] <- "No"
train$BsmtExposure[train$BsmtExposure == "Gd"] <- "Av"
train$BsmtExposure[train$BsmtExposure == "Mn"] <- "No"
table(train$BsmtExposure)
boxplot(train$SalePrice~train$BsmtExposure)
train$BsmtRating[train$BsmtRating=="GLQ"] <- 5
train$BsmtRating[train$BsmtRating=="ALQ"] <- 4
train$BsmtRating[train$BsmtRating=="BLQ"] <- 3
train$BsmtRating[train$BsmtRating=="Rec"] <- 3
train$BsmtRating[train$BsmtRating=="LwQ"] <- 2
train$BsmtRating[train$BsmtRating=="Unf"] <- 1
train$BsmtRating[is.na(train$BsmtRating)] <- 0
train$BsmtRating <- as.numeric(train$BsmtRating)
table(train$BsmtRating)
boxplot(train$SalePrice~train$BsmtRating)
table(train$CentralAir)
boxplot(train$SalePrice~train$CentralAir)
boxplot(train$X1stFlrSF)
q1 <- quantile(train$X1stFlrSF, 0.25)
q3 <- quantile(train$X1stFlrSF, 0.75)
iqr <- q3 - q1
upper <- q3 + 1.5*iqr
lower <- q1 - 1.5*iqr
train <- subset(train, train$X1stFlrSF >= lower & train$X1stFlrSF <= upper)
boxplot(train$LivAreaSF)
q1 <- quantile(train$LivAreaSF, 0.25)
q3 <- quantile(train$LivAreaSF, 0.75)
iqr <- q3 - q1
upper <- q3 + 1.5*iqr
lower <- q1 - 1.5*iqr
train <- subset(train, train$LivAreaSF >= lower & train$LivAreaSF <= upper)
train$FullBath[train$FullBath == 0] <- 1
train$FullBath[train$FullBath == 3] <- 2
table(train$FullBath)
boxplot(train$SalePrice~train$FullBath)
train$HalfBath[train$HalfBath == 2] <- 1
table(train$HalfBath)
boxplot(train$SalePrice~train$HalfBath)
table(train$Bedrooms)
boxplot(train$SalePrice~train$Bedrooms)
table(train$Kitchen)
boxplot(train$SalePrice~train$Kitchen)
train$KitchenQual[train$KitchenQual == "Ex"] <- "Gd"
train$KitchenQual[train$KitchenQual == "Fa"] <- "TA"
table(train$KitchenQual)
boxplot(train$SalePrice~train$KitchenQual)
train$TotRms[train$TotRms == 3] <- 4
train$TotRms[train$TotRms == 10] <- 9
train$TotRms[train$TotRms == 12] <- 9
train$TotRms[train$TotRms == 4] <- 5
train$TotRms[train$TotRms == 5] <- 6
train$TotRms[train$TotRms == 9] <- 8
table(train$TotRms)
boxplot(train$SalePrice~train$TotRms)
train$Functional[train$Functional == "Mod"] <- "Min"
table(train$Functional)
boxplot(train$SalePrice~train$Functional)
train$Fireplaces[train$Fireplaces == 3] <- 1
train$Fireplaces[train$Fireplaces == 2] <- 1
table(train$Fireplaces)
boxplot(train$SalePrice~train$Fireplaces)
train$GarageType[train$GarageType == "Other"] <- "Attchd"
train$GarageType[is.na(train$GarageType)] <- "No"
train$GarageType[train$GarageType == "No"] <- "Detchd"
table(train$GarageType)
boxplot(train$SalePrice~train$GarageType)
train$GarageFinish[is.na(train$GarageFinish)] <- "Unf"
train$GarageFinish[train$GarageFinish == "RFn"] <- "Fin"
table(train$GarageFinish)
boxplot(train$SalePrice~train$GarageFinish)
train$GarageQual[train$GarageQual == "Po"] <- "Fa"
train$GarageQual[train$GarageQual == "Gd"] <- "TA"
train$GarageQual[is.na(train$GarageQual)] <- "Fa"
table(train$GarageQual)
boxplot(train$SalePrice~train$GarageQual)
train$GarageCond[train$GarageCond == "Po"] <- "Fa"
train$GarageCond[train$GarageCond == "Gd"] <- "TA"
train$GarageCond[is.na(train$GarageCond)] <- "Fa"
table(train$GarageCond)
boxplot(train$SalePrice~train$GarageCond)
table(train$MoSold)
boxplot(train$SalePrice~train$MoSold)
train$season <- ifelse(train$MoSold %in% c(12, 1, 2), "winter", 
                       ifelse(train$MoSold %in% c(3, 4, 5), "spring", 
                              ifelse(train$MoSold %in% c(6, 7, 8), "summer", 
                                     ifelse(train$MoSold %in% c(9, 10, 11), "fall", NA))))
table(train$SaleCondition)
boxplot(train$SalePrice~train$SaleCondition)
train$SaleCondition[train$SaleCondition == "Family"] <- "Normal"
train$SaleCondition[train$SaleCondition == "Abnorml"] <- 1
train$SaleCondition[train$SaleCondition == "Normal"] <- 0
train$SaleCondition = as.integer(train$SaleCondition)
train$season <- as.factor(train$season)
boxplot(train$Age)
boxplot(train$X1stFlrSF~train$Functional)
train$Sold <- 2010 - train$YrSold

reg3 <- lm(log(SalePrice)~log(LotArea) + LotConfig + Zone + I(OverallQual^2)
           + I(OverallCond^0.25) + I(Age^0.25) + Sold + BsmtSF
           + BsmtCond + I(X1stFlrSF^0.25) + I(LivAreaSF ^ 0.25) + GarageFinish + Sold + I(Sold^2)+ Functional
           + GarageArea + I(SaleCondition*AgeRemod) + BsmtExposure + I(AgeRemod^0.25), data = train)
summary(reg3)

install.packages("corrplot")
library(car)
test <- read.csv("test.csv", header = TRUE)
test <- as.data.frame(test)
test$Zone[test$Zone == "C"] <- "RH"
test$Zone[test$Zone == "RH"] <- "RM"
test$LotConfig[test$LotConfig == "Inside"] <- "Corner"
test$LotConfig[test$LotConfig == "FR2"] <- "Corner"
test$HouseStyle[test$HouseStyle == "1.5Sto"] <- "1Story"
test$HouseStyle[test$HouseStyle == "2.5Sto"] <- "2Story"
test$OverallQual[test$OverallQual == 3] <- 4
test$OverallQual[test$OverallQual == 9] <- 8
test$Age <- 2010 - test$YearBuilt
test$AgeRemod <- 2010 - test$YearRemodAdd
test$OverallCond[test$OverallCond == 2] <- 4
test$OverallCond[test$OverallCond == 3] <- 4
test$OverallCond[test$OverallCond == 9] <- 8
test$OverallCond[test$OverallCond == 6] <- 7
test$ExterQual[test$ExterQual == "Ex"] <- "Gd"
test$ExterQual[test$ExterQual == "Fa"] <- "TA"
test$ExterCond[test$ExterCond == "Ex"] <- "Gd"
test$ExterCond[test$ExterCond == "Fa"] <- "TA"
test$BsmtQual[test$BsmtQual == "Ex"] <- "Gd"
test$BsmtQual[test$BsmtQual == "Fa"] <- "TA"
test$BsmtQual[is.na(test$BsmtQual)] <- "No"
test$BsmtCond[is.na(test$BsmtCond)] <- "Fa"
test$BsmtCond[test$BsmtCond == "Po"] <- "Fa"
test$BsmtExposure[is.na(test$BsmtExposure)] <- "No"
test$BsmtExposure[test$BsmtExposure == "Gd"] <- "Av"
test$BsmtExposure[test$BsmtExposure == "Mn"] <- "No"
test$BsmtRating[test$BsmtRating == "BLQ"] <- "ALQ"
test$BsmtRating[test$BsmtRating == "LwQ"] <- "ALQ"
test$BsmtRating[test$BsmtRating == "Unf"] <- "ALQ"
test$BsmtRating[is.na(test$BsmtRating)] <- "ALQ"
test$FullBath[test$FullBath == 0] <- 1
test$FullBath[test$FullBath == 3] <- 2
test$HalfBath[test$HalfBath == 2] <- 1
test$KitchenQual[test$KitchenQual == "Ex"] <- "Gd"
test$KitchenQual[test$KitchenQual == "Fa"] <- "TA"
test$TotRms[test$TotRms == 3] <- 4
test$TotRms[test$TotRms == 10] <- 9
test$TotRms[test$TotRms == 12] <- 9
test$TotRms[test$TotRms == 4] <- 5
test$TotRms[test$TotRms == 5] <- 6
test$TotRms[test$TotRms == 9] <- 8
test$Functional[test$Functional == "Mod"] <- "Min"
test$Fireplaces[test$Fireplaces == 3] <- 1
test$Fireplaces[test$Fireplaces == 2] <- 1
test$GarageType[test$GarageType == "Other"] <- "Attchd"
test$GarageType[is.na(test$GarageType)] <- "No"
test$GarageType[test$GarageType == "No"] <- "Detchd"
test$GarageFinish[is.na(test$GarageFinish)] <- "Unf"
test$GarageFinish[test$GarageFinish == "RFn"] <- "Fin"
test$GarageQual[test$GarageQual == "Po"] <- "Fa"
test$GarageQual[test$GarageQual == "Gd"] <- "TA"
test$GarageQual[is.na(test$GarageQual)] <- "Fa"
test$GarageCond[test$GarageCond == "Po"] <- "Fa"
test$GarageCond[test$GarageCond == "Gd"] <- "TA"
test$GarageCond[is.na(test$GarageCond)] <- "Fa"
test$season <- ifelse(test$MoSold %in% c(12, 1, 2), "winter", 
                       ifelse(test$MoSold %in% c(3, 4, 5), "spring", 
                              ifelse(test$MoSold %in% c(6, 7, 8), "summer", 
                                     ifelse(test$MoSold %in% c(9, 10, 11), "fall", NA))))
test$SaleCondition[test$SaleCondition == "Family"] <- "Normal"
test$SaleCondition[test$SaleCondition == "Abnorml"] <- 1
test$SaleCondition[test$SaleCondition == "Normal"] <- 0
test$SaleCondition = as.integer(test$SaleCondition)
test$season <- as.factor(test$season)
test$Sold <- 2010 - test$YrSold

#Produce prediction results

predicted_test <- predict(reg3, newdata = test)
predicted_test <- exp(predicted_test)


#Binding the submission file
df_submit <- cbind(test[,"Id"], predicted_test)
sum(is.na(df_submit))
colnames(df_submit) <- c("Id", "Predicted")
rownames(df_submit) <- c()
write.csv(df_submit, file = "Submission.csv", quote = FALSE, row.names = FALSE)
