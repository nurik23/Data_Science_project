install.packages("dplyr")
install.packages("xgboost")
install.packages("missForest")
install.packages("caret")

library(dplyr)
library(xgboost)
library(missForest)
library(caret)

trainHouse<-read.csv("~/Desktop/Datamaining/train.csv")
testHouse<-read.csv("~/Desktop/Datamaining/test.csv")


cols <- sapply(trainHouse, function(x){sum(is.na(x)/nrow(trainHouse))})<=0.4
trainHouse <- trainHouse[,cols]
cols_test<- sapply(testHouse, function(x){sum(is.na(x)/nrow(testHouse))})<=0.4
testHouse <- testHouse[,cols_test]

miss<- missForest(trainHouse)
trainHouse <- miss$ximp
misst<- missForest(testHouse)
testHouse <- misst$ximp
sum(is.na(trainHouse))
sum(is.na(testHouse))

trainHouse <- trainHouse %>% mutate_if(is.factor, as.numeric)
trainHouse <- trainHouse %>% mutate_if(is.integer, as.numeric)
testHouse <- testHouse %>% mutate_if(is.factor, as.numeric)
testHouse <- testHouse %>% mutate_if(is.integer, as.numeric)

model=lm(SalePrice~.,trainHouse)
step(model,direction = "backward")
model <- lm(formula = SalePrice ~ MSSubClass + LotFrontage + LotArea + 
              Street + LotShape + LandContour + LandSlope + Neighborhood + 
              Condition2 + BldgType + HouseStyle + OverallQual + OverallCond + 
              YearBuilt + RoofStyle + RoofMatl + Exterior1st + MasVnrType + 
              MasVnrArea + ExterQual + BsmtQual + BsmtCond + BsmtExposure + 
              BsmtFinType1 + BsmtFinSF1 + X1stFlrSF + X2ndFlrSF + LowQualFinSF + 
              BsmtFullBath + FullBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + 
              TotRmsAbvGrd + Functional + Fireplaces + GarageYrBlt + GarageCars + 
              GarageCond + WoodDeckSF + ScreenPorch + YrSold + SaleCondition, 
            data = trainHouse)
#summary(model)
predicted <- predict(model, testHouse)

dataFrame <- data.frame(Id = testHouse$Id, SalePrice = predicted)
write.csv(dataFrame, "housePrice1.csv", quote = F, row.names = F)
# Ñëó÷àéíûé Ëåñ
modelRF<- randomForest(SalePrice ~ .,trainHouse,type = "regression",ntree =500,do.trace=TRUE)
predictedRF <- predict(modelRF, testHouse)
dataFrame<- data.frame(Id = testHouse$Id, SalePrice = predictedRF)
write.csv(dataFrame, "HousePriceRF.csv", quote = F, row.names = F)


summary(model)







