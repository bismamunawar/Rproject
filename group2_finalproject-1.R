library(caret)
library(tidyverse)
library(class)
library(C50)
library(randomForest)
library(rpart)
library(gbm)
library(ipred)


house <- read_csv("house_train.csv")
View(house)
#for only numeric values
numeric<- read_csv("house_train.csv")



#na values: lot frontage, alley,masvnrtype,  masvnrArea, GarageYrBuilt, 
#fireplace QU, pool qc, fence, misc, bsmt cond, bsmt qual, bsmt exposure, bsmnt fin type 2, bsmnt fintype1
#garage finish, garage qual, garage cond

#where pool area is 0, pool qc is not there

#lot frontage missing:

medianfrontage <- median(house$LotFrontage[!is.na(house$LotFrontage)])
house$LotFrontage[which(is.na(house$LotFrontage))] <- medianfrontage

#alley, poolQC, fence, miscFeature, Id missing: had more than 90% of missing values so 
#removed columns

house<-subset(house, select = -c(Alley, PoolQC, Fence, MiscFeature, Id))

#GarageYrBuilt missing

house$GarageYrBlt[is.na(house$GarageYrBlt)] <- house$YearBuilt[is.na(house$GarageYrBlt)]

#fireplace QU, Bsmt Qual, GarageQual missing

house <- subset(house, select= -c( GarageQual,BsmtQual, FireplaceQu))

#GarageFinish missing

house <- subset(house, select= -c( GarageFinish))

#GarageType missing


house$GarageType <- ifelse(is.na(house$GarageType), 
                           'No Garage', house$GarageType)

#GarageCond missing

house$GarageCond <- ifelse(is.na(house$GarageCond), 
                   'No Garage', house$GarageCond)
#MasVnrType missing

house$MasVnrType <- ifelse(is.na(house$MasVnrType ), 
                           'No Masonry', house$MasVnrType )

#MasVnrArea mising

house <- subset(house, select= -c( MasVnrArea))

#bsmnt cond missing

house$BsmtCond <- ifelse(is.na(house$BsmtCond ), 
                           'No Basement', house$BsmtCond)

#bsmnt Qual and bsmnt exposure

house <- subset(house, select= -c(BsmtExposure, BsmtQual ))

#bsmntfin type 1

house$BsmtFinType1 <- ifelse(is.na(house$BsmtFinType1 ), 
                         'No Basement', house$BsmtFinType1)

#bsmntfin type 2


house$BsmtFinType2 <- ifelse(is.na(house$BsmtFinType2 ), 
                             'No Basement', house$BsmtFinType2)




names(house)
names(house)[40]<-"FirstFloorArea"
names(house)[41]<-"SecondFloorArea"
names(house)[63]<-"SsnPorch"

#feature engg

house$TotBathrooms <- house$FullBath + (house$HalfBath*0.5) + house$BsmtFullBath +
                    (house$BsmtHalfBath*0.5)

house$Remod <- ifelse(house$YearBuilt==house$YearRemodAdd, 0, 1) #0=No Remodeling, 1=Remodeling

house$IsNew <- ifelse(house$YrSold==house$YearBuilt, 1, 0)

house$TotalSqFeet <- house$GrLivArea + house$TotalBsmtSF

house$TotalPorchSF <- house$OpenPorchSF + house$EnclosedPorch + house$SsnPorch + house$ScreenPorch


#changing char to factors
house$MSZoning<- as.factor(house$MSZoning)
house$Street <- as.factor(house$Street)
house$LotShape <-as.factor(house$LotShape )
house$LandContour<-as.factor(house$LandContour)
house$Utilities<-as.factor(house$Utilities)
house$LotConfig<-as.factor(house$LotConfig)
house$LandSlope<-as.factor(house$LandSlope)
house$Neighborhood<-as.factor(house$Neighborhood)
house$Condition1<-as.factor(house$Condition1)
house$Condition2<-as.factor(house$Condition2)
house$BldgType<-as.factor(house$BldgType)
house$HouseStyle<-as.factor(house$HouseStyle)
house$RoofStyle<-as.factor(house$RoofStyle)
house$RoofMatl<-as.factor(house$RoofMatl)
house$Exterior1st<-as.factor(house$Exterior1st)
house$Exterior2nd<-as.factor(house$Exterior2nd)
house$ExterQual<-as.factor(house$ExterQual)
house$ExterCond<-as.factor(house$ExterCond)
house$Foundation<-as.factor(house$Foundation)
house$Heating<-as.factor(house$Heating)
house$HeatingQC<-as.factor(house$HeatingQC)
house$CentralAir<-as.factor(house$CentralAir)
house$KitchenQual<-as.factor(house$KitchenQual)
house$Functional<-as.factor(house$Functional)
house$PavedDrive<-as.factor(house$PavedDrive)
house$SaleType<-as.factor(house$SaleType)
house$SaleCondition<-as.factor(house$SaleCondition)
house$MasVnrType <- as.factor(house$MasVnrType)
house$BsmtQual <- as.factor(house$BsmtQual)
house$BsmtCond <- as.factor(house$BsmtCond)
house$BsmtExposure <- as.factor(house$BsmtExposure)
house$BsmtFinType1 <- as.factor(house$BsmtFinType1)
house$BsmtFinType2<- as.factor(house$BsmtFinType2)
house$GarageFinish<- as.factor(house$GarageFinish)
house$GarageType<- as.factor(house$GarageType)
house$FireplaceQu<- as.factor(house$FireplaceQu)
house$GarageCond<- as.factor(house$GarageCond)


#removing all unnecessary columns
house <- subset(house, select= -c(LotShape, LandContour, LotConfig, 
                     LandSlope, RoofStyle, ExterQual, GarageCars,
OverallQual,  HeatingQC, Electrical, KitchenQual,YrSold, MoSold, Condition1, Condition2))


numeric <- subset(numeric, select = -c(Alley, PoolQC, Fence, MiscFeature, Id, OverallQual, OverallCond, HeatingQC, Electrical, KitchenQual,  GarageQual,YrSold, MoSold,
                                       PavedDrive,SaleType,SaleCondition,MasVnrType,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,
                                       BsmtFinType2,GarageFinish,GarageType,FireplaceQu,
                                       ExterQual, ExterCond, Foundation, Heating, HeatingQC, CentralAir,KitchenQual, Functional,
                                       MSZoning,Street,LotShape,LandContour,Utilities,LotConfig,LandSlope,Neighborhood,Condition1,
                                       Condition2,GarageCond,BldgType,HouseStyle,RoofStyle,RoofMatl,Exterior1st, Exterior2nd ))


#Visualize correlation
cormat<-signif(cor(numeric),2)
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(cormat, col=col, symm=TRUE,Rowv=NA, Colv = NA )


#see correlation
cor(house$SalePrice , house$LotArea)
cor(house$SalePrice , house$LotFrontage)
cor(house$SalePrice , house$TotalPorchSF)
cor(house$SalePrice , house$TotalSqFeet)
cor(house$SalePrice , house$MiscVal)
cor(house$SalePrice , house$SsnPorch)
cor(house$SalePrice , house$EnclosedPorch)
cor(house$SalePrice , house$OpenPorchSF)
cor(house$SalePrice , house$ScreenPorch)
cor(house$SalePrice , house$WoodDeckSF)
cor(house$SalePrice , house$PavedDrive)
cor(house$SalePrice , house$MasVnrArea)
cor(house$SalePrice , house$BsmtFinSF2)
cor(house$SalePrice , house$BsmtUnfSF)
cor(house$SalePrice , house$GrLivArea)
cor(house$SalePrice , house$PoolArea)
cor(house$SalePrice , house$MiscVal)
cor(house$SalePrice , house$LowQualFinSF)
cor(house$SalePrice , house$FirstFloorArea)
cor(house$SalePrice , house$SecondFloorArea)

#deleting variables that arent correlated
house <- subset(house, select= -c(LowQualFinSF, MiscVal, BsmtFinSF2, EnclosedPorch))

summary(house)

#modelling 

set.seed(201998)
trainRows <- createDataPartition(house$SalePrice, p = 0.8, list = F)
house.training <- house[trainRows,]
house.test <- house[-trainRows,]

#Variables needed to predict

#MSSubclass, MSZoning, LotFrontage, LotArea, Street, Utilities, Neighborhood
#BldgType, OverallCond, RoofMat1, Exterior1st, Exterior2nd,MasVnrType,
#ExterCond,Foundation, BsmntCond, TotalBsmtSF, Heating, CentralAir, 
#1stFlrSF, #2ndFlrSF, GrLibArea, TotBath, Bedroom, Kitchen
#TotRmsAbvGrd, Functional, Fireplaces, GarageType, GarageArea,
#GarageCond, TotalPorch, PoolArea, 

#Random Forest


model <- randomForest(SalePrice ~ Foundation +FirstFloorArea + SecondFloorArea +CentralAir
           + TotalBsmtSF + MSSubClass + MSZoning + ExterCond + TotalBsmtSF
      + LotFrontage + LotArea + Street + Utilities + Neighborhood + BldgType + OverallCond
       +  Exterior1st+ Exterior2nd + MasVnrType +  GrLivArea + TotBathrooms 
      + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Functional + Fireplaces +
        GarageType + GarageArea + GarageCond + TotalPorchSF + PoolArea + IsNew , 
      data = house.training)



house_rf_tunned_pred_test <- predict(model, house.test)
RMSE(house_rf_tunned_pred_test, house.test$SalePrice)

#25727.94

# Boosted Reg Tree ----

boost <- gbm(SalePrice ~ Foundation +FirstFloorArea + SecondFloorArea +CentralAir
             + Heating + TotalBsmtSF + MSSubClass + MSZoning
             + LotFrontage + LotArea + Street + Utilities + Neighborhood + BldgType + OverallCond
             +  Exterior1st+ Exterior2nd + MasVnrType +  GrLivArea + TotBathrooms 
             + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Functional + Fireplaces +
               GarageType + GarageArea + GarageCond + TotalPorchSF + PoolArea + IsNew 
             , data  = house.training, distribution = "gaussian", n.trees = 1500, 
             interaction.depth = 1)
summary(boost)
gbm.perf(boost)
p <- predict(boost, data = house.test, n.trees = 1500)
RMSE(predict(boost, data = house.test, n.trees = 1500), house.test$SalePrice)

#106830.1

#no decision trees or logistic, or knn, or svm coz classification


#ols

m <- lm(SalePrice ~ Foundation +FirstFloorArea + SecondFloorArea +CentralAir
        + TotalBsmtSF + MSSubClass + MSZoning + Heating +Exterior1st 
        + Exterior2nd + LotFrontage + LotArea + Street  + Neighborhood + BldgType + OverallCond
        +   GrLivArea + TotBathrooms + RoofMatl + MasVnrType
        + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Functional + Fireplaces
        + GarageType+ GarageCond+ GarageArea  + TotalPorchSF + PoolArea + IsNew 
        , data = house.training)
RMSE(predict(m,house.test), house.test$SalePrice)

#28114.03



#bagged tree

# bag <- bagging(factor(SalePrice) ~ Foundation +FirstFloorArea + SecondFloorArea +CentralAir
#                       + Heating + TotalBsmtSF + MSSubClass + MSZoning
#                       + LotFrontage + LotArea + Street + Utilities + Neighborhood + BldgType + OverallCond
#                       +  Exterior1st+ Exterior2nd + MasVnrType +  GrLivArea + TotBathrooms 
#                       + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Functional + Fireplaces +
#                         GarageType + GarageArea + GarageCond + TotalPorchSF + PoolArea + IsNew 
#                       , data = house.training)
# 
# # out of sample prediction
# credit.pred <- predict(credit.bag, credit.test)
# confusionMatrix(credit.pred, factor(credit.test$default))


#stacking

house.test$OLS <- predict(m, house.test)
house.test$RF <- predict(model, house.test)
house.test$Boost <-  predict(boost, house.test, n.trees = 1500)
stack <- lm(SalePrice~ OLS+RF+Boost, data = house.test)
summary(stack)

house.test$OLS <- predict(m, house.test)
RMSE(house.test$OLS, house.test$SalePrice)

house.test$RF <- predict(model, house.test)
RMSE(house.test$RF, house.test$SalePrice)

house.test$Boost <- predict(boost, house.test,n.trees = 1500)
RMSE(house.test$Boost, house.test$SalePrice)

RMSE(predict(stack, house.test), house.test$SalePrice)




## Submission ----

true <- read.csv("truth.csv")
View(true)
true<- true[-c(170),]
true<- true[-c( 149), ]
true<- true[-c( 121), ]
true<- true[-c( 98), ]
true<- true[-c( 75), ]
true<- true[-c(48),]

summary(true)
#no need for factors coz they are already factors

#handling missing values

#lot frontage missing:

medianfron <- median(true$LotFrontage[!is.na(true$LotFrontage)])
true$LotFrontage[which(is.na(true$LotFrontage))] <- medianfron

#alley, poolQC, fence, miscFeature, Id missing: had more than 90% of missing values so 
#removed columns

true<-subset(true, select = -c(Alley, PoolQC, Fence, MiscFeature))

#GarageYrBuilt missing

true$GarageYrBlt[is.na(true$GarageYrBlt)] <- true$YearBuilt[is.na(true$GarageYrBlt)]

#fireplace QU, Bsmt Qual, GarageQual missing

true <- subset(true, select= -c( GarageQual,BsmtQual, FireplaceQu))

#GarageFinish missing

true <- subset(true, select= -c( GarageFinish))


#MasVnrArea mising

true<- subset(true, select= -c( MasVnrArea))

#bsmnt cond missing

true$BsmtCond <- ifelse(is.na(true$BsmtCond ), 
                         'No Basement ', true$BsmtCond)

#bsmnt Qual and bsmnt exposure

true <- subset(true, select= -c(BsmtExposure ))

#bsmntfin type 1

true$BsmtFinType1 <- ifelse(is.na(true$BsmtFinType1 ), 
                             'No Basement', true$BsmtFinType1)

#bsmntfin type 2


true$BsmtFinType2 <- ifelse(is.na(true$BsmtFinType2 ), 
                             'No Basement', true$BsmtFinType2)

names(true)
names(true)[1]<-"Id"
names(true)[40]<-"FirstFloorArea"
names(true)[41]<-"SecondFloorArea"
names(true)[63]<-"SsnPorch"
true <- true[complete.cases(true$GarageType), ]
true <- true[complete.cases(true$GarageCond), ]
#feature engg

true$TotBathrooms <- true$FullBath + (true$HalfBath*0.5) + true$BsmtFullBath +
  (true$BsmtHalfBath*0.5)

true$Remod <- ifelse(true$YearBuilt==true$YearRemodAdd, 0, 1) #0=No Remodeling, 1=Remodeling

true$IsNew <- ifelse(true$YrSold==true$YearBuilt, 1, 0)

true$TotalSqFeet <- true$GrLivArea + true$TotalBsmtSF

true$TotalPorchSF <- true$OpenPorchSF + true$EnclosedPorch + true$SsnPorch + true$ScreenPorch


#changing char to factors


true$MSSubClass  <- as.numeric(true$MSSubClass )
true$LotArea  <- as.numeric(true$LotArea )
true$OverallCond <- as.numeric(true$OverallCond)
true$YearBuilt  <- as.numeric(true$YearBuilt )
true$YearRemodAdd <- as.numeric(true$YearRemodAdd)
true$BsmtFinSF1  <- as.numeric(true$BsmtFinSF1 )
true$BsmtFinSF2  <- as.numeric(true$BsmtFinSF2 )
true$BsmtUnfSF <- as.numeric(true$BsmtUnfSF)
true$TotalBsmtSF <- as.numeric(true$TotalBsmtSF)
true$ FirstFloorArea  <- as.numeric(true$ FirstFloorArea )
true$SecondFloorArea  <- as.numeric(true$SecondFloorArea )
true$LowQualFinSF <- as.numeric(true$LowQualFinSF)
true$GrLivArea <- as.numeric(true$GrLivArea)
true$ BsmtFullBath  <- as.numeric(true$BsmtFullBath  )
true$BsmtHalfBath  <- as.numeric(true$BsmtHalfBath )
true$FullBath  <- as.numeric(true$FullBath )
true$HalfBath  <- as.numeric(true$HalfBath  )
true$BedroomAbvGr  <- as.numeric(true$BedroomAbvGr  )
true$KitchenAbvGr  <- as.numeric(true$KitchenAbvGr  )
true$TotRmsAbvGrd  <- as.numeric(true$ TotRmsAbvGrd )
true$GarageYrBlt  <- as.numeric(true$GarageYrBlt )
true$ GarageCars  <- as.numeric(true$GarageCars  )
true$GarageArea  <- as.numeric(true$GarageArea  )
true$TotalPorchSF  <- as.numeric(true$TotalPorchSF )

#deleting unnecessary values

true <- subset(true, select= -c(LotShape, LandContour, LotConfig, 
                                  LandSlope, RoofStyle, ExterQual, GarageCars,
                                  OverallQual,  HeatingQC, Electrical, KitchenQual,YrSold, MoSold, Condition1, Condition2))


#deleting variables that arent correlated
true <- subset(true, select= -c(LowQualFinSF, MiscVal, BsmtFinSF2, EnclosedPorch))


summary(true)
true$OLS <- predict(m, true)
true$Boost <- predict(boost, true, n.trees = 1500)

levels(true$Foundation) <- levels(house$Foundation)
levels(true$FirstFloorArea) <- levels(house$FirstFloorArea)
levels(true$SecondFloorArea) <- levels(house$SecondFloorArea)
levels(true$CentralAir) <- levels(house$CentralAir)

levels(true$TotalBsmtSF) <- levels(house$TotalBsmtSF)
levels(true$MSSubClass) <- levels(house$MSSubClass)
levels(true$MSZoning) <- levels(house$MSZoning)
levels(true$LotFrontage ) <- levels(house$LotFrontage )
levels(true$LotArea) <- levels(house$LotArea)
levels(true$Street) <- levels(house$Street)
levels(true$Utilities) <- levels(house$Utilities)
levels(true$Neighborhood) <- levels(house$Neighborhood)
levels(true$BldgType) <- levels(house$BldgType)
levels(true$OverallCond) <- levels(house$OverallCond)
levels(true$Exterior1st) <- levels(house$Exterior1st)

levels(true$IsNew ) <- levels(house$IsNew )
levels(true$PoolArea) <- levels(house$PoolArea)
levels(true$ TotalPorchSF) <- levels(house$ TotalPorchSF)
levels(true$GarageCond) <- levels(house$GarageCond)
levels(true$GarageArea) <- levels(house$GarageArea)
levels(true$ GarageType) <- levels(house$ GarageType)

levels(true$Fireplaces) <- levels(house$Fireplaces)
levels(true$Functional ) <- levels(house$Functional )
levels(true$TotRmsAbvGrd) <- levels(house$TotRmsAbvGrd)
levels(true$KitchenAbvGr) <- levels(house$KitchenAbvGr)
levels(true$BedroomAbvGr) <- levels(house$BedroomAbvGr)
levels(true$TotBathrooms) <- levels(house$TotBathrooms)
levels(true$GrLivArea) <- levels(house$GrLivArea)
levels(true$MasVnrType) <- levels(house$MasVnrType)
levels(true$Exterior2nd) <- levels(house$Exterior2nd)
levels(true$ExterCond) <- levels(house$ExterCond)

true$RF <- predict(model, true)
View(true)

true <- true[complete.cases(true$OLS), ]
true <- true[complete.cases(true$RF), ]


stack_pred <- predict(stack, true)


true$PredSalePrice <- stack_pred
summary(true)


RMSE(true$PredSalePrice, true$SalePrice)

Submission <- NULL
Submission$Id <- true$Id
Submission$Prediction <- stack_pred

