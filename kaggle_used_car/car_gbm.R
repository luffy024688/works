load("auto1.RData")
set.seed(42)
index <- 1:nrow(auto_train)
testindex <- sample(index, trunc(length(index)*30/100)) #trunc()取整數
trainset <- auto_train[-testindex,]
validationset <- auto_train[testindex,]
#===================================================================================
#install.packages("gbm")
#install.packages("cvAUC")
#install.packages("xgboost")
library(gbm)
library(cvAUC)
library(xgboost)
library(Matrix)

set.seed(173)
#2
gbmmodel <- gbm(price ~ year+notRepairedDamage+fuelType+gearbox+kilometer_f+brand, 
             distribution = "gaussian",
             data = trainset,
             n.trees = 5000,
             interaction.depth = 5,
             shrinkage = 0.01,
             bag.fraction = 0.5,
             train.fraction = 1.0,
             #cv.folds = 3,
             n.cores = NULL)  #will use all cores by default

print(gbmmodel)

#3
pred_result <- predict(gbmmodel,validationset, n.trees = 5000)

#4,
validationR2 = 1 - sum((validationset$price-pred_result)^2)/sum((mean(trainset$price)-validationset$price)^2) 

validationR2 = 1 - sum((validationset$sqrprice-pred_result)^2)/sum((mean(trainset$sqrprice)-validationset$sqrprice)^2) 

# 使用gbm函数建模
#distribution：損失函數的形式。分类问题一般选择bernoulli分布，而回归问题可以选择gaussian分布。根据因变量而定：如果因变量是2个分类变量，则distribution='bernoulli'（伯努利），如果自变量是因子，则bernoulli='multinomial'（多项式）
#shrinkage：學習速率。学习速率是越小越好，但是步子太小的话，步数就得增加，也就是训练的迭代次数需要加大才能使模型达到最优，这样训练所需时间和计算资源也相应加大了。经验法则是设置shrinkage参数在0.01-0.001之间，而n.trees参数在3000-10000之间。
#n.trees：树的数量，值越大，树越多，模型约精确，同时计算量越大  迭代次數
#cv.folds：交叉验证折叠程度。值越大，交叉程度越高？
#interaction.dep：交互深度
#n.minobsinnode：树叶的最小样本量。值越小，树越精细
#verbose：是否print明细
#如果报错：Error in object$var.levels[[i]] : 下标出界。有可能是train中某个变量的分类，在valid中没有样本
model <- gbm(cr2~.,data=trade,shrinkage=0.01,distribution='bernoulli',cv.folds=5,n.trees=5000,verbose=T,interaction.depth=3,n.minobsinnode=10)


#1
# 用交叉检验确定最佳迭代次数
# check performance using 5-fold cross-validation
best.iter <- gbm.perf(gbmmodel,method='OOB')
best.iter <- gbm.perf(gbmmodel,method='cv')
print(best.iter)

#method：重采样方法：boot，boot632，cv，repeatedcv，LOOCV，LGOCV； oob（仅适用于随机森林、...）




# plot the performance # plot variable influence  
summary(gbmmodel,n.trees=best.iter) # based on the estimated best number of trees  





#=============???
#1
train.mx <- sparse.model.matrix( price ~ year+notRepairedDamage+fuelType+gearbox+kilometer_f+brand, data = trainset)
test.mx <- sparse.model.matrix( price ~ year+notRepairedDamage+fuelType+gearbox+kilometer_f+brand, data = validationset)
dtrain <- xgb.DMatrix(train.mx, label = trainset[,"price"])
dtest <- xgb.DMatrix(test.mx, label = validationset[,"price"])

#2
train.gdbt <- xgb.train(params = list(eta = 0.3,
                                      max_depth = 5,
                                      subsample = 1,
                                      colsample_bytree = 0.5), 
                        data = dtrain, 
                        nrounds = 70, 
                        watchlist = list(train = dtrain, test = dtest))
#3
pred_result <- predict(train.gdbt,dtest)

#4,
validationR2 = 1 - sum((validationset$price-pred_result)^2)/sum((mean(trainset$price)-validationset$price)^2) 








library(dplyr)
auto_test$predict <- predict(gbmmodel, auto_test,n.trees=10) #bm???
plot(auto_test$year,auto_test$predict)
submit <- auto_test %>% select(id,predict)
write.csv(submit,"submit.csv",row.names = FALSE)



