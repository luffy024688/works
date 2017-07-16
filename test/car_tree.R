load("auto1.RData")
set.seed(42)
index <- 1:nrow(auto_train)
testindex <- sample(index, trunc(length(index)*30/100)) #trunc()取整數
trainset <- auto_train[-testindex,]
validationset <- auto_train[testindex,]
#===================================================================================
#decision tree
library(rpart)
library(mboost)#??
library(maptree)
#2_0.model
dfmodel=rpart(price ~ year+notRepairedDamage+fuelType+gearbox+kilometer_f+brand ,method='anova',data=auto_train)
dfmodel
draw.tree(dfmodel)
par(mfrow=c(1,2)); rsq.rpart(dfmodel)#图示不同分类的误差



#1.+tune
plotcp(dfmodel)#调用CP与xerror的相关图，一种方法是寻找最小xerror点所对应的CP值，并由此CP值决定树的大小，另一种方法是利用1SE方法，寻找xerror+SE的最小点对应的CP值。
printcp(dfmodel)

#2_1.用prune命令对树模型进行修剪
pdfmodel=prune(dfmodel,cp= dfmodel$cptable[which.min(dfmodel$cptable[,"xerror"]),"CP"])  #0.44      0.53  0.54
pdfmodel=prune(dfmodel,cp= 0.01)   #0.55
pdfmodel=prune(dfmodel,cp= 0.011217   )
pdfmodel=prune(dfmodel,cp=0.015121   )



minsplit：每一個node最少要幾個data
minbucket：在末端的node上最少要幾個data
cp(complexity parameter): 決定精度的參數
maxdepth：Tree的深度

#3.predict
pred_result <- predict(dfmodel,validationset)
pred_result <- predict(pdfmodel,validationset)

#4
validationR2 = 1 - sum((validationset$price-pred_result)^2)/sum((mean(trainset$price)-validationset$price)^2) 

#===========================================================random forest
require(party)
require(randomForest)
#2.
rfmodel <- randomForest(price ~ year+notRepairedDamage+ kilometer_f, data = auto_train)
rfmodel <- randomForest(price ~ year+notRepairedDamage+fuelType+gearbox+kilometer_f+brand, data = auto_train)  #0.67
rfmodel
# See Importance of each predictor.
importance(rfmodel,type = 2)

varImpPlot(rfmodel,type=2)#varImpPlot(rfmodel)

3.
pred_result <- predict(rfmodel,validationset)

#4
validationR2 = 1 - sum((validationset$price-pred_result)^2)/sum((mean(trainset$price)-validationset$price)^2) 



stud_math.rf=randomForest(G3~.,data=stud_math,importance=TRUE,proximity=TRUE,ntree=500,subset=stud_math.train, na.action = na.fail)
#第一個參數(G3~.):表示除了G3屬性之外，其他屬性皆為模型之引數(因為我們要預測G3呀~)
#第二個參數(data=stud_math):表示模型中含有變數的一組資料
#第三個參數(importance=TRUE):是否計算每個模型中各屬性之重要值，資料型態為布林
#第四個參數(proximity=TRUE):是否計算模型的鄰近矩陣，此參數搭配函數MDSplot()使用，資料型態為布林
#第五個參數(ntree=500):表示森林中的樹木數量
#第六個參數(subset=stud_math.train):表示選出的訓練集資料為第幾筆(此參數的資料格式為向量)
#第七個參數(na.action = na.fail):表示遺漏值之處理，na.fail表示不能出現遺漏值


#然而，在隨機森林中有兩個重要的參數，
#ntree指定随机森林所包含的决策树数目，默认为500；
#mtry指定节点中用于二叉树的变量个数，默认情况下数据集变量个数的二次方根（分类模型）或三分之一（预测模型）。一般是需要进行人为的逐次挑选，确定最佳的m值；
#mtry参数是随机森林建模中，构造决策树分支时随机抽样的变量个数；选择合适的mtry值可以降低随机森林模型的预测误差

#1.決定ntree
ntree_fit<-randomForest(price ~ year+notRepairedDamage+fuelType+gearbox+kilometer_f+brand, data = auto_train,mtry=2,ntree=1000)
plot(ntree_fit)
#1.決定mtry
for (i in 1:5){
  test_model <- randomForest(price ~ year+notRepairedDamage+fuelType+gearbox+kilometer_f+brand, data = auto_train,mtry=i,ntry=200)
  mse <- mean(test_model$mse)
  print(mse)
}


#2.
rfmodel <- randomForest(price ~ year+notRepairedDamage+fuelType+gearbox+kilometer_f+brand, data = auto_train,mtry=2,ntree=200)  #0.67
rfmodel <- randomForest(price ~ year+notRepairedDamage+fuelType+gearbox+kilometer_f+brand, data = auto_train,mtry=3,ntree=150)  #0.67
rfmodel
plot(rfmodel)
importance(rfmodel,type = 2)
varImpPlot(rfmodel,type=2)#varImpPlot(rfmodel)
#3.
pred_result <- predict(rfmodel,validationset)

#4
validationR2 = 1 - sum((validationset$price-pred_result)^2)/sum((mean(trainset$price)-validationset$price)^2) 

#===================================================================







str(auto_test)
str(auto_train)
class(validationset$bm)
class(auto_test$bm)
mode(validationset$bm)
mode(auto_test$bm)




# 法一
auto_test[which(auto_test$brand=="lada"),"brand"]<- "toyota"
# 法二
auto_test$brand<-factor(auto_test$brand, levels=levels(auto_train$brand))
auto_test$bm<-factor(auto_test$bm, levels=levels(auto_train$bm))
sauto_test$fuelType<-factor(auto_test$fuelType, levels=levels(auto_train$fuelType))
#法三  auto_train  auto_test合起來再分  ???
library(plyr)
auto <- rbind.fill(auto_train,auto_test)
auto_train<- auto[1:4999,]
auto_test <- auto[5000:9999,]







library(dplyr)
auto_test$predict <- predict(rfmodel, auto_test) #bm???
plot(auto_test$year,auto_test$predict)
submit <- auto_test %>% select(id,predict)
write.csv(submit,"submit.csv",row.names = FALSE)




