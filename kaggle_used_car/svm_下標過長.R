load("auto1.RData")
set.seed(42)
index <- 1:nrow(auto_train)
testindex <- sample(index, trunc(length(index)*30/100)) #trunc()取整數
trainset <- auto_train[-testindex,]
validationset <- auto_train[testindex,]
#================================================


svrmodel_5 <- svm(price ~ year+notRepairedDamage+fuelType+brand+gearbox+kilometer_f , trainset,kernel="radial", cost=64, epsilon=0.4)#0.68
svrmodel_6 <- svm(price ~ year+notRepairedDamage+fuelType+brand+gearbox+kilometer_f+bm , trainset,kernel="radial", cost=512, epsilon=0.4)#0.73


pred_result = predict(svrmodel_5, validationset)
pred_result = predict(svrmodel_6, validationset)


validationR2 = 1 - sum((validationset$price-pred_result)^2)/sum((mean(trainset$price)-validationset$price)^2) 


auto_test2$predict <- predict(svrmodel_5, auto_test2) #bm???
auto_test2$predict <- predict(svrmodel_6, auto_test2) #bm???




a <- levels(trainset$bm);length(a)
a2<- levels(validationset$bm);length(a2)
b <- levels(auto_test$bm);length(b)

b[which(b %in%  a==F)]


#index
fid = which(auto_test$bm %in% auto_train$bm)
temp1 = auto_test[-fid,]
#
temp2 <- auto_test[!auto_test$bm %in% auto_train$bm,]


auto_test2 <- auto_test[auto_test$bm %in% auto_train$bm,]
auto_test2$bm<-factor(auto_test2$bm, levels=levels(trainset$bm))


