install.packages("TeachingDemos")
library(dplyr)
library(TeachingDemos)
library(ggplot2)


auto_train <- read.csv("car_trainset.csv")
auto_test <- read.csv("car_testset.csv")
write.csv(auto2_train,"auto_train.csv", fileEncoding = "big5")
write.csv(auto2_train,"auto_train.csv")
#auto2_train <- read.csv("car_trainset.csv",encoding='iso-8859-1')
colnames(auto_train)
summary(auto_train)
str(auto_train)





#id
length(unique(auto_train$id))
#name
View(table(auto_train$name))

#private
#seller

#abtest  分類

g <- ggplot(type_info,aes(x=type_id,y=unique_count))
g + geom_bar(stat = "identity")




#dateCrawled
auto_train$dateCrawled <- substring( auto_train$dateCrawled,1,10)
View(table(auto_train$dateCrawled))
auto_test$dateCrawled <- substring( auto_test$dateCrawled,1,10)
View(table(auto_test$dateCrawled))
#year:dateCrawled,yearOfRegistration
auto_train$year <- 2016 - auto_train$yearOfRegistration
auto_test$year <- 2016 - auto_test$yearOfRegistration
with(auto_train,plot(year,price,type = "p"))

#dateCreated
auto_train$dateCreated <- substring( auto_train$dateCreated,1,10)
View(table(auto_train$dateCreated))
auto_test$dateCreated <- substring( auto_test$dateCreated,1,10)
View(table(auto_test$dateCreated))
#lastSeen
auto_train$lastSeen <- substring( auto_train$lastSeen,1,10)
auto_test$lastSeen <- substring( auto_test$lastSeen,1,10)
#ad_exist_time
with(auto_train,plot(ad_exist_time,price))

#postalCode
auto_train$postalCode <- as.factor(auto_train$postalCode)
auto_test$postalCode <- as.factor(auto_test$postalCode)


with(auto_train,plot(gearbox,price,type = "h"))  #with讓後面不用加$
#powerPS
with(auto_train,plot(powerPS,price,type = "p"))
auto_train1 =auto_train[auto_train$powerPS<=500,]  #powerPS is not a good explanatory.
with(auto_train1,plot(powerPS,price,type = "p"))

#kilometer
with(auto_train,plot(kilometer,price,type = "p"))  #kilometer is not a good explanatory.
auto_train$kilometer_f <- as.factor(auto_train$kilometer)
auto_test$kilometer_f <- as.factor(auto_test$kilometer)

#monthOfRegistration
with(auto_train,plot(monthOfRegistration,price))
auto_train$monthOfRegistration<- as.factor(auto_train$monthOfRegistration)
auto_test$monthOfRegistration<- as.factor(auto_test$monthOfRegistration)


#vehicleType
with(auto_train,plot(vehicleType,price,type = "h"))

#auto_train$gearbox 手牌多
View(table(auto_train$gearbox))
with(auto_train,plot(gearbox,price,type = "h"))
View(table(auto_test$gearbox))




#brand -lada??? ～改成同類的brand 不然用成虛擬變數後會被歸類成基準類
k1 <- auto_train %>% group_by(brand) %>% summarise(count=n()) %>% arrange(desc(count))
k2 <- auto_test %>% group_by(brand) %>% summarise(count=n()) %>% arrange(desc(count))
k2$brand[k2$brand %in% k1$brand==FALSE]
k1$brand %in% k2$brand

#brand X model
k1<-as.data.frame(table(auto_train$brand,auto_train$model)) %>%filter(Freq!=0)
k2 <- auto_train %>% group_by(brand,model) %>% summarise(count=n()) %>% arrange(desc(count))
View(count(auto_train,vehicleType,model))
View(tapply(auto_train$vehicleType,list(auto_train$model), length))

auto_train$bm <- as.factor(paste0(auto_train$brand,"_",auto_train$model))
k1 <- auto_train %>% group_by(bm) %>% summarise(count=n()) %>% arrange(desc(count))
auto_test$bm <- as.factor(paste0(auto_test$brand,"_",auto_test$model))
k2 <- auto_test %>% group_by(bm) %>% summarise(count=n()) %>% arrange(desc(count))


#auto_train$notRepairedDamage
View(table(auto_train$notRepairedDamage))
with(auto_train,plot(notRepairedDamage,price)) 
View(table(auto_test$notRepairedDamage))

#auto_train$fuelType  刪elektro
View(table(auto_train$fuelType))
with(auto_train,plot(fuelType,price))
View(table(auto_test$fuelType))

#===
##plot
#price distribution (transformation)
hist(auto_train$price)#左偏
hist(log(auto_train$price))
hist(log10(auto_train$price))
hist(auto_train$price^2)
hist(sqrt(auto_train$price))
hist(1/auto_train$price)#左偏
hist(1/sqrt(auto_train$price))
#box-cox transformation
#1.MASS包的boxcox函数 这个函数是针对线性模型计算一个最优的\lambda ，采取的方法是最大似然估计。
library(MASS)
boxcox(model)  #lmmodel
with(auto_train,boxcox(price ~ year+notRepairedDamage+fuelType+gearbox+kilometer_f+brand))  
boxcox(model, lambda = seq(0, 1, 0.1))
auto_train$boxprice <- auto_train$price^0.03
NewModel <- lm( boxprice ~ year,data=auto_train )
NewModel$residuals

library(car)
qq.plot(NewModel$residuals, dist= "norm", col=palette()[1], ylab="Residual Quantiles", main="Normal Probability Plot", pch=19)
 
 
#relationship between price and xxx
boxCoxVariable(auto_train)
vis.boxcox(auto_train$price)

summary(auto_test)
str(auto_test)







#
auto_train <- auto_train %>% select(-nrOfPictures,-dateCrawled,-yearOfRegistration,-seller,-offerType)
auto_test <- auto_test %>% select(-nrOfPictures,-dateCrawled,-yearOfRegistration,-seller,-offerType)
auto_train <- auto_train %>% filter(fuelType !="elektro")
#lada
#kilo
save(auto_test,auto_train,file="auto1.RData")
#====================================================================================
library(plyr)
load("auto1.RData")
set.seed(42)
index <- 1:nrow(auto_train)
testindex <- sample(index, trunc(length(index)*30/100)) #trunc()取整數
trainset <- auto_train[-testindex,]
validationset <- auto_train[testindex,]
#=====================================================SVM
#====線性：lm
with(auto_train,plot(year,price))
# step2: lm model
model <- lm(price ~ year, auto_train)
abline(model)  #線
step(model)
model1 = lm()
summary(step(model))



# step3:predict值
predictedY <- predict(model, auto_train)
points(auto_train$year, predictedY, col = "blue", pch=4)  #點 # display the predictions

predictedY <- predict(NewModel, auto_train)

#step4:score
summary(model)
rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- model$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)  ;predictionRMSE #2274
testR2_svm = 1 - sum((auto_train$price-predictedY)^2)/sum((mean(auto_train$price)-auto_train$price)^2)#0.26




#===非線性：svm_svr
library(e1071) 

#step1:tune-用預設
#step2:model
model_svm <- svm(price ~ year , auto_train)
#step3:predict值
predictedY <- predict(model_svm, auto_train)
points(auto_train$year, predictedY, col = "red", pch=4)
#step4:score
error <- auto_train$year - predictedY
svrPredictionRMSE <- rmse(error)  ;svrPredictionRMSE#3798
summary(model_svm)
testR2_svm = 1 - sum((auto_train$price-predictedY)^2)/sum((mean(auto_train$price)-auto_train$price)^2)
#========== +tune
#step1:tune
tuneResult <- tune(svm, price ~ year,  data = auto_train,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))#trained different 168 models
print(tuneResult) 
plot(tuneResult)# Draw the tuning graph   C between 0 and 100 and ϵϵ between 0.4 and 0.6 have less error.
View(tuneResult$performances)

#step2:model 做model 所以要找出最適合此model trainset 的參數  所以要去tune參數(step 1)
#如果step 1 tune放的trainset1 和step 2 的trainset2不一樣  就是拿trainset1最適參數當trainset2最適參數  剛剛就是tune心酸的 

#法一:type="eps"
model_svm_best1 <- svm(price ~ year ,auto_train,kernel="radial", cost=4, epsilon=0.5)
#法二
model_svm_best2 <- tuneResult$best.model
summary(model_svm_best1)


#step3:predict值 
predictedY_b <- predict(model_svm_best1, auto_train)
points(auto_train$year, predictedY_b, col = "green", pch=4)
#step4:score
error <- auto_train$year - predictedY
tunedModelRMSE <- rmse(error)  # 3916
testR2_svm_b = 1 - sum((auto_train$price-predictedY_b)^2)/sum((mean(auto_train$price)-auto_train$price)^2)#0.48






#========slice之後score
set.seed(42)
index <- 1:nrow(auto_train)
testindex <- sample(index, trunc(length(index)*30/100)) #trunc()取整數
trainset <- auto_train[-testindex,]
validationset <- auto_train[testindex,]

#==進階slice
library(dplyr)
a <- auto_train %>% group_by(brand) %>% summarise(count=n()) %>% filter(count<22) %>% select(brand)
a <- as.character(unlist(a));a
fid = which(auto_train$brand %in% a)
temp1 = auto_train[fid,]
b <- c(1:5000)
<- b[b %in% fid==FALSE]


testindex <- sample(index, trunc(length(index)*30/100)) #trunc()取整數
trainset <- auto_train[-testindex,]
validationset <- auto_train[testindex,]




##lm
#2:model
lmmodel = lm(price~year,data = trainset)
step(lmmodel)
summary(step(lmmodel))
#3:predict
predictedY <- predict(lmmodel, validationset)
plot(validationset$year,validationset$price)
points(validationset$year, predictedY, col = "blue", pch=4)  #點 # display the predictions
#step4:score
testR2_svm = 1 - sum((validationset$price-predictedY)^2)/sum((mean(validationset$price)-validationset$price)^2)#0.23




##lm多元回歸
table(trainset$fuelType) 
lmmodel_m = lm(price~year + notRepairedDamage+kilometer,data = trainset)
step(lmmodel_m)
summary(step(lmmodel_m))#可能overfit 的分數
#3:predict
predictedY <- predict(lmmodel_m, validationset)
plot(validationset$year,validationset$price)
points(validationset$year, predictedY, col = "blue", pch=4)  #點 # display the predictions
#step4:score
testR2 = 1 - sum((validationset$price-predictedY)^2)/sum((mean(trainset$price)-validationset$price)^2)#0.23







##svr
library(e1071)
#1
tuneResult <- tune(svm, price ~ year+notRepairedDamage+fuelType+brand+gearbox+kilometer_f,  data = auto_train,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))#trained different 168 models
print(tuneResult) 
plot(tuneResult)# Draw the tuning graph   C between 0 and 100 and ϵϵ between 0.4 and 0.6 have less error.

#2  
names(auto_train)
svrmodel = tuneResult$best.model
summary(svrmodel)
svrmodel_2 <- svm(price ~ year+notRepairedDamage , trainset,kernel="radial", cost=4, epsilon=0.5)#0.49
svrmodel_3 <- svm(price ~ year+notRepairedDamage+fuelType+brand , trainset,kernel="radial", cost=16, epsilon=0.5)#0.62
svrmodel_4 <- svm(price ~ year+notRepairedDamage+fuelType+brand+gearbox , trainset,kernel="radial", cost=8, epsilon=0.5)#0.63
svrmodel_5 <- svm(price ~ year+notRepairedDamage+fuelType+brand+gearbox+kilometer_f , trainset,kernel="radial", cost=64, epsilon=0.4)#0.68
svrmodel_5.5 <- svm(price ~ year+notRepairedDamage+fuelType+brand+gearbox+kilometer_f , auto_train,kernel="radial", cost=128, epsilon=0.4)#
svrmodel_6 <- svm(price ~ year+notRepairedDamage+fuelType+brand+gearbox+kilometer_f+bm , trainset,kernel="radial", cost=512, epsilon=0.4)#0.73
svrmodel_7 <- svm(price ~ year+notRepairedDamage+bm , trainset,kernel="radial", cost=512, epsilon=0.3)#0.70
svrmodel_8 <- svm(price ~ year+notRepairedDamage+bm+fuelType ,trainset,kernel="radial", cost=512, epsilon=0.3)#0.70
svrmodel_9 <- svm(price ~ year+notRepairedDamage+bm+ kilometer_f ,trainset,kernel="radial", cost=512, epsilon=0.3)#0.73
svrmodel_10 <- svm(price ~ year+notRepairedDamage+bm+ kilometer_f+brand , trainset,kernel="radial", cost=512, epsilon=0.4)#0.72


tuneResult <- tune(svm, price ~  year+notRepairedDamage+fuelType+brand+gearbox+kilometer_f,  data = auto,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))#trained different 168 models
svrmodel_6 <- svm(price  ~ year + notRepairedDamage+bm+kilometer+brand , auto_train,kernel="radial", cost=4, epsilon=1)#0.73
svrmodel_6 <- svm(price  ~ year + notRepairedDamage+bm+kilometer+brand , auto,kernel="radial", cost=4, epsilon=1)#0.73


#3  這裡的validationset 不能含在剛剛model的trainset內  不然可能會overfitting   因為在train model時本來就考慮了validationset，表現當然會不錯  
#所以才要slice!!!!

pred_result = predict(svrmodel_5.5, validationset)



#4
validationR2 = 1 - sum((validationset$price-pred_result)^2)/sum((mean(trainset$price)-validationset$price)^2) 





library(reshape2)#melt
library(ggplot2)
mydata <- with(auto_train,auto_train[, c("price", "year","powerPS","kilometer" , "ad_exist_time"  )]);head(mydata)
mydata$kilometer<- as.numeric(as.character(mydata$kilometer))


##1 原始圖
#1.Compute the correlation 'matrix'
cormat <- round(cor(mydata),2);head(cormat)  
#2.melt  'data.frame'
melted_cormat <- melt(cormat);head(melted_cormat)
#3.heatmap  
ggplot( melt(cormat), aes(x=X1, y=X2, fill=value)) + 
  geom_tile()


quantile_range <- quantile(cormat, probs = seq(0, 1, 0.2))
label_text <- rollapply(round(quantile_range, 2), width = 2, by = 1, FUN = function(i) paste(i, collapse = " : "))
color_palette <- colorRampPalette(c("#3794bf", "#FFFFFF", "#df8640"))(length(quantile_range) - 1)
theme_change <- theme(
  plot.background = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.border = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
)

cormat_2 <- matrix(findInterval(cormat, quantile_range, all.inside = TRUE), nrow = nrow(cormat))
melted_cormat <- melt(cormat_2);head(melted_cormat)
ggplot( melt(cormat_2), aes(x=X1, y=X2, fill=factor(value))) + 
  geom_tile(color="black")+
  scale_fill_manual(values = color_palette, name = "", labels = label_text)+
  theme_change


#--標準化
set.seed(1)
x <- runif(7)
(x - mean(x)) / sd(x)
scale(x)


#===================================================================
auto_train$sqrprice <- sqrt(auto_train$price)



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
auto <- rbind.fill(auto_train,auto_test)
auto_train<- auto[1:4999,]
auto_test <- auto[5000:9999,]










auto_test$predict <- predict(rfmodel, auto_test) #bm???
plot(auto_test$year,auto_test$predict)
submit <- auto_test %>% select(id,predict)
write.csv(submit,"submit.csv",row.names = FALSE)




