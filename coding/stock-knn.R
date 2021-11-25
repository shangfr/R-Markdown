knn基于训练样本和新数据的属性分类，knn不依赖于任何模型，只依赖于记忆。给出新的样本，找到在它周围k个样本中属于某类最多的样本，那么这些最多的样本的归类就是新样本的类别。步骤如下：
1.确定k值
2.计算新样本与所有训练样本的距离
3.排序计算出的距离
4.收集前k个最小距离和它们的类别
5.判断新样本的类别






library(class)
library(dplyr)
library(lubridate)
set.seed(100)

stocks <- read.csv(file.choose())



stocks$Date <- ymd(stocks$Date)
stocksTrain <- year(stocks$Date) < 2014

plot(stocks[,-1]) 
points(5,6,pch=8,cex=3) 

predictors <- cbind(lag(stocks$Apple, default = 210.73), 
                    lag(stocks$Google, default = 619.98),
                    lag(stocks$MSFT, default = 30.48))
colnames(predictors)=c("Apple","Google","MSFT")
plot(stocks[,c(-1,-5)], pch = 20)





train <- predictors[stocksTrain, ] #2014年以前的数据为训练数据
test <- predictors[!stocksTrain, ] #2014年以后的数据为测试数据
cl <- stocks$Increase[stocksTrain] #已知类别
colors <- 3-cl

par(mfrow=c(2,2))
plot(lag(stocks$Apple, default = 210.73), stocks$Apple, pch = ".")
plot(lag(stocks$Google, default = 619.98),stocks$Google, pch = ".",col=20)
plot(lag(stocks$MSFT, default = 30.48),stocks$MSFT, pch = ".",col=21)

par(mfrow=c(3,2))
acf(stocks$Apple)#查看自相关图
pacf(stocks$Apple)#查看偏相关图
acf(stocks$Google)
pacf(stocks$Google)
acf(stocks$MSFT)
pacf(stocks$MSFT)

scatterplot3d(train,col=colors,pch= ".",cex=1,
     xlim=c(0,max(predictors )),ylim=c(0,max(predictors ))) 
points(test,pch= ".") 

cl <- stocks$Increase #已知涨跌
colors <- 3-cl
scatterplot3d(predictors,color=colors, col.axis=5,
      col.grid="lightblue", main="scatterplot3d - 1", pch=20)
 
 

prediction <- knn(train, test, cl, k = 1)  #进行KNN算法分类  
   
table(prediction, stocks$Increase[!stocksTrain])
mean(prediction == stocks$Increase[!stocksTrain])

#通过蒙特卡洛模拟选出最好的k值
accuracy <- rep(0, 10)
k <- 1:10
for(x in k){
  prediction <- knn(predictors[stocksTrain, ], predictors[!stocksTrain, ],
                    stocks$Increase[stocksTrain], k = x)
  accuracy[x] <- mean(prediction == stocks$Increase[!stocksTrain])
}

plot(k, accuracy, type = 'b')


