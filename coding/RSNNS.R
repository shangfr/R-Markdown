#载入程序和数据 
library(RSNNS) 

data(iris) 
#将数据顺序打乱 
head(iris )
iris = iris[sample(nrow(iris),nrow(iris)),1:ncol(iris)]
#定义网络输入 
irisValues= iris[,1:4]
#定义网络输出，并将数据进行格式转换 
irisTargets = decodeClassLabels(iris[,5])
#从中划分出训练样本和检验样本 
iris = splitForTrainingAndTest(irisValues, irisTargets, ratio=0.15)
#数据标准化 
iris = normTrainingAndTestSet(iris)

#利用mlp命令执行前馈反向传播神经网络算法 
model = mlp(iris$inputsTrain, iris$targetsTrain, size=5, learnFunc="Quickprop", learnFuncParams=c(0.1, 2.0, 0.0001, 0.1),maxit=100, inputsTest=iris$inputsTest, targetsTest=iris$targetsTest) 
mod2 = mlp(iris$inputsTrain, iris$targetsTrain, size=5, learnFunc="Quickprop", learnFuncParams=c(0.1, 2.0, 0.0001, 0.1),maxit=100, inputsTest=iris$inputsTest, targetsTest=iris$targetsTest,linOut=T) 

mod3<-mlp(iris$inputsTrain, iris$targetsTrain, size=10,linOut=T)

plotIterativeError(model) 
library("reshape")
plot.nnet(mod3)
par(mar=numeric(4),family='serif')

plot.nnet(mod2)
#利用上面建立的模型进行预测 
predictions = predict(model,iris$inputsTest)

plotRegressionError(iris$targetsTrain, model$fitted.values) 
plotRegressionError(iris$targetsTest, model$fittedTestValues)
hist(model$fitted.values - iris$targetsTrain)
#生成混淆矩阵，观察预测精度 

confusionMatrix(iris$targetsTest,predictions)


plot(irisValues, type = "l") 
plot(irisTargets[1:100], type = "l") 
lines(model$fitted.values[1:100], col = "green")


dd=read.csv(file.choose())

head(as.numeric(dd))
