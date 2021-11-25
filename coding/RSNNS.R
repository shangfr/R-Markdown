#������������ 
library(RSNNS) 

data(iris) 
#������˳����� 
head(iris )
iris = iris[sample(nrow(iris),nrow(iris)),1:ncol(iris)]
#������������ 
irisValues= iris[,1:4]
#��������������������ݽ��и�ʽת�� 
irisTargets = decodeClassLabels(iris[,5])
#���л��ֳ�ѵ�������ͼ������� 
iris = splitForTrainingAndTest(irisValues, irisTargets, ratio=0.15)
#���ݱ�׼�� 
iris = normTrainingAndTestSet(iris)

#����mlp����ִ��ǰ�����򴫲��������㷨 
model = mlp(iris$inputsTrain, iris$targetsTrain, size=5, learnFunc="Quickprop", learnFuncParams=c(0.1, 2.0, 0.0001, 0.1),maxit=100, inputsTest=iris$inputsTest, targetsTest=iris$targetsTest) 
mod2 = mlp(iris$inputsTrain, iris$targetsTrain, size=5, learnFunc="Quickprop", learnFuncParams=c(0.1, 2.0, 0.0001, 0.1),maxit=100, inputsTest=iris$inputsTest, targetsTest=iris$targetsTest,linOut=T) 

mod3<-mlp(iris$inputsTrain, iris$targetsTrain, size=10,linOut=T)

plotIterativeError(model) 
library("reshape")
plot.nnet(mod3)
par(mar=numeric(4),family='serif')

plot.nnet(mod2)
#�������潨����ģ�ͽ���Ԥ�� 
predictions = predict(model,iris$inputsTest)

plotRegressionError(iris$targetsTrain, model$fitted.values) 
plotRegressionError(iris$targetsTest, model$fittedTestValues)
hist(model$fitted.values - iris$targetsTrain)
#���ɻ������󣬹۲�Ԥ�⾫�� 

confusionMatrix(iris$targetsTest,predictions)


plot(irisValues, type = "l") 
plot(irisTargets[1:100], type = "l") 
lines(model$fitted.values[1:100], col = "green")


dd=read.csv(file.choose())

head(as.numeric(dd))