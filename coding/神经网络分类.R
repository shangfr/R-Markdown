library(RSNNS)

test_data =read.csv(file.choose(),head=F)

x=test_data[,-1] #ÊäÈëinputs
y=test_data[,1]   #Ä¿±êtargets

# a matrix containing the training&test-inputs&targets
test_data = splitForTrainingAndTest(x, y, ratio = 0.15)


model = mlp(iris$inputsTrain
           , iris$targetsTrain
           , size=2
           , learnFunc="Quickprop"
           , learnFuncParams=c(0.1, 2.0, 0.0001, 0.1)
           , maxit=100
           , inputsTest=iris$inputsTest
           , targetsTest=iris$targetsTest) 

